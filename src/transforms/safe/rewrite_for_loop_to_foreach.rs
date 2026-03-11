use oxc_allocator::{Allocator, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

use crate::{Transform, TransformCtx};

pub struct RewriteForLoopToForEach;

impl Transform for RewriteForLoopToForEach {
    fn name(&self) -> &'static str {
        "rewriteForLoopToForEach"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, source_type: ctx.source_type, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    source_type: SourceType,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &p.expression,
                _ => return expr,
            }
        }
    }

    fn is_i_ident(&self, expr: &Expression<'a>, i_name: &str) -> bool {
        matches!(self.unwrap_parens(expr), Expression::Identifier(id) if id.name.as_str() == i_name)
    }

    fn match_for_header(&self, for_stmt: &ForStatement<'a>) -> Option<(String, String)> {
        let Some(init) = for_stmt.init.as_ref() else { return None };
        let i_name = match init {
            ForStatementInit::VariableDeclaration(v) => {
                if v.declarations.len() != 1 {
                    return None;
                }
                let decl = &v.declarations[0];
                let BindingPattern::BindingIdentifier(id) = &decl.id else { return None };
                let Some(init_expr) = decl.init.as_ref() else { return None };
                if !matches!(self.unwrap_parens(init_expr), Expression::NumericLiteral(n) if n.value == 0.0) {
                    return None;
                }
                id.name.as_str().to_string()
            }
            _ => {
                let expr = init.as_expression()?;
                let Expression::AssignmentExpression(assign) = self.unwrap_parens(expr) else { return None };
                if assign.operator != AssignmentOperator::Assign {
                    return None;
                }
                let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left else { return None };
                if !matches!(self.unwrap_parens(&assign.right), Expression::NumericLiteral(n) if n.value == 0.0) {
                    return None;
                }
                id.name.as_str().to_string()
            }
        };

        let Some(test) = for_stmt.test.as_ref() else { return None };
        let Expression::BinaryExpression(bin) = self.unwrap_parens(test) else { return None };
        if bin.operator != BinaryOperator::LessThan {
            return None;
        }

        if !self.is_i_ident(&bin.left, &i_name) {
            return None;
        }

        let Expression::StaticMemberExpression(mem) = self.unwrap_parens(&bin.right) else { return None };
        if mem.property.name.as_str() != "length" {
            return None;
        }
        let Expression::Identifier(arr_id) = self.unwrap_parens(&mem.object) else { return None };
        let arr_name = arr_id.name.as_str().to_string();

        let Some(update) = for_stmt.update.as_ref() else { return None };
        let update = self.unwrap_parens(update);
        match update {
            Expression::UpdateExpression(up) => {
                if up.operator != UpdateOperator::Increment || up.prefix {
                    return None;
                }
                let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &up.argument else {
                    return None;
                };
                if id.name.as_str() != i_name {
                    return None;
                }
            }
            Expression::AssignmentExpression(assign) => {
                if assign.operator != AssignmentOperator::Addition {
                    return None;
                }
                let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left else { return None };
                if id.name.as_str() != i_name {
                    return None;
                }
                if !matches!(self.unwrap_parens(&assign.right), Expression::NumericLiteral(n) if n.value == 1.0) {
                    return None;
                }
            }
            _ => return None,
        }

        Some((i_name, arr_name))
    }

    fn arr_index_expr<'b>(&self, expr: &'b Expression<'a>, arr_name: &str, i_name: &str) -> Option<&'b ComputedMemberExpression<'a>> {
        let Expression::ComputedMemberExpression(mem) = self.unwrap_parens(expr) else { return None };
        if !matches!(self.unwrap_parens(&mem.object), Expression::Identifier(id) if id.name.as_str() == arr_name) {
            return None;
        }
        if !matches!(self.unwrap_parens(&mem.expression), Expression::Identifier(id) if id.name.as_str() == i_name) {
            return None;
        }
        Some(mem)
    }

    fn extract_nested_assignment_value<'b>(&self, expr: &'b Expression<'a>) -> &'b Expression<'a> {
        if let Expression::AssignmentExpression(assign) = self.unwrap_parens(expr) {
            if assign.operator == AssignmentOperator::Assign {
                return self.extract_nested_assignment_value(&assign.right);
            }
        }
        expr
    }

    fn match_loop_body(&self, body: &Statement<'a>, arr_name: &str, i_name: &str) -> Option<(String, String, String)> {
        let Statement::ExpressionStatement(es) = body else { return None };
        let Expression::LogicalExpression(log) = self.unwrap_parens(&es.expression) else { return None };
        if log.operator != LogicalOperator::Or {
            return None;
        }

        let Expression::CallExpression(cond_call) = self.unwrap_parens(&log.left) else { return None };
        let Expression::Identifier(cond_ident) = self.unwrap_parens(&cond_call.callee) else { return None };
        if cond_call.arguments.len() != 1 {
            return None;
        }
        let Some(cond_arg0) = cond_call.arguments[0].as_expression() else { return None };
        
        let cond_arg_unwrapped = self.extract_nested_assignment_value(cond_arg0);
        
        let source_obj_name = if let Some(mem) = self.arr_index_expr(cond_arg_unwrapped, arr_name, i_name) {
            None
        } else {
            if let Expression::ComputedMemberExpression(mem) = self.unwrap_parens(cond_arg_unwrapped) {
                if let Expression::Identifier(obj_id) = self.unwrap_parens(&mem.object) {
                    let inner_expr = self.extract_nested_assignment_value(&mem.expression);
                    if self.arr_index_expr(inner_expr, arr_name, i_name).is_some() {
                        Some(obj_id.name.as_str().to_string())
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        };

        let Expression::AssignmentExpression(assign) = self.unwrap_parens(&log.right) else { return None };
        if assign.operator != AssignmentOperator::Assign {
            return None;
        }

        let AssignmentTarget::ComputedMemberExpression(lhs_mem) = &assign.left else { return None };
        let Expression::Identifier(target_ident) = self.unwrap_parens(&lhs_mem.object) else { return None };
        
        let lhs_index_unwrapped = self.extract_nested_assignment_value(&lhs_mem.expression);
        
        let lhs_is_temp_var = if self.arr_index_expr(lhs_index_unwrapped, arr_name, i_name).is_some() {
            false
        } else {
            matches!(self.unwrap_parens(lhs_index_unwrapped), Expression::Identifier(_))
        };

        let rhs_unwrapped = self.extract_nested_assignment_value(&assign.right);
        if source_obj_name.is_some() {
            if !matches!(self.unwrap_parens(rhs_unwrapped), Expression::Identifier(_)) {
                return None;
            }
        } else {
            if !lhs_is_temp_var {
                let _ = self.arr_index_expr(rhs_unwrapped, arr_name, i_name)?;
            } else {
                if !matches!(self.unwrap_parens(rhs_unwrapped), Expression::Identifier(_)) {
                    return None;
                }
            }
        }

        Some((
            cond_ident.name.as_str().to_string(),
            target_ident.name.as_str().to_string(),
            source_obj_name.unwrap_or_else(|| arr_name.to_string()),
        ))
    }

    fn parse_replacement_stmt(
        &self,
        arr_name: &str,
        cond_name: &str,
        target_name: &str,
        source_name: &str,
    ) -> Option<Statement<'a>> {
        let mut code = String::new();
        code.push_str(arr_name);
        code.push_str(".forEach(item => { if (!");
        code.push_str(cond_name);
        code.push('(');
        code.push_str(source_name);
        code.push_str("[item])) ");
        code.push_str(target_name);
        code.push_str("[item] = ");
        code.push_str(source_name);
        code.push_str("[item]; });");

        let temp_allocator = Allocator::default();
        let parse_ret = Parser::new(&temp_allocator, &code, self.source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        if !parse_ret.errors.is_empty() {
            return None;
        }

        let stmt = parse_ret.program.body.first()?;
        Some(stmt.clone_in(self.allocator))
    }

    fn transform_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            if let Statement::ForStatement(for_stmt) = &stmt {
                if let Some((i_name, arr_name)) = self.match_for_header(for_stmt) {
                    if let Some((cond_name, target_name, source_name)) = self.match_loop_body(&for_stmt.body, &arr_name, &i_name) {
                        if let Some(replacement) = self.parse_replacement_stmt(&arr_name, &cond_name, &target_name, &source_name) {
                            out.push(replacement);
                            self.modified = true;
                            continue;
                        }
                    }
                }
            }
            out.push(stmt);
        }

        *stmts = out;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.transform_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
