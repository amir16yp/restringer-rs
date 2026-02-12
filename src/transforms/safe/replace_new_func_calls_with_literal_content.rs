use oxc_allocator::{Allocator, Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

use crate::{Transform, TransformCtx};

pub struct ReplaceNewFuncCallsWithLiteralContent;

impl Transform for ReplaceNewFuncCallsWithLiteralContent {
    fn name(&self) -> &'static str {
        "replaceNewFuncCallsWithLiteralContent"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, source_type: ctx.source_type, modified: false };
        v.visit_program(program);
        v.modified
    }
}

enum Replacement<'a> {
    Expr(Expression<'a>),
    Stmt(Statement<'a>),
    Block(BlockStatement<'a>),
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

    fn is_target_new_function(&self, new_expr: &NewExpression<'a>) -> bool {
        let Expression::Identifier(id) = self.unwrap_parens(&new_expr.callee) else { return false; };
        id.name.as_str() == "Function"
    }

    fn new_function_code_arg(new_expr: &NewExpression<'a>) -> Option<&'a str> {
        if new_expr.arguments.len() != 1 {
            return None;
        }
        let arg0 = new_expr.arguments.first()?.as_expression()?;
        match arg0 {
            Expression::StringLiteral(s) => Some(s.value.as_str()),
            _ => None,
        }
    }

    fn parse_code_string_to_ast(&self, span: oxc_span::Span, code: &str) -> Option<Replacement<'a>> {
        // JS version returns a literal node for empty strings.
        // Here we represent it as a string literal expression.
        if code.is_empty() {
            let s = self.allocator.alloc_str(code);
            return Some(Replacement::Expr(Expression::StringLiteral(ArenaBox::new_in(
                StringLiteral {
                    span,
                    value: s.into(),
                    raw: None,
                    lone_surrogates: false,
                },
                self.allocator,
            ))));
        }

        // The string passed to `new Function(code)` is the function body.
        // Parse it in a function context so `return ...` is valid.
        let mut wrapped = String::with_capacity("(function(){})".len() + code.len());
        wrapped.push_str("(function(){");
        wrapped.push_str(code);
        wrapped.push_str("})");

        let temp_allocator = Allocator::default();
        let parse_ret = Parser::new(&temp_allocator, &wrapped, self.source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        if !parse_ret.errors.is_empty() {
            return None;
        }

        // Extract the synthetic function expression.
        let mut body: Option<&oxc_allocator::Vec<'_, Statement<'_>>> = None;
        for stmt in &parse_ret.program.body {
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::ParenthesizedExpression(p) = &es.expression {
                    if let Expression::FunctionExpression(func) = &p.expression {
                        if let Some(func_body) = func.body.as_ref() {
                            body = Some(&func_body.statements);
                            break;
                        }
                    }
                }
            }
        }

        let body = body?;
        if body.is_empty() {
            return None;
        }

        if body.len() > 1 {
            let mut out = ArenaVec::new_in(self.allocator);
            for s in body.iter() {
                out.push(s.clone_in(self.allocator));
            }
            return Some(Replacement::Block(BlockStatement { span, body: out, scope_id: Default::default() }));
        }

        let stmt = &body[0];
        match stmt {
            Statement::ExpressionStatement(es) => Some(Replacement::Expr(es.expression.clone_in(self.allocator))),
            Statement::ReturnStatement(rs) => {
                let arg = rs.argument.as_ref()?;
                Some(Replacement::Expr(arg.clone_in(self.allocator)))
            }
            _ => Some(Replacement::Stmt(stmt.clone_in(self.allocator))),
        }
    }

    fn maybe_replacement_for_call(&self, call: &CallExpression<'a>) -> Option<Replacement<'a>> {
        if !call.arguments.is_empty() {
            return None;
        }

        let Expression::NewExpression(new_expr) = self.unwrap_parens(&call.callee) else { return None; };
        if !self.is_target_new_function(new_expr) {
            return None;
        }

        let code = Self::new_function_code_arg(new_expr)?;
        self.parse_code_string_to_ast(call.span, code)
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_statement(&mut self, it: &mut Statement<'a>) {
        // Statement context: replace the whole ExpressionStatement when replacement is a Block.
        if let Statement::ExpressionStatement(expr_stmt) = it {
            if let Expression::CallExpression(call) = self.unwrap_parens(&expr_stmt.expression) {
                if let Some(rep) = self.maybe_replacement_for_call(call) {
                    match rep {
                        Replacement::Expr(expr) => {
                            expr_stmt.expression = expr;
                            self.modified = true;
                            return;
                        }
                        Replacement::Stmt(stmt) => {
                            *it = stmt;
                            self.modified = true;
                            return;
                        }
                        Replacement::Block(block) => {
                            *it = Statement::BlockStatement(ArenaBox::new_in(block, self.allocator));
                            self.modified = true;
                            return;
                        }
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_statement(self, it);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Expression context: only replace with expressions.
        if let Expression::CallExpression(call) = it {
            if let Some(Replacement::Expr(expr)) = self.maybe_replacement_for_call(call) {
                *it = expr;
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
