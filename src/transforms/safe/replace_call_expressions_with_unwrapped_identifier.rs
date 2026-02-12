use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ReplaceCallExpressionsWithUnwrappedIdentifier;

impl Transform for ReplaceCallExpressionsWithUnwrappedIdentifier {
    fn name(&self) -> &'static str {
        "replaceCallExpressionsWithUnwrappedIdentifier"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
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

    fn is_unwrappable(&self, expr: &Expression<'a>) -> bool {
        match self.unwrap_parens(expr) {
            Expression::Identifier(_) => true,
            Expression::CallExpression(call) => call.arguments.is_empty(),
            _ => false,
        }
    }

    fn return_expr_from_function_body<'b>(&self, body: &'b FunctionBody<'a>) -> Option<&'b Expression<'a>> {
        if body.statements.len() != 1 {
            return None;
        }
        match &body.statements[0] {
            Statement::ReturnStatement(ret) => {
                let arg = ret.argument.as_ref()?;
                if self.is_unwrappable(arg) { Some(arg) } else { None }
            }
            // OXC may represent expression-bodied arrow functions as a FunctionBody
            // with a single ExpressionStatement.
            Statement::ExpressionStatement(es) => {
                let expr = &es.expression;
                if self.is_unwrappable(expr) { Some(expr) } else { None }
            }
            _ => None,
        }
    }

    fn return_expr_from_function<'b>(&self, func: &'b Function<'a>) -> Option<&'b Expression<'a>> {
        let body = func.body.as_ref()?;
        self.return_expr_from_function_body(body)
    }

    fn return_expr_from_arrow<'b>(&self, arrow: &'b ArrowFunctionExpression<'a>) -> Option<&'b Expression<'a>> {
        // oxc represents arrow function bodies as a FunctionBody.
        self.return_expr_from_function_body(&arrow.body)
    }

    fn candidate_from_statement<'b>(&self, stmt: &'b Statement<'a>) -> Option<(&'b str, Expression<'a>)> {
        match stmt {
            Statement::FunctionDeclaration(func_decl) => {
                let func = &**func_decl;
                let id = func.id.as_ref()?;
                let ret = self.return_expr_from_function(func)?;
                Some((id.name.as_str(), ret.clone_in(self.allocator)))
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue; };
                    let Some(init) = decl.init.as_ref() else { continue; };

                    let ret_expr = match self.unwrap_parens(init) {
                        Expression::FunctionExpression(f) => self.return_expr_from_function(f),
                        Expression::ArrowFunctionExpression(a) => self.return_expr_from_arrow(a),
                        _ => None,
                    }?;

                    return Some((binding.name.as_str(), ret_expr.clone_in(self.allocator)));
                }
                None
            }
            _ => None,
        }
    }

    fn replace_calls_in_statement_list(&mut self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        // Find at most one candidate per pass (like other transforms here), then apply replacements.
        let mut candidate: Option<(String, Expression<'a>)> = None;
        for stmt in stmts.iter() {
            let Some((name, replacement)) = self.candidate_from_statement(stmt) else { continue; };
            candidate = Some((name.to_string(), replacement));
            break;
        }

        let Some((name, replacement)) = candidate else { return; };
        let mut replaced_any = false;
        for s in stmts.iter_mut() {
            if self.replace_calls_in_statement_in_place(&name, &replacement, s) {
                replaced_any = true;
            }
        }

        if replaced_any {
            self.modified = true;
        }
    }

    fn replace_calls_in_statement_in_place(&self, func_name: &str, replacement: &Expression<'a>, stmt: &mut Statement<'a>) -> bool {
        struct Replacer<'a, 'b> {
            allocator: &'a oxc_allocator::Allocator,
            func_name: &'b str,
            replacement: &'b Expression<'a>,
            replaced_any: bool,
        }

        impl<'a> Replacer<'a, '_> {
            fn unwrap_parens<'c>(&self, mut expr: &'c Expression<'a>) -> &'c Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &p.expression,
                        _ => return expr,
                    }
                }
            }
        }

        impl<'a> VisitMut<'a> for Replacer<'a, '_> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                if let Expression::CallExpression(call) = it {
                    if call.arguments.is_empty() {
                        if let Expression::Identifier(id) = self.unwrap_parens(&call.callee) {
                            if id.name.as_str() == self.func_name {
                                *it = self.replacement.clone_in(self.allocator);
                                self.replaced_any = true;
                                return;
                            }
                        }
                    }
                }
                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }
        }

        let mut r = Replacer { allocator: self.allocator, func_name, replacement, replaced_any: false };
        r.visit_statement(stmt);
        r.replaced_any
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.replace_calls_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.replace_calls_in_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.replace_calls_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
    }
}
