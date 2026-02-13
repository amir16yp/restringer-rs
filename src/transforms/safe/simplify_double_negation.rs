use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyDoubleNegation;

impl Transform for SimplifyDoubleNegation {
    fn name(&self) -> &'static str {
        "simplifyDoubleNegation"
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

    fn unwrap_parens_mut<'b>(&self, mut expr: &'b mut Expression<'a>) -> &'b mut Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &mut p.expression,
                _ => return expr,
            }
        }
    }

    fn is_logical_not<'b>(&self, expr: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
        let Expression::UnaryExpression(un) = self.unwrap_parens(expr) else {
            return None;
        };
        if un.operator != oxc_syntax::operator::UnaryOperator::LogicalNot {
            return None;
        }
        Some(&un.argument)
    }

    fn simplify_test_expr(&mut self, test: &mut Expression<'a>) {
        // Only simplify in boolean test contexts.
        // `!!x` -> `x` (for any x) as the test will coerce to boolean anyway.
        let Expression::UnaryExpression(outer) = self.unwrap_parens_mut(test) else {
            return;
        };
        if outer.operator != oxc_syntax::operator::UnaryOperator::LogicalNot {
            return;
        }
        let Expression::UnaryExpression(inner) = self.unwrap_parens_mut(&mut outer.argument) else {
            return;
        };
        if inner.operator != oxc_syntax::operator::UnaryOperator::LogicalNot {
            return;
        }

        let repl = inner.argument.clone_in(self.allocator);
        *test = repl;
        self.modified = true;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_if_statement(&mut self, it: &mut IfStatement<'a>) {
        self.simplify_test_expr(&mut it.test);
        oxc_ast_visit::walk_mut::walk_if_statement(self, it);
    }

    fn visit_while_statement(&mut self, it: &mut WhileStatement<'a>) {
        self.simplify_test_expr(&mut it.test);
        oxc_ast_visit::walk_mut::walk_while_statement(self, it);
    }

    fn visit_do_while_statement(&mut self, it: &mut DoWhileStatement<'a>) {
        self.simplify_test_expr(&mut it.test);
        oxc_ast_visit::walk_mut::walk_do_while_statement(self, it);
    }

    fn visit_for_statement(&mut self, it: &mut ForStatement<'a>) {
        if let Some(test) = it.test.as_mut() {
            self.simplify_test_expr(test);
        }
        oxc_ast_visit::walk_mut::walk_for_statement(self, it);
    }

    fn visit_conditional_expression(&mut self, it: &mut ConditionalExpression<'a>) {
        self.simplify_test_expr(&mut it.test);
        oxc_ast_visit::walk_mut::walk_conditional_expression(self, it);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Avoid simplifying `!!x` in general expression contexts.
        let _ = self.unwrap_parens(it);
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
