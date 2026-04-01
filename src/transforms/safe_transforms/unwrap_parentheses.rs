use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct UnwrapParentheses;

impl Transform for UnwrapParentheses {
    fn name(&self) -> &'static str {
        "unwrapParentheses"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = Visitor { allocator: ctx.allocator, modified: false };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::ParenthesizedExpression(paren) = expr {
            *expr = paren.expression.clone_in(self.allocator);
            self.modified = true;
        }
    }
}
