use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use crate::{Transform, TransformCtx};

pub struct SimplifyJsFuckBooleans;

impl Transform for SimplifyJsFuckBooleans {
    fn name(&self) -> &'static str {
        "simplifyJsFuckBooleans"
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

    fn is_empty_array_literal(&self, expr: &Expression<'a>) -> bool {
        let Expression::ArrayExpression(arr) = self.unwrap_parens(expr) else {
            return false;
        };
        arr.elements.is_empty()
    }

    fn make_bool(&self, span: oxc_span::Span, value: bool) -> Expression<'a> {
        Expression::BooleanLiteral(ArenaBox::new_in(BooleanLiteral { span, value }, self.allocator))
    }

    fn try_rewrite(&self, expr: &Expression<'a>) -> Option<bool> {
        // Match:
        //   ![]  => false
        //   !![] => true
        // Only for the empty array literal `[]` to avoid side effects.
        let Expression::UnaryExpression(un1) = self.unwrap_parens(expr) else {
            return None;
        };
        if un1.operator != oxc_syntax::operator::UnaryOperator::LogicalNot {
            return None;
        }

        let arg1 = self.unwrap_parens(&un1.argument);
        if self.is_empty_array_literal(arg1) {
            return Some(false);
        }

        let Expression::UnaryExpression(un2) = arg1 else {
            return None;
        };
        if un2.operator != oxc_syntax::operator::UnaryOperator::LogicalNot {
            return None;
        }
        let arg2 = self.unwrap_parens(&un2.argument);
        if self.is_empty_array_literal(arg2) {
            return Some(true);
        }

        None
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Some(val) = self.try_rewrite(it) {
            let span = it.span();
            *it = self.make_bool(span, val);
            self.modified = true;
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
