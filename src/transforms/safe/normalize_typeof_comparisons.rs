use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use crate::{Transform, TransformCtx};

pub struct NormalizeTypeofComparisons;

impl Transform for NormalizeTypeofComparisons {
    fn name(&self) -> &'static str {
        "normalizeTypeofComparisons"
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

    fn is_typeof_expr<'b>(&self, expr: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
        let Expression::UnaryExpression(un) = self.unwrap_parens(expr) else {
            return None;
        };
        if un.operator != oxc_syntax::operator::UnaryOperator::Typeof {
            return None;
        }
        Some(&un.argument)
    }

    fn is_string_literal<'b>(&self, expr: &'b Expression<'a>) -> Option<&'b StringLiteral<'a>> {
        let Expression::StringLiteral(s) = self.unwrap_parens(expr) else {
            return None;
        };
        Some(s)
    }

    fn make_typeof_expr(&self, span: Span, arg: Expression<'a>) -> Expression<'a> {
        Expression::UnaryExpression(ArenaBox::new_in(
            UnaryExpression { span, operator: oxc_syntax::operator::UnaryOperator::Typeof, argument: arg },
            self.allocator,
        ))
    }

    fn make_binary(&self, span: Span, op: BinaryOperator, left: Expression<'a>, right: Expression<'a>) -> Expression<'a> {
        Expression::BinaryExpression(ArenaBox::new_in(BinaryExpression { span, operator: op, left, right }, self.allocator))
    }

    fn normalize(&self, bin: &BinaryExpression<'a>) -> Option<Expression<'a>> {
        let op = bin.operator;
        let is_eq = matches!(op, BinaryOperator::Equality | BinaryOperator::StrictEquality);
        let is_neq = matches!(op, BinaryOperator::Inequality | BinaryOperator::StrictInequality);
        if !is_eq && !is_neq {
            return None;
        }

        // Match: "undefined" != typeof X
        // Normalize to: typeof X !== "undefined"
        let (lit, typeof_arg, strictness) = if let Some(lit) = self.is_string_literal(&bin.left) {
            let arg = self.is_typeof_expr(&bin.right)?;
            let strict = matches!(op, BinaryOperator::StrictEquality | BinaryOperator::StrictInequality);
            (lit, arg, strict)
        } else if let Some(lit) = self.is_string_literal(&bin.right) {
            let arg = self.is_typeof_expr(&bin.left)?;
            let strict = matches!(op, BinaryOperator::StrictEquality | BinaryOperator::StrictInequality);
            (lit, arg, strict)
        } else {
            return None;
        };

        // Only normalize the most common noisy cases.
        let v = lit.value.as_str();
        if v != "undefined" && v != "function" && v != "object" {
            return None;
        }

        let new_op = match (is_eq, strictness) {
            (true, true) => BinaryOperator::StrictEquality,
            (true, false) => BinaryOperator::StrictEquality,
            (false, true) => BinaryOperator::StrictInequality,
            (false, false) => BinaryOperator::StrictInequality,
        };

        let typeof_expr = self.make_typeof_expr(bin.span, typeof_arg.clone_in(self.allocator));
        let lit_expr = Expression::StringLiteral(ArenaBox::new_in(lit.clone_in(self.allocator), self.allocator));
        Some(self.make_binary(bin.span, new_op, typeof_expr, lit_expr))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::BinaryExpression(bin) = it {
            if let Some(new_expr) = self.normalize(bin) {
                *it = new_expr;
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
