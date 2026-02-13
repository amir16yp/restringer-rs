use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use crate::{Transform, TransformCtx};

pub struct FoldStringConcatenation;

impl Transform for FoldStringConcatenation {
    fn name(&self) -> &'static str {
        "foldStringConcatenation"
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

    fn make_string_lit(&self, span: Span, value: &str) -> Expression<'a> {
        let s = self.allocator.alloc_str(value);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral { span, value: s.into(), raw: None, lone_surrogates: false },
            self.allocator,
        ))
    }

    fn try_fold_binary(&self, bin: &BinaryExpression<'a>) -> Option<Expression<'a>> {
        if bin.operator != BinaryOperator::Addition {
            return None;
        }

        let left = self.unwrap_parens(&bin.left);
        let right = self.unwrap_parens(&bin.right);

        let Expression::StringLiteral(l) = left else {
            return None;
        };
        let Expression::StringLiteral(r) = right else {
            return None;
        };

        let mut joined = String::with_capacity(l.value.len() + r.value.len());
        joined.push_str(l.value.as_str());
        joined.push_str(r.value.as_str());

        Some(self.make_string_lit(bin.span, &joined))
    }

    fn fold_recursively(&mut self, expr: &mut Expression<'a>) {
        // First, recurse into children so we can fold chains like "a" + "b" + "c".
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        let Expression::BinaryExpression(bin) = expr else {
            return;
        };

        if let Some(repl) = self.try_fold_binary(bin) {
            *expr = repl;
            self.modified = true;
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Override default order: do post-order folding.
        self.fold_recursively(it);
    }
}
