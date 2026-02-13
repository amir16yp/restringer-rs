use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::{GetSpan, Span};

use crate::{Transform, TransformCtx};

pub struct NormalizeVoid0;

impl Transform for NormalizeVoid0 {
    fn name(&self) -> &'static str {
        "normalizeVoid0"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = BindingCollector { undefined_bound: false };
        collector.visit_program(program);
        if collector.undefined_bound {
            return false;
        }

        let mut v = Rewriter { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct BindingCollector {
    undefined_bound: bool,
}

impl<'a> VisitMut<'a> for BindingCollector {
    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        if it.name.as_str() == "undefined" {
            self.undefined_bound = true;
        }
    }
}

struct Rewriter<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Rewriter<'a> {
    fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &p.expression,
                _ => return expr,
            }
        }
    }

    fn make_undefined_ident(&self, span: Span) -> Expression<'a> {
        let name = self.allocator.alloc_str("undefined");
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }

    fn is_void0(&self, expr: &Expression<'a>) -> bool {
        let Expression::UnaryExpression(un) = self.unwrap_parens(expr) else {
            return false;
        };
        if un.operator != oxc_syntax::operator::UnaryOperator::Void {
            return false;
        }
        matches!(self.unwrap_parens(&un.argument), Expression::NumericLiteral(n) if n.value == 0.0)
    }
}

impl<'a> VisitMut<'a> for Rewriter<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if self.is_void0(it) {
            let span = it.span();
            *it = self.make_undefined_ident(span);
            self.modified = true;
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
