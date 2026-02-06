use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use crate::{Transform, TransformCtx};

pub struct ParseTemplateLiteralsIntoStringLiterals;

impl Transform for ParseTemplateLiteralsIntoStringLiterals {
    fn name(&self) -> &'static str {
        "parseTemplateLiteralsIntoStringLiterals"
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
    fn literal_to_string(&self, expr: &Expression<'a>) -> Option<String> {
        match expr {
            Expression::StringLiteral(lit) => Some(lit.value.as_str().to_string()),
            Expression::NumericLiteral(lit) => Some(lit.value.to_string()),
            Expression::BooleanLiteral(lit) => Some(lit.value.to_string()),
            Expression::NullLiteral(_) => Some("null".to_string()),
            Expression::BigIntLiteral(lit) => lit.raw.as_ref().map(|r| r.as_str().to_string()),
            _ => None,
        }
    }

    fn try_convert_template_literal(&self, tl: &TemplateLiteral<'a>) -> Option<String> {
        let mut out = String::new();

        // quasis is always len = expressions + 1
        for i in 0..tl.expressions.len() {
            let quasi = tl.quasis.get(i)?;
            out.push_str(quasi.value.raw.as_str());

            let expr = tl.expressions.get(i)?;
            out.push_str(&self.literal_to_string(expr)?);
        }

        let last_quasi = tl.quasis.last()?;
        out.push_str(last_quasi.value.raw.as_str());

        Some(out)
    }

    fn make_string_literal_expr(&self, span: Span, s: &str) -> Expression<'a> {
        let s = self.allocator.alloc_str(s);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral {
                span,
                value: s.into(),
                raw: None,
                lone_surrogates: false,
            },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::TemplateLiteral(tl) = it {
            if let Some(s) = self.try_convert_template_literal(tl) {
                *it = self.make_string_literal_expr(tl.span, &s);
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
