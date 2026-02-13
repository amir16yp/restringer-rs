use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use crate::{Transform, TransformCtx};

pub struct ReplaceFunctionReturnThis;

impl Transform for ReplaceFunctionReturnThis {
    fn name(&self) -> &'static str {
        "replaceFunctionReturnThis"
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

    fn is_function_return_this(&self, call: &CallExpression<'a>) -> bool {
        if !call.arguments.is_empty() {
            return false;
        }

        let Expression::CallExpression(inner) = self.unwrap_parens(&call.callee) else {
            return false;
        };

        // inner must be Function("return this")
        if inner.arguments.len() != 1 {
            return false;
        }
        let Expression::Identifier(id) = self.unwrap_parens(&inner.callee) else {
            return false;
        };
        if id.name.as_str() != "Function" {
            return false;
        }

        let Some(arg0) = inner.arguments[0].as_expression() else {
            return false;
        };
        let Expression::StringLiteral(s) = self.unwrap_parens(arg0) else {
            return false;
        };
        s.value.as_str() == "return this"
    }

    fn make_global_this_expr(&self, span: Span) -> Expression<'a> {
        let name = self.allocator.alloc_str("globalThis");
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Replace `Function('return this')()` with `globalThis`.
        if let Expression::CallExpression(call) = it {
            if self.is_function_return_this(call) {
                let span = call.span;
                *it = self.make_global_this_expr(span);
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
