use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyModuleFactoryCall;

impl Transform for SimplifyModuleFactoryCall {
    fn name(&self) -> &'static str {
        "simplifyModuleFactoryCall"
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

    fn is_undefined_expr(&self, expr: &Expression<'a>) -> bool {
        match self.unwrap_parens(expr) {
            Expression::Identifier(id) => id.name.as_str() == "undefined",
            Expression::UnaryExpression(un) => {
                un.operator == UnaryOperator::Void && matches!(self.unwrap_parens(&un.argument), Expression::NumericLiteral(_))
            }
            _ => false,
        }
    }

    fn is_call_member_named<'b>(&self, callee: &'b Expression<'a>, name: &str) -> Option<&'b Expression<'a>> {
        match self.unwrap_parens(callee) {
            Expression::StaticMemberExpression(m) => {
                if m.property.name.as_str() == name {
                    Some(&m.object)
                } else {
                    None
                }
            }
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(s) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                if s.value.as_str() == name {
                    Some(&m.object)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn should_simplify_call(&self, call: &CallExpression<'a>) -> bool {
        // Match `factory.call(thisArg, module, exports, require)` and rewrite to `factory(module, exports, require)`.
        // Conservative guard: only allow thisArg that is definitely safe to drop.
        // We accept null / undefined only.
        let Some(_factory_expr) = self.is_call_member_named(&call.callee, "call") else {
            return false;
        };
        if call.arguments.len() < 2 {
            return false;
        }
        let Some(this_arg) = call.arguments[0].as_expression() else {
            return false;
        };
        matches!(self.unwrap_parens(this_arg), Expression::NullLiteral(_)) || self.is_undefined_expr(this_arg)
    }

    fn rewrite_call(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let factory_expr = self.is_call_member_named(&call.callee, "call")?;

        let mut args = ArenaVec::new_in(self.allocator);
        for a in call.arguments.iter().skip(1) {
            args.push(a.clone_in(self.allocator));
        }

        Some(Expression::CallExpression(ArenaBox::new_in(
            CallExpression {
                span: call.span,
                callee: factory_expr.clone_in(self.allocator),
                type_arguments: None,
                arguments: args,
                optional: false,
                pure: call.pure,
            },
            self.allocator,
        )))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if self.should_simplify_call(call) {
                if let Some(repl) = self.rewrite_call(call) {
                    *it = repl;
                    self.modified = true;
                    return;
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
