use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyCalls;

impl Transform for SimplifyCalls {
    fn name(&self) -> &'static str {
        "simplifyCalls"
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

    fn method_name(&self, callee: &Expression<'a>) -> Option<&'static str> {
        match self.unwrap_parens(callee) {
            Expression::StaticMemberExpression(m) => {
                let name = m.property.name.as_str();
                if name == "call" {
                    Some("call")
                } else if name == "apply" {
                    Some("apply")
                } else {
                    None
                }
            }
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(s) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                let name = s.value.as_str();
                if name == "call" {
                    Some("call")
                } else if name == "apply" {
                    Some("apply")
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn member_object<'b>(&self, callee: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
        match self.unwrap_parens(callee) {
            Expression::StaticMemberExpression(m) => Some(&m.object),
            Expression::ComputedMemberExpression(m) => Some(&m.object),
            _ => None,
        }
    }

    fn allowed_context_arg(&self, call: &CallExpression<'a>) -> bool {
        let Some(first) = call.arguments.first() else { return false; };
        let Some(expr) = first.as_expression() else { return false; };
        match self.unwrap_parens(expr) {
            Expression::ThisExpression(_) => true,
            Expression::NullLiteral(_) => true,
            _ => false,
        }
    }

    fn callee_is_excluded(&self, object: &Expression<'a>) -> bool {
        match self.unwrap_parens(object) {
            Expression::Identifier(id) => id.name.as_str() == "Function",
            Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => true,
            _ => false,
        }
    }

    fn simplified_args(&self, call: &CallExpression<'a>, method: &str) -> ArenaVec<'a, Argument<'a>> {
        let mut args = ArenaVec::new_in(self.allocator);
        if method == "call" {
            for a in call.arguments.iter().skip(1) {
                args.push(a.clone_in(self.allocator));
            }
            return args;
        }

        // apply
        let Some(second) = call.arguments.get(1) else {
            return args;
        };
        let Some(expr) = second.as_expression() else {
            return args;
        };
        let Expression::ArrayExpression(arr) = self.unwrap_parens(expr) else {
            return args;
        };
        for el in &arr.elements {
            match el {
                ArrayExpressionElement::SpreadElement(s) => {
                    args.push(Argument::SpreadElement(s.clone_in(self.allocator)));
                }
                ArrayExpressionElement::Elision(_) => {}
                _ => {
                    if let Some(e) = el.as_expression() {
                        args.push(Argument::from(e.clone_in(self.allocator)));
                    }
                }
            }
        }
        args
    }

    fn make_call_expr(&self, span: oxc_span::Span, callee: Expression<'a>, arguments: ArenaVec<'a, Argument<'a>>) -> Expression<'a> {
        Expression::CallExpression(ArenaBox::new_in(
            CallExpression {
                span,
                callee,
                type_arguments: None,
                arguments,
                optional: false,
                pure: false,
            },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if let Some(method) = self.method_name(&call.callee) {
                if self.allowed_context_arg(call) {
                    // `f.call(this)` cannot be safely rewritten to `f()` because it changes the `this` binding.
                    // This breaks cases like `Function.toString.call(this)`.
                    if method == "call" && call.arguments.len() == 1 {
                        if let Some(arg0) = call.arguments.first().and_then(|a| a.as_expression()) {
                            if matches!(self.unwrap_parens(arg0), Expression::ThisExpression(_)) {
                                oxc_ast_visit::walk_mut::walk_expression(self, it);
                                return;
                            }
                        }
                    }
                    if let Some(obj) = self.member_object(&call.callee) {
                        if !self.callee_is_excluded(obj) {
                            let new_callee = obj.clone_in(self.allocator);
                            let args = self.simplified_args(call, method);
                            let span = call.span;
                            *it = self.make_call_expr(span, new_callee, args);
                            self.modified = true;
                            return;
                        }
                    }
                }
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
