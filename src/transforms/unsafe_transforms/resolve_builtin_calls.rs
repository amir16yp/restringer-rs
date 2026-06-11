use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use crate::{Transform, TransformCtx};
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use super::helpers;

pub struct ResolveBuiltinCalls {
    evaluator: JsEvaluator,
}

impl ResolveBuiltinCalls {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }

    fn is_builtin_call(&self, call: &CallExpression) -> bool {
        // Check if all arguments are static literals or regex literals
        let args_ok = call.arguments.iter().all(|arg| {
            if let Some(expr) = arg.as_expression() {
                helpers::is_static_literal(expr) || matches!(expr, Expression::RegExpLiteral(_))
            } else {
                false
            }
        });

        if !args_ok {
            return false;
        }

        match &call.callee {
            Expression::Identifier(ident) => {
                let name = ident.name.as_str();
                matches!(
                    name,
                    "atob"
                        | "btoa"
                        | "parseInt"
                        | "parseFloat"
                        | "decodeURIComponent"
                        | "encodeURIComponent"
                        | "decodeURI"
                        | "encodeURI"
                        | "isNaN"
                        | "isFinite"
                )
            }
            Expression::StaticMemberExpression(mem) => {
                let prop_name = mem.property.name.as_str();
                match &mem.object {
                    Expression::Identifier(id) if id.name.as_str() == "String" => {
                        matches!(prop_name, "fromCharCode" | "fromCodePoint")
                    }
                    Expression::StringLiteral(_) => {
                        matches!(
                            prop_name,
                            "split"
                                | "replace"
                                | "indexOf"
                                | "lastIndexOf"
                                | "charAt"
                                | "charCodeAt"
                                | "substring"
                                | "slice"
                                | "toLowerCase"
                                | "toUpperCase"
                                | "trim"
                                | "concat"
                        )
                    }
                    Expression::ArrayExpression(arr) => {
                        let elements_ok = arr.elements.iter().all(|elem| {
                            match elem {
                                ArrayExpressionElement::SpreadElement(_) => false,
                                ArrayExpressionElement::Elision(_) => true,
                                _ => helpers::is_static_literal(elem.to_expression()),
                            }
                        });
                        elements_ok && matches!(prop_name, "join" | "slice" | "concat" | "indexOf")
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

impl Default for ResolveBuiltinCalls {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveBuiltinCalls {
    fn name(&self) -> &'static str {
        "resolveBuiltinCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = BuiltinVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveBuiltinCalls {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct BuiltinVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveBuiltinCalls,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for BuiltinVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::CallExpression(call) = expr {
            if self.transform.is_builtin_call(call) {
                let call_code = helpers::expression_to_code(expr);
                if !call_code.is_empty() {
                    let full_code = format!("{};\n{}", helpers::EVAL_PRELUDE, call_code);
                    if let Ok(json_res) = self.transform.evaluator.eval_to_json(&full_code) {
                        if let Some(new_expr) = helpers::parse_expression_in(self.allocator, &json_res, expr.span()) {
                            *expr = new_expr;
                            self.modified = true;
                        }
                    }
                }
            }
        }
    }
}
