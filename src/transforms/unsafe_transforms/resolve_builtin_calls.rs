use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use super::helpers;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveBuiltinCalls {
    evaluator: JsEvaluator,
}

impl ResolveBuiltinCalls {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }

    fn is_safe_argument(&self, expr: &Expression) -> bool {
        match expr {
            Expression::NumericLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::NullLiteral(_)
            | Expression::BigIntLiteral(_)
            | Expression::RegExpLiteral(_) => true,
            Expression::UnaryExpression(un) => {
                matches!(
                    un.operator,
                    oxc_syntax::operator::UnaryOperator::UnaryNegation
                        | oxc_syntax::operator::UnaryOperator::UnaryPlus
                ) && self.is_safe_argument(&un.argument)
            }
            Expression::ArrayExpression(arr) => arr.elements.iter().all(|elem| match elem {
                ArrayExpressionElement::SpreadElement(_) => false,
                ArrayExpressionElement::Elision(_) => true,
                _ => self.is_safe_argument(elem.to_expression()),
            }),
            Expression::ObjectExpression(obj) => obj.properties.iter().all(|prop| match prop {
                ObjectPropertyKind::ObjectProperty(p) => {
                    let key_safe = match &p.key {
                        PropertyKey::StaticIdentifier(_) => true,
                        PropertyKey::PrivateIdentifier(_) => false,
                        PropertyKey::NullLiteral(_)
                        | PropertyKey::NumericLiteral(_)
                        | PropertyKey::StringLiteral(_)
                        | PropertyKey::RegExpLiteral(_)
                        | PropertyKey::BigIntLiteral(_)
                        | PropertyKey::TemplateLiteral(_) => true,
                        _ => false,
                    };
                    key_safe && self.is_safe_argument(&p.value)
                }
                ObjectPropertyKind::SpreadProperty(_) => false,
            }),
            Expression::ParenthesizedExpression(p) => self.is_safe_argument(&p.expression),
            _ => false,
        }
    }

    fn is_builtin_call(&self, call: &CallExpression) -> bool {
        let args_ok = call
            .arguments
            .iter()
            .all(|arg| arg.as_expression().map_or(false, |e| self.is_safe_argument(e)));

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
                        | "escape"
                        | "unescape"
                        | "isNaN"
                        | "isFinite"
                        | "String"
                        | "Number"
                        | "Boolean"
                )
            }
            Expression::StaticMemberExpression(mem) => {
                let prop_name = mem.property.name.as_str();
                match &mem.object {
                    Expression::Identifier(id) if id.name.as_str() == "String" => {
                        matches!(prop_name, "fromCharCode" | "fromCodePoint")
                    }
                    Expression::Identifier(id) if id.name.as_str() == "Number" => matches!(
                        prop_name,
                        "isNaN"
                            | "isFinite"
                            | "isInteger"
                            | "isSafeInteger"
                            | "parseInt"
                            | "parseFloat"
                    ),
                    Expression::Identifier(id) if id.name.as_str() == "Math" => matches!(
                        prop_name,
                        "abs"
                            | "acos"
                            | "acosh"
                            | "asin"
                            | "asinh"
                            | "atan"
                            | "atan2"
                            | "atanh"
                            | "cbrt"
                            | "ceil"
                            | "clz32"
                            | "cos"
                            | "cosh"
                            | "exp"
                            | "expm1"
                            | "floor"
                            | "fround"
                            | "hypot"
                            | "imul"
                            | "log"
                            | "log1p"
                            | "log10"
                            | "log2"
                            | "max"
                            | "min"
                            | "pow"
                            | "round"
                            | "sign"
                            | "sin"
                            | "sinh"
                            | "sqrt"
                            | "tan"
                            | "tanh"
                            | "trunc"
                    ),
                    Expression::Identifier(id) if id.name.as_str() == "Object" => matches!(
                        prop_name,
                        "keys" | "values" | "entries" | "getOwnPropertyNames" | "getOwnPropertySymbols"
                    ),
                    Expression::Identifier(id) if id.name.as_str() == "Array" => {
                        matches!(prop_name, "isArray" | "from")
                    }
                    Expression::Identifier(id) if id.name.as_str() == "JSON" => {
                        matches!(prop_name, "stringify")
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
                        arr.elements.iter().all(|elem| match elem {
                            ArrayExpressionElement::SpreadElement(_) => false,
                            ArrayExpressionElement::Elision(_) => true,
                            _ => self.is_safe_argument(elem.to_expression()),
                        }) && matches!(prop_name, "join" | "slice" | "concat" | "indexOf")
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
                        if let Some(new_expr) =
                            helpers::parse_expression_in(self.allocator, &json_res, expr.span())
                        {
                            *expr = new_expr;
                            self.modified = true;
                        }
                    }
                }
            }
        }
    }
}
