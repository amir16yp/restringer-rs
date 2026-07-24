use oxc_ast::ast::*;
use oxc_span::GetSpan;

use super::helpers;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveLiteralIifeResults {
    evaluator: JsEvaluator,
}

impl ResolveLiteralIifeResults {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveLiteralIifeResults {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveLiteralIifeResults {
    fn name(&self) -> &'static str {
        "resolveLiteralIifeResults"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut modified = false;

        for stmt in program.body.iter_mut() {
            let mut replacement: Option<Expression<'a>> = None;

            if let Statement::VariableDeclaration(decl) = stmt {
                for d in &mut decl.declarations {
                    let Some(init) = &d.init else { continue };
                    if is_literal_iife(init) {
                        let code = helpers::expression_to_code(init);
                        if let Ok(json) = self.evaluator.eval_to_json(&code) {
                            if let Some(expr) = helpers::parse_expression_in(ctx.allocator, &json, init.span()) {
                                replacement = Some(expr);
                            }
                        }
                    }
                    if replacement.is_some() {
                        d.init = replacement;
                        modified = true;
                        break;
                    }
                }
            }
        }

        modified
    }
}

impl UnsafeTransform for ResolveLiteralIifeResults {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

fn is_literal_iife(expr: &Expression) -> bool {
    let call = match expr {
        Expression::CallExpression(c) => c,
        _ => return false,
    };

    let mut callee = &call.callee;
    while let Expression::ParenthesizedExpression(paren) = callee {
        callee = &paren.expression;
    }

    if !matches!(callee, Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)) {
        return false;
    }

    call.arguments.iter().all(|arg| {
        let Some(expr) = arg.as_expression() else { return false };
        is_static_literal(expr)
    })
}

fn is_static_literal(expr: &Expression) -> bool {
    helpers::is_static_literal(expr) || matches!(expr, Expression::RegExpLiteral(_))
}
