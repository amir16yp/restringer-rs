use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
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
        let mut visitor = Visitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveLiteralIifeResults {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct Visitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveLiteralIifeResults,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for Visitor<'a, 'b> {
    fn visit_expression(&mut self, expression: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expression);
        if !is_literal_iife(expression) {
            return;
        }
        let code = helpers::expression_to_code(expression);
        let full_code = format!("{};\n{}", helpers::EVAL_PRELUDE, code);
        let Ok(json) = self.transform.evaluator.eval_to_json(&full_code) else {
            return;
        };
        let Some(replacement) =
            helpers::parse_expression_in(self.allocator, &json, expression.span())
        else {
            return;
        };
        *expression = replacement;
        self.modified = true;
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

    if !matches!(
        callee,
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
    ) {
        return false;
    }

    call.arguments.iter().all(|arg| {
        let Some(expr) = arg.as_expression() else {
            return false;
        };
        is_static_literal(expr)
    })
}

fn is_static_literal(expr: &Expression) -> bool {
    helpers::is_static_literal(expr) || matches!(expr, Expression::RegExpLiteral(_))
}
