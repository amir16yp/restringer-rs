use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};
use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;

pub struct ResolveDeterministicConditionalExpressions {
    evaluator: JsEvaluator,
}

impl ResolveDeterministicConditionalExpressions {
    pub fn new() -> Self {
        Self { evaluator: JsEvaluator::new() }
    }
}

impl Default for ResolveDeterministicConditionalExpressions {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveDeterministicConditionalExpressions {
    fn name(&self) -> &'static str {
        "resolveDeterministicConditionalExpressions"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = ConditionalVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveDeterministicConditionalExpressions {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct ConditionalVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveDeterministicConditionalExpressions,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for ConditionalVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::ConditionalExpression(cond) = expr {
            if is_literal(&cond.test) {
                let test_code = match self.transform.expression_to_code(&cond.test) {
                    Ok(c) => c,
                    Err(_) => return,
                };
                match self.transform.evaluator().eval_to_bool(&format!("Boolean({})", test_code)) {
                    Ok(true) => {
                        *expr = cond.consequent.clone_in(self.allocator);
                        self.modified = true;
                    }
                    Ok(false) => {
                        *expr = cond.alternate.clone_in(self.allocator);
                        self.modified = true;
                    }
                    Err(_) => {}
                }
            }
        }
    }
}

fn is_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::StringLiteral(_)
            | Expression::NumericLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::NullLiteral(_)
            | Expression::BigIntLiteral(_)
            | Expression::RegExpLiteral(_)
    )
}
