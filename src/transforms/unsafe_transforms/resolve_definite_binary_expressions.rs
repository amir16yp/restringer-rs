use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveDefiniteBinaryExpressions {
    evaluator: JsEvaluator,
}

impl ResolveDefiniteBinaryExpressions {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveDefiniteBinaryExpressions {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveDefiniteBinaryExpressions {
    fn name(&self) -> &'static str {
        "resolveDefiniteBinaryExpressions"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = DefiniteBinaryVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveDefiniteBinaryExpressions {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct DefiniteBinaryVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveDefiniteBinaryExpressions,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for DefiniteBinaryVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::BinaryExpression(_) = expr {
            if contains_only_literals(expr) {
                let code = match self.transform.expression_to_code(expr) {
                    Ok(c) => c,
                    Err(_) => return,
                };
                match self.transform.evaluator().eval_to_json(&code) {
                    Ok(json) => {
                        if let Some(new_expr) =
                            super::helpers::parse_expression_in(self.allocator, &json, expr.span())
                        {
                            *expr = new_expr;
                            self.modified = true;
                        }
                    }
                    Err(_) => {}
                }
            }
        }
    }
}

fn contains_only_literals(expr: &Expression) -> bool {
    match expr {
        Expression::BinaryExpression(bin) => {
            contains_only_literals(&bin.left) && contains_only_literals(&bin.right)
        }
        Expression::UnaryExpression(un) => contains_only_literals(&un.argument),
        Expression::LogicalExpression(log) => {
            contains_only_literals(&log.left) && contains_only_literals(&log.right)
        }
        Expression::ConditionalExpression(cond) => {
            contains_only_literals(&cond.test)
                && contains_only_literals(&cond.consequent)
                && contains_only_literals(&cond.alternate)
        }
        Expression::SequenceExpression(seq) => seq.expressions.iter().all(contains_only_literals),
        Expression::NumericLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_) => true,
        _ => false,
    }
}
