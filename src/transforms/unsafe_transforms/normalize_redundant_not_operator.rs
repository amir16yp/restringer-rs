use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;
use oxc_syntax::operator::UnaryOperator;

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct NormalizeRedundantNotOperator {
    evaluator: JsEvaluator,
}

impl NormalizeRedundantNotOperator {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for NormalizeRedundantNotOperator {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for NormalizeRedundantNotOperator {
    fn name(&self) -> &'static str {
        "normalizeRedundantNotOperator"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = NotOperatorVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for NormalizeRedundantNotOperator {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct NotOperatorVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b NormalizeRedundantNotOperator,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for NotOperatorVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        let un = match expr {
            Expression::UnaryExpression(un) => un,
            _ => return,
        };
        if un.operator != UnaryOperator::LogicalNot {
            return;
        }
        if !is_not_argument_resolvable(&un.argument) {
            return;
        }
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

fn is_not_argument_resolvable(expr: &Expression) -> bool {
    match expr {
        Expression::StringLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_)
        | Expression::ArrayExpression(_)
        | Expression::ObjectExpression(_) => true,
        Expression::Identifier(id) => id.name == "undefined",
        Expression::TemplateLiteral(tpl) => tpl.expressions.is_empty(),
        Expression::UnaryExpression(un) => is_not_argument_resolvable(&un.argument),
        _ => false,
    }
}
