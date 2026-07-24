use std::collections::HashSet;

use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::{GetSpan, Span};

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveDefiniteMemberExpressions {
    evaluator: JsEvaluator,
}

impl ResolveDefiniteMemberExpressions {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveDefiniteMemberExpressions {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveDefiniteMemberExpressions {
    fn name(&self) -> &'static str {
        "resolveDefiniteMemberExpressions"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = SkipSpanCollector {
            skip_spans: HashSet::new(),
        };
        collector.visit_program(program);

        let mut visitor = DefiniteMemberVisitor {
            allocator: ctx.allocator,
            transform: self,
            skip_spans: collector.skip_spans,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveDefiniteMemberExpressions {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct SkipSpanCollector {
    skip_spans: HashSet<Span>,
}

impl<'a> Visit<'a> for SkipSpanCollector {
    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if matches!(
            &it.callee,
            Expression::StaticMemberExpression(_)
                | Expression::ComputedMemberExpression(_)
                | Expression::PrivateFieldExpression(_)
        ) {
            self.skip_spans.insert(it.callee.span());
        }
        oxc_ast_visit::walk::walk_call_expression(self, it);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if matches!(
            &it.argument,
            SimpleAssignmentTarget::StaticMemberExpression(_)
                | SimpleAssignmentTarget::ComputedMemberExpression(_)
        ) {
            self.skip_spans.insert(it.argument.span());
        }
        oxc_ast_visit::walk::walk_update_expression(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if matches!(
            &it.left,
            AssignmentTarget::StaticMemberExpression(_)
                | AssignmentTarget::ComputedMemberExpression(_)
        ) {
            self.skip_spans.insert(it.left.span());
        }
        oxc_ast_visit::walk::walk_assignment_expression(self, it);
    }
}

struct DefiniteMemberVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveDefiniteMemberExpressions,
    skip_spans: HashSet<Span>,
    modified: bool,
}

impl<'a, 'b> DefiniteMemberVisitor<'a, 'b> {
    fn try_replace(&mut self, expr: &mut Expression<'a>) {
        if !matches!(
            expr,
            Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_)
        ) {
            return;
        }
        if self.skip_spans.contains(&expr.span()) {
            return;
        }
        if !has_valid_property(expr) || !has_valid_object(expr) {
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

impl<'a, 'b> VisitMut<'a> for DefiniteMemberVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        self.try_replace(expr);
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }
}

fn has_valid_property(expr: &Expression) -> bool {
    match expr {
        Expression::StaticMemberExpression(_) => true,
        Expression::ComputedMemberExpression(c) => matches!(
            &c.expression,
            Expression::StringLiteral(_)
                | Expression::NumericLiteral(_)
                | Expression::BooleanLiteral(_)
                | Expression::NullLiteral(_)
                | Expression::BigIntLiteral(_)
                | Expression::RegExpLiteral(_)
        ),
        _ => false,
    }
}

fn has_valid_object(expr: &Expression) -> bool {
    let object = match expr {
        Expression::StaticMemberExpression(s) => &s.object,
        Expression::ComputedMemberExpression(c) => &c.object,
        _ => return false,
    };

    match object {
        Expression::ArrayExpression(arr) => !arr.elements.is_empty(),
        Expression::StringLiteral(s) => !s.value.is_empty(),
        Expression::NumericLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_) => false,
        _ => false,
    }
}
