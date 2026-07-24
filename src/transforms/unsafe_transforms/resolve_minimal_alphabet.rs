use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::GetSpan;
use oxc_syntax::operator::BinaryOperator;

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveMinimalAlphabet {
    evaluator: JsEvaluator,
}

impl ResolveMinimalAlphabet {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveMinimalAlphabet {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveMinimalAlphabet {
    fn name(&self) -> &'static str {
        "resolveMinimalAlphabet"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = MinimalAlphabetVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveMinimalAlphabet {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct MinimalAlphabetVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveMinimalAlphabet,
    modified: bool,
}

impl<'a, 'b> MinimalAlphabetVisitor<'a, 'b> {
    fn try_replace(&mut self, expr: &mut Expression<'a>) {
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

impl<'a, 'b> VisitMut<'a> for MinimalAlphabetVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        // Determine candidate shape without holding interior references so that we
        // can run is_side_effect_free on the whole expression afterwards.
        let is_unary = matches!(expr, Expression::UnaryExpression(_));
        let is_add = matches!(expr, Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Addition);

        if !is_unary && !is_add {
            oxc_ast_visit::walk_mut::walk_expression(self, expr);
            return;
        }

        let side_effect_free = is_side_effect_free(expr);

        let should_eval = match expr {
            Expression::UnaryExpression(un) => {
                if !side_effect_free {
                    false
                } else {
                    let arg = &un.argument;
                    // Skip numeric literals/bigints; arrays and non-numeric literals are allowed.
                    if matches!(
                        arg,
                        Expression::NumericLiteral(_) | Expression::BigIntLiteral(_)
                    ) {
                        false
                    } else if matches!(arg, Expression::ArrayExpression(_)) {
                        true
                    } else {
                        !is_numeric_start_literal(arg)
                    }
                }
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Addition => {
                if !side_effect_free {
                    false
                } else {
                    let left_member = contains_member_expression(&bin.left);
                    let right_member = contains_member_expression(&bin.right);
                    let left_array = matches!(&bin.left, Expression::ArrayExpression(_));
                    let right_array = matches!(&bin.right, Expression::ArrayExpression(_));
                    if left_member || right_member || left_array || right_array {
                        true
                    } else {
                        match &bin.left {
                            Expression::ArrayExpression(_) => true,
                            left => !is_numeric_start_literal(left),
                        }
                    }
                }
            }
            _ => false,
        };

        if should_eval {
            self.try_replace(expr);
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }
}

fn is_side_effect_free(expr: &Expression) -> bool {
    struct SideEffectFreeChecker(bool);
    impl<'a> Visit<'a> for SideEffectFreeChecker {
        fn visit_expression(&mut self, it: &Expression<'a>) {
            if !self.0 {
                return;
            }
            match it {
                Expression::Identifier(_)
                | Expression::ThisExpression(_)
                | Expression::CallExpression(_)
                | Expression::NewExpression(_)
                | Expression::ImportExpression(_)
                | Expression::MetaProperty(_)
                | Expression::Super(_)
                | Expression::UpdateExpression(_)
                | Expression::AssignmentExpression(_)
                | Expression::AwaitExpression(_)
                | Expression::YieldExpression(_) => {
                    self.0 = false;
                    return;
                }
                Expression::ObjectExpression(obj) if obj.properties.is_empty() => {
                    // Empty object literal is a pure value used by JSFuck.
                }
                Expression::ObjectExpression(_) => {
                    self.0 = false;
                    return;
                }
                Expression::ArrayExpression(arr) => {
                    for elem in &arr.elements {
                        if matches!(elem, ArrayExpressionElement::SpreadElement(_)) {
                            self.0 = false;
                            return;
                        }
                    }
                }
                _ => {}
            }
            oxc_ast_visit::walk::walk_expression(self, it);
        }
    }

    let mut checker = SideEffectFreeChecker(true);
    checker.visit_expression(expr);
    checker.0
}

fn contains_member_expression(expr: &Expression) -> bool {
    struct MemberChecker(bool);
    impl<'a> Visit<'a> for MemberChecker {
        fn visit_expression(&mut self, it: &Expression<'a>) {
            if self.0 {
                return;
            }
            if matches!(
                it,
                Expression::StaticMemberExpression(_)
                    | Expression::ComputedMemberExpression(_)
                    | Expression::PrivateFieldExpression(_)
            ) {
                self.0 = true;
                return;
            }
            oxc_ast_visit::walk::walk_expression(self, it);
        }
    }

    let mut checker = MemberChecker(false);
    checker.visit_expression(expr);
    checker.0
}

fn is_numeric_start_literal(expr: &Expression) -> bool {
    match expr {
        Expression::NumericLiteral(_) | Expression::BigIntLiteral(_) => true,
        Expression::StringLiteral(s) => {
            let st = s.value.as_str();
            !st.is_empty() && st.chars().next().unwrap().is_ascii_digit()
        }
        _ => false,
    }
}
