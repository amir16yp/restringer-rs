use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::GetSpan;
use oxc_syntax::operator::BinaryOperator;

use crate::{Transform, TransformCtx};
use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;

pub struct ResolveMinimalAlphabet {
    evaluator: JsEvaluator,
}

impl ResolveMinimalAlphabet {
    pub fn new() -> Self {
        Self { evaluator: JsEvaluator::new() }
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
                if let Some(new_expr) = super::helpers::parse_expression_in(self.allocator, &json, expr.span()) {
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
        let should_eval = match expr {
            Expression::UnaryExpression(un) => {
                let arg = &un.argument;
                // Skip numeric literals/bigints; arrays and non-numeric literals are allowed.
                if matches!(arg, Expression::NumericLiteral(_) | Expression::BigIntLiteral(_)) {
                    false
                } else if matches!(arg, Expression::ArrayExpression(_)) {
                    !contains_this(arg)
                } else {
                    !is_numeric_start_literal(arg) && !contains_this(arg)
                }
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Addition => {
                if contains_this(&bin.left) || contains_this(&bin.right) {
                    false
                } else {
                    match &bin.left {
                        Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_) | Expression::PrivateFieldExpression(_) => false,
                        Expression::ThisExpression(_) => false,
                        Expression::ArrayExpression(_) => true,
                        left => !is_numeric_start_literal(left),
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

fn contains_this(expr: &Expression) -> bool {
    struct ThisChecker(bool);
    impl<'a> Visit<'a> for ThisChecker {
        fn visit_this_expression(&mut self, _it: &ThisExpression) {
            self.0 = true;
        }
    }

    let mut checker = ThisChecker(false);
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
