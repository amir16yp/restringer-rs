use std::cell::Cell;
use oxc_syntax::node::NodeId;
use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::{Span, GetSpan};

use crate::{Transform, TransformCtx};
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;

pub struct EvalConstantExpressions {
    evaluator: JsEvaluator,
}

impl EvalConstantExpressions {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for EvalConstantExpressions {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for EvalConstantExpressions {
    fn name(&self) -> &'static str {
        "evalConstantExpressions"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = EvalVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for EvalConstantExpressions {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct EvalVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b EvalConstantExpressions,
    modified: bool,
}

impl<'a, 'b> EvalVisitor<'a, 'b> {
    fn is_safe_to_eval(&self, expr: &Expression<'a>) -> bool {
        match expr {
            Expression::NumericLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::NullLiteral(_) => false,
            
            Expression::ArrayExpression(_) | Expression::ObjectExpression(_) => false,
            
            Expression::ParenthesizedExpression(paren) => {
                self.is_safe_to_eval_recursive(&paren.expression)
            }
            
            Expression::UnaryExpression(un) => {
                self.is_safe_to_eval_recursive(&un.argument)
            }
            
            Expression::BinaryExpression(bin) => {
                self.is_safe_to_eval_recursive(&bin.left) && self.is_safe_to_eval_recursive(&bin.right)
            }
            
            Expression::LogicalExpression(log) => {
                self.is_safe_to_eval_recursive(&log.left) && self.is_safe_to_eval_recursive(&log.right)
            }
            
            Expression::ConditionalExpression(cond) => {
                self.is_safe_to_eval_recursive(&cond.test)
                    && self.is_safe_to_eval_recursive(&cond.consequent)
                    && self.is_safe_to_eval_recursive(&cond.alternate)
            }
            
            Expression::TemplateLiteral(tmpl) => {
                tmpl.expressions.iter().all(|e| self.is_safe_to_eval_recursive(e))
            }
            
            _ => false,
        }
    }

    fn is_safe_to_eval_recursive(&self, expr: &Expression<'a>) -> bool {
        match expr {
            Expression::NumericLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::NullLiteral(_) => true,
            
            Expression::ParenthesizedExpression(paren) => {
                self.is_safe_to_eval_recursive(&paren.expression)
            }
            
            Expression::ArrayExpression(arr) => {
                arr.elements.iter().all(|elem| {
                    match elem {
                        ArrayExpressionElement::SpreadElement(_) => false,
                        ArrayExpressionElement::Elision(_) => true,
                        _ => self.is_safe_to_eval_recursive(elem.to_expression()),
                    }
                })
            }
            
            Expression::ObjectExpression(obj) => {
                obj.properties.iter().all(|prop| {
                    match prop {
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
                            key_safe && self.is_safe_to_eval_recursive(&p.value)
                        }
                        ObjectPropertyKind::SpreadProperty(_) => false,
                    }
                })
            }
            
            Expression::UnaryExpression(un) => {
                self.is_safe_to_eval_recursive(&un.argument)
            }
            
            Expression::BinaryExpression(bin) => {
                self.is_safe_to_eval_recursive(&bin.left) && self.is_safe_to_eval_recursive(&bin.right)
            }
            
            Expression::LogicalExpression(log) => {
                self.is_safe_to_eval_recursive(&log.left) && self.is_safe_to_eval_recursive(&log.right)
            }
            
            Expression::ConditionalExpression(cond) => {
                self.is_safe_to_eval_recursive(&cond.test)
                    && self.is_safe_to_eval_recursive(&cond.consequent)
                    && self.is_safe_to_eval_recursive(&cond.alternate)
            }
            
            Expression::TemplateLiteral(tmpl) => {
                tmpl.expressions.iter().all(|e| self.is_safe_to_eval_recursive(e))
            }
            
            _ => false,
        }
    }

    fn make_string(&self, span: Span, value: String) -> Expression<'a> {
        let value_str = self.allocator.alloc_str(&value);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value: value_str.into(),
                raw: None,
                lone_surrogates: false,
            },
            self.allocator,
        ))
    }

    fn make_number(&self, span: Span, value: f64) -> Expression<'a> {
        Expression::NumericLiteral(ArenaBox::new_in(
            NumericLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value,
                raw: None,
                base: oxc_syntax::number::NumberBase::Decimal,
            },
            self.allocator,
        ))
    }

    fn make_boolean(&self, span: Span, value: bool) -> Expression<'a> {
        Expression::BooleanLiteral(ArenaBox::new_in(
            BooleanLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value,
            },
            self.allocator,
        ))
    }

    fn try_eval_and_replace(&mut self, expr: &Expression<'a>) -> Option<Expression<'a>> {
        if !self.is_safe_to_eval(expr) {
            return None;
        }

        if let Ok(result_num) = self.transform.eval_expression_to_number(expr) {
            if result_num.is_finite() {
                return Some(self.make_number(expr.span(), result_num));
            }
        }

        if let Ok(result_bool) = self.transform.eval_expression_to_bool(expr) {
            return Some(self.make_boolean(expr.span(), result_bool));
        }

        if let Ok(result_str) = self.transform.eval_expression_to_string(expr) {
            return Some(self.make_string(expr.span(), result_str));
        }

        None
    }
}

impl<'a, 'b> VisitMut<'a> for EvalVisitor<'a, 'b> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        if let Some(replacement) = self.try_eval_and_replace(it) {
            *it = replacement;
            self.modified = true;
        }
    }
}
