use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;
use oxc_syntax::operator::BinaryOperator;
use std::cell::Cell;
use oxc_syntax::node::NodeId;

use crate::{Transform, TransformCtx};

pub struct SimplifyArrayCoercion;

impl Transform for SimplifyArrayCoercion {
    fn name(&self) -> &'static str {
        "simplifyArrayCoercion"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = Visitor { allocator: ctx.allocator, modified: false };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn try_array_to_number(&self, expr: &Expression<'a>) -> Option<f64> {
        match expr {
            Expression::ArrayExpression(arr) => {
                if arr.elements.is_empty() {
                    Some(0.0)
                } else if arr.elements.len() == 1 {
                    if let ArrayExpressionElement::SpreadElement(_) = &arr.elements[0] {
                        return None;
                    }
                    let elem = arr.elements[0].to_expression();
                    self.try_eval_to_number(elem)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn try_eval_to_number(&self, expr: &Expression<'a>) -> Option<f64> {
        match expr {
            Expression::NumericLiteral(n) => Some(n.value),
            Expression::BooleanLiteral(b) => Some(if b.value { 1.0 } else { 0.0 }),
            Expression::StringLiteral(s) => {
                if s.value.is_empty() {
                    Some(0.0)
                } else {
                    s.value.parse::<f64>().ok()
                }
            }
            Expression::ArrayExpression(_) => self.try_array_to_number(expr),
            _ => None,
        }
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

    fn try_simplify_binary(&mut self, expr: &BinaryExpression<'a>) -> Option<Expression<'a>> {
        match expr.operator {
            BinaryOperator::Addition
            | BinaryOperator::Subtraction
            | BinaryOperator::Multiplication
            | BinaryOperator::Division => {
                let left_num = self.try_eval_to_number(&expr.left)?;
                let right_num = self.try_eval_to_number(&expr.right)?;
                
                let result = match expr.operator {
                    BinaryOperator::Addition => left_num + right_num,
                    BinaryOperator::Subtraction => left_num - right_num,
                    BinaryOperator::Multiplication => left_num * right_num,
                    BinaryOperator::Division => {
                        if right_num != 0.0 {
                            left_num / right_num
                        } else {
                            return None;
                        }
                    }
                    _ => return None,
                };
                
                Some(self.make_number(expr.span, result))
            }
            _ => None,
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::BinaryExpression(bin) = expr {
            if let Some(simplified) = self.try_simplify_binary(bin) {
                *expr = simplified;
                self.modified = true;
            }
        }
    }
}
