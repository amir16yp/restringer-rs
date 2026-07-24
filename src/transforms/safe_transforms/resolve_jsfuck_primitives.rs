use oxc_syntax::node::NodeId;
use std::cell::Cell;

use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;
use oxc_syntax::operator::{BinaryOperator, UnaryOperator};

use crate::{Transform, TransformCtx};

pub struct ResolveJSFuckPrimitives;

impl Transform for ResolveJSFuckPrimitives {
    fn name(&self) -> &'static str {
        "resolveJSFuckPrimitives"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Resolver { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Resolver<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Resolver<'a> {
    fn is_empty_array(&self, expr: &Expression<'a>) -> bool {
        if let Expression::ArrayExpression(arr) = expr {
            arr.elements.is_empty()
        } else {
            false
        }
    }

    fn try_eval_to_bool(&self, expr: &Expression<'a>) -> Option<bool> {
        match expr {
            Expression::BooleanLiteral(b) => Some(b.value),
            Expression::UnaryExpression(un) if un.operator == UnaryOperator::LogicalNot => {
                self.try_eval_to_bool(&un.argument).map(|v| !v)
            }
            Expression::ArrayExpression(_) => Some(true),
            Expression::NumericLiteral(n) => {
                if n.value == 0.0 || n.value.is_nan() {
                    Some(false)
                } else {
                    Some(true)
                }
            }
            Expression::StringLiteral(s) => {
                if s.value.is_empty() {
                    Some(false)
                } else {
                    Some(true)
                }
            }
            _ => None,
        }
    }

    fn try_eval_to_number(&self, expr: &Expression<'a>) -> Option<f64> {
        match expr {
            Expression::NumericLiteral(n) => Some(n.value),
            Expression::UnaryExpression(un) if un.operator == UnaryOperator::UnaryPlus => {
                self.try_eval_to_number(&un.argument)
            }
            Expression::UnaryExpression(un) if un.operator == UnaryOperator::UnaryNegation => {
                self.try_eval_to_number(&un.argument).map(|v| -v)
            }
            Expression::UnaryExpression(un) if un.operator == UnaryOperator::LogicalNot => {
                if let Some(bool_val) = self.try_eval_to_bool(&un.argument) {
                    return Some(if !bool_val { 1.0 } else { 0.0 });
                }
                None
            }
            _ if self.is_empty_array(expr) => Some(0.0),
            Expression::ArrayExpression(arr) => {
                // Empty arrays coerce to "" then 0. Non-empty arrays join to a string
                // before coercion, so we cannot statically determine the number.
                if arr.elements.is_empty() {
                    Some(0.0)
                } else {
                    None
                }
            }
            Expression::BooleanLiteral(b) => Some(if b.value { 1.0 } else { 0.0 }),
            Expression::StringLiteral(s) => {
                if s.value.is_empty() {
                    Some(0.0)
                } else {
                    s.value.parse::<f64>().ok()
                }
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Addition => {
                // Addition is string concatenation when either operand coerces to a string
                // (arrays always coerce to strings). Do not treat it as numeric addition.
                if self.has_string_operand(&bin.left) || self.has_string_operand(&bin.right) {
                    return None;
                }
                let left = self.try_eval_to_number(&bin.left)?;
                let right = self.try_eval_to_number(&bin.right)?;
                Some(left + right)
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Subtraction => {
                let left = self.try_eval_to_number(&bin.left)?;
                let right = self.try_eval_to_number(&bin.right)?;
                Some(left - right)
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Multiplication => {
                let left = self.try_eval_to_number(&bin.left)?;
                let right = self.try_eval_to_number(&bin.right)?;
                Some(left * right)
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Division => {
                let left = self.try_eval_to_number(&bin.left)?;
                let right = self.try_eval_to_number(&bin.right)?;
                if right != 0.0 {
                    Some(left / right)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn try_eval_to_string(&self, expr: &Expression<'a>) -> Option<String> {
        match expr {
            Expression::StringLiteral(s) => Some(s.value.to_string()),
            Expression::NumericLiteral(n) => Some(n.value.to_string()),
            Expression::BooleanLiteral(b) => Some(b.value.to_string()),
            Expression::ArrayExpression(arr) if arr.elements.is_empty() => Some(String::new()),
            Expression::UnaryExpression(un) if un.operator == UnaryOperator::LogicalNot => {
                if let Some(bool_val) = self.try_eval_to_bool(&un.argument) {
                    return Some((!bool_val).to_string());
                }
                None
            }
            Expression::BinaryExpression(bin) if bin.operator == BinaryOperator::Addition => {
                let left_str = self.try_eval_to_string(&bin.left)?;
                let right_str = self.try_eval_to_string(&bin.right)?;
                Some(format!("{}{}", left_str, right_str))
            }
            _ => None,
        }
    }

    fn make_boolean(&self, span: Span, value: bool) -> Expression<'a> {
        Expression::BooleanLiteral(ArenaBox::new_in(
            BooleanLiteral { node_id: Cell::new(NodeId::DUMMY), span, value },
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

    fn try_simplify_unary(&mut self, expr: &UnaryExpression<'a>) -> Option<Expression<'a>> {
        match expr.operator {
            UnaryOperator::LogicalNot => {
                if let Some(bool_val) = self.try_eval_to_bool(&expr.argument) {
                    return Some(self.make_boolean(expr.span, !bool_val));
                }
            }
            UnaryOperator::UnaryPlus => {
                if let Some(num_val) = self.try_eval_to_number(&expr.argument) {
                    return Some(self.make_number(expr.span, num_val));
                }
            }
            UnaryOperator::UnaryNegation => {
                if let Some(num_val) = self.try_eval_to_number(&expr.argument) {
                    return Some(self.make_number(expr.span, -num_val));
                }
            }
            _ => {}
        }
        None
    }

    fn has_string_operand(&self, expr: &Expression<'a>) -> bool {
        matches!(expr, Expression::StringLiteral(_) | Expression::ArrayExpression(_))
    }

    fn try_simplify_binary(&mut self, expr: &BinaryExpression<'a>) -> Option<Expression<'a>> {
        match expr.operator {
            BinaryOperator::Addition => {
                let has_string = self.has_string_operand(&expr.left) || self.has_string_operand(&expr.right);
                let expr_ref = Expression::BinaryExpression(ArenaBox::new_in(expr.clone_in(self.allocator), self.allocator));
                
                if has_string {
                    if let Some(str_val) = self.try_eval_to_string(&expr_ref) {
                        return Some(self.make_string(expr.span, str_val));
                    }
                } else {
                    if let Some(num_val) = self.try_eval_to_number(&expr_ref) {
                        return Some(self.make_number(expr.span, num_val));
                    }
                    if let Some(str_val) = self.try_eval_to_string(&expr_ref) {
                        return Some(self.make_string(expr.span, str_val));
                    }
                }
            }
            BinaryOperator::Subtraction
            | BinaryOperator::Multiplication
            | BinaryOperator::Division => {
                let expr_ref = Expression::BinaryExpression(ArenaBox::new_in(expr.clone_in(self.allocator), self.allocator));
                if let Some(num_val) = self.try_eval_to_number(&expr_ref) {
                    return Some(self.make_number(expr.span, num_val));
                }
            }
            _ => {}
        }
        None
    }

    fn try_simplify_member_expr(&mut self, expr: &ComputedMemberExpression<'a>) -> Option<Expression<'a>> {
        let index = self.try_eval_to_number(&expr.expression)?;
        
        if index.fract() != 0.0 || index < 0.0 {
            return None;
        }
        
        let index_usize = index as usize;

        match &expr.object {
            Expression::ArrayExpression(arr) => {
                if index_usize < arr.elements.len() {
                    let elem = &arr.elements[index_usize];
                    if let ArrayExpressionElement::SpreadElement(_) = elem {
                        return None;
                    }
                    return Some(elem.to_expression().clone_in(self.allocator));
                }
            }
            Expression::StringLiteral(s) => {
                let chars: Vec<char> = s.value.chars().collect();
                if index_usize < chars.len() {
                    return Some(self.make_string(expr.span, chars[index_usize].to_string()));
                }
            }
            _ => {}
        }
        None
    }
}

impl<'a> VisitMut<'a> for Resolver<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        loop {
            let simplified = match it {
                Expression::UnaryExpression(un) => self.try_simplify_unary(un),
                Expression::BinaryExpression(bin) => self.try_simplify_binary(bin),
                Expression::ComputedMemberExpression(comp) => self.try_simplify_member_expr(comp),
                _ => None,
            };
            
            if let Some(new_expr) = simplified {
                *it = new_expr;
                self.modified = true;
            } else {
                break;
            }
        }
    }
}
