use std::cell::Cell;

use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;
use oxc_syntax::operator::UnaryOperator;
use oxc_syntax::node::NodeId;

use crate::{Transform, TransformCtx};

pub struct ResolveBuiltinStringCalls;

impl Transform for ResolveBuiltinStringCalls {
    fn name(&self) -> &'static str {
        "resolveBuiltinStringCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &p.expression,
                _ => return expr,
            }
        }
    }

    fn as_integer_literal(&self, expr: &Expression<'a>) -> Option<i64> {
        let expr = self.unwrap_parens(expr);
        match expr {
            Expression::NumericLiteral(n) => {
                let v = n.value;
                if !v.is_finite() || v.fract() != 0.0 {
                    return None;
                }
                if v < (i64::MIN as f64) || v > (i64::MAX as f64) {
                    return None;
                }
                Some(v as i64)
            }
            Expression::UnaryExpression(un) => {
                let inner = self.unwrap_parens(&un.argument);
                let Expression::NumericLiteral(n) = inner else { return None };
                let v = n.value;
                if !v.is_finite() || v.fract() != 0.0 {
                    return None;
                }
                let v = match un.operator {
                    UnaryOperator::UnaryPlus => v,
                    UnaryOperator::UnaryNegation => -v,
                    _ => return None,
                };
                if v < (i64::MIN as f64) || v > (i64::MAX as f64) {
                    return None;
                }
                Some(v as i64)
            }
            _ => None,
        }
    }

    fn make_string_literal(&self, span: Span, value: &str) -> Expression<'a> {
        let value = self.allocator.alloc_str(value);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value: value.into(),
                raw: None,
                lone_surrogates: false,
            },
            self.allocator,
        ))
    }

    fn resolve_char_at(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let callee = self.unwrap_parens(&call.callee);
        let (object, prop) = match callee {
            Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
            Expression::ComputedMemberExpression(m) => (
                &m.object,
                match self.unwrap_parens(&m.expression) {
                    Expression::StringLiteral(s) => s.value.as_str(),
                    _ => return None,
                },
            ),
            _ => return None,
        };
        if prop != "charAt" {
            return None;
        }
        let object = self.unwrap_parens(object);
        let Expression::StringLiteral(s) = object else { return None };

        let first_arg = call.arguments.first()?;
        let idx = self.as_integer_literal(first_arg.as_expression()?)?;
        let text = s.value.as_str();
        let units: Vec<u16> = text.encode_utf16().collect();
        if idx < 0 || idx as usize >= units.len() {
            return Some(self.make_string_literal(call.span, ""));
        }
        let unit = units[idx as usize];
        // Avoid producing unpaired surrogates as literal values.
        if (0xD800..=0xDFFF).contains(&unit) {
            return None;
        }
        let ch = char::from_u32(unit as u32)?;
        Some(self.make_string_literal(call.span, &ch.to_string()))
    }

    fn resolve_from_char_code(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let callee = self.unwrap_parens(&call.callee);
        let (ident, prop) = match callee {
            Expression::StaticMemberExpression(m) => {
                let Expression::Identifier(id) = self.unwrap_parens(&m.object) else {
                    return None;
                };
                (id.name.as_str(), m.property.name.as_str())
            }
            Expression::ComputedMemberExpression(m) => {
                let Expression::Identifier(id) = self.unwrap_parens(&m.object) else {
                    return None;
                };
                let prop = match self.unwrap_parens(&m.expression) {
                    Expression::StringLiteral(s) => s.value.as_str(),
                    _ => return None,
                };
                (id.name.as_str(), prop)
            }
            _ => return None,
        };
        if ident != "String" || prop != "fromCharCode" {
            return None;
        }

        let mut units: Vec<u16> = Vec::with_capacity(call.arguments.len());
        for arg in &call.arguments {
            let expr = arg.as_expression()?;
            let v = self.as_integer_literal(expr)?;
            let u = ((v % 65536 + 65536) % 65536) as u16;
            units.push(u);
        }

        let text = String::from_utf16(&units).ok()?;
        Some(self.make_string_literal(call.span, &text))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        if let Expression::CallExpression(call) = it {
            if let Some(repl) = self.resolve_char_at(call).or_else(|| self.resolve_from_char_code(call)) {
                *it = repl;
                self.modified = true;
            }
        }
    }
}
