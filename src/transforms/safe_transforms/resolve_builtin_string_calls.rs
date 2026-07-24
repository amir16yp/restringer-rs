use std::cell::Cell;

use oxc_allocator::{Box as ArenaBox, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;
use oxc_syntax::node::NodeId;
use oxc_syntax::operator::UnaryOperator;

use crate::{Transform, TransformCtx};

pub struct ResolveBuiltinStringCalls;

impl Transform for ResolveBuiltinStringCalls {
    fn name(&self) -> &'static str {
        "resolveBuiltinStringCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            modified: false,
        };
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
                let Expression::NumericLiteral(n) = inner else {
                    return None;
                };
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

    fn make_number_literal(&self, span: Span, value: i64) -> Expression<'a> {
        Expression::NumericLiteral(ArenaBox::new_in(
            NumericLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value: value as f64,
                raw: None,
                base: oxc_syntax::number::NumberBase::Decimal,
            },
            self.allocator,
        ))
    }

    fn make_boolean_literal(&self, span: Span, value: bool) -> Expression<'a> {
        Expression::BooleanLiteral(ArenaBox::new_in(
            BooleanLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                value,
            },
            self.allocator,
        ))
    }

    fn make_string_array(
        &self,
        span: Span,
        values: impl IntoIterator<Item = String>,
    ) -> Expression<'a> {
        let mut elements = ArenaVec::new_in(self.allocator);
        for value in values {
            elements.push(ArrayExpressionElement::from(
                self.make_string_literal(span, &value),
            ));
        }
        Expression::ArrayExpression(ArenaBox::new_in(
            ArrayExpression {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                elements,
            },
            self.allocator,
        ))
    }

    fn member_object_and_name<'b>(
        &'b self,
        call: &'b CallExpression<'a>,
    ) -> Option<(&'b Expression<'a>, &'b str)> {
        match self.unwrap_parens(&call.callee) {
            Expression::StaticMemberExpression(member) => {
                Some((&member.object, member.property.name.as_str()))
            }
            Expression::ComputedMemberExpression(member) => {
                let Expression::StringLiteral(property) = self.unwrap_parens(&member.expression)
                else {
                    return None;
                };
                Some((&member.object, property.value.as_str()))
            }
            _ => None,
        }
    }

    fn resolve_split(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let (object, property) = self.member_object_and_name(call)?;
        if property != "split" || call.arguments.len() > 2 {
            return None;
        }
        let Expression::StringLiteral(value) = self.unwrap_parens(object) else {
            return None;
        };
        let limit = match call.arguments.get(1) {
            Some(argument) => {
                let limit = self.as_integer_literal(argument.as_expression()?)?;
                u32::try_from(limit).ok()? as usize
            }
            None => u32::MAX as usize,
        };
        if limit == 0 {
            return Some(self.make_string_array(call.span, std::iter::empty()));
        }
        let Some(separator_argument) = call.arguments.first() else {
            return Some(self.make_string_array(call.span, [value.value.to_string()]));
        };
        let Expression::StringLiteral(separator) =
            self.unwrap_parens(separator_argument.as_expression()?)
        else {
            return None;
        };
        let text = value.value.as_str();
        let separator = separator.value.as_str();
        let parts = if separator.is_empty() {
            let units: Vec<u16> = text.encode_utf16().take(limit).collect();
            if units.iter().any(|unit| (0xD800..=0xDFFF).contains(unit)) {
                return None;
            }
            units
                .into_iter()
                .map(|unit| char::from_u32(u32::from(unit)).unwrap().to_string())
                .collect::<Vec<_>>()
        } else {
            text.split(separator)
                .take(limit)
                .map(str::to_string)
                .collect::<Vec<_>>()
        };
        Some(self.make_string_array(call.span, parts))
    }

    fn resolve_join(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let (object, property) = self.member_object_and_name(call)?;
        if property != "join" || call.arguments.len() > 1 {
            return None;
        }
        let Expression::ArrayExpression(array) = self.unwrap_parens(object) else {
            return None;
        };
        let separator = match call.arguments.first() {
            Some(argument) => {
                let Expression::StringLiteral(separator) =
                    self.unwrap_parens(argument.as_expression()?)
                else {
                    return None;
                };
                separator.value.as_str()
            }
            None => ",",
        };
        let mut values = Vec::with_capacity(array.elements.len());
        for element in &array.elements {
            let Expression::StringLiteral(value) = self.unwrap_parens(element.as_expression()?)
            else {
                return None;
            };
            values.push(value.value.as_str());
        }
        Some(self.make_string_literal(call.span, &values.join(separator)))
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
        let Expression::StringLiteral(s) = object else {
            return None;
        };

        let idx = match call.arguments.first() {
            Some(arg) => self.as_integer_literal(arg.as_expression()?)?,
            None => 0,
        };
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

    fn resolve_char_code_at(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
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
        if prop != "charCodeAt" {
            return None;
        }
        let Expression::StringLiteral(s) = self.unwrap_parens(object) else {
            return None;
        };
        let index = match call.arguments.first() {
            Some(arg) => self.as_integer_literal(arg.as_expression()?)?,
            None => 0,
        };
        if index < 0 {
            return None;
        }
        let unit = *s
            .value
            .encode_utf16()
            .collect::<Vec<_>>()
            .get(index as usize)?;
        Some(Expression::NumericLiteral(ArenaBox::new_in(
            NumericLiteral {
                node_id: Cell::new(NodeId::DUMMY),
                span: call.span,
                value: f64::from(unit),
                raw: None,
                base: oxc_syntax::number::NumberBase::Decimal,
            },
            self.allocator,
        )))
    }

    fn resolve_slice(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
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
        if !matches!(prop, "slice" | "substring" | "substr") || call.arguments.len() > 2 {
            return None;
        }
        let Expression::StringLiteral(s) = self.unwrap_parens(object) else {
            return None;
        };
        let start = match call.arguments.first() {
            Some(arg) => self.as_integer_literal(arg.as_expression()?)?,
            None => 0,
        };
        let end_or_length = match call.arguments.get(1) {
            Some(arg) => Some(self.as_integer_literal(arg.as_expression()?)?),
            None => None,
        };
        let units: Vec<u16> = s.value.encode_utf16().collect();
        let len = units.len() as i64;
        let clamp = |index: i64| index.clamp(0, len) as usize;
        let (start, end) = match prop {
            "slice" => {
                let start = if start < 0 { len + start } else { start };
                let end = end_or_length.map_or(len, |end| if end < 0 { len + end } else { end });
                (clamp(start), clamp(end))
            }
            "substring" => {
                let start = clamp(start);
                let end = clamp(end_or_length.unwrap_or(len));
                (start.min(end), start.max(end))
            }
            "substr" => {
                let start = if start < 0 { len + start } else { start };
                let start = clamp(start);
                let end = end_or_length.map_or(len, |length| {
                    if length <= 0 {
                        start as i64
                    } else {
                        start as i64 + length
                    }
                });
                (start, clamp(end))
            }
            _ => return None,
        };
        let text = if start >= end {
            String::new()
        } else {
            String::from_utf16(&units[start..end]).ok()?
        };
        Some(self.make_string_literal(call.span, &text))
    }

    fn resolve_search(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let callee = self.unwrap_parens(&call.callee);
        let (object, prop) = match callee {
            Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(prop) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                (&m.object, prop.value.as_str())
            }
            _ => return None,
        };
        if !matches!(
            prop,
            "indexOf" | "lastIndexOf" | "includes" | "startsWith" | "endsWith"
        ) || call.arguments.is_empty()
            || call.arguments.len() > 2
        {
            return None;
        }
        let Expression::StringLiteral(haystack) = self.unwrap_parens(object) else {
            return None;
        };
        let Expression::StringLiteral(needle) =
            self.unwrap_parens(call.arguments[0].as_expression()?)
        else {
            return None;
        };
        let haystack: Vec<u16> = haystack.value.encode_utf16().collect();
        let needle: Vec<u16> = needle.value.encode_utf16().collect();
        let len = haystack.len();
        let supplied_position = match call.arguments.get(1) {
            Some(arg) => Some(self.as_integer_literal(arg.as_expression()?)?),
            None => None,
        };
        let clamp = |position: i64| position.clamp(0, len as i64) as usize;

        match prop {
            "indexOf" | "includes" => {
                let start = clamp(supplied_position.unwrap_or(0));
                let found = if needle.is_empty() {
                    Some(start)
                } else {
                    haystack[start..]
                        .windows(needle.len())
                        .position(|window| window == needle)
                        .map(|index| start + index)
                };
                if prop == "includes" {
                    Some(self.make_boolean_literal(call.span, found.is_some()))
                } else {
                    Some(
                        self.make_number_literal(call.span, found.map_or(-1, |index| index as i64)),
                    )
                }
            }
            "lastIndexOf" => {
                let position = clamp(supplied_position.unwrap_or(len as i64));
                let found = if needle.is_empty() {
                    Some(position)
                } else if needle.len() > len {
                    None
                } else {
                    let last_start = position.min(len - needle.len());
                    (0..=last_start)
                        .rev()
                        .find(|&index| haystack[index..index + needle.len()] == needle)
                };
                Some(self.make_number_literal(call.span, found.map_or(-1, |index| index as i64)))
            }
            "startsWith" => {
                let start = clamp(supplied_position.unwrap_or(0));
                let matches = haystack
                    .get(start..start.saturating_add(needle.len()))
                    .is_some_and(|slice| slice == needle);
                Some(self.make_boolean_literal(call.span, matches))
            }
            "endsWith" => {
                let end = clamp(supplied_position.unwrap_or(len as i64));
                let matches = end
                    .checked_sub(needle.len())
                    .and_then(|start| haystack.get(start..end))
                    .is_some_and(|slice| slice == needle);
                Some(self.make_boolean_literal(call.span, matches))
            }
            _ => None,
        }
    }

    fn resolve_concat(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let callee = self.unwrap_parens(&call.callee);
        let (object, prop) = match callee {
            Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(prop) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                (&m.object, prop.value.as_str())
            }
            _ => return None,
        };
        if prop != "concat" {
            return None;
        }
        let Expression::StringLiteral(base) = self.unwrap_parens(object) else {
            return None;
        };
        let mut result = base.value.to_string();
        for arg in &call.arguments {
            let Expression::StringLiteral(value) = self.unwrap_parens(arg.as_expression()?) else {
                return None;
            };
            result.push_str(value.value.as_str());
        }
        Some(self.make_string_literal(call.span, &result))
    }

    fn resolve_repeat(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let callee = self.unwrap_parens(&call.callee);
        let (object, prop) = match callee {
            Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(prop) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                (&m.object, prop.value.as_str())
            }
            _ => return None,
        };
        if prop != "repeat" || call.arguments.len() != 1 {
            return None;
        }
        let Expression::StringLiteral(value) = self.unwrap_parens(object) else {
            return None;
        };
        let count = self.as_integer_literal(call.arguments[0].as_expression()?)?;
        if !(0..=100_000).contains(&count)
            || value.value.len().checked_mul(count as usize)? > 1_000_000
        {
            return None;
        }
        Some(self.make_string_literal(call.span, &value.value.repeat(count as usize)))
    }

    fn resolve_from_code_point(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
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
                let Expression::StringLiteral(prop) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                (id.name.as_str(), prop.value.as_str())
            }
            _ => return None,
        };
        if ident != "String" || prop != "fromCodePoint" {
            return None;
        }
        let mut result = String::new();
        for arg in &call.arguments {
            let value = u32::try_from(self.as_integer_literal(arg.as_expression()?)?).ok()?;
            result.push(char::from_u32(value)?);
        }
        Some(self.make_string_literal(call.span, &result))
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
            if let Some(repl) = self
                .resolve_char_at(call)
                .or_else(|| self.resolve_char_code_at(call))
                .or_else(|| self.resolve_split(call))
                .or_else(|| self.resolve_join(call))
                .or_else(|| self.resolve_slice(call))
                .or_else(|| self.resolve_search(call))
                .or_else(|| self.resolve_concat(call))
                .or_else(|| self.resolve_repeat(call))
                .or_else(|| self.resolve_from_char_code(call))
                .or_else(|| self.resolve_from_code_point(call))
            {
                *it = repl;
                self.modified = true;
            }
        }
    }
}
