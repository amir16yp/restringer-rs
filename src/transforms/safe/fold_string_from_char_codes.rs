use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::{GetSpan, Span};

use crate::{Transform, TransformCtx};

pub struct FoldStringFromCharCodes;

impl Transform for FoldStringFromCharCodes {
    fn name(&self) -> &'static str {
        "foldStringFromCharCodes"
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

    fn make_string_lit(&self, span: Span, value: &str) -> Expression<'a> {
        let s = self.allocator.alloc_str(value);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral { span, value: s.into(), raw: None, lone_surrogates: false },
            self.allocator,
        ))
    }

    fn is_string_from_char_code_callee(&self, callee: &Expression<'a>) -> bool {
        let Expression::StaticMemberExpression(mem) = self.unwrap_parens(callee) else {
            return false;
        };
        let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) else {
            return false;
        };
        obj.name.as_str() == "String" && mem.property.name.as_str() == "fromCharCode"
    }

    fn numeric_literal_to_u16(&self, expr: &Expression<'a>) -> Option<u16> {
        let Expression::NumericLiteral(n) = self.unwrap_parens(expr) else {
            return None;
        };
        let v = n.value;
        if !v.is_finite() || v.fract() != 0.0 {
            return None;
        }
        // JS ToUint16 for Number.
        let mut i = v as i64;
        i = i.rem_euclid(65536);
        Some(i as u16)
    }

    fn collect_args_as_char_codes(&self, args: &ArenaVec<'a, Argument<'a>>) -> Option<Vec<u16>> {
        let mut out: Vec<u16> = Vec::with_capacity(args.len());
        for a in args {
            let expr = a.as_expression()?;
            out.push(self.numeric_literal_to_u16(expr)?);
        }
        Some(out)
    }

    fn collect_array_expr_numbers(&self, arr: &ArrayExpression<'a>) -> Option<Vec<u16>> {
        let mut out: Vec<u16> = Vec::with_capacity(arr.elements.len());
        for el in &arr.elements {
            match el {
                ArrayExpressionElement::Elision(_) => return None,
                ArrayExpressionElement::SpreadElement(_) => return None,
                _ => {
                    let expr = el.as_expression()?;
                    out.push(self.numeric_literal_to_u16(expr)?);
                }
            }
        }
        Some(out)
    }

    fn try_fold_direct(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if call.optional {
            return None;
        }
        if !self.is_string_from_char_code_callee(&call.callee) {
            return None;
        }
        let codes = self.collect_args_as_char_codes(&call.arguments)?;
        let s: String = codes.into_iter().map(|c| char::from_u32(c as u32).unwrap_or('\u{FFFD}')).collect();
        Some(self.make_string_lit(call.span, &s))
    }

    fn try_fold_apply(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if call.optional {
            return None;
        }

        // String.fromCharCode.apply(String, [..])
        let Expression::StaticMemberExpression(mem) = self.unwrap_parens(&call.callee) else {
            return None;
        };
        if mem.property.name.as_str() != "apply" {
            return None;
        }
        if !self.is_string_from_char_code_callee(&mem.object) {
            return None;
        }
        if call.arguments.len() != 2 {
            return None;
        }

        let ctx0 = call.arguments.first()?.as_expression()?;
        let Expression::Identifier(id) = self.unwrap_parens(ctx0) else {
            return None;
        };
        if id.name.as_str() != "String" {
            return None;
        }

        let arg1 = call.arguments.get(1)?.as_expression()?;
        let Expression::ArrayExpression(arr) = self.unwrap_parens(arg1) else {
            return None;
        };
        let codes = self.collect_array_expr_numbers(arr)?;
        let s: String = codes.into_iter().map(|c| char::from_u32(c as u32).unwrap_or('\u{FFFD}')).collect();
        Some(self.make_string_lit(call.span, &s))
    }

    fn try_fold_spread(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if call.optional {
            return None;
        }
        if !self.is_string_from_char_code_callee(&call.callee) {
            return None;
        }
        if call.arguments.len() != 1 {
            return None;
        }
        let arg0 = call.arguments.first()?;
        let Argument::SpreadElement(spread) = arg0 else {
            return None;
        };
        let Expression::ArrayExpression(arr) = self.unwrap_parens(&spread.argument) else {
            return None;
        };
        let codes = self.collect_array_expr_numbers(arr)?;
        let s: String = codes.into_iter().map(|c| char::from_u32(c as u32).unwrap_or('\u{FFFD}')).collect();
        Some(self.make_string_lit(call.span, &s))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Post-order: fold children first.
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        let Expression::CallExpression(call) = it else {
            return;
        };

        if let Some(repl) = self.try_fold_direct(call)
            .or_else(|| self.try_fold_spread(call))
            .or_else(|| self.try_fold_apply(call))
        {
            *it = repl;
            self.modified = true;
        }
    }

    fn visit_argument(&mut self, it: &mut Argument<'a>) {
        // Ensure we still traverse spread elements.
        match it {
            Argument::SpreadElement(s) => {
                let mut arg = s.argument.clone_in(self.allocator);
                self.visit_expression(&mut arg);
                s.argument = arg;
            }
            _ => {
                oxc_ast_visit::walk_mut::walk_argument(self, it);
            }
        }
    }
}
