use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

use crate::{Transform, TransformCtx};

pub struct ResolveFunctionConstructorCalls;

impl Transform for ResolveFunctionConstructorCalls {
    fn name(&self) -> &'static str {
        "resolveFunctionConstructorCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, source_type: ctx.source_type, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    source_type: SourceType,
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

    fn is_constructor_member<'b>(&self, callee: &'b Expression<'a>) -> Option<&'b Expression<'a>> {
        match self.unwrap_parens(callee) {
            Expression::StaticMemberExpression(m) => {
                if m.property.name.as_str() == "constructor" {
                    Some(&m.object)
                } else {
                    None
                }
            }
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(s) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                if s.value.as_str() == "constructor" {
                    Some(&m.object)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn literal_string(arg: &Argument<'a>) -> Option<&'a str> {
        let expr = arg.as_expression()?;
        match expr {
            Expression::StringLiteral(s) => Some(s.value.as_str()),
            _ => None,
        }
    }

    fn parse_function_expression(&self, params: &str, body: &str) -> Option<Expression<'a>> {
        let code = format!("(function ({}) {{{}}})", params, body);
        let parse_ret = Parser::new(self.allocator, &code, self.source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        if !parse_ret.errors.is_empty() {
            return None;
        }

        // Find the first expression statement: (function (...) {...})
        for stmt in &parse_ret.program.body {
            if let Statement::ExpressionStatement(es) = stmt {
                return Some(es.expression.clone_in(self.allocator));
            }
        }
        None
    }

    fn maybe_resolve(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let object = self.is_constructor_member(&call.callee)?;

        // Must be `Function.constructor(...)` or `Something.constructor(...)`? JS version doesn't constrain object.
        // Keep conservative: only handle `Function.constructor` or `(function(){}).constructor` etc.
        // We'll accept any object as long as it is the identifier `Function`.
        let Expression::Identifier(id) = self.unwrap_parens(object) else { return None; };
        if id.name.as_str() != "Function" {
            return None;
        }

        if call.arguments.is_empty() {
            return None;
        }

        // All args must be string literals.
        let mut values: Vec<&str> = Vec::with_capacity(call.arguments.len());
        for a in &call.arguments {
            values.push(Self::literal_string(a)?);
        }

        let body = values[values.len() - 1];
        let params = if values.len() > 1 { values[..values.len() - 1].join(", ") } else { String::new() };

        self.parse_function_expression(&params, body)
    }

    fn boxed_expression(&self, expr: Expression<'a>) -> Expression<'a> {
        // Ensure we own arena allocations properly when we replace nodes.
        match expr {
            Expression::CallExpression(c) => Expression::CallExpression(ArenaBox::new_in((*c).clone_in(self.allocator), self.allocator)),
            other => other,
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if let Some(rep) = self.maybe_resolve(call) {
                *it = self.boxed_expression(rep);
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
