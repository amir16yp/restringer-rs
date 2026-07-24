use std::cell::Cell;

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::{GetSpan, Span};
use oxc_syntax::node::NodeId;

use crate::{Transform, TransformCtx};

pub struct ResolveObfuscatorIoProtection;

impl ResolveObfuscatorIoProtection {
    pub fn new() -> Self {
        Self
    }

    const BYPASS_STRING: &'static str = "function () {return \"bypassed!\"}";
}

impl Default for ResolveObfuscatorIoProtection {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveObfuscatorIoProtection {
    fn name(&self) -> &'static str {
        "resolveObfuscatorIoProtection"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = ObfuscatorIoVisitor {
            allocator: ctx.allocator,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct ObfuscatorIoVisitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> ObfuscatorIoVisitor<'a> {
    fn bypass_literal(&self, span: Span) -> Expression<'a> {
        let value = self.allocator.alloc_str(ResolveObfuscatorIoProtection::BYPASS_STRING);
        Expression::StringLiteral(oxc_allocator::Box::new_in(
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
}

impl<'a> VisitMut<'a> for ObfuscatorIoVisitor<'a> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        // Replace any function expression that solely returns 'newState' with the bypass string.
        let should_replace = match expr {
            Expression::FunctionExpression(func) => function_returns_new_state(func),
            Expression::ArrowFunctionExpression(arrow) => arrow_returns_new_state(arrow),
            _ => false,
        };

        if should_replace {
            let replacement = self.bypass_literal(expr.span()).clone_in(self.allocator);
            *expr = replacement;
            self.modified = true;
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }

    fn visit_object_property(&mut self, prop: &mut ObjectProperty<'a>) {
        let is_remove_cookie_key = match &prop.key {
            PropertyKey::StringLiteral(s) => s.value == "removeCookie",
            _ => false,
        };

        if is_remove_cookie_key
            && matches!(
                prop.value,
                Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
            )
        {
            let replacement = self.bypass_literal(prop.value.span()).clone_in(self.allocator);
            prop.value = replacement;
            self.modified = true;
            return;
        }

        self.visit_expression(&mut prop.value);
    }
}

fn function_returns_new_state(func: &Function<'_>) -> bool {
    let Some(body) = &func.body else {
        return false;
    };
    body.statements.iter().any(|stmt| match stmt {
        Statement::ReturnStatement(ret) => match &ret.argument {
            Some(Expression::StringLiteral(s)) => s.value == "newState",
            _ => false,
        },
        Statement::ExpressionStatement(es) => match &es.expression {
            Expression::StringLiteral(s) => s.value == "newState",
            _ => false,
        },
        _ => false,
    })
}

fn arrow_returns_new_state(arrow: &ArrowFunctionExpression<'_>) -> bool {
    arrow.body.statements.iter().any(|stmt| match stmt {
        Statement::ReturnStatement(ret) => match &ret.argument {
            Some(Expression::StringLiteral(s)) => s.value == "newState",
            _ => false,
        },
        Statement::ExpressionStatement(es) => match &es.expression {
            Expression::StringLiteral(s) => s.value == "newState",
            _ => false,
        },
        _ => false,
    })
}
