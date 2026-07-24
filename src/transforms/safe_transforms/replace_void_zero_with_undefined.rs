use std::cell::Cell;

use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_syntax::node::NodeId;
use oxc_syntax::operator::UnaryOperator;

use crate::{Transform, TransformCtx};

pub struct ReplaceVoidZeroWithUndefined;

impl Transform for ReplaceVoidZeroWithUndefined {
    fn name(&self) -> &'static str {
        "replaceVoidZeroWithUndefined"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = UndefinedBindingCollector { found: false };
        collector.visit_program(program);
        if collector.found {
            return false;
        }

        let mut visitor = Visitor {
            allocator: ctx.allocator,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct UndefinedBindingCollector {
    found: bool,
}

impl<'a> Visit<'a> for UndefinedBindingCollector {
    fn visit_binding_identifier(&mut self, it: &BindingIdentifier<'a>) {
        if it.name.as_str() == "undefined" {
            self.found = true;
        }
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn make_undefined(&self, span: oxc_span::Span) -> Expression<'a> {
        let name = self.allocator.alloc_str("undefined");
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference {
                node_id: Cell::new(NodeId::DUMMY),
                span,
                name: name.into(),
                reference_id: None.into(),
            },
            self.allocator,
        ))
    }
}

fn is_zero(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::NumericLiteral(n) => n.value == 0.0,
        Expression::ParenthesizedExpression(p) => is_zero(&p.expression),
        _ => false,
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        if let Expression::UnaryExpression(un) = it {
            if un.operator == UnaryOperator::Void && is_zero(&un.argument) {
                *it = self.make_undefined(un.span);
                self.modified = true;
            }
        }
    }
}
