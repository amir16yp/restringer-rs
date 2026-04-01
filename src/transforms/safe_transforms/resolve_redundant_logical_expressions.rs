use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ResolveRedundantLogicalExpressions;

impl Transform for ResolveRedundantLogicalExpressions {
    fn name(&self) -> &'static str {
        "resolveRedundantLogicalExpressions"
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

fn truthiness(expr: &Expression<'_>) -> Option<bool> {
    match expr {
        Expression::BooleanLiteral(lit) => Some(lit.value),
        Expression::NullLiteral(_) => Some(false),
        Expression::NumericLiteral(lit) => Some(lit.value != 0.0),
        Expression::BigIntLiteral(lit) => {
            let raw = lit.raw.as_ref()?;
            let s = raw.as_str();
            Some(s != "0" && s != "0n")
        }
        Expression::StringLiteral(lit) => Some(!lit.value.as_str().is_empty()),
        Expression::RegExpLiteral(_) => Some(true),
        Expression::ArrayExpression(_) => Some(true),
        Expression::ObjectExpression(_) => Some(true),
        Expression::FunctionExpression(_) => Some(true),
        Expression::ArrowFunctionExpression(_) => Some(true),
        Expression::Identifier(ident) => {
            // Treat global `undefined` as falsy.
            if ident.name.as_str() == "undefined" {
                Some(false)
            } else {
                None
            }
        }
        _ => None,
    }
}

impl<'a> Visitor<'a> {
    fn simplify_logical(&self, expr: &LogicalExpression<'a>) -> Option<Expression<'a>> {
        let left_truthy = truthiness(&expr.left);
        let right_truthy = truthiness(&expr.right);

        match expr.operator {
            LogicalOperator::And => {
                // truthy left => right, falsy left => left
                if let Some(t) = left_truthy {
                    return Some(if t {
                        expr.right.clone_in(self.allocator)
                    } else {
                        expr.left.clone_in(self.allocator)
                    });
                }
                // truthy right => left, falsy right => right
                if let Some(t) = right_truthy {
                    return Some(if t {
                        expr.left.clone_in(self.allocator)
                    } else {
                        expr.right.clone_in(self.allocator)
                    });
                }
            }
            LogicalOperator::Or => {
                // truthy left => left, falsy left => right
                if let Some(t) = left_truthy {
                    return Some(if t {
                        expr.left.clone_in(self.allocator)
                    } else {
                        expr.right.clone_in(self.allocator)
                    });
                }
                // truthy right => right, falsy right => left
                if let Some(t) = right_truthy {
                    return Some(if t {
                        expr.right.clone_in(self.allocator)
                    } else {
                        expr.left.clone_in(self.allocator)
                    });
                }
            }
            _ => {}
        }

        None
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_if_statement(&mut self, it: &mut IfStatement<'a>) {
        if let Expression::LogicalExpression(logical) = &it.test {
            if matches!(logical.operator, LogicalOperator::And | LogicalOperator::Or) {
                if let Some(simplified) = self.simplify_logical(logical) {
                    it.test = simplified;
                    self.modified = true;
                }
            }
        }
        oxc_ast_visit::walk_mut::walk_if_statement(self, it);
    }
}
