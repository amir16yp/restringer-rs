use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ResolveDeterministicIfStatements;

impl Transform for ResolveDeterministicIfStatements {
    fn name(&self) -> &'static str {
        "resolveDeterministicIfStatements"
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

fn is_nan(n: f64) -> bool {
    n != n
}

fn truthiness_literal(expr: &Expression<'_>) -> Option<bool> {
    match expr {
        Expression::BooleanLiteral(lit) => Some(lit.value),
        Expression::NullLiteral(_) => Some(false),
        Expression::NumericLiteral(lit) => {
            if is_nan(lit.value) {
                Some(false)
            } else {
                Some(lit.value != 0.0)
            }
        }
        Expression::BigIntLiteral(lit) => {
            let raw = lit.raw.as_ref()?;
            let s = raw.as_str();
            Some(s != "0" && s != "0n")
        }
        Expression::StringLiteral(lit) => Some(!lit.value.as_str().is_empty()),
        Expression::Identifier(ident) => {
            if ident.name.as_str() == "undefined" {
                Some(false)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn eval_unary(op: oxc_syntax::operator::UnaryOperator, arg: &Expression<'_>) -> Option<bool> {
    use oxc_syntax::operator::UnaryOperator;

    // Only evaluate if the argument is a literal we can reason about.
    match op {
        UnaryOperator::LogicalNot => truthiness_literal(arg).map(|t| !t),
        UnaryOperator::UnaryPlus => match arg {
            Expression::NumericLiteral(_) | Expression::BigIntLiteral(_) => truthiness_literal(arg),
            _ => None,
        },
        UnaryOperator::UnaryNegation => match arg {
            Expression::NumericLiteral(lit) => {
                let v = -lit.value;
                if is_nan(v) {
                    Some(false)
                } else {
                    Some(v != 0.0)
                }
            }
            _ => None,
        },
        UnaryOperator::BitwiseNot => match arg {
            Expression::NumericLiteral(lit) => {
                // JS ToInt32 semantics are approximated here.
                let i = lit.value as i32;
                let v = !i;
                Some(v != 0)
            }
            _ => None,
        },
        _ => None,
    }
}

fn deterministic_truthiness(expr: &Expression<'_>) -> Option<bool> {
    if let Some(t) = truthiness_literal(expr) {
        return Some(t);
    }

    if let Expression::UnaryExpression(un) = expr {
        return eval_unary(un.operator, &un.argument);
    }

    None
}

impl<'a> Visitor<'a> {
    fn simplify_statement(&mut self, stmt: &Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if let Statement::IfStatement(if_stmt) = stmt {
            if let Some(test_truthy) = deterministic_truthiness(&if_stmt.test) {
                let replacement = if test_truthy {
                    Some(if_stmt.consequent.clone_in(self.allocator))
                } else {
                    if_stmt.alternate.as_ref().map(|a| a.clone_in(self.allocator))
                };

                if let Some(rep) = replacement {
                    out.push(rep);
                }

                self.modified = true;
                return;
            }
        }

        out.push(stmt.clone_in(self.allocator));
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = stmts.clone_in(self.allocator);
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in &original {
            self.simplify_statement(stmt, &mut new_body);
        }
        *stmts = new_body;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.simplify_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
