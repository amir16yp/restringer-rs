use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ReplaceFunctionShellsWithWrappedValueIIFE;

impl Transform for ReplaceFunctionShellsWithWrappedValueIIFE {
    fn name(&self) -> &'static str {
        "replaceFunctionShellsWithWrappedValueIIFE"
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

    fn returnable_from_iife(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if !call.arguments.is_empty() {
            return None;
        }

        let callee = self.unwrap_parens(&call.callee);
        let func = match callee {
            Expression::FunctionExpression(f) => &**f,
            _ => return None,
        };

        let body = func.body.as_ref()?;
        if body.statements.len() != 1 {
            return None;
        }

        let Statement::ReturnStatement(ret) = &body.statements[0] else { return None; };
        let arg = ret.argument.as_ref()?;

        match arg {
            Expression::Identifier(_) |
            Expression::StringLiteral(_) |
            Expression::NumericLiteral(_) |
            Expression::BooleanLiteral(_) |
            Expression::NullLiteral(_) |
            Expression::BigIntLiteral(_) |
            Expression::RegExpLiteral(_) => Some(arg.clone_in(self.allocator)),
            _ => None,
        }
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            let mut replaced = false;
            if let Statement::ExpressionStatement(es) = &stmt {
                if let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) {
                    if let Some(rep) = self.returnable_from_iife(call) {
                        // Replace the entire call expression in-place.
                        let mut new_es = es.clone_in(self.allocator);
                        new_es.expression = rep;
                        out.push(Statement::ExpressionStatement(new_es));
                        self.modified = true;
                        replaced = true;
                    }
                }
            }

            if !replaced {
                out.push(stmt);
            }
        }

        *stmts = out;
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
