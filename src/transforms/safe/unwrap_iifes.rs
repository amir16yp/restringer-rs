use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct UnwrapIIFEs;

impl Transform for UnwrapIIFEs {
    fn name(&self) -> &'static str {
        "unwrapIIFEs"
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
                Expression::ParenthesizedExpression(p) => {
                    expr = &p.expression;
                }
                _ => return expr,
            }
        }
    }

    fn is_anonymous_iife(&self, call: &CallExpression<'a>) -> bool {
        if !call.arguments.is_empty() {
            return false;
        }

        match self.unwrap_parens(&call.callee) {
            Expression::FunctionExpression(func) => func.id.is_none(),
            Expression::ArrowFunctionExpression(_) => true,
            _ => false,
        }
    }

    fn iife_return_expr(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if !self.is_anonymous_iife(call) {
            return None;
        }

        match self.unwrap_parens(&call.callee) {
            Expression::ArrowFunctionExpression(arrow) => {
                // oxc represents arrow bodies without a dedicated enum; handle the common safe case:
                // body contains a single return statement.
                let body = &arrow.body;
                if body.statements.len() != 1 {
                    return None;
                }
                let Statement::ReturnStatement(ret) = &body.statements[0] else { return None; };
                ret.argument.as_ref().map(|e| e.clone_in(self.allocator))
            }
            Expression::FunctionExpression(func) => {
                let body = func.body.as_ref()?;
                if body.statements.len() != 1 {
                    return None;
                }
                let Statement::ReturnStatement(ret) = &body.statements[0] else { return None; };
                ret.argument.as_ref().map(|e| e.clone_in(self.allocator))
            }
            _ => None,
        }
    }

    fn iife_statement_body(&self, call: &CallExpression<'a>) -> Option<ArenaVec<'a, Statement<'a>>> {
        if !self.is_anonymous_iife(call) {
            return None;
        }

        let body = match self.unwrap_parens(&call.callee) {
            Expression::FunctionExpression(func) => func.body.as_ref()?,
            Expression::ArrowFunctionExpression(arrow) => &arrow.body,
            _ => return None,
        };

        // Avoid introducing illegal `return` statements into surrounding statement list.
        if body.statements.iter().any(|s| matches!(s, Statement::ReturnStatement(_))) {
            return None;
        }

        let mut out = ArenaVec::new_in(self.allocator);
        for s in &body.statements {
            out.push(CloneIn::clone_in(s, self.allocator));
        }
        Some(out)
    }

    fn unwrap_iife_in_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            let mut replaced = false;
            if let Statement::ExpressionStatement(expr_stmt) = &stmt {
                if let Expression::CallExpression(call) = self.unwrap_parens(&expr_stmt.expression) {
                    if let Some(body) = self.iife_statement_body(call) {
                        for s in body {
                            out.push(s);
                        }
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
        self.unwrap_iife_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.unwrap_iife_in_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.unwrap_iife_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_variable_declarator(&mut self, it: &mut VariableDeclarator<'a>) {
        if let Some(init) = it.init.as_mut() {
            let mut target: &mut Expression<'a> = &mut *init;
            while let Expression::ParenthesizedExpression(p) = target {
                target = &mut p.expression;
            }
            if let Expression::CallExpression(call) = target {
                if let Some(expr) = self.iife_return_expr(call) {
                    *init = expr;
                    self.modified = true;
                    return;
                }
            }
        }
        oxc_ast_visit::walk_mut::walk_variable_declarator(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &mut AssignmentExpression<'a>) {
        if let Expression::CallExpression(call) = &it.right {
            if let Some(expr) = self.iife_return_expr(call) {
                it.right = expr;
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_assignment_expression(self, it);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // For cases like `const x = (function(){ return y; })();` inside expressions
        let mut target: &mut Expression<'a> = &mut *it;
        while let Expression::ParenthesizedExpression(p) = target {
            target = &mut p.expression;
        }
        if let Expression::CallExpression(call) = target {
            if let Some(expr) = self.iife_return_expr(call) {
                *it = expr;
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
