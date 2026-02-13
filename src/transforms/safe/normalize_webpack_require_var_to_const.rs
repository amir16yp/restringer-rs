use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct NormalizeWebpackRequireVarToConst;

impl Transform for NormalizeWebpackRequireVarToConst {
    fn name(&self) -> &'static str {
        "normalizeWebpackRequireVarToConst"
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

    fn decl_is_simple_webpack_require(&self, decl: &VariableDeclarator<'a>) -> bool {
        let BindingPattern::BindingIdentifier(_) = &decl.id else {
            return false;
        };
        let Some(init) = decl.init.as_ref() else {
            return false;
        };
        let Expression::CallExpression(call) = self.unwrap_parens(init) else {
            return false;
        };
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else {
            return false;
        };
        if callee_id.name.as_str() != "__webpack_require__" {
            return false;
        }
        if call.arguments.len() != 1 {
            return false;
        }
        let Some(arg0) = call.arguments[0].as_expression() else {
            return false;
        };
        matches!(self.unwrap_parens(arg0), Expression::NumericLiteral(_))
    }

    fn maybe_rewrite_var_decl_stmt(&mut self, stmt: &mut Statement<'a>) {
        let Statement::VariableDeclaration(vd) = stmt else {
            return;
        };
        if vd.kind != VariableDeclarationKind::Var {
            return;
        }
        if vd.declarations.is_empty() {
            return;
        }
        if !vd.declarations.iter().all(|d| self.decl_is_simple_webpack_require(d)) {
            return;
        }

        let mut new_vd = vd.as_ref().clone_in(self.allocator);
        new_vd.kind = VariableDeclarationKind::Const;
        *stmt = Statement::VariableDeclaration(ArenaBox::new_in(new_vd, self.allocator));
        self.modified = true;
    }

    fn rewrite_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        for stmt in stmts.iter_mut() {
            self.maybe_rewrite_var_decl_stmt(stmt);
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.rewrite_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.rewrite_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.rewrite_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
