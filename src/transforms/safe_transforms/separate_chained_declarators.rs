use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SeparateChainedDeclarators;

impl Transform for SeparateChainedDeclarators {
    fn name(&self) -> &'static str {
        "separateChainedDeclarators"
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
    fn split_var_decl_stmt(&mut self, decl: &VariableDeclaration<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if decl.declarations.len() <= 1 {
            out.push(Statement::VariableDeclaration(ArenaBox::new_in(
                decl.clone_in(self.allocator),
                self.allocator,
            )));
            return;
        }

        self.modified = true;
        for d in &decl.declarations {
            let mut one = decl.clone_in(self.allocator);
            one.declarations = ArenaVec::new_in(self.allocator);
            one.declarations.push(d.clone_in(self.allocator));
            out.push(Statement::VariableDeclaration(ArenaBox::new_in(one, self.allocator)));
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let original = std::mem::replace(&mut it.body, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);

        for stmt in original {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    self.split_var_decl_stmt(&var_decl, &mut new_body);
                }
                other => new_body.push(other),
            }
        }

        it.body = new_body;
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let original = std::mem::replace(&mut it.body, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);

        for stmt in original {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    self.split_var_decl_stmt(&var_decl, &mut new_body);
                }
                other => new_body.push(other),
            }
        }

        it.body = new_body;
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
