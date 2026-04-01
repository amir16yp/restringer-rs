use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct RemoveRedundantBlockStatements;

impl Transform for RemoveRedundantBlockStatements {
    fn name(&self) -> &'static str {
        "removeRedundantBlockStatements"
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
    fn flatten_statements(&mut self, body: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        let mut new_body = oxc_allocator::Vec::new_in(self.allocator);
        for stmt in body.drain(..) {
            match stmt {
                Statement::BlockStatement(block) => {
                    self.modified = true;
                    // If nested block has 1 statement, unwrap it.
                    if block.body.len() == 1 {
                        new_body.push(block.body[0].clone_in(self.allocator));
                    } else {
                        for s in &block.body {
                            new_body.push(s.clone_in(self.allocator));
                        }
                    }
                }
                other => new_body.push(other),
            }
        }
        *body = new_body;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.flatten_statements(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.flatten_statements(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
