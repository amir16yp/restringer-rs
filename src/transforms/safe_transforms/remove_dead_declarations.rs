use std::collections::HashSet;

use oxc_allocator::{Allocator, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};

use crate::{Transform, TransformCtx};

pub struct RemoveDeadDeclarations;

impl Transform for RemoveDeadDeclarations {
    fn name(&self) -> &'static str {
        "removeDeadDeclarations"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = ReadCollector {
            reads: HashSet::new(),
        };
        collector.visit_program(program);
        let reads = collector.reads;

        let mut visitor = Visitor {
            allocator: ctx.allocator,
            modified: false,
            reads,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct ReadCollector {
    reads: HashSet<String>,
}

impl<'a> Visit<'a> for ReadCollector {
    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        self.reads.insert(it.name.to_string());
    }
}

struct Visitor<'a> {
    allocator: &'a Allocator,
    modified: bool,
    reads: HashSet<String>,
}

impl<'a> Visitor<'a> {
    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>, is_root: bool) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let has_eval = original.iter().any(|stmt| has_eval_call(stmt));
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in original {
            if !is_root && !has_eval && is_dead_declaration(&stmt, &self.reads) {
                self.modified = true;
                continue;
            }
            new_body.push(stmt);
        }
        *stmts = new_body;
    }
}

fn is_dead_declaration(stmt: &Statement<'_>, reads: &HashSet<String>) -> bool {
    match stmt {
        Statement::FunctionDeclaration(func) => func
            .id
            .as_ref()
            .map_or(false, |id| !reads.contains(id.name.as_str())),
        Statement::ClassDeclaration(class) => class
            .id
            .as_ref()
            .map_or(false, |id| !reads.contains(id.name.as_str())),
        _ => false,
    }
}

fn has_eval_call(stmt: &Statement<'_>) -> bool {
    struct EvalDetector {
        found: bool,
    }
    impl<'a> Visit<'a> for EvalDetector {
        fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
            if let Expression::Identifier(id) = &it.callee {
                if id.name.as_str() == "eval" {
                    self.found = true;
                    return;
                }
            }
            oxc_ast_visit::walk::walk_call_expression(self, it);
        }
    }
    let mut detector = EvalDetector { found: false };
    detector.visit_statement(stmt);
    detector.found
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.simplify_statement_list(&mut it.body, true);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.simplify_statement_list(&mut it.statements, false);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.simplify_statement_list(&mut it.body, false);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
