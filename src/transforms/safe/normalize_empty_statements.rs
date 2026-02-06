use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct NormalizeEmptyStatements;

impl Transform for NormalizeEmptyStatements {
    fn name(&self) -> &'static str {
        "normalizeEmptyStatements"
    }

    fn run<'a>(&self, _ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor {
    modified: bool,
}

impl<'a> VisitMut<'a> for Visitor {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let before = it.body.len();
        it.body.retain(|stmt| !matches!(stmt, Statement::EmptyStatement(_)));
        if it.body.len() != before {
            self.modified = true;
        }
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let before = it.statements.len();
        it.statements.retain(|stmt| !matches!(stmt, Statement::EmptyStatement(_)));
        if it.statements.len() != before {
            self.modified = true;
        }
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let before = it.body.len();
        it.body.retain(|stmt| !matches!(stmt, Statement::EmptyStatement(_)));
        if it.body.len() != before {
            self.modified = true;
        }
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_if_statement(&mut self, it: &mut IfStatement<'a>) {
        // Preserve EmptyStatement bodies (JS allows `if (cond);`)
        self.visit_expression(&mut it.test);
        self.visit_statement(&mut it.consequent);
        if let Some(alt) = it.alternate.as_mut() {
            self.visit_statement(alt);
        }
    }

    fn visit_for_statement(&mut self, it: &mut ForStatement<'a>) {
        // Preserve EmptyStatement body (JS allows `for(;;);`)
        if let Some(init) = it.init.as_mut() {
            self.visit_for_statement_init(init);
        }
        if let Some(test) = it.test.as_mut() {
            self.visit_expression(test);
        }
        if let Some(update) = it.update.as_mut() {
            self.visit_expression(update);
        }
        self.visit_statement(&mut it.body);
    }

    fn visit_for_in_statement(&mut self, it: &mut ForInStatement<'a>) {
        // Preserve EmptyStatement body
        self.visit_for_statement_left(&mut it.left);
        self.visit_expression(&mut it.right);
        self.visit_statement(&mut it.body);
    }

    fn visit_for_of_statement(&mut self, it: &mut ForOfStatement<'a>) {
        // Preserve EmptyStatement body
        self.visit_for_statement_left(&mut it.left);
        self.visit_expression(&mut it.right);
        self.visit_statement(&mut it.body);
    }

    fn visit_while_statement(&mut self, it: &mut WhileStatement<'a>) {
        // Preserve EmptyStatement body
        self.visit_expression(&mut it.test);
        self.visit_statement(&mut it.body);
    }

    fn visit_do_while_statement(&mut self, it: &mut DoWhileStatement<'a>) {
        // Preserve EmptyStatement body
        self.visit_statement(&mut it.body);
        self.visit_expression(&mut it.test);
    }
}
