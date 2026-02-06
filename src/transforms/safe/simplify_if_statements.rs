use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyIfStatements;

impl Transform for SimplifyIfStatements {
    fn name(&self) -> &'static str {
        "simplifyIfStatements"
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

fn is_empty(stmt: &Statement<'_>) -> bool {
    matches!(stmt, Statement::EmptyStatement(_))
        || matches!(stmt, Statement::BlockStatement(block) if block.body.is_empty())
}

impl<'a> Visitor<'a> {
    fn invert_test(&self, span: oxc_span::Span, test: &Expression<'a>) -> Expression<'a> {
        Expression::UnaryExpression(ArenaBox::new_in(
            UnaryExpression {
                span,
                operator: oxc_syntax::operator::UnaryOperator::LogicalNot,
                argument: test.clone_in(self.allocator),
            },
            self.allocator,
        ))
    }

    fn simplify_statement(&mut self, stmt: Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if let Statement::IfStatement(if_stmt) = &stmt {
            let consequent_empty = is_empty(&if_stmt.consequent);
            let alternate_empty = if_stmt.alternate.as_ref().is_none_or(is_empty);

            if consequent_empty {
                if alternate_empty {
                    if if_stmt.alternate.is_some() {
                        out.push(stmt);
                        return;
                    }
                    // if (test) ; else ;  => test;
                    out.push(Statement::ExpressionStatement(ArenaBox::new_in(
                        ExpressionStatement {
                            span: if_stmt.span,
                            expression: if_stmt.test.clone_in(self.allocator),
                        },
                        self.allocator,
                    )));
                    self.modified = true;
                    return;
                }

                // if (test) ; else X  => if (!test) X
                let mut new_if = (**if_stmt).clone_in(self.allocator);
                new_if.test = self.invert_test(if_stmt.span, &if_stmt.test);
                new_if.consequent = new_if.alternate.take().unwrap();
                new_if.alternate = None;
                out.push(Statement::IfStatement(ArenaBox::new_in(new_if, self.allocator)));
                self.modified = true;
                return;
            }

            if let Some(alt) = if_stmt.alternate.as_ref() {
                if is_empty(alt) {
                    // if (test) X else ;  => if (test) X
                    let mut new_if = (**if_stmt).clone_in(self.allocator);
                    new_if.alternate = None;
                    out.push(Statement::IfStatement(ArenaBox::new_in(new_if, self.allocator)));
                    self.modified = true;
                    return;
                }
            }
        }

        out.push(stmt);
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in original {
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
