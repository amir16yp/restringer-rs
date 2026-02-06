use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ReplaceSequencesWithExpressions;

impl Transform for ReplaceSequencesWithExpressions {
    fn name(&self) -> &'static str {
        "replaceSequencesWithExpressions"
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
    fn seq_to_expression_statements(
        &self,
        span: oxc_span::Span,
        seq: &SequenceExpression<'a>,
    ) -> ArenaVec<'a, Statement<'a>> {
        let mut out = ArenaVec::new_in(self.allocator);
        for expr in &seq.expressions {
            out.push(Statement::ExpressionStatement(ArenaBox::new_in(
                ExpressionStatement { span, expression: expr.clone_in(self.allocator) },
                self.allocator,
            )));
        }
        out
    }

    fn transform_statement_in_list(&mut self, stmt: Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if let Statement::ExpressionStatement(expr_stmt) = &stmt {
            if let Expression::SequenceExpression(seq) = &expr_stmt.expression {
                if seq.expressions.len() > 1 {
                    let expanded = self.seq_to_expression_statements(expr_stmt.span, seq);
                    for s in expanded {
                        out.push(s);
                    }
                    self.modified = true;
                    return;
                }
            }
        }

        out.push(stmt);
    }

    fn transform_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);
        for stmt in original {
            self.transform_statement_in_list(stmt, &mut out);
        }
        *stmts = out;
    }

    fn maybe_wrap_sequence_statement(&mut self, stmt: &mut Statement<'a>) {
        let Statement::ExpressionStatement(expr_stmt) = stmt else { return; };
        let Expression::SequenceExpression(seq) = &expr_stmt.expression else { return; };
        if seq.expressions.len() <= 1 {
            return;
        }

        let body = self.seq_to_expression_statements(expr_stmt.span, seq);
        *stmt = Statement::BlockStatement(ArenaBox::new_in(
            BlockStatement { span: expr_stmt.span, body, scope_id: Default::default() },
            self.allocator,
        ));
        self.modified = true;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.transform_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_if_statement(&mut self, it: &mut IfStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.consequent);
        if let Some(alt) = it.alternate.as_mut() {
            self.maybe_wrap_sequence_statement(alt);
        }
        oxc_ast_visit::walk_mut::walk_if_statement(self, it);
    }

    fn visit_while_statement(&mut self, it: &mut WhileStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.body);
        oxc_ast_visit::walk_mut::walk_while_statement(self, it);
    }

    fn visit_do_while_statement(&mut self, it: &mut DoWhileStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.body);
        oxc_ast_visit::walk_mut::walk_do_while_statement(self, it);
    }

    fn visit_for_statement(&mut self, it: &mut ForStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.body);
        oxc_ast_visit::walk_mut::walk_for_statement(self, it);
    }

    fn visit_for_in_statement(&mut self, it: &mut ForInStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.body);
        oxc_ast_visit::walk_mut::walk_for_in_statement(self, it);
    }

    fn visit_for_of_statement(&mut self, it: &mut ForOfStatement<'a>) {
        self.maybe_wrap_sequence_statement(&mut it.body);
        oxc_ast_visit::walk_mut::walk_for_of_statement(self, it);
    }
}
