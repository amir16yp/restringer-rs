use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use crate::{Transform, TransformCtx};

pub struct RearrangeSequences;

impl Transform for RearrangeSequences {
    fn name(&self) -> &'static str {
        "rearrangeSequences"
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
    fn split_sequence_to_statements(&self, span: Span, seq: &SequenceExpression<'a>) -> (ArenaVec<'a, Statement<'a>>, Expression<'a>) {
        let mut extracted = ArenaVec::new_in(self.allocator);
        let last_idx = seq.expressions.len().saturating_sub(1);
        for (i, expr) in seq.expressions.iter().enumerate() {
            if i == last_idx {
                break;
            }
            extracted.push(Statement::ExpressionStatement(ArenaBox::new_in(
                ExpressionStatement { span, expression: expr.clone_in(self.allocator) },
                self.allocator,
            )));
        }
        let last_expr = seq
            .expressions
            .get(last_idx)
            .map(|e| e.clone_in(self.allocator))
            .unwrap_or_else(|| {
                Expression::NullLiteral(ArenaBox::new_in(NullLiteral { span }, self.allocator))
            });
        (extracted, last_expr)
    }

    fn transform_statement_in_list(&mut self, stmt: &Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        match stmt {
            Statement::ReturnStatement(ret) => {
                if let Some(Expression::SequenceExpression(seq)) = ret.argument.as_ref() {
                    let (mut extracted, last_expr) = self.split_sequence_to_statements(ret.span, seq);
                    for s in extracted.drain(..) {
                        out.push(s);
                    }
                    let mut new_ret = ret.clone_in(self.allocator);
                    new_ret.argument = Some(last_expr);
                    out.push(Statement::ReturnStatement(new_ret));
                    self.modified = true;
                    return;
                }
            }
            Statement::IfStatement(if_stmt) => {
                if let Expression::SequenceExpression(seq) = &if_stmt.test {
                    let (mut extracted, last_expr) = self.split_sequence_to_statements(if_stmt.span, seq);
                    for s in extracted.drain(..) {
                        out.push(s);
                    }
                    let mut new_if = if_stmt.clone_in(self.allocator);
                    new_if.test = last_expr;
                    out.push(Statement::IfStatement(new_if));
                    self.modified = true;
                    return;
                }
            }
            _ => {}
        }

        out.push(stmt.clone_in(self.allocator));
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let original = it.body.clone_in(self.allocator);
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in &original {
            self.transform_statement_in_list(stmt, &mut new_body);
        }
        it.body = new_body;
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let original = it.statements.clone_in(self.allocator);
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in &original {
            self.transform_statement_in_list(stmt, &mut new_body);
        }
        it.statements = new_body;
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let original = it.body.clone_in(self.allocator);
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in &original {
            self.transform_statement_in_list(stmt, &mut new_body);
        }
        it.body = new_body;
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
