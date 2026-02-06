use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ReplaceBooleanExpressionsWithIf;

impl Transform for ReplaceBooleanExpressionsWithIf {
    fn name(&self) -> &'static str {
        "replaceBooleanExpressionsWithIf"
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

    fn transform_statement(&mut self, stmt: Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if let Statement::ExpressionStatement(expr_stmt) = &stmt {
            if let Expression::LogicalExpression(logical) = &expr_stmt.expression {
                if matches!(logical.operator, oxc_syntax::operator::LogicalOperator::And | oxc_syntax::operator::LogicalOperator::Or)
                {
                    let test = match logical.operator {
                        oxc_syntax::operator::LogicalOperator::Or => self.invert_test(expr_stmt.span, &logical.left),
                        _ => logical.left.clone_in(self.allocator),
                    };

                    let right_stmt = Statement::ExpressionStatement(ArenaBox::new_in(
                        ExpressionStatement {
                            span: expr_stmt.span,
                            expression: logical.right.clone_in(self.allocator),
                        },
                        self.allocator,
                    ));

                    let mut body = ArenaVec::new_in(self.allocator);
                    body.push(right_stmt);

                    let consequent = Statement::BlockStatement(ArenaBox::new_in(
                        BlockStatement { span: expr_stmt.span, body, scope_id: Default::default() },
                        self.allocator,
                    ));

                    let if_stmt = Statement::IfStatement(ArenaBox::new_in(
                        IfStatement {
                            span: expr_stmt.span,
                            test,
                            consequent,
                            alternate: None,
                        },
                        self.allocator,
                    ));

                    out.push(if_stmt);
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
            self.transform_statement(stmt, &mut out);
        }
        *stmts = out;
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
}
