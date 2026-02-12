use oxc_allocator::{Allocator, Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

use crate::{Transform, TransformCtx};

pub struct ReplaceEvalCallsWithLiteralContent;

impl Transform for ReplaceEvalCallsWithLiteralContent {
    fn name(&self) -> &'static str {
        "replaceEvalCallsWithLiteralContent"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, source_type: ctx.source_type, modified: false };
        v.visit_program(program);
        v.modified
    }
}

enum Replacement<'a> {
    Expr(Expression<'a>),
    Stmt(Statement<'a>),
    Block(BlockStatement<'a>),
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    source_type: SourceType,
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

    fn is_eval_call(&self, call: &CallExpression<'a>) -> bool {
        let Expression::Identifier(id) = self.unwrap_parens(&call.callee) else { return false; };
        id.name.as_str() == "eval"
    }

    fn first_arg_string(call: &CallExpression<'a>) -> Option<&'a str> {
        let arg0 = call.arguments.first()?.as_expression()?;
        match arg0 {
            Expression::StringLiteral(s) => Some(s.value.as_str()),
            _ => None,
        }
    }

    fn parse_eval_argument(&self, span: oxc_span::Span, code: &str) -> Option<Replacement<'a>> {
        // Parse into temporary arena so we don't grow the main arena on failed parses.
        let temp_allocator = Allocator::default();
        let parse_ret = Parser::new(&temp_allocator, code, self.source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        if !parse_ret.errors.is_empty() {
            return None;
        }

        let body = &parse_ret.program.body;
        if body.is_empty() {
            return None;
        }

        if body.len() > 1 {
            let mut out = ArenaVec::new_in(self.allocator);
            for s in body {
                out.push(s.clone_in(self.allocator));
            }
            return Some(Replacement::Block(BlockStatement { span, body: out, scope_id: Default::default() }));
        }

        let stmt = &body[0];
        if let Statement::ExpressionStatement(es) = stmt {
            return Some(Replacement::Expr(es.expression.clone_in(self.allocator)));
        }

        Some(Replacement::Stmt(stmt.clone_in(self.allocator)))
    }

    fn maybe_eval_replacement(&self, call: &CallExpression<'a>) -> Option<Replacement<'a>> {
        if !self.is_eval_call(call) {
            return None;
        }
        let code = Self::first_arg_string(call)?;
        self.parse_eval_argument(call.span, code)
    }

    fn replace_eval_as_callee(&mut self, call: &mut CallExpression<'a>) -> bool {
        // Handle: eval('Function')('code')
        let Expression::CallExpression(inner) = self.unwrap_parens(&call.callee) else { return false; };
        let Some(rep) = self.maybe_eval_replacement(inner) else { return false; };
        let Replacement::Expr(expr) = rep else { return false; };

        call.callee = expr;
        true
    }

    fn apply_eval_expr_replacement(&mut self, it: &mut Expression<'a>) -> bool {
        let Expression::CallExpression(call) = it else { return false; };
        let Some(rep) = self.maybe_eval_replacement(call) else { return false; };

        // Only safe to replace an expression with another expression.
        let Replacement::Expr(expr) = rep else { return false; };
        *it = expr;
        true
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_statement(&mut self, it: &mut Statement<'a>) {
        // Handle statement-level replacements, including blocks and non-expression statements.
        if let Statement::ExpressionStatement(expr_stmt) = it {
            // `eval('a; b;')` => `{ a; b; }`
            if let Expression::CallExpression(call) = self.unwrap_parens(&expr_stmt.expression) {
                if let Some(rep) = self.maybe_eval_replacement(call) {
                    match rep {
                        Replacement::Expr(expr) => {
                            expr_stmt.expression = expr;
                            self.modified = true;
                            return;
                        }
                        Replacement::Stmt(stmt) => {
                            *it = stmt;
                            self.modified = true;
                            return;
                        }
                        Replacement::Block(block) => {
                            *it = Statement::BlockStatement(ArenaBox::new_in(block, self.allocator));
                            self.modified = true;
                            return;
                        }
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_statement(self, it);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // First handle: eval('Function')('code')
        if let Expression::CallExpression(call) = it {
            if self.replace_eval_as_callee(call) {
                self.modified = true;
                return;
            }
        }

        if self.apply_eval_expr_replacement(it) {
            self.modified = true;
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
