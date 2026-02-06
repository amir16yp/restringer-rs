use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ReplaceFunctionShellsWithWrappedValue;

impl Transform for ReplaceFunctionShellsWithWrappedValue {
    fn name(&self) -> &'static str {
        "replaceFunctionShellsWithWrappedValue"
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

    fn is_returnable(expr: &Expression<'a>) -> bool {
        matches!(expr, Expression::Identifier(_) | Expression::StringLiteral(_) | Expression::NumericLiteral(_) | Expression::BooleanLiteral(_) | Expression::NullLiteral(_) | Expression::BigIntLiteral(_) | Expression::RegExpLiteral(_))
    }

    fn return_expr_from_function<'b>(&self, func: &'b Function<'a>) -> Option<&'b Expression<'a>> {
        let body = func.body.as_ref()?;
        if body.statements.len() != 1 {
            return None;
        }
        let Statement::ReturnStatement(ret) = &body.statements[0] else { return None; };
        let arg = ret.argument.as_ref()?;
        if Self::is_returnable(arg) {
            Some(arg)
        } else {
            None
        }
    }

    fn replace_calls_in_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        for stmt in stmts.iter_mut() {
            let Statement::FunctionDeclaration(func_decl) = stmt else { continue; };
            let func = &**func_decl;
            let Some(id) = func.id.as_ref() else { continue; };
            let Some(ret_expr) = self.return_expr_from_function(func) else { continue; };

            let span_name = id.name.as_str();
            let ret_clone = ret_expr.clone_in(self.allocator);

            // Walk statements again to replace `name()` calls within this list.
            // This is conservative: same statement list scope only.
            let mut replaced_any = false;
            for s in stmts.iter_mut() {
                if self.replace_calls_in_statement_in_place(span_name, &ret_clone, s) {
                    replaced_any = true;
                }
            }

            if replaced_any {
                self.modified = true;
            }
            // Only do one function shell per pass in this list to avoid quadratic blowups.
            break;
        }
    }

    fn replace_calls_in_statement_in_place(
        &self,
        func_name: &str,
        replacement: &Expression<'a>,
        stmt: &mut Statement<'a>,
    ) -> bool {
        struct Replacer<'a, 'b> {
            allocator: &'a oxc_allocator::Allocator,
            func_name: &'b str,
            replacement: &'b Expression<'a>,
            replaced_any: bool,
        }

        impl<'a> Replacer<'a, '_> {
            fn unwrap_parens<'c>(&self, mut expr: &'c Expression<'a>) -> &'c Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &p.expression,
                        _ => return expr,
                    }
                }
            }
        }

        impl<'a> VisitMut<'a> for Replacer<'a, '_> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                if let Expression::CallExpression(call) = it {
                    if call.arguments.is_empty() {
                        if let Expression::Identifier(id) = self.unwrap_parens(&call.callee) {
                            if id.name.as_str() == self.func_name {
                                *it = self.replacement.clone_in(self.allocator);
                                self.replaced_any = true;
                                return;
                            }
                        }
                    }
                }
                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }
        }

        let mut r = Replacer { allocator: self.allocator, func_name, replacement, replaced_any: false };
        r.visit_statement(stmt);
        r.replaced_any
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.replace_calls_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.replace_calls_in_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.replace_calls_in_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        // Don't let outer statement-list replacement traverse the function header.
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
    }
}
