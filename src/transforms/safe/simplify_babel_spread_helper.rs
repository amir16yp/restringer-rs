use std::collections::HashSet;

use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyBabelSpreadHelper;

impl Transform for SimplifyBabelSpreadHelper {
    fn name(&self) -> &'static str {
        "simplifyBabelSpreadHelper"
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

    fn helper_names_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut out = HashSet::new();
        for s in stmts {
            let Statement::FunctionDeclaration(fd) = s else {
                continue;
            };
            let func = &**fd;
            let Some(id) = func.id.as_ref() else {
                continue;
            };
            if self.is_babel_spread_helper(func) {
                out.insert(id.name.as_str().to_string());
            }
        }
        out
    }

    fn is_babel_spread_helper(&self, func: &Function<'a>) -> bool {
        // Heuristic: function contains string literal:
        // "Invalid attempt to spread non-iterable instance"
        // This is stable across many Babel helper variants.
        let Some(body) = func.body.as_ref() else {
            return false;
        };

        struct Finder {
            found: bool,
        }

        impl<'a> VisitMut<'a> for Finder {
            fn visit_string_literal(&mut self, it: &mut StringLiteral<'a>) {
                if self.found {
                    return;
                }
                if it.value.as_str().contains("Invalid attempt to spread non-iterable instance") {
                    self.found = true;
                }
            }
        }

        let mut finder = Finder { found: false };
        let mut body_clone = body.clone_in(self.allocator);
        finder.visit_function_body(&mut body_clone);
        finder.found
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let helper_names = self.helper_names_in_statement_list(stmts);
        if helper_names.is_empty() {
            return;
        }

        struct Rewriter<'a> {
            allocator: &'a oxc_allocator::Allocator,
            helper_names: HashSet<String>,
            modified: bool,
        }

        impl<'a> Rewriter<'a> {
            fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &p.expression,
                        _ => return expr,
                    }
                }
            }

            fn is_helper_call(&self, expr: &Expression<'a>) -> Option<(oxc_span::Span, Expression<'a>)> {
                let Expression::CallExpression(call) = self.unwrap_parens(expr) else {
                    return None;
                };
                let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else {
                    return None;
                };
                if !self.helper_names.contains(callee_id.name.as_str()) {
                    return None;
                }
                if call.arguments.len() != 1 {
                    return None;
                }
                let Some(arg0) = call.arguments[0].as_expression() else {
                    return None;
                };

                // Be conservative: only unwrap helper when the argument is already array-like.
                // - Identifier (e.g. args)
                // - MemberExpression (e.g. n.args)
                // - ArrayExpression
                match self.unwrap_parens(arg0) {
                    Expression::Identifier(_) => {}
                    Expression::StaticMemberExpression(_) => {}
                    Expression::ComputedMemberExpression(_) => {}
                    Expression::ArrayExpression(_) => {}
                    _ => return None,
                }

                Some((call.span, arg0.clone_in(self.allocator)))
            }

            fn is_apply_member(&self, callee: &Expression<'a>) -> bool {
                match self.unwrap_parens(callee) {
                    Expression::StaticMemberExpression(mem) => mem.property.name.as_str() == "apply",
                    Expression::ComputedMemberExpression(mem) => {
                        // Only accept obj["apply"]
                        if let Expression::StringLiteral(s) = self.unwrap_parens(&mem.expression) {
                            return s.value.as_str() == "apply";
                        }
                        false
                    }
                    _ => false,
                }
            }
        }

        impl<'a> VisitMut<'a> for Rewriter<'a> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                // Pattern: something.apply(thisArg, helper(argsLike))
                let Expression::CallExpression(call) = it else {
                    oxc_ast_visit::walk_mut::walk_expression(self, it);
                    return;
                };

                if call.arguments.len() == 2 {
                    if self.is_apply_member(&call.callee) {
                        if let Some(arg1) = call.arguments[1].as_expression() {
                            if let Some((_span, unwrapped_arg)) = self.is_helper_call(arg1) {
                                call.arguments[1] = Argument::from(unwrapped_arg);
                                self.modified = true;
                            }
                        }
                    }
                }

                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }
        }

        let mut rw = Rewriter {
            allocator: self.allocator,
            helper_names,
            modified: false,
        };

        for s in stmts.iter_mut() {
            rw.visit_statement(s);
        }

        if rw.modified {
            self.modified = true;
        }
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
