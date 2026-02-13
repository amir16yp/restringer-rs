use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct UnwrapIifeReturningIdentifier;

impl Transform for UnwrapIifeReturningIdentifier {
    fn name(&self) -> &'static str {
        "unwrapIifeReturningIdentifier"
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

    fn iife_returning_identifier(
        &self,
        outer_name: &'a str,
        call: &CallExpression<'a>,
    ) -> Option<(ArenaVec<'a, Statement<'a>>, IdentifierReference<'a>)> {
        if !call.arguments.is_empty() {
            return None;
        }

        let Expression::FunctionExpression(func) = self.unwrap_parens(&call.callee) else {
            return None;
        };
        if func.id.is_some() {
            return None;
        }
        let body = func.body.as_ref()?;
        if body.statements.is_empty() {
            return None;
        }

        let Statement::ReturnStatement(ret) = body.statements.last()? else {
            return None;
        };
        let arg = ret.argument.as_ref()?;

        // Babel sometimes does `return (a = 1, b = 2, C);`
        // i.e. a SequenceExpression whose last expression is the returned identifier.
        let mut return_identifier: Option<IdentifierReference<'a>> = None;
        let mut return_prefix_exprs: ArenaVec<'a, Expression<'a>> = ArenaVec::new_in(self.allocator);

        match self.unwrap_parens(arg) {
            Expression::Identifier(id) => {
                return_identifier = Some((**id).clone_in(self.allocator));
            }
            Expression::SequenceExpression(seq) => {
                if seq.expressions.is_empty() {
                    return None;
                }
                let last = seq.expressions.last().unwrap();
                let Expression::Identifier(id) = self.unwrap_parens(last) else {
                    return None;
                };
                return_identifier = Some((**id).clone_in(self.allocator));
                for e in &seq.expressions[..seq.expressions.len() - 1] {
                    return_prefix_exprs.push(e.clone_in(self.allocator));
                }
            }
            _ => return None,
        }

        let id = return_identifier?;

        let mut stmts = ArenaVec::new_in(self.allocator);

        // Rename inner `var <outer_name>` bindings to avoid collisions in the lifted scope.
        // This is a best-effort hygiene pass for the common Babel class-factory IIFE which uses `var t, i, r;`.
        let inner_rename_to = self.allocator.alloc_str(&format!("{outer_name}_tmp"));

        struct Renamer<'a> {
            from: &'a str,
            to: &'a str,
        }

        impl<'a> VisitMut<'a> for Renamer<'a> {
            fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
                if it.name.as_str() == self.from {
                    it.name = self.to.into();
                }
            }

            fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
                if it.name.as_str() == self.from {
                    it.name = self.to.into();
                }
            }
        }

        let mut renamer = Renamer { from: outer_name, to: inner_rename_to };
        for s in &body.statements[..body.statements.len() - 1] {
            // Skip directives; they don't make sense outside of function scope.
            if matches!(s, Statement::ExpressionStatement(es) if matches!(&es.expression, Expression::StringLiteral(_))) {
                continue;
            }
            let mut cloned = CloneIn::clone_in(s, self.allocator);
            // Apply renaming to avoid inner/outer collisions.
            renamer.visit_statement(&mut cloned);
            stmts.push(cloned);
        }

        // Lift prefix expressions from `return ( ... , ident )` into statements.
        for e in return_prefix_exprs {
            let mut expr = e;
            renamer.visit_expression(&mut expr);
            stmts.push(Statement::ExpressionStatement(ArenaBox::new_in(
                ExpressionStatement { span: ret.span, expression: expr },
                self.allocator,
            )));
        }

        Some((stmts, id))
    }

    fn rewrite_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            let mut replaced = false;

            if let Statement::VariableDeclaration(vd) = &stmt {
                if vd.declarations.len() == 1 {
                    let decl = &vd.declarations[0];
                    if let Some(init) = decl.init.as_ref() {
                        let Expression::CallExpression(call) = self.unwrap_parens(init) else {
                            out.push(stmt);
                            continue;
                        };

                        let outer_name = match &decl.id {
                            BindingPattern::BindingIdentifier(bi) => bi.name.as_str(),
                            _ => {
                                out.push(stmt);
                                continue;
                            }
                        };

                        if let Some((lifted, ret_id)) = self.iife_returning_identifier(outer_name, call) {
                            for s in lifted {
                                out.push(s);
                            }

                            let mut new_vd = vd.clone_in(self.allocator);
                            let new_decl = &mut new_vd.declarations[0];
                            new_decl.init = Some(Expression::Identifier(ArenaBox::new_in(ret_id, self.allocator)));
                            out.push(Statement::VariableDeclaration(new_vd));

                            self.modified = true;
                            replaced = true;
                        }
                    }
                }
            }

            if !replaced {
                out.push(stmt);
            }
        }

        *stmts = out;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.rewrite_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.rewrite_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.rewrite_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
