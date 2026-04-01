use std::collections::HashMap;

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ResolveProxyReferences;

impl Transform for ResolveProxyReferences {
    fn name(&self) -> &'static str {
        "resolveProxyReferences"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, map: HashMap::new(), modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    map: HashMap<String, Expression<'a>>,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn collect_proxy_declarators_from_statement_list(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue };

            if var_decl.kind != VariableDeclarationKind::Const {
                continue;
            }

            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue };
                let Some(init) = decl.init.as_ref() else { continue };

                // Support Identifier and MemberExpressions (static/computed).
                if !matches!(
                    init,
                    Expression::Identifier(_)
                        | Expression::StaticMemberExpression(_)
                        | Expression::ComputedMemberExpression(_)
                ) {
                    continue;
                }

                let proxy_name = binding.name.as_str();
                self.map.insert(proxy_name.to_string(), init.clone_in(self.allocator));
            }
        }
    }

    fn clone_expr_for_replacement(&self, expr: &Expression<'a>) -> Expression<'a> {
        expr.clone_in(self.allocator)
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_proxy_declarators_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_declarators_from_statement_list(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.map = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_declarators_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.map = prev;
    }

    fn visit_variable_declarator(&mut self, it: &mut VariableDeclarator<'a>) {
        let removed = if let BindingPattern::BindingIdentifier(binding) = &it.id {
            self.map.remove(binding.name.as_str())
        } else {
            None
        };

        oxc_ast_visit::walk_mut::walk_variable_declarator(self, it);

        if let (Some(expr), BindingPattern::BindingIdentifier(binding)) = (removed, &it.id) {
            self.map.insert(binding.name.as_str().to_string(), expr);
        }
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::Identifier(idref) = it {
            if let Some(replacement) = self.map.get(idref.name.as_str()) {
                let span = idref.span;
                let mut new_expr = self.clone_expr_for_replacement(replacement);

                if let Expression::Identifier(new_id) = &mut new_expr {
                    new_id.span = span;
                }

                *it = new_expr;
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        // Don’t rewrite declaration sites.
        let _ = it;
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let prev = std::mem::take(&mut self.map);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.map = prev;
    }
}
