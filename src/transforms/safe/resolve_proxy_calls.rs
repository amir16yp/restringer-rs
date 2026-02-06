use std::collections::HashMap;

use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ResolveProxyCalls;

impl Transform for ResolveProxyCalls {
    fn name(&self) -> &'static str {
        "resolveProxyCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, map: HashMap::new(), modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    // proxy function name -> target function name
    map: HashMap<String, String>,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn collect_proxy_functions_from_statements(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            let Statement::FunctionDeclaration(func) = stmt else { continue; };
            let f = &**func;
            let Some(id) = f.id.as_ref() else { continue; };
            let name = id.name.as_str();
            if let Some(target) = Self::is_proxy_call(f) {
                self.map.insert(name.to_string(), target.to_string());
            }
        }
    }

    fn is_proxy_call(func: &Function<'a>) -> Option<&'a str> {
        let body = func.body.as_ref()?;
        if body.statements.len() != 1 {
            return None;
        }

        let Statement::ReturnStatement(ret) = &body.statements[0] else { return None; };
        let arg = ret.argument.as_ref()?;
        let Expression::CallExpression(call) = arg else { return None; };
        let Expression::Identifier(target) = &call.callee else { return None; };

        // Parameters must be identifiers and passed through in exact order.
        if func.params.items.len() != call.arguments.len() {
            return None;
        }
        for (p, a) in func.params.items.iter().zip(call.arguments.iter()) {
            let BindingPattern::BindingIdentifier(pat_id) = &p.pattern else { return None; };
            let Some(arg_expr) = a.as_expression() else { return None; };
            let Expression::Identifier(arg_id) = arg_expr else { return None; };
            if pat_id.name.as_str() != arg_id.name.as_str() {
                return None;
            }
        }

        Some(target.name.as_str())
    }

    fn make_ident_expr(&self, span: oxc_span::Span, name: &str) -> Expression<'a> {
        let name = self.allocator.alloc_str(name);
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_proxy_functions_from_statements(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        // New scope: only consider proxy functions declared inside this body.
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_functions_from_statements(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.map = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        // New scope: only consider proxy functions declared inside this block.
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_functions_from_statements(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.map = prev;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::Identifier(idref) = it {
            if let Some(target) = self.map.get(idref.name.as_str()) {
                let span = idref.span;
                *it = self.make_ident_expr(span, target);
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        // Don't let outer mappings rewrite inside the function header; reset at boundary.
        let prev = std::mem::take(&mut self.map);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.map = prev;
    }
}
