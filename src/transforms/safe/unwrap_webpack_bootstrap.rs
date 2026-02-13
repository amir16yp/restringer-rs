use oxc_allocator::CloneIn;
use oxc_allocator::Vec as ArenaVec;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;
use std::collections::HashSet;

use crate::{Transform, TransformCtx};

pub struct UnwrapWebpackBootstrap;

impl Transform for UnwrapWebpackBootstrap {
    fn name(&self) -> &'static str {
        "unwrapWebpackBootstrap"
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

    fn unwrap_parens_mut<'b>(&self, mut expr: &'b mut Expression<'a>) -> &'b mut Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &mut p.expression,
                _ => return expr,
            }
        }
    }

    fn is_require_entry_call(&self, call: &CallExpression<'a>) -> Option<&'a str> {
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else {
            return None;
        };
        if call.arguments.len() != 1 {
            return None;
        }
        let Some(arg0) = call.arguments[0].as_expression() else {
            return None;
        };

        // Match: __req((__req.s = 228)) or __req(__req.s = 228)
        let expr0 = self.unwrap_parens(arg0);
        let Expression::AssignmentExpression(assign) = expr0 else {
            return None;
        };
        if assign.operator != AssignmentOperator::Assign {
            return None;
        }
        let AssignmentTarget::StaticMemberExpression(mem) = &assign.left else {
            return None;
        };
        let Expression::Identifier(obj_id) = self.unwrap_parens(&mem.object) else {
            return None;
        };
        if obj_id.name.as_str() != callee_id.name.as_str() {
            return None;
        }
        if mem.property.name.as_str() != "s" {
            return None;
        }

        Some(callee_id.name.as_str())
    }

    fn function_contains_binding_named(&self, func: &Function<'a>, name: &str) -> bool {
        if func.params.items.iter().any(|p| matches!(&p.pattern, BindingPattern::BindingIdentifier(id) if id.name.as_str() == name))
        {
            return true;
        }

        let Some(body) = func.body.as_ref() else {
            return false;
        };

        struct Finder<'a> {
            name: &'a str,
            found: bool,
        }

        impl<'a> VisitMut<'a> for Finder<'a> {
            fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
                if it.name.as_str() == self.name {
                    self.found = true;
                }
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: ScopeFlags) {
                // Do not descend into nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Do not descend into nested functions.
            }
        }

        let mut f = Finder { name, found: false };
        let mut body_clone = body.clone_in(self.allocator);
        f.visit_function_body(&mut body_clone);
        f.found
    }

    fn rename_identifier_in_function_body(&mut self, func: &mut Function<'a>, from: &'a str, to: &'a str) {
        let Some(body) = func.body.as_mut() else {
            return;
        };

        let to_alloc = self.allocator.alloc_str(to);

        struct Renamer<'a> {
            allocator: &'a oxc_allocator::Allocator,
            from: &'a str,
            to: &'a str,
        }

        impl<'a> Renamer<'a> {
            fn unwrap_parens_mut<'b>(&self, mut expr: &'b mut Expression<'a>) -> &'b mut Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &mut p.expression,
                        _ => return expr,
                    }
                }
            }

            fn function_shadows_name(&self, func: &Function<'a>) -> bool {
                if let Some(id) = func.id.as_ref() {
                    if id.name.as_str() == self.from {
                        return true;
                    }
                }
                func.params.items.iter().any(|p| matches!(&p.pattern, BindingPattern::BindingIdentifier(id) if id.name.as_str() == self.from))
            }

            fn arrow_shadows_name(&self, arrow: &ArrowFunctionExpression<'a>) -> bool {
                arrow
                    .params
                    .items
                    .iter()
                    .any(|p| matches!(&p.pattern, BindingPattern::BindingIdentifier(id) if id.name.as_str() == self.from))
            }
        }

        impl<'a> VisitMut<'a> for Renamer<'a> {
            fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
                if it.name.as_str() == self.from {
                    let name = self.allocator.alloc_str(self.to);
                    it.name = name.into();
                }
            }

            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                match self.unwrap_parens_mut(it) {
                    Expression::Identifier(id) => {
                        if id.name.as_str() == self.from {
                            let name = self.allocator.alloc_str(self.to);
                            id.name = name.into();
                        }
                    }
                    _ => {}
                }

                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }

            fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
                if self.function_shadows_name(it) {
                    return;
                }
                oxc_ast_visit::walk_mut::walk_function(self, it, flags);
            }

            fn visit_arrow_function_expression(&mut self, it: &mut ArrowFunctionExpression<'a>) {
                if self.arrow_shadows_name(it) {
                    return;
                }
                oxc_ast_visit::walk_mut::walk_arrow_function_expression(self, it);
            }
        }

        let mut r = Renamer { allocator: self.allocator, from, to: to_alloc };
        r.visit_function_body(body);
    }

    fn rename_require_binding_in_function_scope(&mut self, func: &mut Function<'a>, from: &'a str, to: &'a str) {
        let Some(body) = func.body.as_mut() else {
            return;
        };

        // 1) function r(...) { ... }
        for stmt in body.statements.iter_mut() {
            let Statement::FunctionDeclaration(fd) = stmt else { continue };
            let f = &mut **fd;
            let Some(id) = f.id.as_mut() else { continue };
            if id.name.as_str() == from {
                let to_alloc = self.allocator.alloc_str(to);
                id.name = to_alloc.into();
                self.modified = true;
                return;
            }
        }

        // 2) var r = function(...) { ... }  / const r = (...) => ...
        for stmt in body.statements.iter_mut() {
            let Statement::VariableDeclaration(vd) = stmt else { continue };
            for decl in vd.declarations.iter_mut() {
                let BindingPattern::BindingIdentifier(id) = &mut decl.id else { continue };
                if id.name.as_str() != from {
                    continue;
                }
                let to_alloc = self.allocator.alloc_str(to);
                id.name = to_alloc.into();
                self.modified = true;
                return;
            }
        }
    }

    fn try_normalize_require_name(&mut self, func: &mut Function<'a>) {
        let Some(body) = func.body.as_ref() else {
            return;
        };

        let mut require_name: Option<&str> = None;
        for stmt in &body.statements {
            match stmt {
                Statement::ReturnStatement(ret) => {
                    let Some(arg) = ret.argument.as_ref() else {
                        continue;
                    };
                    let Expression::CallExpression(call) = self.unwrap_parens(arg) else {
                        continue;
                    };
                    require_name = self.is_require_entry_call(call);
                }
                Statement::ExpressionStatement(es) => {
                    let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else {
                        continue;
                    };
                    require_name = self.is_require_entry_call(call);
                }
                _ => {}
            }
            if require_name.is_some() {
                break;
            }
        }

        let Some(require_name) = require_name else {
            return;
        };
        if require_name == "__webpack_require__" {
            return;
        }

        // Avoid renaming if __webpack_require__ already exists in this function scope.
        if self.function_contains_binding_named(func, "__webpack_require__") {
            return;
        }

        // Rename the binding itself first (so __webpack_require__ is defined), then update references.
        self.rename_require_binding_in_function_scope(func, require_name, "__webpack_require__");
        self.rename_identifier_in_function_body(func, require_name, "__webpack_require__");
        self.modified = true;
    }

    fn unwrap_top_level_bang_iife(&mut self, expr: &mut Expression<'a>) -> bool {
        let Expression::UnaryExpression(un) = self.unwrap_parens_mut(expr) else {
            return false;
        };
        if un.operator != UnaryOperator::LogicalNot {
            return false;
        }
        let Expression::CallExpression(call) = self.unwrap_parens_mut(&mut un.argument) else {
            return false;
        };

        // `!callExpr` used as a statement -> unwrap to `callExpr`.
        let call_clone = call.clone_in(self.allocator);
        *expr = Expression::CallExpression(call_clone);
        self.modified = true;

        true
    }

    fn unwrap_top_level_plain_iife(&mut self, expr: &mut Expression<'a>) -> bool {
        // If the program already has a plain IIFE expression statement, keep it as-is,
        // but return true to indicate we should attempt to normalize webpack require name.
        let Expression::CallExpression(_call) = self.unwrap_parens_mut(expr) else {
            return false;
        };
        true
    }

    fn collect_used_require_props(&self, program: &Program<'a>) -> HashSet<String> {
        struct Collector {
            used: HashSet<String>,
        }

        impl<'a> Collector {
            fn record_member(&mut self, expr: &Expression<'a>) {
                let Expression::StaticMemberExpression(mem) = expr else {
                    return;
                };
                self.used.insert(mem.property.name.as_str().to_string());
            }
        }

        impl<'a> VisitMut<'a> for Collector {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                self.record_member(it);
                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }

            fn visit_call_expression(&mut self, it: &mut CallExpression<'a>) {
                if let Expression::StaticMemberExpression(mem) = &it.callee {
                    // Record calls like `i.r(exports)` as well as `__webpack_require__.r(exports)`.
                    // This prevents us from pruning helper assignments before other transforms
                    // normalize the identifier to `__webpack_require__`.
                    self.used.insert(mem.property.name.as_str().to_string());
                }
                oxc_ast_visit::walk_mut::walk_call_expression(self, it);
            }

            fn visit_assignment_expression(&mut self, it: &mut AssignmentExpression<'a>) {
                // Do not treat writes (`__webpack_require__.x = ...`) as usage.
                oxc_ast_visit::walk_mut::walk_expression(self, &mut it.right);
            }
        }

        let mut program_clone = program.clone_in(self.allocator);
        let mut c = Collector { used: HashSet::new() };
        c.visit_program(&mut program_clone);
        c.used
    }

    fn prune_unused_require_helper_assignments(&mut self, func: &mut Function<'a>, used_props: &HashSet<String>) {
        let Some(body) = func.body.as_mut() else {
            return;
        };

        let original = std::mem::replace(&mut body.statements, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            let mut keep = true;
            if let Statement::ExpressionStatement(es) = &stmt {
                if let Expression::AssignmentExpression(assign) = self.unwrap_parens(&es.expression) {
                    if assign.operator == AssignmentOperator::Assign {
                        if let AssignmentTarget::StaticMemberExpression(mem) = &assign.left {
                            if let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) {
                                if obj.name.as_str() == "__webpack_require__" {
                                    let prop = mem.property.name.as_str();
                                    if !used_props.contains(prop) {
                                        keep = false;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if keep {
                out.push(stmt);
            } else {
                self.modified = true;
            }
        }

        body.statements = out;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let used_props = self.collect_used_require_props(it);

        for stmt in &mut it.body {
            let Statement::ExpressionStatement(es) = stmt else {
                continue;
            };

            let should_try_normalize = self.unwrap_top_level_bang_iife(&mut es.expression)
                || self.unwrap_top_level_plain_iife(&mut es.expression);

            if should_try_normalize {
                // If callee is a function expression, try to normalize require name.
                let expr = self.unwrap_parens_mut(&mut es.expression);
                if let Expression::CallExpression(call) = expr {
                    let callee = self.unwrap_parens_mut(&mut call.callee);
                    if let Expression::FunctionExpression(func) = callee {
                        self.try_normalize_require_name(func);
                        self.prune_unused_require_helper_assignments(func, &used_props);
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_program(self, it);
    }
}
