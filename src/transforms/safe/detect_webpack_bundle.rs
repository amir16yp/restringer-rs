use std::collections::HashSet;

use oxc_allocator::{Allocator, Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::{ParseOptions, Parser};
use oxc_syntax::scope::ScopeFlags;
use oxc_span::SourceType;

use crate::{Transform, TransformCtx};

pub struct DetectWebpackBundle;

impl Transform for DetectWebpackBundle {
    fn name(&self) -> &'static str {
        "detectWebpackBundle"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            source_type: ctx.source_type,
            modified: false,
            hoisted: Vec::new(),
        };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    source_type: SourceType,
    modified: bool,
    hoisted: Vec<Statement<'a>>,
}

impl<'a> Visitor<'a> {
    fn parse_function_declaration_stub(&self, name: &str) -> Option<Function<'a>> {
        let mut code = String::with_capacity("function  (){}".len() + name.len());
        code.push_str("function ");
        code.push_str(name);
        code.push_str("(){}");

        let temp_allocator = Allocator::default();
        let parse_ret = Parser::new(&temp_allocator, &code, self.source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        if !parse_ret.errors.is_empty() {
            return None;
        }

        for stmt in &parse_ret.program.body {
            if let Statement::FunctionDeclaration(fd) = stmt {
                return Some((**fd).clone_in(self.allocator));
            }
        }
        None
    }

    fn make_ident_expr(&self, span: oxc_span::Span, name: &str) -> Expression<'a> {
        let name = self.allocator.alloc_str(name);
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }

    fn collect_top_level_function_names(&self, program: &Program<'a>) -> HashSet<String> {
        let mut out = HashSet::new();
        for stmt in &program.body {
            let Statement::FunctionDeclaration(fd) = stmt else { continue };
            let Some(id) = (**fd).id.as_ref() else { continue };
            out.insert(id.name.as_str().to_string());
        }
        out
    }

    fn key_to_numeric_id(&self, key: &PropertyKey<'a>) -> Option<u32> {
        match key {
            PropertyKey::NumericLiteral(n) => {
                if n.value.is_finite() && n.value.fract() == 0.0 && n.value >= 0.0 {
                    Some(n.value as u32)
                } else {
                    None
                }
            }
            PropertyKey::StringLiteral(s) => s.value.as_str().parse::<u32>().ok(),
            _ => None,
        }
    }

    fn hoist_module_factories_in_object(&mut self, top_level_fn_names: &HashSet<String>, obj: &mut ObjectExpression<'a>) {
        for prop in obj.properties.iter_mut() {
            let ObjectPropertyKind::ObjectProperty(p) = prop else { continue };
            let span = p.span;
            let ObjectProperty { key, value, .. } = &mut **p;

            let Some(id) = self.key_to_numeric_id(key) else { continue };
            let Expression::FunctionExpression(func_expr) = value else { continue };

            let hoisted_name = format!("_wp_mod_{id}");
            if top_level_fn_names.contains(&hoisted_name) {
                continue;
            }

            // Normalize module factory param usages first (require/module/exports), then hoist.
            self.maybe_normalize_factory_params(&mut **func_expr);

            let Some(mut func_decl) = self.parse_function_declaration_stub(&hoisted_name) else {
                continue;
            };

            // Copy function contents into the declaration stub (preserving id from stub).
            let src = &**func_expr;
            func_decl.r#async = src.r#async;
            func_decl.generator = src.generator;
            func_decl.params = src.params.clone_in(self.allocator);
            func_decl.body = src.body.clone_in(self.allocator);
            func_decl.type_parameters = src.type_parameters.clone_in(self.allocator);
            func_decl.return_type = src.return_type.clone_in(self.allocator);
            func_decl.this_param = src.this_param.clone_in(self.allocator);

            self.hoisted.push(Statement::FunctionDeclaration(ArenaBox::new_in(func_decl, self.allocator)));

            // Replace table entry with identifier reference.
            *value = self.make_ident_expr(span, &hoisted_name);
            self.modified = true;
        }
    }

    fn hoist_module_factories_in_array(&mut self, top_level_fn_names: &HashSet<String>, arr: &mut ArrayExpression<'a>) {
        for (idx, el) in arr.elements.iter_mut().enumerate() {
            let Some(expr) = el.as_expression_mut() else { continue };
            let Expression::FunctionExpression(func_expr) = expr else { continue };

            let hoisted_name = format!("_wp_mod_{idx}");
            if top_level_fn_names.contains(&hoisted_name) {
                continue;
            }

            self.maybe_normalize_factory_params(&mut **func_expr);

            let Some(mut func_decl) = self.parse_function_declaration_stub(&hoisted_name) else {
                continue;
            };

            let src = &**func_expr;
            func_decl.r#async = src.r#async;
            func_decl.generator = src.generator;
            func_decl.params = src.params.clone_in(self.allocator);
            func_decl.body = src.body.clone_in(self.allocator);
            func_decl.type_parameters = src.type_parameters.clone_in(self.allocator);
            func_decl.return_type = src.return_type.clone_in(self.allocator);
            func_decl.this_param = src.this_param.clone_in(self.allocator);

            self.hoisted.push(Statement::FunctionDeclaration(ArenaBox::new_in(func_decl, self.allocator)));

            // Replace array element with identifier.
            *expr = self.make_ident_expr(func_expr.span, &hoisted_name);
            self.modified = true;
        }
    }

    fn function_contains_binding_named(&self, func: &Function<'a>, name: &str) -> bool {
        if func.params.items.iter().any(|p| matches!(&p.pattern, BindingPattern::BindingIdentifier(id) if id.name.as_str() == name)) {
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

    fn rename_identifier_in_function_body(&self, func: &mut Function<'a>, from: &'a str, to: &'a str) {
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
                // Descend into nested functions unless they shadow the name.
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

    fn rename_function_param_binding(&self, func: &mut Function<'a>, index: usize, to: &str) -> Option<&'a str> {
        let param = func.params.items.get_mut(index)?;
        let BindingPattern::BindingIdentifier(id) = &mut param.pattern else {
            return None;
        };
        let from = id.name.as_str();
        let to_alloc = self.allocator.alloc_str(to);
        id.name = to_alloc.into();
        Some(from)
    }

    fn function_looks_like_webpack_module_factory(&self, func: &Function<'a>) -> Option<(&'a str, &'a str, &'a str)> {
        if func.params.items.len() != 3 {
            return None;
        }
        let BindingPattern::BindingIdentifier(p0) = &func.params.items[0].pattern else { return None; };
        let BindingPattern::BindingIdentifier(p1) = &func.params.items[1].pattern else { return None; };
        let BindingPattern::BindingIdentifier(p2) = &func.params.items[2].pattern else { return None; };
        Some((p0.name.as_str(), p1.name.as_str(), p2.name.as_str()))
    }

    fn maybe_normalize_factory_params(&mut self, func: &mut Function<'a>) {
        let Some((p0, p1, p2)) = self.function_looks_like_webpack_module_factory(func) else {
            return;
        };

        // Normalize parameter bindings + their uses.
        // Important: to keep semantics, we must rename the *binding identifier* too, not only references.
        //
        // We always try to normalize param0/param1 (`module`/`exports`) when safe.
        // This avoids strict-mode SyntaxErrors where a top-level `class e {}` (or `let e`) collides with
        // a factory parameter named `e`.

        // param0: module
        if p0 != "module" && !self.function_contains_binding_named(func, "module") {
            if let Some(from) = self.rename_function_param_binding(func, 0, "module") {
                self.rename_identifier_in_function_body(func, from, "module");
                self.modified = true;
            }
        }

        // param1: exports
        if p1 != "exports" && !self.function_contains_binding_named(func, "exports") {
            if let Some(from) = self.rename_function_param_binding(func, 1, "exports") {
                self.rename_identifier_in_function_body(func, from, "exports");
                self.modified = true;
            }
        }

        // Only normalize param2 to `__webpack_require__` if the 3rd parameter is used like a webpack require function.
        if self.function_uses_require_like_call_in_body(func, p2) {
            if p2 != "__webpack_require__" && !self.function_contains_binding_named(func, "__webpack_require__") {
                if let Some(from) = self.rename_function_param_binding(func, 2, "__webpack_require__") {
                    self.rename_identifier_in_function_body(func, from, "__webpack_require__");
                    self.modified = true;
                }
            }
        }
    }

    fn function_uses_require_like_call_in_body(&self, func: &Function<'a>, require_name: &str) -> bool {
        let Some(body) = func.body.as_ref() else {
            return false;
        };

        struct Finder<'a> {
            require_name: &'a str,
            found: bool,
        }

        impl<'a> VisitMut<'a> for Finder<'a> {
            fn visit_call_expression(&mut self, it: &mut CallExpression<'a>) {
                if self.found {
                    return;
                }

                if let Expression::Identifier(id) = &it.callee {
                    if id.name.as_str() == self.require_name {
                        if let Some(arg0) = it.arguments.first().and_then(|a| a.as_expression()) {
                            if matches!(arg0, Expression::NumericLiteral(_)) {
                                self.found = true;
                                return;
                            }
                        }
                    }
                }

                oxc_ast_visit::walk_mut::walk_call_expression(self, it);
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: ScopeFlags) {
                // Do not descend into nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Do not descend into nested arrows.
            }
        }

        let mut f = Finder { require_name, found: false };
        let mut body_clone = body.clone_in(self.allocator);
        f.visit_function_body(&mut body_clone);
        f.found
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        // Phase 0: hoist module factories from top-level module tables.
        // We only look at top-level variable initializers / assignments, to keep this safe.
        self.hoisted.clear();

        let top_level_fn_names = self.collect_top_level_function_names(it);

        for stmt in it.body.iter_mut() {
            match stmt {
                Statement::VariableDeclaration(var) => {
                    for decl in var.declarations.iter_mut() {
                        let Some(init) = decl.init.as_mut() else { continue };
                        match init {
                            Expression::ObjectExpression(obj) => {
                                self.hoist_module_factories_in_object(&top_level_fn_names, obj);
                            }
                            Expression::ArrayExpression(arr) => {
                                self.hoist_module_factories_in_array(&top_level_fn_names, arr);
                            }
                            _ => {}
                        }
                    }
                }
                Statement::ExpressionStatement(es) => {
                    // Handle `x = {...}` or `x = [ ... ]` patterns.
                    if let Expression::AssignmentExpression(assign) = &mut es.expression {
                        if assign.operator != AssignmentOperator::Assign {
                            continue;
                        }
                        let Expression::ObjectExpression(obj) = &mut assign.right else {
                            if let Expression::ArrayExpression(arr) = &mut assign.right {
                                self.hoist_module_factories_in_array(&top_level_fn_names, arr);
                            }
                            continue;
                        };
                        self.hoist_module_factories_in_object(&top_level_fn_names, obj);
                    }
                }
                _ => {}
            }
        }

        if !self.hoisted.is_empty() {
            let mut new_body = ArenaVec::new_in(self.allocator);
            for s in self.hoisted.drain(..) {
                new_body.push(s);
            }
            for s in it.body.drain(..) {
                new_body.push(s);
            }
            it.body = new_body;
            self.modified = true;
        }

        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        self.maybe_normalize_factory_params(it);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::FunctionExpression(fe) = it {
            self.maybe_normalize_factory_params(&mut **fe);
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
