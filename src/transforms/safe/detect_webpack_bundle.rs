use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct DetectWebpackBundle;

impl Transform for DetectWebpackBundle {
    fn name(&self) -> &'static str {
        "detectWebpackBundle"
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

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: ScopeFlags) {
                // Do not rename inside nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Do not rename inside nested arrows.
            }
        }

        let mut r = Renamer { allocator: self.allocator, from, to: to_alloc };
        r.visit_function_body(body);
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

        // Only normalize if the 3rd parameter is used like a webpack require function.
        if !self.function_uses_require_like_call_in_body(func, p2) {
            return;
        }

        // Normalize `require`-like param to `__webpack_require__` when safe.
        if p2 != "__webpack_require__" {
            if !self.function_contains_binding_named(func, "__webpack_require__") {
                self.rename_identifier_in_function_body(func, p2, "__webpack_require__");
                self.modified = true;
            }
        }

        // Optionally normalize `module`/`exports` parameter names too, but only if safe.
        if p0 != "module" {
            if !self.function_contains_binding_named(func, "module") {
                self.rename_identifier_in_function_body(func, p0, "module");
                self.modified = true;
            }
        }

        if p1 != "exports" {
            if !self.function_contains_binding_named(func, "exports") {
                self.rename_identifier_in_function_body(func, p1, "exports");
                self.modified = true;
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
