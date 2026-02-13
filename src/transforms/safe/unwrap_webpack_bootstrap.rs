use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

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

    fn is_require_entry_return(&self, ret: &ReturnStatement<'a>) -> Option<&'a str> {
        let arg = ret.argument.as_ref()?;
        let Expression::CallExpression(call) = self.unwrap_parens(arg) else {
            return None;
        };

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
                // do not rename inside nested functions
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // do not rename inside nested arrows
            }
        }

        let mut r = Renamer { allocator: self.allocator, from, to: to_alloc };
        r.visit_function_body(body);
    }

    fn try_normalize_require_name(&mut self, func: &mut Function<'a>) {
        let Some(body) = func.body.as_ref() else {
            return;
        };

        let mut require_name: Option<&str> = None;
        for stmt in &body.statements {
            let Statement::ReturnStatement(ret) = stmt else {
                continue;
            };
            require_name = self.is_require_entry_return(ret);
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
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        for stmt in &mut it.body {
            let Statement::ExpressionStatement(es) = stmt else {
                continue;
            };

            if self.unwrap_top_level_bang_iife(&mut es.expression) {
                // If callee is a function expression, try to normalize require name.
                let expr = self.unwrap_parens_mut(&mut es.expression);
                if let Expression::CallExpression(call) = expr {
                    let callee = self.unwrap_parens_mut(&mut call.callee);
                    if let Expression::FunctionExpression(func) = callee {
                        self.try_normalize_require_name(func);
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_program(self, it);
    }
}
