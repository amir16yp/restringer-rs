use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct UnwrapBindNullLiteral;

impl Transform for UnwrapBindNullLiteral {
    fn name(&self) -> &'static str {
        "unwrapBindNullLiteral"
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

    fn is_undefined_expr(&self, expr: &Expression<'a>) -> bool {
        match self.unwrap_parens(expr) {
            Expression::Identifier(id) => id.name.as_str() == "undefined",
            Expression::UnaryExpression(un) => {
                un.operator == UnaryOperator::Void && matches!(self.unwrap_parens(&un.argument), Expression::NumericLiteral(_))
            }
            _ => false,
        }
    }

    fn is_member_named<'b>(&self, callee: &'b Expression<'a>, name: &str) -> Option<&'b Expression<'a>> {
        match self.unwrap_parens(callee) {
            Expression::StaticMemberExpression(m) => {
                if m.property.name.as_str() == name {
                    Some(&m.object)
                } else {
                    None
                }
            }
            Expression::ComputedMemberExpression(m) => {
                let Expression::StringLiteral(s) = self.unwrap_parens(&m.expression) else {
                    return None;
                };
                if s.value.as_str() == name {
                    Some(&m.object)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn function_is_safe_to_drop_this_and_arguments(&self, func: &Function<'a>) -> bool {
        let Some(body) = func.body.as_ref() else {
            return false;
        };

        struct Finder {
            found: bool,
        }

        impl<'a> VisitMut<'a> for Finder {
            fn visit_this_expression(&mut self, _it: &mut ThisExpression) {
                self.found = true;
            }

            fn visit_meta_property(&mut self, _it: &mut MetaProperty) {
                // `new.target` etc.
                self.found = true;
            }

            fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
                if it.name.as_str() == "arguments" {
                    self.found = true;
                }
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: ScopeFlags) {
                // Do not descend into nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Do not descend into nested arrows.
            }
        }

        let mut f = Finder { found: false };
        let mut body_clone = body.clone_in(self.allocator);
        f.visit_function_body(&mut body_clone);
        !f.found
    }

    fn rewrite_bound_function(&self, func: &Function<'a>, param_name: &'a str, bound_value: &Expression<'a>) -> Function<'a> {
        let mut new_func = func.clone_in(self.allocator);

        // Drop parameters.
        new_func.params.items.clear();

        // Replace all references to param with the bound value.
        if let Some(body) = new_func.body.as_mut() {
            struct Replacer<'a> {
                allocator: &'a oxc_allocator::Allocator,
                param_name: &'a str,
                bound_value: Expression<'a>,
            }

            impl<'a> Replacer<'a> {
                fn unwrap_parens_mut<'b>(&self, mut expr: &'b mut Expression<'a>) -> &'b mut Expression<'a> {
                    loop {
                        match expr {
                            Expression::ParenthesizedExpression(p) => expr = &mut p.expression,
                            _ => return expr,
                        }
                    }
                }
            }

            impl<'a> VisitMut<'a> for Replacer<'a> {
                fn visit_expression(&mut self, it: &mut Expression<'a>) {
                    match self.unwrap_parens_mut(it) {
                        Expression::Identifier(id) => {
                            if id.name.as_str() == self.param_name {
                                *it = self.bound_value.clone_in(self.allocator);
                                return;
                            }
                        }
                        _ => {}
                    }
                    oxc_ast_visit::walk_mut::walk_expression(self, it);
                }

                fn visit_function(&mut self, _it: &mut Function<'a>, _flags: ScopeFlags) {
                    // Do not replace inside nested functions.
                }

                fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                    // Do not replace inside nested arrows.
                }
            }

            let mut r = Replacer {
                allocator: self.allocator,
                param_name,
                bound_value: bound_value.clone_in(self.allocator),
            };
            r.visit_function_body(body);
        }

        new_func
    }

    fn try_rewrite(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        // Match: (function (t) { ... }).bind(null, r)
        // Rewrite: function () { ... with t replaced by r ... }
        let target = self.is_member_named(&call.callee, "bind")?;
        let Expression::FunctionExpression(func_expr) = self.unwrap_parens(target) else {
            return None;
        };

        let func: &Function<'a> = func_expr;
        if func.r#async || func.generator {
            return None;
        }

        if call.arguments.len() != 2 {
            return None;
        }
        let Some(this_arg) = call.arguments[0].as_expression() else {
            return None;
        };
        if !matches!(self.unwrap_parens(this_arg), Expression::NullLiteral(_)) && !self.is_undefined_expr(this_arg) {
            return None;
        }
        let Some(bound_value) = call.arguments[1].as_expression() else {
            return None;
        };

        // Only support exactly one simple identifier parameter for now.
        if func.params.items.len() != 1 {
            return None;
        }
        let BindingPattern::BindingIdentifier(param_id) = &func.params.items[0].pattern else {
            return None;
        };
        let param_name: &'a str = param_id.name.as_str();

        if !self.function_is_safe_to_drop_this_and_arguments(func) {
            return None;
        }

        let new_func = self.rewrite_bound_function(func, param_name, bound_value);
        Some(Expression::FunctionExpression(ArenaBox::new_in(new_func, self.allocator)))
    }

    fn maybe_rewrite_expr(&mut self, it: &mut Expression<'a>) -> bool {
        let Expression::CallExpression(call) = it else {
            return false;
        };

        if let Some(repl) = self.try_rewrite(call) {
            *it = repl;
            self.modified = true;
            return true;
        }

        false
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        let _ = self.maybe_rewrite_expr(it);
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_call_expression(&mut self, it: &mut CallExpression<'a>) {
        // After rewriting `.bind(...)` into a function expression, don't visit the old structure.
        // Default walker will still handle nested expressions.
        oxc_ast_visit::walk_mut::walk_call_expression(self, it);
    }
}
