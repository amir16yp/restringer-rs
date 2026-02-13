use std::collections::HashSet;

use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyBabelClassHelpers;

impl Transform for SimplifyBabelClassHelpers {
    fn name(&self) -> &'static str {
        "simplifyBabelClassHelpers"
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

    fn is_object_define_property_call(&self, call: &CallExpression<'a>) -> bool {
        let Expression::StaticMemberExpression(mem) = self.unwrap_parens(&call.callee) else {
            return false;
        };
        let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) else {
            return false;
        };
        obj.name.as_str() == "Object" && mem.property.name.as_str() == "defineProperty"
    }

    fn is_define_properties_helper(&self, func: &Function<'a>) -> bool {
        // Heuristic: function has 2 params and contains a call:
        // Object.defineProperty(<param0>, <something>.key, <same something>)
        if func.params.items.len() != 2 {
            return false;
        }
        let BindingPattern::BindingIdentifier(target_param) = &func.params.items[0].pattern else {
            return false;
        };

        let Some(body) = func.body.as_ref() else {
            return false;
        };

        let target_name = target_param.name.as_str();

        struct Finder<'a> {
            target_name: &'a str,
            found: bool,
        }

        impl<'a> Finder<'a> {
            fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &p.expression,
                        _ => return expr,
                    }
                }
            }

            fn is_object_define_property_call(&self, call: &CallExpression<'a>) -> bool {
                let Expression::StaticMemberExpression(mem) = self.unwrap_parens(&call.callee) else {
                    return false;
                };
                let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) else {
                    return false;
                };
                obj.name.as_str() == "Object" && mem.property.name.as_str() == "defineProperty"
            }
        }

        impl<'a> VisitMut<'a> for Finder<'a> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                if self.found {
                    return;
                }
                if let Expression::CallExpression(call) = it {
                    if self.is_object_define_property_call(call) {
                        if call.arguments.len() == 3 {
                            let Some(arg0) = call.arguments[0].as_expression() else {
                                return;
                            };
                            let Expression::Identifier(id0) = self.unwrap_parens(arg0) else {
                                return;
                            };
                            if id0.name.as_str() != self.target_name {
                                return;
                            }

                            // arg1 should be something.key
                            let Some(arg1) = call.arguments[1].as_expression() else {
                                return;
                            };
                            let Expression::StaticMemberExpression(mem) = self.unwrap_parens(arg1) else {
                                return;
                            };
                            if mem.property.name.as_str() != "key" {
                                return;
                            }

                            // arg2 should be same "something" identifier
                            let Some(arg2) = call.arguments[2].as_expression() else {
                                return;
                            };
                            let Expression::Identifier(desc_id) = self.unwrap_parens(arg2) else {
                                return;
                            };
                            let Expression::Identifier(desc_obj_id) = self.unwrap_parens(&mem.object) else {
                                return;
                            };
                            if desc_id.name.as_str() != desc_obj_id.name.as_str() {
                                return;
                            }

                            self.found = true;
                            return;
                        }
                    }
                }

                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {
                // Don't descend into nested functions
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Don't descend into nested functions
            }
        }

        let mut finder = Finder { target_name, found: false };
        let mut body_clone = body.clone_in(self.allocator);
        finder.visit_function_body(&mut body_clone);
        finder.found
    }

    fn helper_names_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut out = HashSet::new();
        for s in stmts {
            let Statement::FunctionDeclaration(fd) = s else { continue; };
            let func = &**fd;
            let Some(id) = func.id.as_ref() else { continue; };
            if self.is_define_properties_helper(func) {
                out.insert(id.name.as_str().to_string());
            }
        }
        out
    }

    fn descriptor_key_value<'b>(
        &self,
        obj: &'b ObjectExpression<'a>,
    ) -> Option<(&'b StringLiteral<'a>, &'b Expression<'a>)> {
        let mut key: Option<&StringLiteral<'a>> = None;
        let mut value: Option<&Expression<'a>> = None;

        for prop in &obj.properties {
            let ObjectPropertyKind::ObjectProperty(p) = prop else { return None; };
            let PropertyKey::StaticIdentifier(ident) = &p.key else { return None; };
            let name = ident.name.as_str();
            if name == "key" {
                let Expression::StringLiteral(s) = self.unwrap_parens(&p.value) else { return None; };
                key = Some(s);
            } else if name == "value" {
                value = Some(&p.value);
            } else {
                // Be conservative: ignore descriptors with extra fields (get/set/enumerable/etc)
                return None;
            }
        }

        Some((key?, value?))
    }

    fn inline_define_properties_call(&self, helper_name: &str, stmt: &Statement<'a>) -> Option<ArenaVec<'a, Statement<'a>>> {
        let Statement::ExpressionStatement(es) = stmt else { return None; };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else { return None; };
        let Expression::Identifier(id) = self.unwrap_parens(&call.callee) else { return None; };
        if id.name.as_str() != helper_name {
            return None;
        }
        if call.arguments.len() != 2 {
            return None;
        }
        let Some(target_expr) = call.arguments[0].as_expression() else { return None; };
        let Some(arr_expr) = call.arguments[1].as_expression() else { return None; };
        let Expression::ArrayExpression(arr) = self.unwrap_parens(arr_expr) else { return None; };

        let mut out = ArenaVec::new_in(self.allocator);

        for el in &arr.elements {
            let Some(el_expr) = el.as_expression() else { return None; };
            let Expression::ObjectExpression(obj) = self.unwrap_parens(el_expr) else { return None; };
            let (key, value_expr) = self.descriptor_key_value(obj)?;

            // target["key"] = value;
            let key_expr = Expression::StringLiteral(ArenaBox::new_in(key.clone_in(self.allocator), self.allocator));
            let member_expr = MemberExpression::ComputedMemberExpression(ArenaBox::new_in(
                ComputedMemberExpression {
                    span: call.span,
                    object: target_expr.clone_in(self.allocator),
                    expression: key_expr,
                    optional: false,
                },
                self.allocator,
            ));

            let assign = Expression::AssignmentExpression(ArenaBox::new_in(
                AssignmentExpression {
                    span: call.span,
                    operator: AssignmentOperator::Assign,
                    left: AssignmentTarget::from(member_expr),
                    right: value_expr.clone_in(self.allocator),
                },
                self.allocator,
            ));

            out.push(Statement::ExpressionStatement(ArenaBox::new_in(
                ExpressionStatement { span: call.span, expression: assign },
                self.allocator,
            )));
        }

        Some(out)
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let helper_names = self.helper_names_in_statement_list(stmts);
        if helper_names.is_empty() {
            return;
        }

        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            let mut replaced = false;
            for helper_name in &helper_names {
                if let Some(repl) = self.inline_define_properties_call(helper_name, &stmt) {
                    for s in repl {
                        out.push(s);
                    }
                    replaced = true;
                    self.modified = true;
                    break;
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

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        // Ensure we still walk and simplify nested blocks.
        let _ = self.unwrap_parens_mut(it);
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
