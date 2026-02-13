use std::collections::HashSet;
use std::collections::HashMap;

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

    fn function_names_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut out = HashSet::new();
        for s in stmts {
            let Statement::FunctionDeclaration(fd) = s else { continue; };
            let func = &**fd;
            let Some(id) = func.id.as_ref() else { continue; };
            out.insert(id.name.as_str().to_string());
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
            } else if name == "enumerable" || name == "configurable" || name == "writable" {
                // Common Babel descriptor flags, safe to ignore for restructuring.
                continue;
            } else if name == "get" || name == "set" {
                // Accessor descriptors require special handling (not done here).
                return None;
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

    fn unwrap_if_truthy_ident_then_helper_call(
        &self,
        helper_name: &str,
        fn_names: &HashSet<String>,
        stmt: &Statement<'a>,
    ) -> Option<Statement<'a>> {
        // Pattern:
        // if (fnIdent) { helper(target, arrId); }
        // If fnIdent is a function declaration in this statement list, this condition is always truthy.
        // Rewrite to `helper(target, arrId);`.
        let Statement::IfStatement(ifstmt) = stmt else { return None; };
        if ifstmt.alternate.is_some() {
            return None;
        }
        let Expression::Identifier(test_id) = self.unwrap_parens(&ifstmt.test) else {
            return None;
        };
        if !fn_names.contains(test_id.name.as_str()) {
            return None;
        }
        let Statement::BlockStatement(cons) = &ifstmt.consequent else { return None; };
        if cons.body.len() != 1 {
            return None;
        }
        let Statement::ExpressionStatement(es) = &cons.body[0] else { return None; };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else { return None; };
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else { return None; };
        if callee_id.name.as_str() != helper_name {
            return None;
        }
        Some(Statement::ExpressionStatement(ArenaBox::new_in(
            ExpressionStatement { span: call.span, expression: Expression::CallExpression(call.clone_in(self.allocator)) },
            self.allocator,
        )))
    }

    fn unwrap_if_truthy_array_ident_then_helper_call(
        &self,
        helper_name: &str,
        array_assigns: &HashMap<String, ArenaBox<'a, ArrayExpression<'a>>>,
        stmt: &Statement<'a>,
    ) -> Option<Statement<'a>> {
        // Pattern:
        // if (arrIdent) { helper(target, arrIdent); }
        // If arrIdent was assigned an array literal earlier, it is always truthy.
        let Statement::IfStatement(ifstmt) = stmt else { return None; };
        if ifstmt.alternate.is_some() {
            return None;
        }
        let Expression::Identifier(test_id) = self.unwrap_parens(&ifstmt.test) else {
            return None;
        };
        if !array_assigns.contains_key(test_id.name.as_str()) {
            return None;
        }
        let Statement::BlockStatement(cons) = &ifstmt.consequent else { return None; };
        if cons.body.len() != 1 {
            return None;
        }
        let Statement::ExpressionStatement(es) = &cons.body[0] else { return None; };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else { return None; };
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else { return None; };
        if callee_id.name.as_str() != helper_name {
            return None;
        }
        if call.arguments.len() != 2 {
            return None;
        }
        let Some(arg1) = call.arguments[1].as_expression() else { return None; };
        if !matches!(self.unwrap_parens(arg1), Expression::Identifier(id) if id.name.as_str() == test_id.name.as_str()) {
            return None;
        }

        Some(Statement::ExpressionStatement(ArenaBox::new_in(
            ExpressionStatement { span: call.span, expression: Expression::CallExpression(call.clone_in(self.allocator)) },
            self.allocator,
        )))
    }

    fn inline_define_properties_call_with_array(
        &self,
        helper_name: &str,
        target_expr: &Expression<'a>,
        arr: &ArrayExpression<'a>,
        span: oxc_span::Span,
    ) -> Option<ArenaVec<'a, Statement<'a>>> {
        let mut out = ArenaVec::new_in(self.allocator);

        for el in &arr.elements {
            let Some(el_expr) = el.as_expression() else { return None; };
            let Expression::ObjectExpression(obj) = self.unwrap_parens(el_expr) else { return None; };
            let (key, value_expr) = self.descriptor_key_value(obj)?;

            // target["key"] = value;
            let key_expr = Expression::StringLiteral(ArenaBox::new_in(key.clone_in(self.allocator), self.allocator));
            let member_expr = MemberExpression::ComputedMemberExpression(ArenaBox::new_in(
                ComputedMemberExpression {
                    span,
                    object: target_expr.clone_in(self.allocator),
                    expression: key_expr,
                    optional: false,
                },
                self.allocator,
            ));

            let assign = Expression::AssignmentExpression(ArenaBox::new_in(
                AssignmentExpression {
                    span,
                    operator: AssignmentOperator::Assign,
                    left: AssignmentTarget::from(member_expr),
                    right: value_expr.clone_in(self.allocator),
                },
                self.allocator,
            ));

            out.push(Statement::ExpressionStatement(ArenaBox::new_in(
                ExpressionStatement { span, expression: assign },
                self.allocator,
            )));
        }

        let _ = helper_name;
        Some(out)
    }

    fn unwrap_if_assigned_array_then_helper_call(
        &self,
        helper_name: &str,
        stmt: &Statement<'a>,
    ) -> Option<ArenaVec<'a, Statement<'a>>> {
        // Pattern:
        // if (arrId = [ ... ]) { helper(target, arrId); }
        // We rewrite to:
        // arrId = [ ... ];
        // helper(target, arrId);
        let Statement::IfStatement(ifstmt) = stmt else {
            return None;
        };
        if ifstmt.alternate.is_some() {
            return None;
        }
        let Expression::AssignmentExpression(ae) = self.unwrap_parens(&ifstmt.test) else {
            return None;
        };
        if ae.operator != AssignmentOperator::Assign {
            return None;
        }
        let AssignmentTarget::AssignmentTargetIdentifier(arr_id) = &ae.left else {
            return None;
        };
        let Expression::ArrayExpression(_) = self.unwrap_parens(&ae.right) else {
            return None;
        };

        let Statement::BlockStatement(cons) = &ifstmt.consequent else {
            return None;
        };
        if cons.body.len() != 1 {
            return None;
        }
        let Statement::ExpressionStatement(es) = &cons.body[0] else {
            return None;
        };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else {
            return None;
        };
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else {
            return None;
        };
        if callee_id.name.as_str() != helper_name {
            return None;
        }
        if call.arguments.len() != 2 {
            return None;
        }
        let Some(arg1) = call.arguments[1].as_expression() else {
            return None;
        };
        if !matches!(self.unwrap_parens(arg1), Expression::Identifier(id) if id.name.as_str() == arr_id.name.as_str()) {
            return None;
        }

        // Emit `arrId = [..];`
        let assign_expr = Expression::AssignmentExpression(ArenaBox::new_in(
            AssignmentExpression {
                span: ae.span,
                operator: AssignmentOperator::Assign,
                left: ae.left.clone_in(self.allocator),
                right: ae.right.clone_in(self.allocator),
            },
            self.allocator,
        ));
        let assign_stmt = Statement::ExpressionStatement(ArenaBox::new_in(
            ExpressionStatement { span: ae.span, expression: assign_expr },
            self.allocator,
        ));

        // Emit `helper(target, arrId);`
        let helper_stmt = Statement::ExpressionStatement(ArenaBox::new_in(
            ExpressionStatement {
                span: call.span,
                expression: Expression::CallExpression(ArenaBox::new_in(
                    CallExpression {
                        span: call.span,
                        callee: call.callee.clone_in(self.allocator),
                        type_arguments: None,
                        arguments: call.arguments.clone_in(self.allocator),
                        optional: false,
                        pure: false,
                    },
                    self.allocator,
                )),
            },
            self.allocator,
        ));

        let mut out = ArenaVec::new_in(self.allocator);
        out.push(assign_stmt);
        out.push(helper_stmt);
        Some(out)
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let helper_names = self.helper_names_in_statement_list(stmts);
        if helper_names.is_empty() {
            return;
        }

        let fn_names = self.function_names_in_statement_list(stmts);

        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        // Track recent pattern `tmp = [ ... ];` so later `helper(target, tmp)` can be inlined.
        // This is intentionally simple (statement-local, no control-flow tracking).
        let mut array_assigns: HashMap<String, ArenaBox<'a, ArrayExpression<'a>>> = HashMap::new();

        for stmt in original {
            let mut replaced = false;

            // Rewrite `if (fnIdent) { helper(...); }` into `helper(...);` when safe.
            for helper_name in &helper_names {
                if let Some(unwrapped) = self.unwrap_if_truthy_ident_then_helper_call(helper_name, &fn_names, &stmt) {
                    out.push(unwrapped);
                    replaced = true;
                    self.modified = true;
                    break;
                }
                if let Some(unwrapped) = self.unwrap_if_truthy_array_ident_then_helper_call(helper_name, &array_assigns, &stmt) {
                    out.push(unwrapped);
                    replaced = true;
                    self.modified = true;
                    break;
                }
            }
            if replaced {
                continue;
            }

            // Capture `id = [ ... ];`
            if let Statement::ExpressionStatement(es) = &stmt {
                if let Expression::AssignmentExpression(ae) = self.unwrap_parens(&es.expression) {
                    if ae.operator == AssignmentOperator::Assign {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &ae.left {
                            if let Expression::ArrayExpression(arr) = self.unwrap_parens(&ae.right) {
                                array_assigns.insert(
                                    id.name.as_str().to_string(),
                                    ArenaBox::new_in((**arr).clone_in(self.allocator), self.allocator),
                                );
                            }
                        }
                    }
                }
            }

            for helper_name in &helper_names {
                if let Some(repl) = self.unwrap_if_assigned_array_then_helper_call(helper_name, &stmt) {
                    for s in repl {
                        out.push(s);
                    }
                    replaced = true;
                    self.modified = true;
                    break;
                }

                // Inline `helper(target, tmp)` when `tmp` was previously assigned an array literal.
                if let Statement::ExpressionStatement(es) = &stmt {
                    if let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) {
                        if let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) {
                            if callee_id.name.as_str() == helper_name && call.arguments.len() == 2 {
                                if let (Some(target_expr), Some(arg1)) = (
                                    call.arguments[0].as_expression(),
                                    call.arguments[1].as_expression(),
                                ) {
                                    if let Expression::Identifier(tmp_id) = self.unwrap_parens(arg1) {
                                        if let Some(tmp_arr) = array_assigns.get(tmp_id.name.as_str()) {
                                            if let Some(repl) = self.inline_define_properties_call_with_array(
                                                helper_name,
                                                target_expr,
                                                tmp_arr.as_ref(),
                                                call.span,
                                            ) {
                                                for s in repl {
                                                    out.push(s);
                                                }
                                                replaced = true;
                                                self.modified = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

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
