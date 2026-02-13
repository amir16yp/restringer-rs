use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::{ContentEq, Span};

use std::collections::HashSet;
use std::collections::HashMap;

use crate::{Transform, TransformCtx};

pub struct SimplifyBabelEs5ClassToClass;

impl Transform for SimplifyBabelEs5ClassToClass {
    fn name(&self) -> &'static str {
        "simplifyBabelEs5ClassToClass"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, modified: false, reserved_param_stack: Vec::new() };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
    reserved_param_stack: Vec<HashSet<String>>,
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

    fn current_reserved_param_names(&self) -> Option<&HashSet<String>> {
        self.reserved_param_stack.last()
    }

    fn collect_param_names(params: &FormalParameters<'a>) -> HashSet<String> {
        let mut out = HashSet::new();
        for p in &params.items {
            if let BindingPattern::BindingIdentifier(id) = &p.pattern {
                out.insert(id.name.as_str().to_string());
            }
        }
        out
    }

    fn make_safe_class_name(&self, desired: &str, reserved: Option<&HashSet<String>>) -> String {
        let Some(reserved) = reserved else {
            return desired.to_string();
        };
        if !reserved.contains(desired) {
            return desired.to_string();
        }
        // Choose a deterministic name that avoids collisions with in-scope parameter bindings.
        // Keep it simple: prefix '_' until it's free.
        let mut candidate = format!("_{desired}");
        while reserved.contains(candidate.as_str()) {
            candidate = format!("_{candidate}");
        }
        candidate
    }

    fn record_declared_names_in_stmt(&self, stmt: &Statement<'a>, out: &mut HashSet<String>) {
        match stmt {
            Statement::FunctionDeclaration(fd) => {
                if let Some(id) = (**fd).id.as_ref() {
                    out.insert(id.name.as_str().to_string());
                }
            }
            Statement::ClassDeclaration(cd) => {
                if let Some(id) = (**cd).id.as_ref() {
                    out.insert(id.name.as_str().to_string());
                }
            }
            Statement::VariableDeclaration(vd) => {
                for decl in &vd.declarations {
                    if let BindingPattern::BindingIdentifier(id) = &decl.id {
                        out.insert(id.name.as_str().to_string());
                    }
                }
            }
            _ => {}
        }
    }

    fn make_safe_class_name_from_sets(&self, desired: &str, reserved: Option<&HashSet<String>>, declared: &HashSet<String>) -> String {
        let mut name = self.make_safe_class_name(desired, reserved);
        if !declared.contains(&name) {
            return name;
        }
        let mut candidate = format!("_{name}");
        while declared.contains(&candidate) {
            candidate = format!("_{candidate}");
        }
        name = candidate;
        name
    }

    fn rename_identifier_references_in_statement_list(
        &self,
        stmts: &mut ArenaVec<'a, Statement<'a>>,
        from: &str,
        to: &str,
    ) {
        if from == to {
            return;
        }
        let from_owned = from.to_string();
        let to_alloc = self.allocator.alloc_str(to);

        struct Renamer<'a> {
            allocator: &'a oxc_allocator::Allocator,
            from: String,
            to: &'a str,
        }

        impl<'a> VisitMut<'a> for Renamer<'a> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                if let Expression::Identifier(id) = it {
                    if id.name.as_str() == self.from.as_str() {
                        let name = self.allocator.alloc_str(self.to);
                        id.name = name.into();
                    }
                }
                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {
                // Do not descend into nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Do not descend into nested arrows.
            }
        }

        let mut r = Renamer { allocator: self.allocator, from: from_owned, to: to_alloc };
        for s in stmts.iter_mut() {
            r.visit_statement(s);
        }
    }

    fn descriptor_key_value<'b>(
        &self,
        obj: &'b ObjectExpression<'a>,
    ) -> Option<(&'b StringLiteral<'a>, &'b Function<'a>)> {
        // Accept minimal Babel descriptors: { key: "x", value: function(...) { ... } }
        // Ignore common flags (enumerable/configurable/writable).
        // Reject accessors or extra fields.
        let mut key: Option<&StringLiteral<'a>> = None;
        let mut value: Option<&Function<'a>> = None;

        for prop in &obj.properties {
            let ObjectPropertyKind::ObjectProperty(p) = prop else {
                return None;
            };
            let PropertyKey::StaticIdentifier(ident) = &p.key else {
                return None;
            };
            match ident.name.as_str() {
                "key" => {
                    let Expression::StringLiteral(s) = self.unwrap_parens(&p.value) else {
                        return None;
                    };
                    key = Some(s);
                }
                "value" => {
                    let Expression::FunctionExpression(f) = self.unwrap_parens(&p.value) else {
                        return None;
                    };
                    value = Some(&**f);
                }
                "enumerable" | "configurable" | "writable" => {
                    // ignore
                }
                "get" | "set" => {
                    return None;
                }
                _ => {
                    return None;
                }
            }
        }

        Some((key?, value?))
    }

    fn method_from_descriptor(
        &self,
        span: Span,
        key_lit: &StringLiteral<'a>,
        func: &Function<'a>,
        is_static: bool,
        super_class: Option<&Expression<'a>>,
    ) -> Option<ClassElement<'a>> {
        let key_str = key_lit.value.as_str();
        if key_str.is_empty() {
            return None;
        }
        let key = IdentifierName { span: key_lit.span, name: self.allocator.alloc_str(key_str).into() };
        Some(self.method_from_function(span, key, func, is_static, super_class))
    }

    fn is_proto_of_class_alias(&self, class_names: &HashSet<String>, expr: &Expression<'a>) -> Option<String> {
        // Match: <Alias>.prototype
        let Expression::StaticMemberExpression(mem) = self.unwrap_parens(expr) else {
            return None;
        };
        if mem.property.name.as_str() != "prototype" {
            return None;
        }
        let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) else {
            return None;
        };
        if !class_names.contains(obj.name.as_str()) {
            return None;
        }
        Some(obj.name.as_str().to_string())
    }

    fn is_class_alias_ident(&self, class_names: &HashSet<String>, expr: &Expression<'a>) -> Option<String> {
        let Expression::Identifier(id) = self.unwrap_parens(expr) else {
            return None;
        };
        if !class_names.contains(id.name.as_str()) {
            return None;
        }
        Some(id.name.as_str().to_string())
    }

    fn match_define_properties_helper_call(
        &self,
        class_names: &HashSet<String>,
        array_assigns: &HashMap<String, ArenaBox<'a, ArrayExpression<'a>>>,
        expr: &Expression<'a>,
    ) -> Option<(Span, bool, ArenaBox<'a, ArrayExpression<'a>>)> {
        // Match: helper(<Alias>.prototype, arr) OR helper(<Alias>, arr)
        // where arr is an array literal OR an identifier previously assigned an array literal.
        let Expression::CallExpression(call) = self.unwrap_parens(expr) else {
            return None;
        };
        if call.arguments.len() != 2 {
            return None;
        }
        let Some(target_expr) = call.arguments[0].as_expression() else {
            return None;
        };
        let Some(props_expr) = call.arguments[1].as_expression() else {
            return None;
        };

        let is_static = if self.is_proto_of_class_alias(class_names, target_expr).is_some() {
            false
        } else if self.is_class_alias_ident(class_names, target_expr).is_some() {
            true
        } else {
            return None;
        };

        let arr = match self.unwrap_parens(props_expr) {
            Expression::ArrayExpression(a) => ArenaBox::new_in((**a).clone_in(self.allocator), self.allocator),
            Expression::Identifier(id) => array_assigns.get(id.name.as_str())?.clone_in(self.allocator),
            _ => return None,
        };

        Some((call.span, is_static, arr))
    }

    fn call_expr_args_are_all_param_idents(&self, call: &CallExpression<'a>, func: &Function<'a>) -> bool {
        if call.arguments.len() != func.params.items.len() {
            return false;
        }
        for (arg, param) in call.arguments.iter().zip(func.params.items.iter()) {
            let Some(arg_expr) = arg.as_expression() else {
                return false;
            };
            let Expression::Identifier(arg_id) = self.unwrap_parens(arg_expr) else {
                return false;
            };
            let BindingPattern::BindingIdentifier(param_id) = &param.pattern else {
                return false;
            };
            if arg_id.name.as_str() != param_id.name.as_str() {
                return false;
            }
        }
        true
    }

    fn match_inherits_call(&self, class_names: &HashSet<String>, stmt: &Statement<'a>) -> Option<Expression<'a>> {
        // Pattern: helper(Sub, Super);
        // Only accept if Sub is in class_names.
        let Statement::ExpressionStatement(es) = stmt else {
            return None;
        };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else {
            return None;
        };
        let Expression::Identifier(_callee_id) = self.unwrap_parens(&call.callee) else {
            return None;
        };
        if call.arguments.len() != 2 {
            return None;
        }
        let Some(arg0) = call.arguments[0].as_expression() else {
            return None;
        };
        let Some(arg1) = call.arguments[1].as_expression() else {
            return None;
        };
        let Expression::Identifier(sub_id) = self.unwrap_parens(arg0) else {
            return None;
        };
        if !class_names.contains(sub_id.name.as_str()) {
            return None;
        }
        Some(arg1.clone_in(self.allocator))
    }

    fn rewrite_constructor_super_call(&self, func: &Function<'a>, super_expr: &Expression<'a>) -> Function<'a> {
        // Pattern: function C(a,b){ Super(a,b); ... }
        // Rewrite first statement into `super(a,b);`.
        let mut out = func.clone_in(self.allocator);
        let Some(body) = out.body.as_mut() else {
            return out;
        };
        if body.statements.is_empty() {
            return out;
        }
        let Statement::ExpressionStatement(es) = &body.statements[0] else {
            return out;
        };
        let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else {
            return out;
        };
        if !self.call_expr_args_are_all_param_idents(call, func) {
            return out;
        }
        if !super_expr.content_eq(&call.callee) {
            return out;
        }

        let super_callee = Expression::Super(ArenaBox::new_in(Super { span: call.span }, self.allocator));
        let new_call = Expression::CallExpression(ArenaBox::new_in(
            CallExpression {
                span: call.span,
                callee: super_callee,
                type_arguments: None,
                arguments: call.arguments.clone_in(self.allocator),
                optional: false,
                pure: call.pure,
            },
            self.allocator,
        ));

        body.statements[0] = Statement::ExpressionStatement(ArenaBox::new_in(
            ExpressionStatement { span: es.span, expression: new_call },
            self.allocator,
        ));

        out
    }

    fn is_identifier_ref_named(&self, expr: &Expression<'a>, name: &str) -> bool {
        let Expression::Identifier(id) = self.unwrap_parens(expr) else {
            return false;
        };
        id.name.as_str() == name
    }

    fn match_module_exports_assign(&self, class_names: &HashSet<String>, stmt: &Statement<'a>) -> bool {
        // Pattern: module.exports = C;
        let Statement::ExpressionStatement(es) = stmt else {
            return false;
        };
        let Expression::AssignmentExpression(ae) = self.unwrap_parens(&es.expression) else {
            return false;
        };
        if ae.operator != AssignmentOperator::Assign {
            return false;
        }
        let AssignmentTarget::StaticMemberExpression(mem) = &ae.left else {
            return false;
        };
        let Expression::Identifier(obj) = self.unwrap_parens(&mem.object) else {
            return false;
        };
        if obj.name.as_str() != "module" {
            return false;
        }
        if mem.property.name.as_str() != "exports" {
            return false;
        }
        let Expression::Identifier(rhs_id) = self.unwrap_parens(&ae.right) else {
            return false;
        };
        class_names.contains(rhs_id.name.as_str())
    }

    fn rewrite_super_prototype_member_in_function(&self, func: &mut Function<'a>, super_expr: &Expression<'a>) {
        // Rewrite `Super.prototype.x` to `super.x` in this function body.
        let Expression::Identifier(super_id) = self.unwrap_parens(super_expr) else {
            return;
        };
        let super_name = super_id.name.as_str();

        let Some(body) = func.body.as_mut() else {
            return;
        };

        struct Rewriter<'a> {
            allocator: &'a oxc_allocator::Allocator,
            super_name: &'a str,
            modified: bool,
        }

        impl<'a> Rewriter<'a> {
            fn unwrap_parens_mut<'b>(&self, mut expr: &'b mut Expression<'a>) -> &'b mut Expression<'a> {
                loop {
                    match expr {
                        Expression::ParenthesizedExpression(p) => expr = &mut p.expression,
                        _ => return expr,
                    }
                }
            }

            fn try_rewrite_member(&mut self, expr: &mut Expression<'a>) {
                let Expression::StaticMemberExpression(outer) = self.unwrap_parens_mut(expr) else {
                    return;
                };
                let Expression::StaticMemberExpression(inner) = &outer.object else {
                    return;
                };
                if inner.property.name.as_str() != "prototype" {
                    return;
                }
                let Expression::Identifier(obj_id) = &inner.object else {
                    return;
                };
                if obj_id.name.as_str() != self.super_name {
                    return;
                }

                // Turn `Super.prototype.x` into `super.x`
                let super_expr = Expression::Super(ArenaBox::new_in(Super { span: outer.span }, self.allocator));
                let new_mem = Expression::StaticMemberExpression(ArenaBox::new_in(
                    StaticMemberExpression {
                        span: outer.span,
                        object: super_expr,
                        property: outer.property.clone_in(self.allocator),
                        optional: false,
                    },
                    self.allocator,
                ));
                *expr = new_mem;
                self.modified = true;
            }
        }

        impl<'a> VisitMut<'a> for Rewriter<'a> {
            fn visit_expression(&mut self, it: &mut Expression<'a>) {
                self.try_rewrite_member(it);
                oxc_ast_visit::walk_mut::walk_expression(self, it);
            }

            fn visit_function(&mut self, _it: &mut Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {
                // Don't descend into nested functions.
            }

            fn visit_arrow_function_expression(&mut self, _it: &mut ArrowFunctionExpression<'a>) {
                // Don't descend into nested arrows.
            }
        }

        let mut r = Rewriter { allocator: self.allocator, super_name, modified: false };
        r.visit_function_body(body);
        let _ = r.modified;
    }

    fn match_proto_assignment(
        &self,
        class_names: &HashSet<String>,
        expr: &Expression<'a>,
    ) -> Option<(Span, IdentifierName<'a>, Function<'a>, bool)> {
        // Returns: (span, method_name, function, is_static)
        let Expression::AssignmentExpression(assign) = self.unwrap_parens(expr) else {
            return None;
        };
        if assign.operator != AssignmentOperator::Assign {
            return None;
        }

        match &assign.left {
            AssignmentTarget::StaticMemberExpression(smem) => {
                // C.prototype.foo = function(){}  OR  C.bar = function(){}
                let left_obj = self.unwrap_parens(&smem.object);

                // Static: C.bar
                if let Expression::Identifier(id) = self.unwrap_parens(left_obj) {
                    if class_names.contains(id.name.as_str()) {
                        let Expression::FunctionExpression(func) = self.unwrap_parens(&assign.right) else {
                            return None;
                        };
                        return Some((
                            assign.span,
                            smem.property.clone_in(self.allocator),
                            (**func).clone_in(self.allocator),
                            true,
                        ));
                    }
                }

                // Prototype: C.prototype.foo
                let Expression::StaticMemberExpression(proto_mem) = left_obj else {
                    return None;
                };
                if proto_mem.property.name.as_str() != "prototype" {
                    return None;
                }
                if let Expression::Identifier(id) = self.unwrap_parens(&proto_mem.object) {
                    if !class_names.contains(id.name.as_str()) {
                        return None;
                    }
                } else {
                    return None;
                }

                let Expression::FunctionExpression(func) = self.unwrap_parens(&assign.right) else {
                    return None;
                };
                Some((assign.span, smem.property.clone_in(self.allocator), (**func).clone_in(self.allocator), false))
            }
            AssignmentTarget::ComputedMemberExpression(cmem) => {
                // C.prototype["foo"] = function(){} OR C["bar"] = function(){}
                let Expression::StringLiteral(key_lit) = self.unwrap_parens(&cmem.expression) else {
                    return None;
                };
                let key_str = key_lit.value.as_str();
                // Only handle keys that can become IdentifierName safely.
                if key_str.is_empty() {
                    return None;
                }
                let key = IdentifierName { span: key_lit.span, name: self.allocator.alloc_str(key_str).into() };

                let left_obj = self.unwrap_parens(&cmem.object);
                // Static: C["bar"]
                if let Expression::Identifier(id) = self.unwrap_parens(left_obj) {
                    if class_names.contains(id.name.as_str()) {
                        let Expression::FunctionExpression(func) = self.unwrap_parens(&assign.right) else {
                            return None;
                        };
                        return Some((assign.span, key, (**func).clone_in(self.allocator), true));
                    }
                }

                // Prototype: C.prototype["foo"]
                let Expression::StaticMemberExpression(proto_mem) = left_obj else {
                    return None;
                };
                if proto_mem.property.name.as_str() != "prototype" {
                    return None;
                }
                if let Expression::Identifier(id) = self.unwrap_parens(&proto_mem.object) {
                    if !class_names.contains(id.name.as_str()) {
                        return None;
                    }
                } else {
                    return None;
                }
                let Expression::FunctionExpression(func) = self.unwrap_parens(&assign.right) else {
                    return None;
                };
                Some((assign.span, key, (**func).clone_in(self.allocator), false))
            }
            _ => None,
        }
    }

    fn method_from_function(
        &self,
        span: Span,
        key: IdentifierName<'a>,
        func: &Function<'a>,
        is_static: bool,
        super_class: Option<&Expression<'a>>,
    ) -> ClassElement<'a> {
        let mut f = func.clone_in(self.allocator);
        f.id = None;
        f.r#type = FunctionType::FunctionExpression;

        if !is_static {
            if let Some(sc) = super_class {
                self.rewrite_super_prototype_member_in_function(&mut f, sc);
            }
        }

        let md = MethodDefinition {
            span,
            r#type: MethodDefinitionType::MethodDefinition,
            decorators: ArenaVec::new_in(self.allocator),
            key: PropertyKey::StaticIdentifier(ArenaBox::new_in(key, self.allocator)),
            value: ArenaBox::new_in(f, self.allocator),
            kind: MethodDefinitionKind::Method,
            computed: false,
            r#static: is_static,
            r#override: false,
            optional: false,
            accessibility: None,
        };

        ClassElement::MethodDefinition(ArenaBox::new_in(md, self.allocator))
    }

    fn constructor_from_function(&self, span: Span, func: &Function<'a>) -> ClassElement<'a> {
        let mut f = func.clone_in(self.allocator);
        f.id = None;
        f.r#type = FunctionType::FunctionExpression;

        let key = IdentifierName { span, name: self.allocator.alloc_str("constructor").into() };

        let md = MethodDefinition {
            span,
            r#type: MethodDefinitionType::MethodDefinition,
            decorators: ArenaVec::new_in(self.allocator),
            key: PropertyKey::StaticIdentifier(ArenaBox::new_in(key, self.allocator)),
            value: ArenaBox::new_in(f, self.allocator),
            kind: MethodDefinitionKind::Constructor,
            computed: false,
            r#static: false,
            r#override: false,
            optional: false,
            accessibility: None,
        };

        ClassElement::MethodDefinition(ArenaBox::new_in(md, self.allocator))
    }

    fn rewrite_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        // Track identifiers already declared in this statement list scope.
        // This avoids emitting `class e {}` after earlier `function e(){}` / `var e = ...` in the same scope.
        let mut declared_names: HashSet<String> = HashSet::new();

        let mut i = 0;
        while i < original.len() {
            let stmt = &original[i];

            let Statement::FunctionDeclaration(fd) = stmt else {
                let cloned = original[i].clone_in(self.allocator);
                self.record_declared_names_in_stmt(&cloned, &mut declared_names);
                out.push(cloned);
                i += 1;
                continue;
            };

            let func = &**fd;
            let Some(id) = func.id.as_ref() else {
                let cloned = original[i].clone_in(self.allocator);
                self.record_declared_names_in_stmt(&cloned, &mut declared_names);
                out.push(cloned);
                i += 1;
                continue;
            };
            let class_name = id.name.as_str();

            let mut class_names: HashSet<String> = HashSet::new();
            class_names.insert(class_name.to_string());

            let mut super_class: Option<Expression<'a>> = None;

            let mut elements = ArenaVec::new_in(self.allocator);
            // Constructor may be rewritten later if we find an inherits call.
            elements.push(self.constructor_from_function(func.span, func));

            let mut trailing_export_stmts: ArenaVec<'a, Statement<'a>> = ArenaVec::new_in(self.allocator);

            let mut j = i + 1;
            let mut consumed_any = false;

            // Track recent `id = [ ... ];` so we can resolve defineProperties helper calls that use temp arrays.
            let mut array_assigns: HashMap<String, ArenaBox<'a, ArrayExpression<'a>>> = HashMap::new();

            // Collect simple aliases like `var t = e;` or `t = e;`
            while j < original.len() {
                match &original[j] {
                    Statement::VariableDeclaration(vd) if vd.declarations.iter().all(|d| d.init.is_none()) => {
                        // Skip `var t; var i;` noise.
                        j += 1;
                        continue;
                    }
                    Statement::VariableDeclaration(vd) => {
                        if vd.declarations.len() != 1 {
                            break;
                        }
                        let decl = &vd.declarations[0];
                        let BindingPattern::BindingIdentifier(bi) = &decl.id else { break; };
                        let Some(init) = decl.init.as_ref() else { break; };
                        if self.is_identifier_ref_named(init, class_name) {
                            class_names.insert(bi.name.as_str().to_string());
                            j += 1;
                            continue;
                        }
                        break;
                    }
                    Statement::ExpressionStatement(es) => {
                        let Expression::AssignmentExpression(ae) = self.unwrap_parens(&es.expression) else { break; };
                        if ae.operator != AssignmentOperator::Assign {
                            break;
                        }
                        let AssignmentTarget::AssignmentTargetIdentifier(lhs) = &ae.left else { break; };
                        if self.is_identifier_ref_named(&ae.right, class_name) {
                            class_names.insert(lhs.name.as_str().to_string());
                            j += 1;
                            continue;
                        }
                        break;
                    }
                    _ => break,
                }
            }

            // Look for a simple inherits call next: helper(C, Super);
            if j < original.len() {
                if let Some(sc) = self.match_inherits_call(&class_names, &original[j]) {
                    super_class = Some(sc);
                    j += 1;
                }
            }

            while j < original.len() {
                // Skip temp array assignments like `i = [ ... ];` which are only used to feed helper calls.
                if let Statement::ExpressionStatement(es) = &original[j] {
                    if let Expression::AssignmentExpression(ae) = self.unwrap_parens(&es.expression) {
                        if ae.operator == AssignmentOperator::Assign {
                            if matches!(self.unwrap_parens(&ae.right), Expression::ArrayExpression(_)) {
                                // Record for later call-site consumption.
                                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &ae.left {
                                    if let Expression::ArrayExpression(arr) = self.unwrap_parens(&ae.right) {
                                        array_assigns.insert(
                                            id.name.as_str().to_string(),
                                            ArenaBox::new_in((**arr).clone_in(self.allocator), self.allocator),
                                        );
                                    }
                                }
                                j += 1;
                                continue;
                            }
                        }
                    }
                }

                let Statement::ExpressionStatement(es) = &original[j] else {
                    break;
                };

                // Match Babel/webpack defineProperties helper calls:
                // helper(Alias.prototype, propsArr) or helper(Alias, propsArr)
                if let Some((span, is_static, arr)) =
                    self.match_define_properties_helper_call(&class_names, &array_assigns, &es.expression)
                {
                    for el in &arr.elements {
                        let Some(el_expr) = el.as_expression() else {
                            // be conservative
                            break;
                        };
                        let Expression::ObjectExpression(obj) = self.unwrap_parens(el_expr) else {
                            break;
                        };
                        let Some((k, f)) = self.descriptor_key_value(obj) else {
                            break;
                        };
                        let Some(ce) = self.method_from_descriptor(span, k, f, is_static, super_class.as_ref()) else {
                            break;
                        };
                        elements.push(ce);
                        consumed_any = true;
                    }
                    j += 1;
                    continue;
                }

                // Allow `module.exports = C;` to appear before prototype assignments.
                if self.match_module_exports_assign(&class_names, &original[j]) {
                    trailing_export_stmts.push(original[j].clone_in(self.allocator));
                    j += 1;
                    continue;
                }

                let Some((span, key, rhs_func, is_static)) = self.match_proto_assignment(&class_names, &es.expression) else {
                    break;
                };

                elements.push(self.method_from_function(span, key, &rhs_func, is_static, super_class.as_ref()));
                consumed_any = true;
                j += 1;
            }

            if !consumed_any {
                let cloned = original[i].clone_in(self.allocator);
                self.record_declared_names_in_stmt(&cloned, &mut declared_names);
                out.push(cloned);
                i += 1;
                continue;
            }

            // If we detected `extends`, rewrite the constructor's leading `Super(...)` call into `super(...)`.
            if let Some(sc) = super_class.as_ref() {
                let rewritten_ctor = self.rewrite_constructor_super_call(func, sc);
                elements[0] = self.constructor_from_function(func.span, &rewritten_ctor);
                self.modified = true;
            }

            let reserved = self.current_reserved_param_names();
            let original_name = id.name.as_str();
            let safe_name = self.make_safe_class_name_from_sets(original_name, reserved, &declared_names);
            let mut class_id = id.clone_in(self.allocator);
            if safe_name != original_name {
                let safe_alloc = self.allocator.alloc_str(&safe_name);
                class_id.name = safe_alloc.into();
            }

            let class_body = ClassBody { span: func.span, body: elements };
            let class = Class {
                span: func.span,
                r#type: ClassType::ClassDeclaration,
                decorators: ArenaVec::new_in(self.allocator),
                id: Some(class_id),
                type_parameters: None,
                super_class,
                super_type_arguments: None,
                implements: ArenaVec::new_in(self.allocator),
                body: ArenaBox::new_in(class_body, self.allocator),
                r#abstract: false,
                declare: false,
                scope_id: Default::default(),
            };

            out.push(Statement::ClassDeclaration(ArenaBox::new_in(class, self.allocator)));

            declared_names.insert(safe_name.clone());

            if safe_name != original_name {
                // Update references in any trailing statements we preserved (e.g. `module.exports = C;`).
                let mut trailing = trailing_export_stmts;
                self.rename_identifier_references_in_statement_list(&mut trailing, original_name, &safe_name);
                for s in trailing {
                    out.push(s);
                }
            } else {
                for s in trailing_export_stmts {
                    out.push(s);
                }
            }

            self.modified = true;
            i = j;
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

    fn visit_function(&mut self, it: &mut Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        let reserved = Self::collect_param_names(&it.params);
        self.reserved_param_stack.push(reserved);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        let _ = self.reserved_param_stack.pop();
    }

    fn visit_arrow_function_expression(&mut self, it: &mut ArrowFunctionExpression<'a>) {
        let reserved = Self::collect_param_names(&it.params);
        self.reserved_param_stack.push(reserved);
        oxc_ast_visit::walk_mut::walk_arrow_function_expression(self, it);
        let _ = self.reserved_param_stack.pop();
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        let _ = self.unwrap_parens(it);
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
