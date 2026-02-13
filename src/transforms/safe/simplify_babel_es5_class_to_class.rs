use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;

use std::collections::HashSet;

use crate::{Transform, TransformCtx};

pub struct SimplifyBabelEs5ClassToClass;

impl Transform for SimplifyBabelEs5ClassToClass {
    fn name(&self) -> &'static str {
        "simplifyBabelEs5ClassToClass"
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

    fn is_identifier_ref_named(&self, expr: &Expression<'a>, name: &str) -> bool {
        let Expression::Identifier(id) = self.unwrap_parens(expr) else {
            return false;
        };
        id.name.as_str() == name
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

    fn method_from_function(&self, span: Span, key: IdentifierName<'a>, func: &Function<'a>, is_static: bool) -> ClassElement<'a> {
        let mut f = func.clone_in(self.allocator);
        f.id = None;
        f.r#type = FunctionType::FunctionExpression;

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
        if stmts.is_empty() {
            return;
        }

        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        let mut i = 0;
        while i < original.len() {
            let stmt = &original[i];

            let Statement::FunctionDeclaration(fd) = stmt else {
                out.push(original[i].clone_in(self.allocator));
                i += 1;
                continue;
            };

            let func = &**fd;
            let Some(id) = func.id.as_ref() else {
                out.push(original[i].clone_in(self.allocator));
                i += 1;
                continue;
            };
            let class_name = id.name.as_str();

            let mut class_names: HashSet<String> = HashSet::new();
            class_names.insert(class_name.to_string());

            let mut elements = ArenaVec::new_in(self.allocator);
            elements.push(self.constructor_from_function(func.span, func));

            let mut j = i + 1;
            let mut consumed_any = false;

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

            while j < original.len() {
                // Skip temp array assignments like `i = [ ... ];` which are only used to feed helper calls.
                if let Statement::ExpressionStatement(es) = &original[j] {
                    if let Expression::AssignmentExpression(ae) = self.unwrap_parens(&es.expression) {
                        if ae.operator == AssignmentOperator::Assign {
                            if matches!(self.unwrap_parens(&ae.right), Expression::ArrayExpression(_)) {
                                j += 1;
                                continue;
                            }
                        }
                    }
                }

                let Statement::ExpressionStatement(es) = &original[j] else {
                    break;
                };

                let Some((span, key, rhs_func, is_static)) = self.match_proto_assignment(&class_names, &es.expression) else {
                    break;
                };

                elements.push(self.method_from_function(span, key, &rhs_func, is_static));
                consumed_any = true;
                j += 1;
            }

            if !consumed_any {
                out.push(original[i].clone_in(self.allocator));
                i += 1;
                continue;
            }

            let class_body = ClassBody { span: func.span, body: elements };
            let class = Class {
                span: func.span,
                r#type: ClassType::ClassDeclaration,
                decorators: ArenaVec::new_in(self.allocator),
                id: Some(id.clone_in(self.allocator)),
                type_parameters: None,
                super_class: None,
                super_type_arguments: None,
                implements: ArenaVec::new_in(self.allocator),
                body: ArenaBox::new_in(class_body, self.allocator),
                r#abstract: false,
                declare: false,
                scope_id: Default::default(),
            };

            out.push(Statement::ClassDeclaration(ArenaBox::new_in(class, self.allocator)));

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

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        let _ = self.unwrap_parens(it);
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
