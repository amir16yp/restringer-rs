use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use std::collections::HashSet;

use crate::{Transform, TransformCtx};

pub struct RemoveDeadNodes;

impl Transform for RemoveDeadNodes {
    fn name(&self) -> &'static str {
        "removeDeadNodes"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = ReferenceCollector::new();
        collector.visit_program(program);
        
        let mut remover = DeadNodeRemover {
            allocator: ctx.allocator,
            declared: collector.declared,
            referenced: collector.referenced,
            modified: false,
        };
        remover.visit_program(program);
        remover.modified
    }
}

struct ReferenceCollector {
    declared: HashSet<String>,
    referenced: HashSet<String>,
    current_scope_declarations: Vec<HashSet<String>>,
}

impl ReferenceCollector {
    fn new() -> Self {
        Self {
            declared: HashSet::new(),
            referenced: HashSet::new(),
            current_scope_declarations: vec![HashSet::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.current_scope_declarations.push(HashSet::new());
    }

    fn exit_scope(&mut self) {
        if let Some(scope_decls) = self.current_scope_declarations.pop() {
            self.declared.extend(scope_decls);
        }
    }

    fn declare(&mut self, name: &str) {
        if let Some(current) = self.current_scope_declarations.last_mut() {
            current.insert(name.to_string());
        }
    }

    fn reference(&mut self, name: &str) {
        self.referenced.insert(name.to_string());
    }
}

impl<'a> Visit<'a> for ReferenceCollector {
    fn visit_variable_declarator(&mut self, it: &VariableDeclarator<'a>) {
        if let BindingPattern::BindingIdentifier(id) = &it.id {
            self.declare(id.name.as_str());
        }
        if let Some(init) = &it.init {
            self.visit_expression(init);
        }
    }

    fn visit_function(&mut self, it: &Function<'a>, _flags: oxc_syntax::scope::ScopeFlags) {
        if let Some(id) = &it.id {
            self.declare(id.name.as_str());
        }
        self.enter_scope();
        for param in &it.params.items {
            self.visit_formal_parameter(param);
        }
        if let Some(body) = &it.body {
            self.visit_function_body(body);
        }
        self.exit_scope();
    }

    fn visit_class(&mut self, it: &Class<'a>) {
        if let Some(id) = &it.id {
            self.declare(id.name.as_str());
        }
        if let Some(super_class) = &it.super_class {
            self.visit_expression(super_class);
        }
        self.visit_class_body(&it.body);
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        self.reference(it.name.as_str());
    }

    fn visit_block_statement(&mut self, it: &BlockStatement<'a>) {
        self.enter_scope();
        for stmt in &it.body {
            self.visit_statement(stmt);
        }
        self.exit_scope();
    }
}

struct DeadNodeRemover<'a> {
    allocator: &'a oxc_allocator::Allocator,
    declared: HashSet<String>,
    referenced: HashSet<String>,
    modified: bool,
}

impl<'a> DeadNodeRemover<'a> {
    fn is_dead(&self, name: &str) -> bool {
        self.declared.contains(name) && !self.referenced.contains(name)
    }
}

impl<'a> VisitMut<'a> for DeadNodeRemover<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.process_statements(&mut it.body);
    }

    fn visit_statements(&mut self, it: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        self.process_statements(it);
    }
}

impl<'a> DeadNodeRemover<'a> {
    fn process_statements(&mut self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        stmts.retain_mut(|stmt| {
            if let Statement::VariableDeclaration(var_decl) = stmt {
                var_decl.declarations.retain(|decl| {
                    if let BindingPattern::BindingIdentifier(id) = &decl.id {
                        if self.is_dead(id.name.as_str()) {
                            self.modified = true;
                            return false;
                        }
                    }
                    true
                });
                return !var_decl.declarations.is_empty();
            }
            self.should_keep_statement(stmt)
        });
    }

    fn should_keep_statement(&mut self, stmt: &Statement<'a>) -> bool {
        match stmt {
            Statement::FunctionDeclaration(func) => {
                if let Some(id) = &func.id {
                    if self.is_dead(id.name.as_str()) {
                        self.modified = true;
                        return false;
                    }
                }
                true
            }
            Statement::ClassDeclaration(class) => {
                if let Some(id) = &class.id {
                    if self.is_dead(id.name.as_str()) {
                        self.modified = true;
                        return false;
                    }
                }
                true
            }
            Statement::ExpressionStatement(expr_stmt) => {
                if let Expression::AssignmentExpression(assign) = &expr_stmt.expression {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left {
                        if self.is_dead(id.name.as_str()) {
                            self.modified = true;
                            return false;
                        }
                    }
                }
                true
            }
            _ => true,
        }
    }
}
