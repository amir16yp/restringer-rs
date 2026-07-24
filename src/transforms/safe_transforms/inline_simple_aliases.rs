use std::collections::{HashMap, HashSet};

use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::GetSpan;

use crate::{Transform, TransformCtx};

pub struct InlineSimpleAliases;

impl Transform for InlineSimpleAliases {
    fn name(&self) -> &'static str {
        "inlineSimpleAliases"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        // First pass: collect names that are assigned exactly once to another identifier.
        let mut collector = AliasCollector {
            assignments: HashMap::new(),
            excluded: HashSet::new(),
        };
        collector.visit_program(program);

        let aliases: HashMap<String, String> = collector
            .assignments
            .into_iter()
            .filter(|(name, _)| !collector.excluded.contains(name))
            .map(|(name, a)| (name, a.target))
            .collect();

        if aliases.is_empty() {
            return false;
        }

        let mut visitor = AliasInliner {
            allocator: ctx.allocator,
            aliases: &aliases,
            shadowed: vec![HashSet::new()],
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct AliasAssignment {
    target: String,
}

struct AliasCollector {
    /// name -> target of a single identifier assignment seen so far
    assignments: HashMap<String, AliasAssignment>,
    /// names that are assigned more than once, are parameters, catch bindings, etc.
    excluded: HashSet<String>,
}

impl AliasCollector {
    fn record_assignment_target(&mut self, name: &str) {
        self.assignments.remove(name);
        self.excluded.insert(name.to_string());
    }

    fn record_alias(&mut self, name: &str, target: &str) {
        if self.excluded.contains(name) || name == target {
            return;
        }
        if self.assignments.contains_key(name) {
            self.assignments.remove(name);
            self.excluded.insert(name.to_string());
        } else {
            self.assignments.insert(
                name.to_string(),
                AliasAssignment {
                    target: target.to_string(),
                },
            );
        }
    }

    fn collect_statement_list(&mut self, stmts: &[Statement<'_>]) {
        for stmt in stmts {
            self.collect_statement(stmt);
        }
    }

    fn collect_statement(&mut self, stmt: &Statement<'_>) {
        match stmt {
            Statement::VariableDeclaration(decl) => {
                for d in &decl.declarations {
                    let BindingPattern::BindingIdentifier(binding) = &d.id else {
                        self.excluded
                            .insert(binding_pattern_name(&d.id).unwrap_or_default());
                        continue;
                    };
                    let name = binding.name.as_str();
                    if let Some(init) = d.init.as_ref() {
                        if let Expression::Identifier(target) = init {
                            self.record_alias(name, target.name.as_str());
                        } else {
                            self.record_assignment_target(name);
                        }
                    }
                }
            }
            Statement::ExpressionStatement(es) => {
                if let Expression::AssignmentExpression(assign) = &es.expression {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left {
                        if let Expression::Identifier(target) = &assign.right {
                            self.record_alias(id.name.as_str(), target.name.as_str());
                        } else {
                            self.record_assignment_target(id.name.as_str());
                        }
                    }
                }
            }
            Statement::BlockStatement(block) => {
                self.collect_statement_list(&block.body);
            }
            Statement::IfStatement(if_stmt) => {
                self.collect_statement(&if_stmt.consequent);
                if let Some(alt) = if_stmt.alternate.as_ref() {
                    self.collect_statement(alt);
                }
            }
            _ => {}
        }
    }
}

impl<'a> Visit<'a> for AliasCollector {
    fn visit_program(&mut self, it: &Program<'a>) {
        self.collect_statement_list(&it.body);
        oxc_ast_visit::walk::walk_program(self, it);
    }

    fn visit_function(&mut self, it: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        for param in &it.params.items {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                self.excluded.insert(name);
            }
        }
        oxc_ast_visit::walk::walk_function(self, it, flags);
    }

    fn visit_function_body(&mut self, it: &FunctionBody<'a>) {
        self.collect_statement_list(&it.statements);
        oxc_ast_visit::walk::walk_function_body(self, it);
    }

    fn visit_arrow_function_expression(&mut self, it: &ArrowFunctionExpression<'a>) {
        for param in &it.params.items {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                self.excluded.insert(name);
            }
        }
        oxc_ast_visit::walk::walk_arrow_function_expression(self, it);
    }

    fn visit_catch_clause(&mut self, it: &CatchClause<'a>) {
        if let Some(param) = it.param.as_ref() {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                self.excluded.insert(name);
            }
        }
        oxc_ast_visit::walk::walk_catch_clause(self, it);
    }
}

fn binding_pattern_name(pattern: &BindingPattern<'_>) -> Option<String> {
    match pattern {
        BindingPattern::BindingIdentifier(id) => Some(id.name.as_str().to_string()),
        _ => None,
    }
}

struct AliasInliner<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    aliases: &'b HashMap<String, String>,
    shadowed: Vec<HashSet<String>>,
    modified: bool,
}

impl<'a, 'b> AliasInliner<'a, 'b> {
    fn is_shadowed(&self, name: &str) -> bool {
        self.shadowed.iter().any(|s| s.contains(name))
    }

    fn maybe_replace_ident(&mut self, idref: &mut IdentifierReference<'a>) {
        let name = idref.name.as_str();
        if self.is_shadowed(name) {
            return;
        }
        if let Some(target) = self.aliases.get(name) {
            let name_in_arena = self.allocator.alloc_str(target);
            idref.name = name_in_arena.into();
            self.modified = true;
        }
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in original {
            match stmt {
                Statement::ExpressionStatement(es) => {
                    if let Expression::AssignmentExpression(assign) = &es.expression {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left {
                            if self.aliases.contains_key(id.name.as_str()) {
                                self.modified = true;
                                continue;
                            }
                        }
                    }
                    new_body.push(Statement::ExpressionStatement(es));
                }
                Statement::VariableDeclaration(var_decl) => {
                    let mut kept = ArenaVec::new_in(self.allocator);
                    let mut any_removed = false;
                    for d in &var_decl.declarations {
                        let remove = match &d.id {
                            BindingPattern::BindingIdentifier(binding) => {
                                self.aliases.contains_key(binding.name.as_str())
                            }
                            _ => false,
                        };
                        if remove {
                            any_removed = true;
                            self.modified = true;
                        } else {
                            kept.push(d.clone_in(self.allocator));
                        }
                    }
                    if kept.is_empty() {
                        continue;
                    }
                    if any_removed {
                        let mut new_decl = (*var_decl).clone_in(self.allocator);
                        new_decl.declarations = kept;
                        new_body.push(Statement::VariableDeclaration(oxc_allocator::Box::new_in(
                            new_decl,
                            self.allocator,
                        )));
                    } else {
                        new_body.push(Statement::VariableDeclaration(var_decl));
                    }
                }
                other => {
                    new_body.push(other);
                }
            }
        }
        *stmts = new_body;
    }
}

impl<'a, 'b> VisitMut<'a> for AliasInliner<'a, 'b> {
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

    fn visit_function(&mut self, it: &mut Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        let mut shadowed = HashSet::new();
        for param in &it.params.items {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                shadowed.insert(name);
            }
        }
        self.shadowed.push(shadowed);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.shadowed.pop();
    }

    fn visit_arrow_function_expression(&mut self, it: &mut ArrowFunctionExpression<'a>) {
        let mut shadowed = HashSet::new();
        for param in &it.params.items {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                shadowed.insert(name);
            }
        }
        self.shadowed.push(shadowed);
        oxc_ast_visit::walk_mut::walk_arrow_function_expression(self, it);
        self.shadowed.pop();
    }

    fn visit_catch_clause(&mut self, it: &mut CatchClause<'a>) {
        let mut shadowed = HashSet::new();
        if let Some(param) = it.param.as_ref() {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                shadowed.insert(name);
            }
        }
        self.shadowed.push(shadowed);
        oxc_ast_visit::walk_mut::walk_catch_clause(self, it);
        self.shadowed.pop();
    }

    fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
        self.maybe_replace_ident(it);
    }
}
