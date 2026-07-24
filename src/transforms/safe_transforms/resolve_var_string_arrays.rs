use std::collections::{HashMap, HashSet};

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

const MIN_ARRAY_LENGTH: usize = 20;

pub struct ResolveVarStringArrays;

impl Transform for ResolveVarStringArrays {
    fn name(&self) -> &'static str {
        "resolveVarStringArrays"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut mutated = MutatedNameCollector::default();
        mutated.visit_program(program);

        let mut v = Visitor {
            allocator: ctx.allocator,
            arrays: HashMap::new(),
            mutated: mutated.names,
            modified: false,
        };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Default)]
struct MutatedNameCollector {
    names: HashSet<String>,
}

impl<'a> Visit<'a> for MutatedNameCollector {
    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        match &it.left {
            AssignmentTarget::AssignmentTargetIdentifier(id) => {
                self.names.insert(id.name.as_str().to_string());
            }
            AssignmentTarget::ComputedMemberExpression(mem) => {
                if let Expression::Identifier(id) = &mem.object {
                    self.names.insert(id.name.as_str().to_string());
                }
            }
            AssignmentTarget::StaticMemberExpression(mem) => {
                if let Expression::Identifier(id) = &mem.object {
                    self.names.insert(id.name.as_str().to_string());
                }
            }
            AssignmentTarget::PrivateFieldExpression(_) => {}
            _ => {}
        }
        oxc_ast_visit::walk::walk_assignment_expression(self, it);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &it.argument {
            self.names.insert(id.name.as_str().to_string());
        }
        oxc_ast_visit::walk::walk_update_expression(self, it);
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    arrays: HashMap<String, oxc_allocator::Vec<'a, ArrayExpressionElement<'a>>>,
    mutated: HashSet<String>,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn collect_arrays_from_statement_list(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else {
                continue;
            };

            if var_decl.kind != VariableDeclarationKind::Var {
                continue;
            }

            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else {
                    continue;
                };
                let Some(init) = decl.init.as_ref() else {
                    continue;
                };
                let Expression::ArrayExpression(arr) = init else {
                    continue;
                };

                if arr.elements.len() <= MIN_ARRAY_LENGTH {
                    continue;
                }

                let name = binding.name.as_str();
                if self.mutated.contains(name) {
                    continue;
                }

                self.arrays
                    .insert(name.to_string(), arr.elements.clone_in(self.allocator));
            }
        }
    }

    fn numeric_literal_index(expr: &Expression<'a>) -> Option<usize> {
        match expr {
            Expression::NumericLiteral(lit) => {
                let v = lit.value;
                if !v.is_finite() {
                    return None;
                }
                if v.fract() != 0.0 {
                    return None;
                }
                if v < 0.0 {
                    return None;
                }
                Some(v as usize)
            }
            _ => None,
        }
    }

    fn replace_computed_member_if_possible(
        &mut self,
        mem: &ComputedMemberExpression<'a>,
    ) -> Option<Expression<'a>> {
        let Expression::Identifier(obj) = &mem.object else {
            return None;
        };
        let name = obj.name.as_str();
        let elements = self.arrays.get(name)?;

        let idx = Self::numeric_literal_index(&mem.expression)?;
        if idx >= elements.len() {
            return None;
        }

        let el = &elements[idx];
        let expr = el.as_expression()?;
        Some(expr.clone_in(self.allocator))
    }

    fn drop_array_binding_in_declarator(
        &mut self,
        decl: &VariableDeclarator<'a>,
    ) -> Option<oxc_allocator::Vec<'a, ArrayExpressionElement<'a>>> {
        let BindingPattern::BindingIdentifier(binding) = &decl.id else {
            return None;
        };
        self.arrays.remove(binding.name.as_str())
    }

    fn restore_array_binding_in_declarator(
        &mut self,
        decl: &VariableDeclarator<'a>,
        elements: oxc_allocator::Vec<'a, ArrayExpressionElement<'a>>,
    ) {
        let BindingPattern::BindingIdentifier(binding) = &decl.id else {
            return;
        };
        self.arrays
            .insert(binding.name.as_str().to_string(), elements);
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_arrays_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let prev = std::mem::take(&mut self.arrays);
        self.collect_arrays_from_statement_list(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.arrays = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev = std::mem::take(&mut self.arrays);
        self.collect_arrays_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.arrays = prev;
    }

    fn visit_variable_declarator(&mut self, it: &mut VariableDeclarator<'a>) {
        let removed = self.drop_array_binding_in_declarator(it);
        oxc_ast_visit::walk_mut::walk_variable_declarator(self, it);
        if let Some(el) = removed {
            self.restore_array_binding_in_declarator(it, el);
        }
    }

    fn visit_assignment_expression(&mut self, it: &mut AssignmentExpression<'a>) {
        let prev = self.modified;

        match &mut it.left {
            AssignmentTarget::ComputedMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.expression);
            }
            AssignmentTarget::StaticMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
            }
            _ => {
                oxc_ast_visit::walk_mut::walk_assignment_target(self, &mut it.left);
            }
        }

        self.modified = prev;
        oxc_ast_visit::walk_mut::walk_expression(self, &mut it.right);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::ComputedMemberExpression(mem) = it {
            if let Some(repl) = self.replace_computed_member_if_possible(mem) {
                *it = repl;
                self.modified = true;
                return;
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let prev = std::mem::take(&mut self.arrays);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.arrays = prev;
    }
}
