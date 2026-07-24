use std::collections::{HashMap, HashSet};

use oxc_ast::ast::*;
use oxc_ast_visit::Visit;

use crate::{Transform, TransformCtx};

const MIN_ARRAY_LENGTH: usize = 20;

pub struct RemoveResolvedVarStringArrays;

impl Transform for RemoveResolvedVarStringArrays {
    fn name(&self) -> &'static str {
        "removeResolvedVarStringArrays"
    }

    fn run<'a>(&self, _ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut candidates = HashSet::new();

        for stmt in &program.body {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue };
            if var_decl.kind != VariableDeclarationKind::Var {
                continue;
            }
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue };
                let Some(init) = decl.init.as_ref() else { continue };
                let Expression::ArrayExpression(arr) = init else { continue };
                if arr.elements.len() <= MIN_ARRAY_LENGTH {
                    continue;
                }
                candidates.insert(binding.name.as_str().to_string());
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let counts = {
            let mut ref_collector = RefCollector {
                candidates: &candidates,
                counts: HashMap::new(),
            };
            ref_collector.visit_program(program);
            ref_collector.counts
        };

        let to_remove: HashSet<String> = candidates
            .into_iter()
            .filter(|name| counts.get(name).copied().unwrap_or(0) == 0)
            .collect();

        if to_remove.is_empty() {
            return false;
        }

        let mut modified = false;
        program.body.retain(|stmt| {
            let Statement::VariableDeclaration(var_decl) = stmt else { return true };
            if var_decl.kind != VariableDeclarationKind::Var {
                return true;
            }
            let all_removable = var_decl.declarations.iter().all(|decl| {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { return false };
                to_remove.contains(binding.name.as_str())
            });
            if all_removable {
                modified = true;
                return false;
            }
            true
        });

        modified
    }
}

struct RefCollector<'a> {
    candidates: &'a HashSet<String>,
    counts: HashMap<String, usize>,
}

impl<'a, 'b> Visit<'a> for RefCollector<'b> {
    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        let name = it.name.as_str();
        if self.candidates.contains(name) {
            *self.counts.entry(name.to_string()).or_insert(0) += 1;
        }
    }
}
