use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use std::collections::HashMap;

use super::helpers::{self, is_static_literal};
use crate::{Transform, TransformCtx};

pub struct ResolvePartialLocalCalls;

impl Transform for ResolvePartialLocalCalls {
    fn name(&self) -> &'static str {
        "resolvePartialLocalCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = FunctionCollector {
            candidates: HashMap::new(),
        };
        collector.visit_program(program);

        let mut visitor = InlineVisitor {
            allocator: ctx.allocator,
            candidates: collector.candidates,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct Candidate {
    params: Vec<String>,
    body_expr_code: String,
}

struct FunctionCollector {
    candidates: HashMap<String, Candidate>,
}

impl<'a> Visit<'a> for FunctionCollector {
    fn visit_statement(&mut self, statement: &Statement<'a>) {
        if let Statement::FunctionDeclaration(func_decl) = statement {
            if let Some(candidate) = extract_candidate(&**func_decl) {
                if let Some(id) = &func_decl.id {
                    self.candidates.insert(id.name.to_string(), candidate);
                }
            }
        }
        if let Statement::VariableDeclaration(var_decl) = statement {
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(id) = &decl.id else {
                    continue;
                };
                let Some(Expression::FunctionExpression(func_expr)) = decl.init.as_ref() else {
                    continue;
                };
                let Some(func) = extract_candidate(func_expr) else {
                    continue;
                };
                self.candidates.insert(id.name.to_string(), func);
            }
        }
        oxc_ast_visit::walk::walk_statement(self, statement);
    }
}

fn extract_candidate(func: &Function<'_>) -> Option<Candidate> {
    let body = func.body.as_ref()?;
    if body.statements.len() != 1 {
        return None;
    }
    let Statement::ReturnStatement(ret) = &body.statements[0] else {
        return None;
    };
    let arg = ret.argument.as_ref()?;

    let mut free = FreeIdentifierCollector {
        names: Vec::new(),
    };
    free.visit_expression(arg);

    let params: Vec<String> = func
        .params
        .items
        .iter()
        .filter_map(|p| match &p.pattern {
            BindingPattern::BindingIdentifier(id) => Some(id.name.to_string()),
            _ => None,
        })
        .collect();

    let param_set: std::collections::HashSet<&str> = params.iter().map(|s| s.as_str()).collect();
    for name in &free.names {
        if param_set.contains(name.as_str()) {
            continue;
        }
        if helpers::SKIP_IDENTIFIERS.contains(&name.as_str()) {
            return None;
        }
        // Allow common safe globals. Anything else is treated as a closure capture
        // and we skip inlining to preserve semantics.
        if !is_known_global(name.as_str()) {
            return None;
        }
    }

    Some(Candidate {
        params,
        body_expr_code: helpers::expression_to_code(arg),
    })
}

fn is_known_global(name: &str) -> bool {
    const GLOBALS: &[&str] = &[
        "Array", "Boolean", "Date", "Error", "Function", "JSON", "Math", "Number", "Object",
        "RegExp", "String", "Symbol", "parseInt", "parseFloat", "isNaN", "isFinite", "atob",
        "btoa", "escape", "unescape", "encodeURI", "encodeURIComponent", "decodeURI",
        "decodeURIComponent", "undefined", "NaN", "Infinity",
    ];
    GLOBALS.contains(&name)
}

struct InlineVisitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    candidates: HashMap<String, Candidate>,
    modified: bool,
}

impl<'a> VisitMut<'a> for InlineVisitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        let Expression::CallExpression(call) = it else {
            return;
        };
        let Expression::Identifier(callee_id) = &call.callee else {
            return;
        };
        let Some(candidate) = self.candidates.get(callee_id.name.as_str()) else {
            return;
        };
        if call.arguments.len() > candidate.params.len() {
            return;
        }

        // Only inline arguments that are side-effect-free (literals or identifiers).
        for arg in &call.arguments {
            let Some(expr) = arg.as_expression() else {
                return;
            };
            if !is_static_literal(expr) && !matches!(expr, Expression::Identifier(_)) {
                return;
            }
        }

        let parsed = helpers::parse_expression_in(self.allocator, &candidate.body_expr_code, call.span);
        let Some(mut body_expr) = parsed else {
            return;
        };

        let mut args: HashMap<String, Expression<'a>> = HashMap::new();
        for (i, param_name) in candidate.params.iter().enumerate() {
            let Some(arg) = call.arguments.get(i) else {
                break;
            };
            let Some(expr) = arg.as_expression() else {
                return;
            };
            args.insert(param_name.clone(), expr.clone_in(self.allocator));
        }

        let mut substitutor = Substitutor {
            allocator: self.allocator,
            args,
            modified: false,
        };
        substitutor.visit_expression(&mut body_expr);

        *it = body_expr;
        self.modified = true;
    }
}

struct Substitutor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    args: HashMap<String, Expression<'a>>,
    modified: bool,
}

impl<'a> VisitMut<'a> for Substitutor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::Identifier(id) = it {
            if let Some(arg) = self.args.get(id.name.as_str()) {
                *it = arg.clone_in(self.allocator);
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}

struct FreeIdentifierCollector {
    names: Vec<String>,
}

impl<'a> Visit<'a> for FreeIdentifierCollector {
    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        self.names.push(it.name.to_string());
    }
}
