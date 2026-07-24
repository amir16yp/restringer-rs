use std::collections::HashMap;
use std::collections::HashSet;

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::{GetSpan, SourceType};

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveFunctionToArray {
    evaluator: JsEvaluator,
}

impl ResolveFunctionToArray {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveFunctionToArray {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveFunctionToArray {
    fn name(&self) -> &'static str {
        "resolveFunctionToArray"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let functions = collect_functions(program);
        let safe_names = collect_safe_names(program, &functions);

        let mut visitor = FunctionToArrayVisitor {
            allocator: ctx.allocator,
            transform: self,
            functions,
            safe_names,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveFunctionToArray {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct FunctionToArrayVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveFunctionToArray,
    functions: HashMap<String, (String, bool)>,
    safe_names: HashSet<String>,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for FunctionToArrayVisitor<'a, 'b> {
    fn visit_variable_declarator(&mut self, decl: &mut VariableDeclarator<'a>) {
        oxc_ast_visit::walk_mut::walk_variable_declarator(self, decl);

        let call = match decl.init.as_ref() {
            Some(Expression::CallExpression(c)) => c,
            _ => return,
        };
        let callee_name = match &call.callee {
            Expression::Identifier(id) => id.name.as_str(),
            _ => return,
        };
        let (func_source, mutates) = match self.functions.get(callee_name) {
            Some(info) => info,
            None => return,
        };
        if *mutates {
            return;
        }
        let var_name = match &decl.id {
            BindingPattern::BindingIdentifier(id) => id.name.as_str(),
            _ => return,
        };
        if !self.safe_names.contains(var_name) {
            return;
        }

        let call_expr = Expression::CallExpression(call.clone_in(self.allocator));
        let call_code = match self.transform.expression_to_code(&call_expr) {
            Ok(c) => c,
            Err(_) => return,
        };

        let context = format!("{};\n{};", func_source, call_code);
        if super::helpers::has_unresolved_references(&context, SourceType::mjs()) {
            return;
        }

        match self.transform.evaluator().eval_to_json(&context) {
            Ok(json) => {
                if let Some(replacement) =
                    super::helpers::parse_expression_in(self.allocator, &json, call.span())
                {
                    decl.init = Some(replacement);
                    self.modified = true;
                }
            }
            Err(_) => {}
        }
    }
}

fn collect_functions(program: &Program) -> HashMap<String, (String, bool)> {
    let mut map = HashMap::new();
    for stmt in &program.body {
        match stmt {
            Statement::FunctionDeclaration(func) => {
                if let Some(id) = &func.id {
                    let name = id.name.to_string();
                    let mutates = function_decl_self_mutation(func, &name);
                    let source = super::helpers::statement_to_code(stmt);
                    map.insert(name, (source, mutates));
                }
            }
            Statement::VariableDeclaration(decl) => {
                for declarator in &decl.declarations {
                    let BindingPattern::BindingIdentifier(id) = &declarator.id else {
                        continue;
                    };
                    let name = id.name.to_string();
                    let Some(init) = &declarator.init else {
                        continue;
                    };
                    if matches!(
                        init,
                        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
                    ) {
                        let mutates = function_expr_self_mutation(init, &name);
                        let source = super::helpers::statement_to_code(stmt);
                        map.insert(name, (source, mutates));
                    }
                }
            }
            _ => {}
        }
    }
    map
}

fn function_decl_self_mutation(func: &Function, name: &str) -> bool {
    let Some(body) = &func.body else {
        return false;
    };
    let mut checker = SelfMutationChecker { name, mutates: false };
    checker.visit_function_body(body);
    checker.mutates
}

fn function_expr_self_mutation(expr: &Expression, name: &str) -> bool {
    let body = match expr {
        Expression::FunctionExpression(func) => match &func.body {
            Some(b) => &b.statements,
            None => return false,
        },
        Expression::ArrowFunctionExpression(arr) => &arr.body.statements,
        _ => return true,
    };
    let mut checker = SelfMutationChecker { name, mutates: false };
    checker.visit_statements(body);
    checker.mutates
}

struct SelfMutationChecker<'a> {
    name: &'a str,
    mutates: bool,
}

impl<'a> Visit<'a> for SelfMutationChecker<'a> {
    fn visit_assignment_expression(&mut self, expr: &AssignmentExpression<'a>) {
        if self.mutates {
            return;
        }
        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &expr.left
        {
            if id.name == self.name {
                self.mutates = true;
                return;
            }
        }
        oxc_ast_visit::walk::walk_assignment_expression(self, expr);
    }

    fn visit_update_expression(&mut self, expr: &UpdateExpression<'a>) {
        if self.mutates {
            return;
        }
        if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &expr.argument {
            if id.name == self.name {
                self.mutates = true;
                return;
            }
        }
        oxc_ast_visit::walk::walk_update_expression(self, expr);
    }
}

fn collect_safe_names(program: &Program, functions: &HashMap<String, (String, bool)>) -> HashSet<String> {
    let mut candidate_names = HashSet::new();
    for stmt in &program.body {
        gather_candidate_names(stmt, functions, &mut candidate_names);
    }

    let mut safe_names = HashSet::new();
    for name in &candidate_names {
        if all_usages_are_member_reads(program, name) {
            safe_names.insert(name.clone());
        }
    }
    safe_names
}

fn gather_candidate_names(
    stmt: &Statement,
    functions: &HashMap<String, (String, bool)>,
    out: &mut HashSet<String>,
) {
    let Statement::VariableDeclaration(decl) = stmt else {
        return;
    };
    for declarator in &decl.declarations {
        let BindingPattern::BindingIdentifier(id) = &declarator.id else {
            continue;
        };
        let Some(init) = &declarator.init else {
            continue;
        };
        let Expression::CallExpression(call) = init else {
            continue;
        };
        let Expression::Identifier(callee) = &call.callee else {
            continue;
        };
        if functions.contains_key(callee.name.as_str()) {
            out.insert(id.name.to_string());
        }
    }
}

fn all_usages_are_member_reads(program: &Program, name: &str) -> bool {
    let mut checker = UsageChecker {
        name,
        total: 0,
        safe: 0,
    };
    checker.visit_program(program);
    checker.total > 0 && checker.total == checker.safe
}

struct UsageChecker<'a> {
    name: &'a str,
    total: usize,
    safe: usize,
}

impl<'a> Visit<'a> for UsageChecker<'a> {
    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        if it.name == self.name {
            self.total += 1;
        }
    }

    fn visit_member_expression(&mut self, it: &MemberExpression<'a>) {
        if let Expression::Identifier(id) = it.object() {
            if id.name == self.name {
                self.safe += 1;
            }
        }
        oxc_ast_visit::walk::walk_member_expression(self, it);
    }
}
