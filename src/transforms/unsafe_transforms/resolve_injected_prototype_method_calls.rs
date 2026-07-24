use std::collections::HashMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::GetSpan;

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveInjectedPrototypeMethodCalls {
    evaluator: JsEvaluator,
}

impl ResolveInjectedPrototypeMethodCalls {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveInjectedPrototypeMethodCalls {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveInjectedPrototypeMethodCalls {
    fn name(&self) -> &'static str {
        "resolveInjectedPrototypeMethodCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = PrototypeCollector {
            program_context: String::new(),
            context_stack: Vec::new(),
            prototypes: Vec::new(),
        };
        collector.visit_program(program);

        if collector.prototypes.is_empty() {
            return false;
        }

        let mut contexts: HashMap<String, (String, String)> = HashMap::new();
        for p in collector.prototypes {
            contexts.insert(p.method_name, (p.type_name, p.context_code));
        }

        let mut visitor = InjectedMethodVisitor {
            allocator: ctx.allocator,
            transform: self,
            contexts,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveInjectedPrototypeMethodCalls {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

fn extract_prototype_assignment<'b>(
    target: &'b AssignmentTarget<'b>,
) -> Option<(&'b str, &'b str)> {
    match target {
        AssignmentTarget::StaticMemberExpression(s) => extract_prototype_assignment_static(s),
        AssignmentTarget::ComputedMemberExpression(c) => {
            if !matches!(c.expression, Expression::StringLiteral(_)) {
                return None;
            }
            extract_prototype_assignment_computed(c)
        }
        _ => None,
    }
}

fn extract_prototype_assignment_static<'b>(
    mem: &'b StaticMemberExpression<'b>,
) -> Option<(&'b str, &'b str)> {
    let outer = match &mem.object {
        Expression::StaticMemberExpression(s) => s,
        _ => return None,
    };
    if outer.property.name.as_str() != "prototype" {
        return None;
    }
    let type_name = match &outer.object {
        Expression::Identifier(id) => id.name.as_str(),
        _ => return None,
    };
    Some((type_name, mem.property.name.as_str()))
}

fn extract_prototype_assignment_computed<'b>(
    mem: &'b ComputedMemberExpression<'b>,
) -> Option<(&'b str, &'b str)> {
    let outer = match &mem.object {
        Expression::StaticMemberExpression(s) => s,
        _ => return None,
    };
    if outer.property.name.as_str() != "prototype" {
        return None;
    }
    let type_name = match &outer.object {
        Expression::Identifier(id) => id.name.as_str(),
        _ => return None,
    };
    let method_name = match &mem.expression {
        Expression::StringLiteral(s) => s.value.as_str(),
        _ => return None,
    };
    Some((type_name, method_name))
}

fn is_valid_prototype_value(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::FunctionExpression(_)
            | Expression::ArrowFunctionExpression(_)
            | Expression::Identifier(_)
    )
}

fn should_include_in_context(stmt: &Statement) -> bool {
    match stmt {
        Statement::VariableDeclaration(_) | Statement::FunctionDeclaration(_) => true,
        Statement::ExpressionStatement(es) => {
            matches!(es.expression, Expression::AssignmentExpression(_))
        }
        Statement::IfStatement(_) | Statement::BlockStatement(_) => true,
        _ => false,
    }
}

fn append_context<'a>(out: &mut String, stmts: &[Statement<'a>]) {
    for stmt in stmts {
        if should_include_in_context(stmt) {
            out.push_str(&super::helpers::statement_to_code(stmt));
            out.push('\n');
        }
    }
}

fn build_context<'a>(stmts: &[Statement<'a>]) -> String {
    let mut out = String::new();
    append_context(&mut out, stmts);
    out
}

struct CollectedPrototype {
    type_name: String,
    method_name: String,
    context_code: String,
}

struct PrototypeCollector {
    program_context: String,
    context_stack: Vec<String>,
    prototypes: Vec<CollectedPrototype>,
}

impl PrototypeCollector {
    fn enclosing_context(&self) -> String {
        if self.context_stack.is_empty() {
            return self.program_context.clone();
        }
        self.context_stack.join("\n")
    }
}

impl<'a> Visit<'a> for PrototypeCollector {
    fn visit_program(&mut self, it: &Program<'a>) {
        self.program_context = build_context(&it.body);
        oxc_ast_visit::walk::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &FunctionBody<'a>) {
        self.context_stack.push(build_context(&it.statements));
        oxc_ast_visit::walk::walk_function_body(self, it);
        self.context_stack.pop();
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if let Some((type_name, method_name)) = extract_prototype_assignment(&it.left) {
            if is_valid_prototype_value(&it.right) {
                let context_code = self.enclosing_context();
                let allocator = Allocator::default();
                let assignment = Expression::AssignmentExpression(oxc_allocator::Box::new_in(
                    it.clone_in(&allocator),
                    &allocator,
                ));
                let assignment_code =
                    format!("{};", super::helpers::expression_to_code(&assignment));
                self.prototypes.push(CollectedPrototype {
                    type_name: type_name.to_string(),
                    method_name: method_name.to_string(),
                    context_code: format!("{}\n{}", context_code, assignment_code),
                });
            }
        }
        oxc_ast_visit::walk::walk_assignment_expression(self, it);
    }
}

fn literal_type_name(expr: &Expression) -> Option<&'static str> {
    match expr {
        Expression::StringLiteral(_) => Some("String"),
        Expression::NumericLiteral(_) => Some("Number"),
        Expression::BooleanLiteral(_) => Some("Boolean"),
        Expression::ArrayExpression(_) => Some("Array"),
        Expression::ObjectExpression(_) => Some("Object"),
        Expression::RegExpLiteral(_) => Some("RegExp"),
        _ => None,
    }
}

struct InjectedMethodVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveInjectedPrototypeMethodCalls,
    contexts: HashMap<String, (String, String)>,
    modified: bool,
}

impl<'a, 'b> InjectedMethodVisitor<'a, 'b> {
    fn try_resolve(&mut self, expr: &mut Expression<'a>) {
        let (object, prop_name) = match expr {
            Expression::CallExpression(call) => match &call.callee {
                Expression::StaticMemberExpression(s) => (&s.object, s.property.name.as_str()),
                Expression::ComputedMemberExpression(c) => match &c.expression {
                    Expression::StringLiteral(s) => (&c.object, s.value.as_str()),
                    _ => return,
                },
                _ => return,
            },
            _ => return,
        };

        let Some((proto_type, context_code)) = self.contexts.get(prop_name) else {
            return;
        };

        // For literal receivers, ensure the prototype type matches.
        if let Some(object_type) = literal_type_name(object) {
            if object_type != proto_type {
                return;
            }
        }

        let call_code = match self.transform.expression_to_code(expr) {
            Ok(c) => c,
            Err(_) => return,
        };

        let full_code = format!(
            "(function () {{\ntry {{\n{};\n}} catch (__e) {{}}\nreturn ({});\n}})()",
            context_code, call_code
        );
        match self.transform.evaluator().eval_to_json(&full_code) {
            Ok(json) => {
                if let Some(new_expr) =
                    super::helpers::parse_expression_in(self.allocator, &json, expr.span())
                {
                    *expr = new_expr;
                    self.modified = true;
                }
            }
            Err(_) => {}
        }
    }
}

impl<'a, 'b> VisitMut<'a> for InjectedMethodVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        // Resolve before walking so the callee is still a call expression.
        self.try_resolve(expr);
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }
}
