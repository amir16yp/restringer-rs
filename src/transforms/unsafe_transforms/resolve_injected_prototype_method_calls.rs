use std::collections::HashMap;

use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use crate::{Transform, TransformCtx};
use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;

pub struct ResolveInjectedPrototypeMethodCalls {
    evaluator: JsEvaluator,
}

impl ResolveInjectedPrototypeMethodCalls {
    pub fn new() -> Self {
        Self { evaluator: JsEvaluator::new() }
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
        // Collect prototype method assignments.
        let mut prototypes: HashMap<String, (String /* type name */, String /* assignment source */)> = HashMap::new();
        for stmt in &program.body {
            if let Statement::ExpressionStatement(es) = stmt {
                if let Expression::AssignmentExpression(assign) = &es.expression {
                    if let Some((type_name, method_name)) = extract_prototype_assignment(&assign.left) {
                        if is_valid_prototype_value(&assign.right) {
                            let code = super::helpers::statement_to_code(stmt);
                            prototypes.insert(method_name.to_string(), (type_name.to_string(), code));
                        }
                    }
                }
            }
        }

        if prototypes.is_empty() {
            return false;
        }

        let context: String = prototypes.values().map(|(_, code)| code.as_str()).collect::<Vec<_>>().join("\n");

        let mut visitor = InjectedMethodVisitor {
            allocator: ctx.allocator,
            transform: self,
            prototypes,
            context,
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

fn extract_prototype_assignment<'b>(target: &'b AssignmentTarget<'b>) -> Option<(&'b str, &'b str)> {
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

fn extract_prototype_assignment_static<'b>(mem: &'b StaticMemberExpression<'b>) -> Option<(&'b str, &'b str)> {
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

fn extract_prototype_assignment_computed<'b>(mem: &'b ComputedMemberExpression<'b>) -> Option<(&'b str, &'b str)> {
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
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) | Expression::Identifier(_)
    )
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
    prototypes: HashMap<String, (String, String)>,
    context: String,
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

        let Some(object_type) = literal_type_name(object) else { return };
        let Some((proto_type, _assignment_code)) = self.prototypes.get(prop_name) else { return };
        if proto_type != object_type {
            return;
        }

        let call_code = match self.transform.expression_to_code(expr) {
            Ok(c) => c,
            Err(_) => return,
        };

        let full_code = format!("{};\n{}", self.context, call_code);
        match self.transform.evaluator().eval_to_json(&full_code) {
            Ok(json) => {
                if let Some(new_expr) = super::helpers::parse_expression_in(self.allocator, &json, expr.span()) {
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
