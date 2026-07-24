use std::collections::HashSet;

use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::{GetSpan, Span};

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveMemberExpressionsLocalReferences {
    evaluator: JsEvaluator,
}

impl ResolveMemberExpressionsLocalReferences {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveMemberExpressionsLocalReferences {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveMemberExpressionsLocalReferences {
    fn name(&self) -> &'static str {
        "resolveMemberExpressionsLocalReferences"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        // Gather top-level declaration context.
        let mut context_parts = Vec::new();
        let mut declared_names: HashSet<String> = HashSet::new();
        for stmt in &program.body {
            let code = super::helpers::statement_to_code(stmt);
            let is_var_decl = matches!(stmt, Statement::VariableDeclaration(_));
            // Variable declarations are usually the obfuscated string-array initializer;
            // skip-word checks on the generated source often produce false positives from
            // encoded string literals, so we only apply them to function declarations.
            if !is_var_decl && (super::helpers::contains_skip_word(&code) || code.len() > 5000) {
                continue;
            }
            match stmt {
                Statement::FunctionDeclaration(func) => {
                    if let Some(id) = &func.id {
                        declared_names.insert(id.name.to_string());
                        context_parts.push(code);
                    }
                }
                Statement::VariableDeclaration(decl) => {
                    let mut has_skip = false;
                    for d in &decl.declarations {
                        if let BindingPattern::BindingIdentifier(id) = &d.id {
                            if super::helpers::SKIP_IDENTIFIERS.contains(&id.name.as_str()) {
                                has_skip = true;
                                break;
                            }
                        }
                    }
                    if has_skip {
                        continue;
                    }
                    for d in &decl.declarations {
                        if let BindingPattern::BindingIdentifier(id) = &d.id {
                            declared_names.insert(id.name.to_string());
                        }
                    }
                    context_parts.push(code);
                }
                _ => {}
            }
        }

        if context_parts.is_empty() {
            return false;
        }
        let context_code = context_parts.join(";\n");

        let mut collector = SkipSpanCollector {
            skip_spans: HashSet::new(),
        };
        collector.visit_program(program);

        let mut visitor = LocalReferenceVisitor {
            allocator: ctx.allocator,
            transform: self,
            declared_names,
            context_code,
            skip_spans: collector.skip_spans,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveMemberExpressionsLocalReferences {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct SkipSpanCollector {
    skip_spans: HashSet<Span>,
}

impl<'a> Visit<'a> for SkipSpanCollector {
    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if matches!(
            &it.callee,
            Expression::StaticMemberExpression(_)
                | Expression::ComputedMemberExpression(_)
                | Expression::PrivateFieldExpression(_)
        ) {
            self.skip_spans.insert(it.callee.span());
        }
        oxc_ast_visit::walk::walk_call_expression(self, it);
    }

    fn visit_update_expression(&mut self, it: &UpdateExpression<'a>) {
        if matches!(
            &it.argument,
            SimpleAssignmentTarget::StaticMemberExpression(_)
                | SimpleAssignmentTarget::ComputedMemberExpression(_)
        ) {
            self.skip_spans.insert(it.argument.span());
        }
        oxc_ast_visit::walk::walk_update_expression(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        if matches!(
            &it.left,
            AssignmentTarget::StaticMemberExpression(_)
                | AssignmentTarget::ComputedMemberExpression(_)
        ) {
            self.skip_spans.insert(it.left.span());
        }
        oxc_ast_visit::walk::walk_assignment_expression(self, it);
    }
}

struct LocalReferenceVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveMemberExpressionsLocalReferences,
    declared_names: HashSet<String>,
    context_code: String,
    skip_spans: HashSet<Span>,
    modified: bool,
}

impl<'a, 'b> LocalReferenceVisitor<'a, 'b> {
    fn try_replace(&mut self, expr: &mut Expression<'a>) {
        match expr {
            Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_) => {
                if self.skip_spans.contains(&expr.span()) {
                    return;
                }
            }
            _ => return,
        }

        let (object, prop_name) = match expr {
            Expression::StaticMemberExpression(s) => (&s.object, Some(s.property.name.as_str())),
            Expression::ComputedMemberExpression(c) => {
                (&c.object, computed_property_name(&c.expression))
            }
            _ => return,
        };

        let obj_name = match object {
            Expression::Identifier(id) => id.name.as_str(),
            _ => return,
        };

        if !self.declared_names.contains(obj_name) {
            return;
        }
        if let Some(prop) = prop_name {
            if super::helpers::SKIP_PROPERTIES.contains(&prop) {
                return;
            }
        }

        let member_code = match self.transform.expression_to_code(expr) {
            Ok(c) => c,
            Err(_) => return,
        };
        let full_code = format!("{};\n{}", self.context_code, member_code);

        match self.transform.evaluator().eval_to_json(&full_code) {
            Ok(json) => {
                if let Some(new_expr) =
                    super::helpers::parse_expression_in(self.allocator, &json, expr.span())
                {
                    if !is_empty_replacement(&new_expr) {
                        *expr = new_expr;
                        self.modified = true;
                    }
                }
            }
            Err(_) => {}
        }
    }
}

impl<'a, 'b> VisitMut<'a> for LocalReferenceVisitor<'a, 'b> {
    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        self.try_replace(expr);
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }
}

fn computed_property_name<'a>(expr: &'a Expression<'a>) -> Option<&'a str> {
    match expr {
        Expression::StringLiteral(s) => Some(s.value.as_str()),
        Expression::NumericLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_) => Some(""),
        _ => None,
    }
}

fn is_empty_replacement(expr: &Expression) -> bool {
    match expr {
        Expression::ArrayExpression(arr) => arr.elements.is_empty(),
        Expression::ObjectExpression(obj) => obj.properties.is_empty(),
        Expression::NullLiteral(_) => true,
        _ => false,
    }
}
