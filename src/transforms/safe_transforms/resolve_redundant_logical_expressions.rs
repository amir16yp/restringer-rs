use std::collections::HashMap;

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ResolveRedundantLogicalExpressions;

impl Transform for ResolveRedundantLogicalExpressions {
    fn name(&self) -> &'static str {
        "resolveRedundantLogicalExpressions"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            modified: false,
            known: Vec::new(),
        };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Clone)]
enum KnownValue {
    Boolean(bool),
    Number(f64),
    String(String),
    Null,
    Undefined,
    Truthy,
    Falsy,
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
    known: Vec<HashMap<String, KnownValue>>,
}

fn binding_pattern_name<'a>(pat: &BindingPattern<'a>) -> Option<String> {
    match pat {
        BindingPattern::BindingIdentifier(id) => Some(id.name.to_string()),
        _ => None,
    }
}

fn known_value(expr: &Expression<'_>) -> Option<KnownValue> {
    match expr {
        Expression::BooleanLiteral(lit) => Some(KnownValue::Boolean(lit.value)),
        Expression::NullLiteral(_) => Some(KnownValue::Null),
        Expression::NumericLiteral(lit) => Some(KnownValue::Number(lit.value)),
        Expression::StringLiteral(lit) => Some(KnownValue::String(lit.value.to_string())),
        Expression::Identifier(ident) if ident.name.as_str() == "undefined" => {
            Some(KnownValue::Undefined)
        }
        Expression::ArrayExpression(_)
        | Expression::ObjectExpression(_)
        | Expression::RegExpLiteral(_) => Some(KnownValue::Truthy),
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
            Some(KnownValue::Truthy)
        }
        _ => None,
    }
}

fn is_truthy(value: &KnownValue) -> bool {
    match value {
        KnownValue::Boolean(b) => *b,
        KnownValue::Number(n) => *n != 0.0 && !n.is_nan(),
        KnownValue::String(s) => !s.is_empty(),
        KnownValue::Null | KnownValue::Undefined | KnownValue::Falsy => false,
        KnownValue::Truthy => true,
    }
}

impl<'a> Visitor<'a> {
    fn push_scope(&mut self) {
        self.known.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.known.pop();
    }

    fn lookup(&self, name: &str) -> Option<KnownValue> {
        for scope in self.known.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn bind(&mut self, name: String, value: KnownValue) {
        if let Some(scope) = self.known.last_mut() {
            scope.insert(name, value);
        }
    }

    fn assign(&mut self, name: &str, value: Option<KnownValue>) {
        for scope in self.known.iter_mut().rev() {
            if scope.contains_key(name) {
                if let Some(value) = value {
                    scope.insert(name.to_string(), value);
                } else {
                    scope.remove(name);
                }
                return;
            }
        }
    }

    fn collect_declarations(&mut self, stmts: &[Statement<'_>]) {
        for stmt in stmts {
            if let Statement::VariableDeclaration(decl) = stmt {
                for d in &decl.declarations {
                    if let Some(name) = binding_pattern_name(&d.id) {
                        if let Some(init) = &d.init {
                            if let Some(v) = known_value(init) {
                                self.bind(name, v);
                            }
                        } else {
                            self.bind(name, KnownValue::Undefined);
                        }
                    }
                }
            }
        }
    }

    fn truthiness(&self, expr: &Expression<'_>) -> Option<bool> {
        match expr {
            Expression::BooleanLiteral(lit) => Some(lit.value),
            Expression::NullLiteral(_) => Some(false),
            Expression::NumericLiteral(lit) => Some(lit.value != 0.0 && !lit.value.is_nan()),
            Expression::StringLiteral(lit) => Some(!lit.value.as_str().is_empty()),
            Expression::BigIntLiteral(lit) => {
                let raw = lit.raw.as_ref()?;
                let s = raw.as_str();
                Some(s != "0" && s != "0n")
            }
            Expression::RegExpLiteral(_)
            | Expression::ArrayExpression(_)
            | Expression::ObjectExpression(_) => Some(true),
            Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
                Some(true)
            }
            Expression::Identifier(ident) => {
                if ident.name.as_str() == "undefined" {
                    Some(false)
                } else if let Some(v) = self.lookup(ident.name.as_str()) {
                    Some(is_truthy(&v))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn simplify_logical(&mut self, expr: &mut Expression<'a>) {
        if !matches!(expr, Expression::LogicalExpression(_)) {
            return;
        }
        let Expression::LogicalExpression(logical) = expr else {
            return;
        };

        let left_truthy = self.truthiness(&logical.left);
        let right_truthy = self.truthiness(&logical.right);

        let replacement = match logical.operator {
            LogicalOperator::And => {
                if let Some(t) = left_truthy {
                    Some(if t {
                        logical.right.clone_in(self.allocator)
                    } else {
                        logical.left.clone_in(self.allocator)
                    })
                } else if let Some(t) = right_truthy {
                    Some(if t {
                        logical.left.clone_in(self.allocator)
                    } else {
                        logical.right.clone_in(self.allocator)
                    })
                } else {
                    None
                }
            }
            LogicalOperator::Or => {
                if let Some(t) = left_truthy {
                    Some(if t {
                        logical.left.clone_in(self.allocator)
                    } else {
                        logical.right.clone_in(self.allocator)
                    })
                } else if let Some(t) = right_truthy {
                    Some(if t {
                        logical.right.clone_in(self.allocator)
                    } else {
                        logical.left.clone_in(self.allocator)
                    })
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(replacement) = replacement {
            *expr = replacement;
            self.modified = true;
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);
        self.simplify_logical(it);
    }

    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.push_scope();
        self.collect_declarations(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
        self.pop_scope();
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        self.push_scope();
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.pop_scope();
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.collect_declarations(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_arrow_function_expression(&mut self, it: &mut ArrowFunctionExpression<'a>) {
        self.push_scope();
        oxc_ast_visit::walk_mut::walk_arrow_function_expression(self, it);
        self.pop_scope();
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        // `var` declarations are function-scoped; process blocks in the current scope.
        self.collect_declarations(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_assignment_expression(&mut self, it: &mut AssignmentExpression<'a>) {
        oxc_ast_visit::walk_mut::walk_assignment_expression(self, it);
        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &it.left {
            let value = if it.operator == AssignmentOperator::Assign {
                known_value(&it.right)
            } else {
                None
            };
            self.assign(id.name.as_str(), value);
        }
    }

    fn visit_update_expression(&mut self, it: &mut UpdateExpression<'a>) {
        oxc_ast_visit::walk_mut::walk_update_expression(self, it);
        if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &it.argument {
            self.assign(id.name.as_str(), None);
        }
    }
}
