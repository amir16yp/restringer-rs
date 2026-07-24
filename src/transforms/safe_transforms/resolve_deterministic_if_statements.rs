use std::collections::HashMap;

use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct ResolveDeterministicIfStatements;

impl Transform for ResolveDeterministicIfStatements {
    fn name(&self) -> &'static str {
        "resolveDeterministicIfStatements"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            modified: false,
            known_values: vec![HashMap::new()],
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
    TruthyFunction,
    TruthyArray,
    TruthyObject,
    TruthyRegExp,
    Unknown,
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
    known_values: Vec<HashMap<String, KnownValue>>,
}

fn is_nan(n: f64) -> bool {
    n != n
}

fn is_boolean_literal(expr: &Expression<'_>, value: bool) -> bool {
    matches!(
        expr,
        Expression::BooleanLiteral(lit) if lit.value == value
    )
}

fn equal_value(left: &KnownValue, right: &KnownValue) -> Option<bool> {
    use std::f64::NAN;
    match (left, right) {
        (KnownValue::Boolean(a), KnownValue::Boolean(b)) => Some(a == b),
        (KnownValue::Number(a), KnownValue::Number(b)) => {
            if is_nan(*a) || is_nan(*b) {
                Some(false)
            } else {
                Some(a == b)
            }
        }
        (KnownValue::String(a), KnownValue::String(b)) => Some(a == b),
        (KnownValue::Null, KnownValue::Null)
        | (KnownValue::Undefined, KnownValue::Undefined) => Some(true),
        _ => Some(false),
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
        Expression::ArrayExpression(_) => Some(KnownValue::TruthyArray),
        Expression::ObjectExpression(_) => Some(KnownValue::TruthyObject),
        Expression::RegExpLiteral(_) => Some(KnownValue::TruthyRegExp),
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
            Some(KnownValue::TruthyFunction)
        }
        _ => None,
    }
}

fn truthiness(value: &KnownValue) -> Option<bool> {
    match value {
        KnownValue::Boolean(v) => Some(*v),
        KnownValue::Number(v) => Some(!is_nan(*v) && *v != 0.0),
        KnownValue::String(s) => Some(!s.is_empty()),
        KnownValue::Null | KnownValue::Undefined => Some(false),
        KnownValue::TruthyFunction
        | KnownValue::TruthyArray
        | KnownValue::TruthyObject
        | KnownValue::TruthyRegExp => Some(true),
        KnownValue::Unknown => None,
    }
}

/// `x == true` for known values.
fn equal_true(value: &KnownValue) -> Option<bool> {
    match value {
        KnownValue::Boolean(v) => Some(*v),
        KnownValue::Number(v) => {
            if is_nan(*v) {
                Some(false)
            } else {
                Some(*v == 1.0)
            }
        }
        KnownValue::String(s) => Some(s == "1"),
        KnownValue::Null | KnownValue::Undefined => Some(false),
        KnownValue::TruthyFunction
        | KnownValue::TruthyArray
        | KnownValue::TruthyObject
        | KnownValue::TruthyRegExp => Some(false),
        KnownValue::Unknown => None,
    }
}

/// `x == false` for known values.
fn equal_false(value: &KnownValue) -> Option<bool> {
    match value {
        KnownValue::Boolean(v) => Some(!*v),
        KnownValue::Number(v) => {
            if is_nan(*v) {
                Some(false)
            } else {
                Some(*v == 0.0)
            }
        }
        KnownValue::String(s) => Some(s.is_empty()),
        KnownValue::Null | KnownValue::Undefined => Some(false),
        KnownValue::TruthyFunction
        | KnownValue::TruthyArray
        | KnownValue::TruthyObject
        | KnownValue::TruthyRegExp => Some(false),
        KnownValue::Unknown => None,
    }
}

fn deterministic_condition(expr: &Expression<'_>, known: &[HashMap<String, KnownValue>]) -> Option<bool> {
    if let Expression::UnaryExpression(un) = expr {
        use oxc_syntax::operator::UnaryOperator;
        if un.operator == UnaryOperator::LogicalNot {
            return deterministic_truthiness(&un.argument, known).map(|t| !t);
        }
        return None;
    }

    if let Expression::BinaryExpression(bin) = expr {
        use oxc_syntax::operator::BinaryOperator;
        let operand = deterministic_operand(&bin.left, known)?;
        let right_known = deterministic_operand(&bin.right, known);
        let result = match bin.operator {
            BinaryOperator::Equality | BinaryOperator::StrictEquality => {
                if let Some(ref r) = right_known {
                    equal_value(&operand, r)
                } else if is_boolean_literal(&bin.right, true) {
                    equal_true(&operand)
                } else if is_boolean_literal(&bin.right, false) {
                    equal_false(&operand)
                } else {
                    None
                }
            }
            BinaryOperator::Inequality | BinaryOperator::StrictInequality => {
                if let Some(ref r) = right_known {
                    equal_value(&operand, r).map(|v| !v)
                } else if is_boolean_literal(&bin.right, true) {
                    equal_true(&operand).map(|v| !v)
                } else if is_boolean_literal(&bin.right, false) {
                    equal_false(&operand).map(|v| !v)
                } else {
                    None
                }
            }
            _ => None,
        };
        return result;
    }

    deterministic_truthiness(expr, known)
}

fn lookup_known<'a>(
    known: &'a [HashMap<String, KnownValue>],
    name: &str,
) -> Option<&'a KnownValue> {
    known.iter().rev().find_map(|scope| scope.get(name))
}

fn deterministic_truthiness(
    expr: &Expression<'_>,
    known: &[HashMap<String, KnownValue>],
) -> Option<bool> {
    if let Some(v) = known_value(expr) {
        return truthiness(&v);
    }

    if let Expression::Identifier(ident) = expr {
        if let Some(v) = lookup_known(known, ident.name.as_str()) {
            return truthiness(v);
        }
    }

    None
}

fn deterministic_operand(
    expr: &Expression<'_>,
    known: &[HashMap<String, KnownValue>],
) -> Option<KnownValue> {
    if let Some(v) = known_value(expr) {
        return Some(v);
    }
    if let Expression::Identifier(ident) = expr {
        return lookup_known(known, ident.name.as_str()).cloned();
    }
    None
}

impl<'a> Visitor<'a> {
    fn enter_scope(&mut self) {
        self.known_values.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.known_values.pop();
    }

    fn current_scope(&mut self) -> &mut HashMap<String, KnownValue> {
        self.known_values.last_mut().unwrap()
    }

    fn set_current(&mut self, name: String, value: KnownValue) {
        self.current_scope().insert(name, value);
    }

    fn remove_current(&mut self, name: &str) {
        self.current_scope().remove(name);
    }

    fn add_shadowed_param(&mut self, name: String) {
        self.current_scope().insert(name, KnownValue::Unknown);
    }

    fn record_statement(&mut self, stmt: &Statement<'a>) {
        match stmt {
            Statement::VariableDeclaration(decl) => {
                for d in &decl.declarations {
                    let BindingPattern::BindingIdentifier(binding) = &d.id else {
                        continue;
                    };
                    let name = binding.name.as_str().to_string();
                    if let Some(init) = d.init.as_ref() {
                        if let Some(v) = known_value(init) {
                            self.set_current(name, v);
                        } else {
                            self.remove_current(&name);
                        }
                    } else {
                        self.remove_current(&name);
                    }
                }
            }
            Statement::FunctionDeclaration(decl) => {
                if let Some(id) = decl.id.as_ref() {
                    self.set_current(id.name.as_str().to_string(), KnownValue::TruthyFunction);
                }
            }
            Statement::ExpressionStatement(es) => {
                if let Expression::AssignmentExpression(assign) = &es.expression {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left {
                        let name = id.name.as_str().to_string();
                        if let Some(v) = known_value(&assign.right) {
                            self.set_current(name, v);
                        } else {
                            self.remove_current(&name);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn simplify_statement(&mut self, stmt: Statement<'a>, out: &mut ArenaVec<'a, Statement<'a>>) {
        if let Statement::IfStatement(if_stmt) = &stmt {
            if let Some(test_truthy) = deterministic_condition(&if_stmt.test, &self.known_values) {
                let replacement = if test_truthy {
                    Some(if_stmt.consequent.clone_in(self.allocator))
                } else {
                    if_stmt
                        .alternate
                        .as_ref()
                        .map(|a| a.clone_in(self.allocator))
                };

                if let Some(rep) = replacement {
                    out.push(rep);
                }

                self.modified = true;
                return;
            }
        }

        out.push(stmt);
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in original {
            self.record_statement(&stmt);
            self.simplify_statement(stmt, &mut new_body);
        }
        *stmts = new_body;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.enter_scope();
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
        self.exit_scope();
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.enter_scope();
        self.simplify_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.exit_scope();
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        // Blocks do not introduce a new scope for `var` declarations, which are
        // the norm in this obfuscated code. Process them in the current scope.
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        self.enter_scope();
        for param in &it.params.items {
            if let Some(name) = binding_pattern_name(&param.pattern) {
                self.add_shadowed_param(name);
            }
        }
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.exit_scope();
    }
}

fn binding_pattern_name(pattern: &BindingPattern<'_>) -> Option<String> {
    match pattern {
        BindingPattern::BindingIdentifier(id) => Some(id.name.as_str().to_string()),
        _ => None,
    }
}
