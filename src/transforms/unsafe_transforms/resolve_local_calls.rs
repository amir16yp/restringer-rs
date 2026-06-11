use std::collections::HashSet;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;

use crate::{Transform, TransformCtx};
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use super::helpers;

pub struct ResolveLocalCalls {
    evaluator: JsEvaluator,
}

impl ResolveLocalCalls {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveLocalCalls {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveLocalCalls {
    fn name(&self) -> &'static str {
        "resolveLocalCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        // Step 1: Collect top-level statements that define our environment context.
        let mut context_parts = Vec::new();
        let mut local_function_names = HashSet::new();

        for stmt in &program.body {
            let code = helpers::statement_to_code(stmt);
            if code.len() > 5000 || helpers::contains_skip_word(&code) {
                continue;
            }

            match stmt {
                Statement::FunctionDeclaration(func) => {
                    if let Some(id) = &func.id {
                        let name = id.name.to_string();
                        if !helpers::SKIP_IDENTIFIERS.contains(&name.as_str()) {
                            local_function_names.insert(name);
                            context_parts.push(code);
                        }
                    }
                }
                Statement::VariableDeclaration(decl) => {
                    let mut has_skip = false;
                    for d in &decl.declarations {
                        if let BindingPattern::BindingIdentifier(id) = &d.id {
                            if helpers::SKIP_IDENTIFIERS.contains(&id.name.as_str()) {
                                has_skip = true;
                                break;
                            }
                        }
                    }
                    if !has_skip {
                        // Collect declared variable names too, in case we need to track them.
                        for d in &decl.declarations {
                            if let BindingPattern::BindingIdentifier(id) = &d.id {
                                local_function_names.insert(id.name.to_string());
                            }
                        }
                        context_parts.push(code);
                    }
                }
                Statement::ExpressionStatement(expr_stmt) => {
                    // Check if it is an IIFE (commonly used to rotate arrays)
                    if let Expression::CallExpression(call) = &expr_stmt.expression {
                        if matches!(call.callee, Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)) {
                            context_parts.push(code);
                        }
                    }
                }
                _ => {}
            }
        }

        if local_function_names.is_empty() {
            return false;
        }

        // Initialize sandbox with prelude + accumulated context
        let mut context_code = helpers::EVAL_PRELUDE.to_string();
        context_code.push_str(";\n");
        context_code.push_str(&context_parts.join(";\n"));
        context_code.push_str(";\n");

        let mut visitor = LocalCallsVisitor {
            allocator: ctx.allocator,
            transform: self,
            local_functions: local_function_names,
            context_code,
            current_function: None,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

impl UnsafeTransform for ResolveLocalCalls {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct LocalCallsVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolveLocalCalls,
    local_functions: HashSet<String>,
    context_code: String,
    current_function: Option<String>,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for LocalCallsVisitor<'a, 'b> {
    fn visit_function(&mut self, func: &mut Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        let prev = self.current_function.clone();
        if let Some(id) = &func.id {
            self.current_function = Some(id.name.to_string());
        }
        oxc_ast_visit::walk_mut::walk_function(self, func, flags);
        self.current_function = prev;
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expr);

        if let Expression::CallExpression(call) = expr {
            if let Expression::Identifier(ident) = &call.callee {
                let callee_name = ident.name.as_str();
                
                // Do not evaluate if we are inside the function itself to avoid recursion,
                // or if it's a global we should skip.
                if Some(callee_name) == self.current_function.as_deref() {
                    return;
                }

                if self.local_functions.contains(callee_name) {
                    // Check if all arguments are static literals
                    let args_are_literal = call.arguments.iter().all(|arg| {
                        if let Some(expr) = arg.as_expression() {
                            helpers::is_static_literal(expr)
                        } else {
                            false
                        }
                    });

                    if args_are_literal {
                        let call_code = helpers::expression_to_code(expr);
                        if !call_code.is_empty() {
                            let full_code = format!("{};\n{}", self.context_code, call_code);
                            if let Ok(json_res) = self.transform.evaluator.eval_to_json(&full_code) {
                                if let Some(new_expr) = helpers::parse_expression_in(self.allocator, &json_res, expr.span()) {
                                    *expr = new_expr;
                                    self.modified = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
