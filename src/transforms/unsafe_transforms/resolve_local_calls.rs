use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};
use oxc_span::GetSpan;
use std::collections::HashSet;

use super::helpers;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

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
        let mut function_collector = FunctionCollector {
            context_parts: Vec::new(),
            names: HashSet::new(),
        };
        function_collector.visit_program(&*program);
        let mut context_parts = function_collector.context_parts;
        let mut local_function_names = function_collector.names;

        for stmt in &program.body {
            let code = helpers::statement_to_code(stmt);
            let is_static_array_declaration = match stmt {
                Statement::VariableDeclaration(declaration) => {
                    !declaration.declarations.is_empty()
                        && declaration.declarations.iter().all(|declarator| {
                            matches!(
                                declarator.init.as_ref(),
                                Some(Expression::ArrayExpression(array))
                                    if helpers::is_static_literal_array(array)
                            )
                        })
                }
                _ => false,
            };
            let is_function_value_declaration = match stmt {
                Statement::VariableDeclaration(declaration) => {
                    declaration.declarations.iter().any(|declarator| {
                        matches!(
                            declarator.init.as_ref(),
                            Some(
                                Expression::FunctionExpression(_)
                                    | Expression::ArrowFunctionExpression(_)
                            )
                        )
                    })
                }
                _ => false,
            };
            if (code.len() > 5_000
                && !is_static_array_declaration
                && !is_function_value_declaration)
                || (helpers::contains_skip_word(&code) && !is_function_value_declaration)
            {
                continue;
            }

            match stmt {
                Statement::FunctionDeclaration(_) => {}
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
                        if matches!(
                            call.callee,
                            Expression::FunctionExpression(_)
                                | Expression::ArrowFunctionExpression(_)
                        ) {
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

struct FunctionCollector {
    context_parts: Vec<String>,
    names: HashSet<String>,
}

impl<'a> Visit<'a> for FunctionCollector {
    fn visit_statement(&mut self, statement: &Statement<'a>) {
        if let Statement::FunctionDeclaration(function) = statement {
            if let Some(id) = &function.id {
                let code = helpers::statement_to_code(statement);
                let name = id.name.to_string();
                if code.len() <= 5_000
                    && !helpers::contains_skip_word(&code)
                    && !helpers::SKIP_IDENTIFIERS.contains(&name.as_str())
                {
                    self.names.insert(name);
                    self.context_parts.push(code);
                }
            }
        }
        if let Statement::VariableDeclaration(var_decl) = statement {
            let has_function_init = var_decl.declarations.iter().any(|declarator| {
                matches!(
                    declarator.init.as_ref(),
                    Some(Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_))
                )
            });
            if has_function_init {
                let code = helpers::statement_to_code(statement);
                if code.len() <= 5_000 && !helpers::contains_skip_word(&code) {
                    for declarator in &var_decl.declarations {
                        let BindingPattern::BindingIdentifier(id) = &declarator.id else {
                            continue;
                        };
                        if helpers::SKIP_IDENTIFIERS.contains(&id.name.as_str()) {
                            continue;
                        }
                        if matches!(
                            declarator.init.as_ref(),
                            Some(
                                Expression::FunctionExpression(_)
                                    | Expression::ArrowFunctionExpression(_)
                            )
                        ) {
                            self.names.insert(id.name.to_string());
                        }
                    }
                    self.context_parts.push(code);
                }
            }
        }
        oxc_ast_visit::walk::walk_statement(self, statement);
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
                            if let Ok(json_res) = self.transform.evaluator.eval_to_json(&full_code)
                            {
                                if let Some(new_expr) = helpers::parse_expression_in(
                                    self.allocator,
                                    &json_res,
                                    expr.span(),
                                ) {
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
