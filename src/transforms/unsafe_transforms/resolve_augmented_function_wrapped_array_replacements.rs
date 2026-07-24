use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use super::helpers;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveAugmentedFunctionWrappedArrayReplacements {
    evaluator: JsEvaluator,
}

impl ResolveAugmentedFunctionWrappedArrayReplacements {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveAugmentedFunctionWrappedArrayReplacements {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveAugmentedFunctionWrappedArrayReplacements {
    fn name(&self) -> &'static str {
        "resolveAugmentedFunctionWrappedArrayReplacements"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        // Find top-level statements that are candidates
        let mut iife_index = None;
        let mut target_name = None;

        for (idx, stmt) in program.body.iter().enumerate() {
            if let Statement::ExpressionStatement(expr_stmt) = stmt {
                if let Expression::CallExpression(call) = &expr_stmt.expression {
                    let mut callee = &call.callee;
                    while let Expression::ParenthesizedExpression(paren) = callee {
                        callee = &paren.expression;
                    }
                    if matches!(callee, Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)) {
                        let code = helpers::statement_to_code(stmt);
                        if code.contains(".push(") && code.contains(".shift(") {
                            // Find target argument name (the first arg is usually the array/function reference)
                            if let Some(first_arg) = call.arguments.first() {
                                if let Some(expr) = first_arg.as_expression() {
                                    if let Expression::Identifier(id) = expr {
                                        target_name = Some(id.name.to_string());
                                        iife_index = Some(idx);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let (Some(iife_idx), Some(name)) = (iife_index, target_name) else {
            return false;
        };

        // Find the declaration of the target name
        let mut decl_stmt = None;
        for stmt in &program.body {
            match stmt {
                Statement::FunctionDeclaration(func) => {
                    if let Some(id) = &func.id {
                        if id.name.as_str() == name {
                            decl_stmt = Some(helpers::statement_to_code(stmt));
                            break;
                        }
                    }
                }
                Statement::VariableDeclaration(decl) => {
                    let mut matches = false;
                    for d in &decl.declarations {
                        if let BindingPattern::BindingIdentifier(id) = &d.id {
                            if id.name.as_str() == name {
                                matches = true;
                                break;
                            }
                        }
                    }
                    if matches {
                        decl_stmt = Some(helpers::statement_to_code(stmt));
                        break;
                    }
                }
                _ => {}
            }
        }

        let Some(decl_code) = decl_stmt else {
            return false;
        };

        // Evaluate the declaration + the IIFE inside QuickJS, then retrieve the rotated array
        let iife_code = helpers::statement_to_code(&program.body[iife_idx]);
        // To get the array, if name is a function, we call it: `name()`, otherwise we just evaluate `name`.
        let retrieve_code = format!(
            "{};\n{};\n(typeof {} === 'function' ? {}() : {});",
            decl_code, iife_code, name, name, name
        );

        let Ok(json_res) = self.evaluator.eval_to_json(&retrieve_code) else {
            return false;
        };

        // Parse the JSON result array back into an AST Expression
        let Some(rotated_expr) = helpers::parse_expression_in(ctx.allocator, &json_res, oxc_span::SPAN) else {
            return false;
        };

        let Expression::ArrayExpression(rotated_arr) = rotated_expr else {
            return false;
        };

        // Replace the elements in the original array definition in the program body
        let mut replacer = ArrayReplacer {
            allocator: ctx.allocator,
            target_name: name,
            new_elements: rotated_arr.unbox().elements,
            modified: false,
        };
        replacer.visit_program(program);

        if replacer.modified {
            // Remove the IIFE statement from the program body
            program.body.remove(iife_idx);
            return true;
        }

        false
    }
}

impl UnsafeTransform for ResolveAugmentedFunctionWrappedArrayReplacements {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct ArrayReplacer<'a> {
    allocator: &'a oxc_allocator::Allocator,
    target_name: String,
    new_elements: ArenaVec<'a, ArrayExpressionElement<'a>>,
    modified: bool,
}

impl<'a> VisitMut<'a> for ArrayReplacer<'a> {
    fn visit_statement(&mut self, stmt: &mut Statement<'a>) {
        // If the statement is a VariableDeclaration of target_name, replace its initializer.
        if let Statement::VariableDeclaration(decl) = stmt {
            for d in &mut decl.declarations {
                if let BindingPattern::BindingIdentifier(id) = &d.id {
                    if id.name.as_str() == self.target_name {
                        if let Some(init) = &mut d.init {
                            if let Expression::ArrayExpression(arr) = init {
                                arr.elements = self.new_elements.clone_in(self.allocator);
                                self.modified = true;
                                return;
                            }
                        }
                    }
                }
            }
        }

        // If the statement is a FunctionDeclaration of target_name, look for the array inside it.
        if let Statement::FunctionDeclaration(func) = stmt {
            if let Some(id) = &func.id {
                if id.name.as_str() == self.target_name {
                    // Visit function body to find and replace the internal array expression
                    oxc_ast_visit::walk_mut::walk_statement(self, stmt);
                    return;
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_statement(self, stmt);
    }

    fn visit_expression(&mut self, expr: &mut Expression<'a>) {
        if let Expression::ArrayExpression(arr) = expr {
            // If we are inside the target function/variable, replace the first non-empty array we see
            if !self.modified {
                arr.elements = self.new_elements.clone_in(self.allocator);
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, expr);
    }
}
