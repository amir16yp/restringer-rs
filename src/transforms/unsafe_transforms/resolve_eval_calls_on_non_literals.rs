use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_parser::Parser;
use oxc_span::SourceType;

use super::engine::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveEvalCallsOnNonLiterals {
    evaluator: JsEvaluator,
}

impl ResolveEvalCallsOnNonLiterals {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveEvalCallsOnNonLiterals {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveEvalCallsOnNonLiterals {
    fn name(&self) -> &'static str {
        "resolveEvalCallsOnNonLiterals"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut modified = false;
        let mut i = 0;
        while i < program.body.len() {
            let arg_code = if let Statement::ExpressionStatement(es) = &program.body[i] {
                if let Expression::CallExpression(call) = &es.expression {
                    if is_eval_call_with_non_literal(call) {
                        let arg = call.arguments[0].as_expression().unwrap();
                        Some(super::helpers::expression_to_code(arg))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            };

            if let Some(arg_code) = arg_code {
                if super::helpers::has_unresolved_references(&arg_code, SourceType::mjs()) {
                    i += 1;
                    continue;
                }
                match self.evaluator().eval_to_string(&arg_code) {
                    Ok(code_str) => {
                        if let Some(new_stmts) = parse_statements(ctx.allocator, &code_str) {
                            let insert_count = new_stmts.len();
                            program.body.remove(i);
                            for (j, stmt) in new_stmts.into_iter().enumerate() {
                                program.body.insert(i + j, stmt);
                            }
                            modified = true;
                            i += insert_count;
                            continue;
                        }
                    }
                    Err(_) => {}
                }
            }

            i += 1;
        }

        modified
    }
}

impl UnsafeTransform for ResolveEvalCallsOnNonLiterals {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

fn is_eval_call_with_non_literal(call: &CallExpression) -> bool {
    let is_eval = match &call.callee {
        Expression::Identifier(id) => id.name.as_str() == "eval",
        _ => false,
    };
    if !is_eval || call.arguments.len() != 1 {
        return false;
    }
    let Some(arg) = call.arguments[0].as_expression() else {
        return false;
    };
    !matches!(arg, Expression::StringLiteral(_))
}

fn parse_statements<'a>(
    allocator: &'a oxc_allocator::Allocator,
    code: &str,
) -> Option<Vec<Statement<'a>>> {
    let source = allocator.alloc_str(&format!("{}\n", code));
    let mut ret = Parser::new(allocator, source, SourceType::mjs()).parse();
    if !ret.errors.is_empty() {
        // Try a repair heuristic similar to the JS implementation: insert newlines after closing brackets.
        let repaired = code
            .replace(")", ")\n")
            .replace("]", "]\n")
            .replace("}", "}\n");
        let source2 = allocator.alloc_str(&format!("{}\n", repaired));
        ret = Parser::new(allocator, source2, SourceType::mjs()).parse();
        if !ret.errors.is_empty() {
            return None;
        }
    }
    if ret.program.body.is_empty() {
        return None;
    }
    Some(
        ret.program
            .body
            .into_iter()
            .map(|stmt| stmt.clone_in(allocator))
            .collect(),
    )
}
