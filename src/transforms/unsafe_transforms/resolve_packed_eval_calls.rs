use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_parser::Parser;
use oxc_span::{GetSpan, SourceType};

use super::helpers;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolvePackedEvalCalls {
    evaluator: JsEvaluator,
}

impl ResolvePackedEvalCalls {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolvePackedEvalCalls {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolvePackedEvalCalls {
    fn name(&self) -> &'static str {
        "resolvePackedEvalCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut modified = false;
        let mut index = 0;

        while index < program.body.len() {
            let Some(argument_code) = packed_eval_argument_code(&program.body[index]) else {
                index += 1;
                continue;
            };

            let mut context = helpers::EVAL_PRELUDE.to_string();
            context.push(';');
            for statement in &program.body[..index] {
                let code = helpers::statement_to_code(statement);
                if code.len() > 50_000 || helpers::contains_skip_word(&code) {
                    context.clear();
                    break;
                }
                context.push_str(&code);
                context.push(';');
            }
            if context.is_empty() || context.len() + argument_code.len() > 2_000_000 {
                index += 1;
                continue;
            }
            context.push_str(&argument_code);

            let Ok(unpacked) = self.evaluator().eval_to_string(&context) else {
                index += 1;
                continue;
            };
            let Some(statements) = parse_statements(ctx.allocator, &unpacked) else {
                index += 1;
                continue;
            };

            let count = statements.len();
            program.body.remove(index);
            for (offset, statement) in statements.into_iter().enumerate() {
                program.body.insert(index + offset, statement);
            }
            modified = true;
            index += count;
        }

        let mut visitor = NestedPackedEvalVisitor {
            allocator: ctx.allocator,
            transform: self,
            modified: false,
        };
        visitor.visit_program(program);
        modified || visitor.modified
    }
}

impl UnsafeTransform for ResolvePackedEvalCalls {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

struct NestedPackedEvalVisitor<'a, 'b> {
    allocator: &'a oxc_allocator::Allocator,
    transform: &'b ResolvePackedEvalCalls,
    modified: bool,
}

impl<'a, 'b> VisitMut<'a> for NestedPackedEvalVisitor<'a, 'b> {
    fn visit_expression(&mut self, expression: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, expression);

        let Expression::CallExpression(call) = expression else {
            return;
        };
        let Expression::Identifier(callee) = &call.callee else {
            return;
        };
        if callee.name.as_str() != "eval" || call.arguments.len() != 1 {
            return;
        }
        let Some(argument) = call.arguments.first().and_then(Argument::as_expression) else {
            return;
        };
        let Expression::CallExpression(packer_call) = argument else {
            return;
        };
        if !matches!(
            packer_call.callee,
            Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
        ) {
            return;
        }

        let code = helpers::expression_to_code(argument);
        let context = format!("{};{}", helpers::EVAL_PRELUDE, code);
        if helpers::has_unresolved_references(&context, SourceType::mjs()) {
            return;
        }
        let Ok(result) = self.transform.evaluator().eval_to_json(&context) else {
            return;
        };
        let Some(replacement) =
            helpers::parse_expression_in(self.allocator, &result, argument.span())
        else {
            return;
        };
        call.arguments[0] = Argument::from(replacement);
        self.modified = true;
    }
}

fn packed_eval_argument_code(statement: &Statement) -> Option<String> {
    let Statement::ExpressionStatement(expression_statement) = statement else {
        return None;
    };
    let Expression::CallExpression(call) = &expression_statement.expression else {
        return None;
    };
    let Expression::Identifier(callee) = &call.callee else {
        return None;
    };
    if callee.name.as_str() != "eval" || call.arguments.len() != 1 {
        return None;
    }
    let argument = call.arguments.first()?.as_expression()?;
    let Expression::CallExpression(packer_call) = argument else {
        return None;
    };
    if !matches!(
        packer_call.callee,
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
    ) {
        return None;
    }
    Some(helpers::expression_to_code(argument))
}

fn parse_statements<'a>(
    allocator: &'a oxc_allocator::Allocator,
    code: &str,
) -> Option<Vec<Statement<'a>>> {
    let source = allocator.alloc_str(&format!("{}\n", code));
    let parsed = Parser::new(allocator, source, SourceType::mjs()).parse();
    if !parsed.errors.is_empty() || parsed.program.body.is_empty() {
        return None;
    }
    Some(
        parsed
            .program
            .body
            .into_iter()
            .map(|statement| statement.clone_in(allocator))
            .collect(),
    )
}
