use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::transforms::unsafe_transforms::helpers;
use crate::{Transform, TransformCtx};

pub struct ResolveConstructorLiterals;

impl Transform for ResolveConstructorLiterals {
    fn name(&self) -> &'static str {
        "resolveConstructorLiterals"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = Visitor {
            allocator: ctx.allocator,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        oxc_ast_visit::walk_mut::walk_expression(self, it);

        let Expression::NewExpression(new_expr) = it else {
            return;
        };
        let Expression::Identifier(callee) = &new_expr.callee else {
            return;
        };

        match callee.name.as_str() {
            "RegExp" => {
                if let Some(lit_expr) = regexp_literal_from_new(&new_expr.arguments, self.allocator)
                {
                    *it = lit_expr;
                    self.modified = true;
                }
            }
            "Object" if new_expr.arguments.is_empty() => {
                *it = Expression::ObjectExpression(oxc_allocator::Box::new_in(
                    ObjectExpression {
                        node_id: new_expr.node_id.clone(),
                        span: new_expr.span,
                        properties: oxc_allocator::Vec::new_in(self.allocator),
                    },
                    self.allocator,
                ));
                self.modified = true;
            }
            _ => {}
        }
    }
}

fn regexp_literal_from_new<'a>(
    args: &[Argument<'a>],
    allocator: &'a oxc_allocator::Allocator,
) -> Option<Expression<'a>> {
    let pattern_arg = args.first()?.as_expression()?;
    let Expression::StringLiteral(pattern_lit) = pattern_arg else {
        return None;
    };
    let mut pattern = pattern_lit.value.as_str().to_string();
    let mut flags = String::new();
    if let Some(flags_arg) = args.get(1) {
        let Expression::StringLiteral(flags_lit) = flags_arg.as_expression()? else {
            return None;
        };
        flags = flags_lit.value.as_str().to_string();
    }

    // Avoid patterns that cannot be represented as a regex literal.
    if pattern.contains('\n') || pattern.contains('\r') {
        return None;
    }
    // Escape backslashes first, then forward slashes, so we don't double-escape.
    pattern = pattern.replace('\\', "\\\\").replace('/', "\\/");

    let source = if pattern.is_empty() {
        if flags.is_empty() {
            "(/(?:)/)".to_string()
        } else {
            format!("(/(?:)/{})", flags)
        }
    } else if flags.is_empty() {
        format!("(/{}/)", pattern)
    } else {
        format!("(/{}/{})", pattern, flags)
    };

    helpers::parse_expression_in(allocator, &source, pattern_lit.span)
}
