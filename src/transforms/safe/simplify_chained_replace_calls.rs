use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::SPAN;

use crate::{Transform, TransformCtx};

pub struct SimplifyChainedReplaceCalls;

impl Transform for SimplifyChainedReplaceCalls {
    fn name(&self) -> &'static str {
        "simplifyChainedReplaceCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn unwrap_parens<'b>(&self, mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
        loop {
            match expr {
                Expression::ParenthesizedExpression(p) => expr = &p.expression,
                _ => return expr,
            }
        }
    }

    fn is_replace_call(&self, expr: &'a Expression<'a>) -> Option<(&'a Expression<'a>, &'a Argument<'a>, &'a Argument<'a>)> {
        if let Expression::CallExpression(call) = self.unwrap_parens(expr) {
            if let Expression::StaticMemberExpression(member) = self.unwrap_parens(&call.callee) {
                if member.property.name.as_str() == "replace" && call.arguments.len() == 2 {
                    return Some((&member.object, &call.arguments[0], &call.arguments[1]));
                }
            }
        }
        None
    }

    fn is_simple_backslash_removal(&self, pattern: &Argument<'a>, replacement: &Argument<'a>) -> bool {
        if let Some(pattern_expr) = pattern.as_expression() {
            if let Some(repl_expr) = replacement.as_expression() {
                if let Expression::StringLiteral(pattern_str) = self.unwrap_parens(pattern_expr) {
                    if let Expression::StringLiteral(repl_str) = self.unwrap_parens(repl_expr) {
                        return pattern_str.value.as_str() == "\\" && repl_str.value.as_str() == "";
                    }
                }
            }
        }
        false
    }

    fn is_complex_escape_regex(&self, pattern: &Argument<'a>) -> bool {
        if let Some(pattern_expr) = pattern.as_expression() {
            if let Expression::RegExpLiteral(regex) = self.unwrap_parens(pattern_expr) {
                let pattern_str = regex.regex.pattern.text.as_str();
                return pattern_str.contains(r"\\(\[)") || pattern_str.contains(r"\[([^\]\[]*)\]");
            }
        }
        false
    }

    fn create_simplified_replace(&self, obj: &Expression<'a>) -> Expression<'a> {
        let pattern_str = self.allocator.alloc_str(r"\\?\[|\]|\\(.)");
        let pattern = RegExpLiteral {
            span: SPAN,
            raw: None,
            regex: RegExp {
                pattern: RegExpPattern { 
                    text: pattern_str.into(),
                    pattern: None,
                },
                flags: RegExpFlags::G,
            },
        };

        let replacement_str = self.allocator.alloc_str("$1");
        let replacement = StringLiteral {
            span: SPAN,
            value: replacement_str.into(),
            raw: None,
            lone_surrogates: false,
        };

        let replace_name = self.allocator.alloc_str("replace");
        let member = StaticMemberExpression {
            span: SPAN,
            object: obj.clone_in(self.allocator),
            property: IdentifierName {
                span: SPAN,
                name: replace_name.into(),
            },
            optional: false,
        };

        let mut args = ArenaVec::new_in(self.allocator);
        args.push(Argument::from(Expression::RegExpLiteral(ArenaBox::new_in(pattern, self.allocator))));
        args.push(Argument::from(Expression::StringLiteral(ArenaBox::new_in(replacement, self.allocator))));

        Expression::CallExpression(ArenaBox::new_in(
            CallExpression {
                span: SPAN,
                callee: Expression::StaticMemberExpression(ArenaBox::new_in(member, self.allocator)),
                type_arguments: None,
                arguments: args,
                optional: false,
                pure: false,
            },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        let should_replace = if let Expression::CallExpression(call) = it {
            if let Expression::StaticMemberExpression(member) = self.unwrap_parens(&call.callee) {
                if member.property.name.as_str() == "replace" && call.arguments.len() == 2 {
                    if self.is_complex_escape_regex(&call.arguments[0]) {
                        if let Expression::CallExpression(inner_call) = self.unwrap_parens(&member.object) {
                            if let Expression::StaticMemberExpression(inner_member) = self.unwrap_parens(&inner_call.callee) {
                                if inner_member.property.name.as_str() == "replace" && inner_call.arguments.len() == 2 {
                                    if self.is_simple_backslash_removal(&inner_call.arguments[0], &inner_call.arguments[1]) {
                                        Some(inner_member.object.clone_in(self.allocator))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some(base_obj) = should_replace {
            *it = self.create_simplified_replace(&base_obj);
            self.modified = true;
            return;
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
