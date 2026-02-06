use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct NormalizeComputed;

impl Transform for NormalizeComputed {
    fn name(&self) -> &'static str {
        "normalizeComputed"
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

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else { return false; };
    if !(first.is_ascii_alphabetic() || first == '_' || first == '$') {
        return false;
    }
    for c in chars {
        if !(c.is_ascii_alphanumeric() || c == '_' || c == '$') {
            return false;
        }
    }
    true
}

impl<'a> Visitor<'a> {
    fn make_identifier_name(&self, span: oxc_span::Span, name: &str) -> IdentifierName<'a> {
        let name = self.allocator.alloc_str(name);
        IdentifierName { span, name: name.into() }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_member_expression(&mut self, it: &mut MemberExpression<'a>) {
        match it {
            MemberExpression::ComputedMemberExpression(c) => {
                if let Expression::StringLiteral(lit) = &c.expression {
                    let prop = lit.value.as_str();
                    if is_valid_identifier(prop) {
                        let new_obj = c.object.clone_in(self.allocator);
                        let key = self.make_identifier_name(lit.span, prop);
                        *it = MemberExpression::StaticMemberExpression(ArenaBox::new_in(
                            StaticMemberExpression {
                                span: c.span,
                                object: new_obj,
                                property: key,
                                optional: c.optional,
                            },
                            self.allocator,
                        ));
                        self.modified = true;
                        return;
                    }
                }
            }
            _ => {}
        }
        oxc_ast_visit::walk_mut::walk_member_expression(self, it);
    }
}
