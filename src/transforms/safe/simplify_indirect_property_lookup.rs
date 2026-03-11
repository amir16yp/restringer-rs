use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct SimplifyIndirectPropertyLookup;

impl Transform for SimplifyIndirectPropertyLookup {
    fn name(&self) -> &'static str {
        "simplifyIndirectPropertyLookup"
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

    fn is_hasownproperty_check(&self, callee: &Expression<'a>, args: &'a [Argument<'a>]) -> Option<(&'a Expression<'a>, &'a Expression<'a>)> {
        if args.len() != 2 {
            return None;
        }

        let arg0 = args[0].as_expression()?;
        let arg1 = args[1].as_expression()?;

        let callee = self.unwrap_parens(callee);
        
        if let Expression::Identifier(id) = callee {
            if id.name.as_str() == "m" || id.name.as_str() == "hasOwnProperty" {
                return Some((arg0, arg1));
            }
        }

        None
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::ConditionalExpression(cond) = it {
            if let Expression::CallExpression(test_call) = self.unwrap_parens(&cond.test) {
                if let Some((obj, key)) = self.is_hasownproperty_check(&test_call.callee, &test_call.arguments) {
                    if let Expression::CallExpression(cons_call) = self.unwrap_parens(&cond.consequent) {
                        if let Expression::ComputedMemberExpression(member) = self.unwrap_parens(&cons_call.callee) {
                            let obj_match = match (self.unwrap_parens(&member.object), self.unwrap_parens(obj)) {
                                (Expression::Identifier(id1), Expression::Identifier(id2)) => id1.name == id2.name,
                                _ => false,
                            };

                            let key_match = match (self.unwrap_parens(&member.expression), self.unwrap_parens(key)) {
                                (Expression::Identifier(id1), Expression::Identifier(id2)) => id1.name == id2.name,
                                (Expression::StringLiteral(s1), Expression::StringLiteral(s2)) => s1.value == s2.value,
                                _ => false,
                            };

                            if obj_match && key_match {
                                *it = cond.consequent.clone_in(self.allocator);
                                self.modified = true;
                                return;
                            }
                        }
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
