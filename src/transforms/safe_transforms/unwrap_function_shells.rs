use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct UnwrapFunctionShells;

impl Transform for UnwrapFunctionShells {
    fn name(&self) -> &'static str {
        "unwrapFunctionShells"
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
    fn is_apply_property(&self, mem: &MemberExpression<'a>) -> bool {
        match mem {
            MemberExpression::StaticMemberExpression(s) => s.property.name.as_str() == "apply",
            MemberExpression::ComputedMemberExpression(c) => {
                matches!(&c.expression, Expression::StringLiteral(lit) if lit.value.as_str() == "apply")
            }
            _ => false,
        }
    }

    fn is_this_and_arguments(&self, args: &oxc_allocator::Vec<'a, Argument<'a>>) -> bool {
        if args.len() != 2 {
            return false;
        }

        let a0 = args[0].as_expression();
        let a1 = args[1].as_expression();
        matches!(a0, Some(Expression::ThisExpression(_)))
            && matches!(a1, Some(Expression::Identifier(id)) if id.name.as_str() == "arguments")
    }

    fn try_unwrap(&mut self, func: &mut Function<'a>) -> bool {
        let body = match func.body.as_ref() {
            Some(b) => b,
            None => return false,
        };
        if body.statements.len() != 1 {
            return false;
        }

        let Statement::ReturnStatement(ret) = &body.statements[0] else { return false; };
        let Some(arg) = ret.argument.as_ref() else { return false; };
        let Expression::CallExpression(call) = arg else { return false; };

        if call.arguments.len() != 2 {
            return false;
        }

        let Some(mem) = call.callee.as_member_expression() else { return false; };
        if !self.is_apply_property(mem) {
            return false;
        }

        let inner_func = match mem {
            MemberExpression::StaticMemberExpression(s) => match &s.object {
                Expression::FunctionExpression(f) => f,
                _ => return false,
            },
            MemberExpression::ComputedMemberExpression(c) => match &c.object {
                Expression::FunctionExpression(f) => f,
                _ => return false,
            },
            _ => return false,
        };

        if !self.is_this_and_arguments(&call.arguments) {
            return false;
        }

        let mut replacement = (**inner_func).clone_in(self.allocator);

        // Transfer identifier from outer if inner is anonymous
        if func.id.is_some() && replacement.id.is_none() {
            replacement.id = func.id.clone_in(self.allocator);
        }

        // Transfer params from outer if inner has none
        if !func.params.items.is_empty() && replacement.params.items.is_empty() {
            replacement.params = func.params.clone_in(self.allocator);
        }

        // Overwrite in place
        *func = replacement;
        true
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        if self.try_unwrap(it) {
            self.modified = true;
            return;
        }
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
    }
}
