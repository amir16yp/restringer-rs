use std::collections::HashSet;

use oxc_allocator::{Allocator, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::{Visit, VisitMut};

use crate::{Transform, TransformCtx};

pub struct RemoveUnusedVariables;

impl Transform for RemoveUnusedVariables {
    fn name(&self) -> &'static str {
        "removeUnusedVariables"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut read_collector = ReadCollector {
            reads: HashSet::new(),
        };
        read_collector.visit_program(program);
        let reads = read_collector.reads;

        let mut v = Visitor {
            allocator: ctx.allocator,
            modified: false,
            reads,
        };
        v.visit_program(program);
        v.modified
    }
}

struct ReadCollector {
    reads: HashSet<String>,
}

impl<'a> Visit<'a> for ReadCollector {
    fn visit_assignment_expression(&mut self, it: &AssignmentExpression<'a>) {
        // The left-hand side is a write, not a read.
        self.visit_expression(&it.right);
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        self.reads.insert(it.name.to_string());
    }
}

struct Visitor<'a> {
    allocator: &'a Allocator,
    modified: bool,
    reads: HashSet<String>,
}

impl<'a> Visitor<'a> {
    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut new_body = ArenaVec::new_in(self.allocator);
        for stmt in original {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let mut kept = ArenaVec::new_in(self.allocator);
                    let mut any_removed = false;
                    for d in &var_decl.declarations {
                        let name = match binding_pattern_name(&d.id) {
                            Some(n) => n,
                            None => {
                                kept.push(d.clone_in(self.allocator));
                                continue;
                            }
                        };
                        let removable = !self.reads.contains(&name)
                            && (d.init.is_none() || is_effect_free(d.init.as_ref().unwrap()));
                        if removable {
                            any_removed = true;
                            self.modified = true;
                        } else {
                            kept.push(d.clone_in(self.allocator));
                        }
                    }
                    if kept.is_empty() {
                        continue;
                    }
                    if any_removed {
                        let mut new_decl = (*var_decl).clone_in(self.allocator);
                        new_decl.declarations = kept;
                        new_body.push(Statement::VariableDeclaration(oxc_allocator::Box::new_in(
                            new_decl,
                            self.allocator,
                        )));
                    } else {
                        new_body.push(Statement::VariableDeclaration(
                            var_decl.clone_in(self.allocator),
                        ));
                    }
                }
                Statement::ExpressionStatement(es) => {
                    if let Expression::AssignmentExpression(assign) = &es.expression {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = &assign.left {
                            let name = id.name.to_string();
                            if !self.reads.contains(&name) && is_effect_free(&assign.right) {
                                self.modified = true;
                                continue;
                            }
                        }
                    }
                    new_body.push(Statement::ExpressionStatement(es));
                }
                _ => {
                    new_body.push(stmt);
                }
            }
        }
        *stmts = new_body;
    }
}

fn binding_pattern_name(pat: &BindingPattern<'_>) -> Option<String> {
    match pat {
        BindingPattern::BindingIdentifier(id) => Some(id.name.to_string()),
        _ => None,
    }
}

fn is_effect_free(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_)
        | Expression::Identifier(_)
        | Expression::ThisExpression(_)
        | Expression::Super(_) => true,
        Expression::ArrayExpression(arr) => arr
            .elements
            .iter()
            .all(|e| is_effect_free(e.to_expression())),
        Expression::ObjectExpression(obj) => obj.properties.iter().all(|p| match p {
            ObjectPropertyKind::ObjectProperty(prop) => is_effect_free(&prop.value),
            ObjectPropertyKind::SpreadProperty(spread) => is_effect_free(&spread.argument),
        }),
        Expression::StaticMemberExpression(s) => is_effect_free(&s.object),
        Expression::ComputedMemberExpression(c) => {
            is_effect_free(&c.object) && is_effect_free(&c.expression)
        }
        Expression::PrivateFieldExpression(p) => is_effect_free(&p.object),
        Expression::UnaryExpression(unary) => {
            !matches!(unary.operator, oxc_syntax::operator::UnaryOperator::Delete)
                && is_effect_free(&unary.argument)
        }
        Expression::BinaryExpression(bin) => {
            is_effect_free(&bin.left) && is_effect_free(&bin.right)
        }
        Expression::LogicalExpression(log) => {
            is_effect_free(&log.left) && is_effect_free(&log.right)
        }
        Expression::ConditionalExpression(cond) => {
            is_effect_free(&cond.test)
                && is_effect_free(&cond.consequent)
                && is_effect_free(&cond.alternate)
        }
        Expression::SequenceExpression(seq) => seq.expressions.iter().all(is_effect_free),
        Expression::TemplateLiteral(t) => t.expressions.iter().all(is_effect_free),
        Expression::ParenthesizedExpression(p) => is_effect_free(&p.expression),
        _ => false,
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.simplify_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
