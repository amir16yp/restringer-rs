use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct RearrangeSwitches;

impl Transform for RearrangeSwitches {
    fn name(&self) -> &'static str {
        "rearrangeSwitches"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, modified: false };
        v.visit_program(program);
        v.modified
    }
}

const MAX_REPETITION: usize = 50;

#[derive(Clone, Debug, PartialEq)]
enum LitVal {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    BigInt(String),
}

fn extract_lit(expr: &Expression<'_>) -> Option<LitVal> {
    match expr {
        Expression::NullLiteral(_) => Some(LitVal::Null),
        Expression::BooleanLiteral(l) => Some(LitVal::Bool(l.value)),
        Expression::NumericLiteral(l) => Some(LitVal::Number(l.value)),
        Expression::StringLiteral(l) => Some(LitVal::String(l.value.as_str().to_string())),
        Expression::BigIntLiteral(l) => l.raw.as_ref().map(|r| LitVal::BigInt(r.as_str().to_string())),
        _ => None,
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn extract_state_var_init(&self, stmt: &Statement<'a>) -> Option<(&'a str, LitVal)> {
        let Statement::VariableDeclaration(var_decl) = stmt else { return None; };
        if var_decl.declarations.len() != 1 {
            return None;
        }
        let decl = &var_decl.declarations[0];
        let ident = match &decl.id {
            BindingPattern::BindingIdentifier(ident) => ident,
            _ => return None,
        };
        let init = decl.init.as_ref()?;
        let val = extract_lit(init)?;
        Some((ident.name.as_str(), val))
    }

    fn switch_discriminant_ident<'b>(&self, stmt: &'b Statement<'a>) -> Option<&'b str> {
        let Statement::SwitchStatement(sw) = stmt else { return None; };
        match &sw.discriminant {
            Expression::Identifier(ident) => Some(ident.name.as_str()),
            _ => None,
        }
    }

    fn case_matches_value(&self, case: &SwitchCase<'a>, current: &LitVal) -> bool {
        match case.test.as_ref() {
            None => true, // default
            Some(test_expr) => extract_lit(test_expr).as_ref() == Some(current),
        }
    }

    fn is_break_statement(&self, stmt: &Statement<'a>) -> bool {
        matches!(stmt, Statement::BreakStatement(_))
    }

    fn direct_assignment_to(&self, stmt: &Statement<'a>, name: &str) -> Option<LitVal> {
        let Statement::ExpressionStatement(expr_stmt) = stmt else { return None; };
        let Expression::AssignmentExpression(assign) = &expr_stmt.expression else { return None; };

        let target = assign.left.as_simple_assignment_target()?;
        let left_name = match target {
            SimpleAssignmentTarget::AssignmentTargetIdentifier(id) => id.name.as_str(),
            _ => return None,
        };

        if left_name != name {
            return None;
        }

        extract_lit(&assign.right)
    }

    fn linearize_switch(&self, sw: &SwitchStatement<'a>, state_name: &str, init: LitVal) -> Option<ArenaVec<'a, Statement<'a>>> {
        let mut ordered = ArenaVec::new_in(self.allocator);
        let mut current = Some(init);
        let mut counter = 0usize;

        while let Some(val) = current.take() {
            if counter >= MAX_REPETITION {
                break;
            }

            let mut current_case: Option<&SwitchCase<'a>> = None;
            for case in &sw.cases {
                if self.case_matches_value(case, &val) {
                    current_case = Some(case);
                    break;
                }
            }
            let Some(case) = current_case else { break; };

            for stmt in &case.consequent {
                if !self.is_break_statement(stmt) {
                    ordered.push(stmt.clone_in(self.allocator));
                }
            }

            let mut assignments = Vec::new();
            for stmt in &case.consequent {
                if let Some(next) = self.direct_assignment_to(stmt, state_name) {
                    assignments.push(next);
                }
            }

            if assignments.len() == 1 {
                current = Some(assignments.remove(0));
            } else {
                break;
            }

            counter += 1;
        }

        if ordered.is_empty() { None } else { Some(ordered) }
    }

    fn transform_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let mut out = ArenaVec::new_in(self.allocator);

        let mut it = original.into_iter().peekable();
        while let Some(stmt) = it.next() {
            let try_rewrite = if let Some(next) = it.peek() {
                if let Some((state_name, init_val)) = self.extract_state_var_init(&stmt) {
                    if let Some(sw_ident) = self.switch_discriminant_ident(next) {
                        if sw_ident == state_name {
                            if let Statement::SwitchStatement(sw) = next {
                                if let Some(ordered) = self.linearize_switch(sw, state_name, init_val) {
                                    for s in ordered {
                                        out.push(s);
                                    }
                                    self.modified = true;
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if try_rewrite {
                // consume the switch statement
                let _ = it.next();
                continue;
            }

            out.push(stmt);
        }

        *stmts = out;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.transform_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        self.transform_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
    }
}
