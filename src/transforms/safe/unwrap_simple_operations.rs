use std::collections::HashMap;

use oxc_allocator::{Box as ArenaBox, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::Span;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct UnwrapSimpleOperations;

impl Transform for UnwrapSimpleOperations {
    fn name(&self) -> &'static str {
        "unwrapSimpleOperations"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut collector = Collector { map: HashMap::new() };
        collector.visit_program(program);

        if collector.map.is_empty() {
            return false;
        }

        let mut v = Rewriter { allocator: ctx.allocator, map: collector.map, modified: false };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Clone)]
enum OpKind {
    Binary { operator: BinaryOperator },
    Logical { operator: LogicalOperator },
    Unary { operator: oxc_syntax::operator::UnaryOperator },
}

#[derive(Clone)]
struct OpInfo {
    kind: OpKind,
    arity: usize,
    // For unary wrappers, which param index is used (currently always 0)
    unary_param_index: usize,
}

struct Collector {
    map: HashMap<String, OpInfo>,
}

impl<'a> Collector {
    fn is_allowed_unary(op: oxc_syntax::operator::UnaryOperator) -> bool {
        use oxc_syntax::operator::UnaryOperator;
        matches!(op, UnaryOperator::LogicalNot | UnaryOperator::BitwiseNot | UnaryOperator::UnaryPlus | UnaryOperator::UnaryNegation | UnaryOperator::Typeof | UnaryOperator::Void)
    }

    fn extract_param_names(params: &FormalParameters<'a>) -> Option<Vec<&'a str>> {
        let mut out = Vec::new();
        for p in &params.items {
            let ident = match &p.pattern {
                BindingPattern::BindingIdentifier(id) => id,
                _ => return None,
            };
            out.push(ident.name.as_str());
        }
        Some(out)
    }

    fn match_function_decl(&mut self, f: &Function<'a>) {
        let Some(id) = f.id.as_ref() else { return; };
        let name = id.name.as_str();

        let params = match Self::extract_param_names(&f.params) {
            Some(p) => p,
            None => return,
        };

        let Some(body) = f.body.as_ref() else { return; };
        if body.statements.len() != 1 {
            return;
        }

        let Statement::ReturnStatement(ret) = &body.statements[0] else { return; };
        let Some(arg) = ret.argument.as_ref() else { return; };

        match arg {
            Expression::BinaryExpression(bin) => {
                let Expression::Identifier(l) = &bin.left else { return; };
                let Expression::Identifier(r) = &bin.right else { return; };
                if params.len() == 2 && l.name.as_str() == params[0] && r.name.as_str() == params[1] {
                    self.map.insert(
                        name.to_string(),
                        OpInfo { kind: OpKind::Binary { operator: bin.operator }, arity: 2, unary_param_index: 0 },
                    );
                }
            }
            Expression::LogicalExpression(log) => {
                let Expression::Identifier(l) = &log.left else { return; };
                let Expression::Identifier(r) = &log.right else { return; };
                if params.len() == 2 && l.name.as_str() == params[0] && r.name.as_str() == params[1] {
                    self.map.insert(
                        name.to_string(),
                        OpInfo { kind: OpKind::Logical { operator: log.operator }, arity: 2, unary_param_index: 0 },
                    );
                }
            }
            Expression::UnaryExpression(un) => {
                if !Self::is_allowed_unary(un.operator) {
                    return;
                }
                let Expression::Identifier(id_expr) = &un.argument else { return; };
                if params.len() == 1 && id_expr.name.as_str() == params[0] {
                    self.map.insert(
                        name.to_string(),
                        OpInfo {
                            kind: OpKind::Unary { operator: un.operator },
                            arity: 1,
                            unary_param_index: 0,
                        },
                    );
                }
            }
            _ => {}
        }
    }
}

impl<'a> VisitMut<'a> for Collector {
    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        self.match_function_decl(it);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
    }
}

struct Rewriter<'a> {
    allocator: &'a oxc_allocator::Allocator,
    map: HashMap<String, OpInfo>,
    modified: bool,
}

impl<'a> Rewriter<'a> {
    fn make_binary_expr(&self, span: Span, op: BinaryOperator, left: Expression<'a>, right: Expression<'a>) -> Expression<'a> {
        Expression::BinaryExpression(ArenaBox::new_in(
            BinaryExpression { span, operator: op, left, right },
            self.allocator,
        ))
    }

    fn make_logical_expr(&self, span: Span, op: LogicalOperator, left: Expression<'a>, right: Expression<'a>) -> Expression<'a> {
        Expression::LogicalExpression(ArenaBox::new_in(
            LogicalExpression { span, operator: op, left, right },
            self.allocator,
        ))
    }

    fn make_unary_expr(
        &self,
        span: Span,
        op: oxc_syntax::operator::UnaryOperator,
        argument: Expression<'a>,
    ) -> Expression<'a> {
        Expression::UnaryExpression(ArenaBox::new_in(
            UnaryExpression { span, operator: op, argument },
            self.allocator,
        ))
    }
}

impl<'a> VisitMut<'a> for Rewriter<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if let Expression::Identifier(callee) = &call.callee {
                if let Some(info) = self.map.get(callee.name.as_str()) {
                    if call.arguments.len() == info.arity {
                        let mut args: Vec<Expression<'a>> = Vec::new();
                        for a in &call.arguments {
                            let Some(e) = a.as_expression() else {
                                args.clear();
                                break;
                            };
                            args.push(e.clone_in(self.allocator));
                        }

                        if args.len() == info.arity {
                            let span = call.span;
                            let replacement = match info.kind {
                                OpKind::Binary { operator } => self.make_binary_expr(span, operator, args[0].clone_in(self.allocator), args[1].clone_in(self.allocator)),
                                OpKind::Logical { operator } => self.make_logical_expr(span, operator, args[0].clone_in(self.allocator), args[1].clone_in(self.allocator)),
                                OpKind::Unary { operator } => {
                                    let idx = info.unary_param_index;
                                    self.make_unary_expr(span, operator, args[idx].clone_in(self.allocator))
                                }
                            };

                            *it = replacement;
                            self.modified = true;
                            return;
                        }
                    }
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }
}
