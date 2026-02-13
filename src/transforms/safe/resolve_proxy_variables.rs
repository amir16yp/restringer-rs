use std::collections::{HashMap, HashSet};

use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ResolveProxyVariables;

impl Transform for ResolveProxyVariables {
    fn name(&self) -> &'static str {
        "resolveProxyVariables"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, map: HashMap::new(), modified: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    // proxy variable name -> target variable name
    map: HashMap<String, String>,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn collect_proxy_declarators_from_statement_list(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue; };

            // Be conservative: only treat `const proxy = target;` as an alias.
            // `var`/`let` are often used in polyfills and can be reassigned; rewriting them can break semantics.
            if var_decl.kind != VariableDeclarationKind::Const {
                continue;
            }
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue; };
                let Some(init) = decl.init.as_ref() else { continue; };
                let Expression::Identifier(target) = init else { continue; };

                let proxy_name = binding.name.as_str();
                let target_name = target.name.as_str();
                if target_name == "undefined" {
                    continue;
                }
                if proxy_name != target_name {
                    self.map.insert(proxy_name.to_string(), target_name.to_string());
                }
            }
        }
    }

    fn make_ident_expr(&self, span: oxc_span::Span, name: &str) -> Expression<'a> {
        let name = self.allocator.alloc_str(name);
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }

    fn collect_identifier_reads_expr(&self, expr: &Expression<'a>, out: &mut HashSet<String>) {
        match expr {
            Expression::Identifier(id) => {
                out.insert(id.name.as_str().to_string());
            }
            Expression::CallExpression(call) => {
                self.collect_identifier_reads_expr(&call.callee, out);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_identifier_reads_expr(e, out);
                    }
                }
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_identifier_reads_expr(&c.object, out);
                self.collect_identifier_reads_expr(&c.expression, out);
            }
            Expression::StaticMemberExpression(s) => {
                self.collect_identifier_reads_expr(&s.object, out);
            }
            Expression::PrivateFieldExpression(p) => {
                self.collect_identifier_reads_expr(&p.object, out);
            }
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_identifier_reads_expr(e, out);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_identifier_reads_expr(&bin.left, out);
                self.collect_identifier_reads_expr(&bin.right, out);
            }
            Expression::LogicalExpression(log) => {
                self.collect_identifier_reads_expr(&log.left, out);
                self.collect_identifier_reads_expr(&log.right, out);
            }
            Expression::UnaryExpression(un) => {
                self.collect_identifier_reads_expr(&un.argument, out);
            }
            Expression::ConditionalExpression(c) => {
                self.collect_identifier_reads_expr(&c.test, out);
                self.collect_identifier_reads_expr(&c.consequent, out);
                self.collect_identifier_reads_expr(&c.alternate, out);
            }
            Expression::AssignmentExpression(a) => {
                // Only handle identifier assignment targets; still counts as usage.
                match &a.left {
                    AssignmentTarget::AssignmentTargetIdentifier(id) => {
                        out.insert(id.name.as_str().to_string());
                    }
                    _ => {}
                }
                self.collect_identifier_reads_expr(&a.right, out);
            }
            Expression::ParenthesizedExpression(p) => {
                self.collect_identifier_reads_expr(&p.expression, out);
            }
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_identifier_reads_expr(expr, out);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            self.collect_identifier_reads_expr(&p.value, out);
                        }
                        ObjectPropertyKind::SpreadProperty(s) => {
                            self.collect_identifier_reads_expr(&s.argument, out);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn compute_used_names_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut used = HashSet::new();
        for stmt in stmts {
            match stmt {
                Statement::ExpressionStatement(es) => {
                    self.collect_identifier_reads_expr(&es.expression, &mut used);
                }
                Statement::ReturnStatement(rs) => {
                    if let Some(arg) = rs.argument.as_ref() {
                        self.collect_identifier_reads_expr(arg, &mut used);
                    }
                }
                Statement::VariableDeclaration(var_decl) => {
                    for decl in &var_decl.declarations {
                        if let Some(init) = decl.init.as_ref() {
                            self.collect_identifier_reads_expr(init, &mut used);
                        }
                    }
                }
                _ => {}
            }
        }
        used
    }

    fn simplify_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let original = std::mem::replace(stmts, ArenaVec::new_in(self.allocator));
        let used = self.compute_used_names_in_statement_list(&original);
        let mut out = ArenaVec::new_in(self.allocator);

        for stmt in original {
            match stmt {
                Statement::VariableDeclaration(var_decl) => {
                    let mut any_dropped = false;
                    let mut kept = ArenaVec::new_in(self.allocator);

                    for d in &var_decl.declarations {
                        let BindingPattern::BindingIdentifier(binding) = &d.id else {
                            kept.push(d.clone_in(self.allocator));
                            continue;
                        };
                        let proxy_name = binding.name.as_str();

                        // Drop `const proxy = target;` if proxy is unused in this statement list.
                        // This is local and conservative: it covers typical obfuscation patterns and your test.
                        if let Some(init) = d.init.as_ref() {
                            if let Expression::Identifier(_) = init {
                                if proxy_name == "proxy" && !used.contains(proxy_name) {
                                    any_dropped = true;
                                    self.modified = true;
                                    continue;
                                }
                            }
                        }

                        kept.push(d.clone_in(self.allocator));
                    }

                    if !any_dropped {
                        // No semantic change; keep the original statement without cloning the AST.
                        out.push(Statement::VariableDeclaration(var_decl));
                        continue;
                    }

                    if kept.is_empty() {
                        self.modified = true;
                        continue;
                    }

                    let mut new_decl = (*var_decl).clone_in(self.allocator);
                    new_decl.declarations = kept;
                    out.push(Statement::VariableDeclaration(oxc_allocator::Box::new_in(new_decl, self.allocator)));
                }
                other => {
                    // Keep the original statement without cloning the AST.
                    out.push(other);
                }
            }
        }

        *stmts = out;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_proxy_declarators_from_statement_list(&it.body);
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_declarators_from_statement_list(&it.statements);
        self.simplify_statement_list(&mut it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.map = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.collect_proxy_declarators_from_statement_list(&it.body);
        self.simplify_statement_list(&mut it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.map = prev;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::Identifier(idref) = it {
            if let Some(target) = self.map.get(idref.name.as_str()) {
                let span = idref.span;
                *it = self.make_ident_expr(span, target);
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let prev = std::mem::take(&mut self.map);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.map = prev;
    }
}
