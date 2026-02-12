use std::collections::{HashMap, HashSet};

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ReplaceIdentifierWithFixedAssignedValue;

impl Transform for ReplaceIdentifierWithFixedAssignedValue {
    fn name(&self) -> &'static str {
        "replaceIdentifierWithFixedAssignedValue"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, map: HashMap::new(), modified: false, in_object_shorthand: false };
        v.visit_program(program);
        v.modified
    }
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    // identifier -> literal expression
    map: HashMap<String, Expression<'a>>,
    modified: bool,
    // When visiting `{ a }` shorthand, OXC represents value as an Identifier expression.
    // Replacing it would change object structure (`{ a: 3 }`). Keep conservative.
    in_object_shorthand: bool,
}

impl<'a> Visitor<'a> {
    fn is_literal_expr(expr: &Expression<'a>) -> bool {
        matches!(
            expr,
            Expression::StringLiteral(_)
                | Expression::NumericLiteral(_)
                | Expression::BooleanLiteral(_)
                | Expression::NullLiteral(_)
                | Expression::BigIntLiteral(_)
                | Expression::RegExpLiteral(_)
        )
    }

    fn collect_candidates_from_statement_list(&self, stmts: &[Statement<'a>]) -> HashMap<String, Expression<'a>> {
        let mut out = HashMap::new();
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue; };
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue; };
                let Some(init) = decl.init.as_ref() else { continue; };
                if !Self::is_literal_expr(init) {
                    continue;
                }
                out.insert(binding.name.as_str().to_string(), init.clone_in(self.allocator));
            }
        }
        out
    }

    fn collect_modified_names_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut modified = HashSet::new();
        for stmt in stmts {
            self.collect_modified_names_in_statement(stmt, &mut modified);
        }
        modified
    }

    fn collect_modified_names_in_statement(&self, stmt: &Statement<'a>, out: &mut HashSet<String>) {
        match stmt {
            Statement::ExpressionStatement(es) => self.collect_modified_names_in_expression(&es.expression, out),
            Statement::ReturnStatement(rs) => {
                if let Some(arg) = rs.argument.as_ref() {
                    self.collect_modified_names_in_expression(arg, out);
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    if let Some(init) = decl.init.as_ref() {
                        self.collect_modified_names_in_expression(init, out);
                    }
                }
            }
            Statement::ForInStatement(for_in) => {
                if let Some(target) = for_in.left.as_assignment_target() {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                        out.insert(id.name.as_str().to_string());
                    }
                }
                self.collect_modified_names_in_expression(&for_in.right, out);
                self.collect_modified_names_in_statement(&for_in.body, out);
            }
            Statement::ForOfStatement(for_of) => {
                if let Some(target) = for_of.left.as_assignment_target() {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                        out.insert(id.name.as_str().to_string());
                    }
                }
                self.collect_modified_names_in_expression(&for_of.right, out);
                self.collect_modified_names_in_statement(&for_of.body, out);
            }
            Statement::BlockStatement(block) => {
                for s in &block.body {
                    self.collect_modified_names_in_statement(s, out);
                }
            }
            Statement::IfStatement(ifstmt) => {
                self.collect_modified_names_in_expression(&ifstmt.test, out);
                self.collect_modified_names_in_statement(&ifstmt.consequent, out);
                if let Some(alt) = ifstmt.alternate.as_ref() {
                    self.collect_modified_names_in_statement(alt, out);
                }
            }
            Statement::WhileStatement(ws) => {
                self.collect_modified_names_in_expression(&ws.test, out);
                self.collect_modified_names_in_statement(&ws.body, out);
            }
            Statement::DoWhileStatement(ws) => {
                self.collect_modified_names_in_statement(&ws.body, out);
                self.collect_modified_names_in_expression(&ws.test, out);
            }
            Statement::ForStatement(fs) => {
                if let Some(init) = fs.init.as_ref() {
                    if let Some(e) = init.as_expression() {
                        self.collect_modified_names_in_expression(e, out);
                    }
                }
                if let Some(test) = fs.test.as_ref() {
                    self.collect_modified_names_in_expression(test, out);
                }
                if let Some(update) = fs.update.as_ref() {
                    self.collect_modified_names_in_expression(update, out);
                }
                self.collect_modified_names_in_statement(&fs.body, out);
            }
            _ => {}
        }
    }

    fn collect_modified_names_in_expression(&self, expr: &Expression<'a>, out: &mut HashSet<String>) {
        match expr {
            Expression::AssignmentExpression(a) => {
                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left {
                    out.insert(id.name.as_str().to_string());
                }
                self.collect_modified_names_in_expression(&a.right, out);
            }
            Expression::UpdateExpression(u) => {
                if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument {
                    out.insert(id.name.as_str().to_string());
                }
            }
            Expression::CallExpression(call) => {
                self.collect_modified_names_in_expression(&call.callee, out);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_modified_names_in_expression(e, out);
                    }
                }
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_modified_names_in_expression(&c.object, out);
                self.collect_modified_names_in_expression(&c.expression, out);
            }
            Expression::StaticMemberExpression(s) => {
                self.collect_modified_names_in_expression(&s.object, out);
            }
            Expression::PrivateFieldExpression(p) => {
                self.collect_modified_names_in_expression(&p.object, out);
            }
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_modified_names_in_expression(e, out);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_modified_names_in_expression(&bin.left, out);
                self.collect_modified_names_in_expression(&bin.right, out);
            }
            Expression::LogicalExpression(log) => {
                self.collect_modified_names_in_expression(&log.left, out);
                self.collect_modified_names_in_expression(&log.right, out);
            }
            Expression::UnaryExpression(un) => {
                self.collect_modified_names_in_expression(&un.argument, out);
            }
            Expression::ConditionalExpression(c) => {
                self.collect_modified_names_in_expression(&c.test, out);
                self.collect_modified_names_in_expression(&c.consequent, out);
                self.collect_modified_names_in_expression(&c.alternate, out);
            }
            Expression::ParenthesizedExpression(p) => self.collect_modified_names_in_expression(&p.expression, out),
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_modified_names_in_expression(expr, out);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            self.collect_modified_names_in_expression(&p.value, out);
                        }
                        ObjectPropertyKind::SpreadProperty(s) => {
                            self.collect_modified_names_in_expression(&s.argument, out);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn load_map_for_statement_list(&mut self, stmts: &[Statement<'a>]) {
        let mut candidates = self.collect_candidates_from_statement_list(stmts);
        let modified = self.collect_modified_names_in_statement_list(stmts);
        candidates.retain(|k, _| !modified.contains(k));
        self.map = candidates;
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.load_map_for_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
        self.map = prev;
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.load_map_for_statement_list(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.map = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev = std::mem::take(&mut self.map);
        self.load_map_for_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.map = prev;
    }

    fn visit_object_property(&mut self, it: &mut ObjectProperty<'a>) {
        let prev = self.in_object_shorthand;
        if it.shorthand {
            self.in_object_shorthand = true;
        }
        // Don't walk the key (property name) at all; only the value can contain real references.
        self.visit_expression(&mut it.value);
        self.in_object_shorthand = prev;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if !self.in_object_shorthand {
            if let Expression::Identifier(idref) = it {
                if let Some(replacement) = self.map.get(idref.name.as_str()) {
                    let span = idref.span;
                    let mut new_expr = replacement.clone_in(self.allocator);

                    if let Expression::Identifier(new_id) = &mut new_expr {
                        new_id.span = span;
                    }

                    *it = new_expr;
                    self.modified = true;
                    return;
                }
            }
        }

        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        // Don’t rewrite declaration sites.
        let _ = it;
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let prev = std::mem::take(&mut self.map);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.map = prev;
    }
}
