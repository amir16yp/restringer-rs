use std::collections::{HashMap, HashSet};

use oxc_allocator::CloneIn;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ResolveMemberExpressionsWithDirectAssignment;

impl Transform for ResolveMemberExpressionsWithDirectAssignment {
    fn name(&self) -> &'static str {
        "resolveMemberExpressionsWithDirectAssignment"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            map: HashMap::new(),
            modified: false,
        };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum PropKey {
    Str(String),
    Num(i64),
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    // (object identifier, property key) -> literal expression
    map: HashMap<(String, PropKey), Expression<'a>>,
    modified: bool,
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

    fn prop_key_from_static(mem: &StaticMemberExpression<'a>) -> Option<PropKey> {
        Some(PropKey::Str(mem.property.name.as_str().to_string()))
    }

    fn prop_key_from_computed(mem: &ComputedMemberExpression<'a>) -> Option<PropKey> {
        match &mem.expression {
            Expression::StringLiteral(s) => Some(PropKey::Str(s.value.as_str().to_string())),
            Expression::NumericLiteral(n) => {
                let v = n.value;
                if !v.is_finite() || v.fract() != 0.0 {
                    return None;
                }
                if v < (i64::MIN as f64) || v > (i64::MAX as f64) {
                    return None;
                }
                Some(PropKey::Num(v as i64))
            }
            _ => None,
        }
    }

    fn key_from_member_expr(mem: &Expression<'a>) -> Option<(String, PropKey)> {
        match mem {
            Expression::StaticMemberExpression(s) => {
                let Expression::Identifier(obj) = &s.object else { return None };
                Some((obj.name.as_str().to_string(), Self::prop_key_from_static(s)?))
            }
            Expression::ComputedMemberExpression(c) => {
                let Expression::Identifier(obj) = &c.object else { return None };
                Some((obj.name.as_str().to_string(), Self::prop_key_from_computed(c)?))
            }
            _ => None,
        }
    }

    fn collect_modified_idents_in_statement_list(&self, stmts: &[Statement<'a>]) -> HashSet<String> {
        let mut out = HashSet::new();
        for stmt in stmts {
            self.collect_modified_idents_in_statement(stmt, &mut out);
        }
        out
    }

    fn collect_modified_idents_in_statement(&self, stmt: &Statement<'a>, out: &mut HashSet<String>) {
        match stmt {
            Statement::ExpressionStatement(es) => self.collect_modified_idents_in_expression(&es.expression, out),
            Statement::ReturnStatement(rs) => {
                if let Some(arg) = rs.argument.as_ref() {
                    self.collect_modified_idents_in_expression(arg, out);
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    if let Some(init) = decl.init.as_ref() {
                        self.collect_modified_idents_in_expression(init, out);
                    }
                }
            }
            Statement::BlockStatement(block) => {
                for s in &block.body {
                    self.collect_modified_idents_in_statement(s, out);
                }
            }
            Statement::IfStatement(ifstmt) => {
                self.collect_modified_idents_in_expression(&ifstmt.test, out);
                self.collect_modified_idents_in_statement(&ifstmt.consequent, out);
                if let Some(alt) = ifstmt.alternate.as_ref() {
                    self.collect_modified_idents_in_statement(alt, out);
                }
            }
            Statement::WhileStatement(ws) => {
                self.collect_modified_idents_in_expression(&ws.test, out);
                self.collect_modified_idents_in_statement(&ws.body, out);
            }
            Statement::DoWhileStatement(ws) => {
                self.collect_modified_idents_in_statement(&ws.body, out);
                self.collect_modified_idents_in_expression(&ws.test, out);
            }
            Statement::ForStatement(fs) => {
                if let Some(init) = fs.init.as_ref() {
                    if let Some(e) = init.as_expression() {
                        self.collect_modified_idents_in_expression(e, out);
                    }
                }
                if let Some(test) = fs.test.as_ref() {
                    self.collect_modified_idents_in_expression(test, out);
                }
                if let Some(update) = fs.update.as_ref() {
                    self.collect_modified_idents_in_expression(update, out);
                }
                self.collect_modified_idents_in_statement(&fs.body, out);
            }
            Statement::ForInStatement(for_in) => {
                if let Some(target) = for_in.left.as_assignment_target() {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                        out.insert(id.name.as_str().to_string());
                    }
                }
                self.collect_modified_idents_in_expression(&for_in.right, out);
                self.collect_modified_idents_in_statement(&for_in.body, out);
            }
            Statement::ForOfStatement(for_of) => {
                if let Some(target) = for_of.left.as_assignment_target() {
                    if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                        out.insert(id.name.as_str().to_string());
                    }
                }
                self.collect_modified_idents_in_expression(&for_of.right, out);
                self.collect_modified_idents_in_statement(&for_of.body, out);
            }
            _ => {}
        }
    }

    fn collect_modified_idents_in_expression(&self, expr: &Expression<'a>, out: &mut HashSet<String>) {
        match expr {
            Expression::AssignmentExpression(a) => {
                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left {
                    out.insert(id.name.as_str().to_string());
                }
                self.collect_modified_idents_in_expression(&a.right, out);
            }
            Expression::UpdateExpression(u) => {
                if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument {
                    out.insert(id.name.as_str().to_string());
                }
            }
            Expression::CallExpression(call) => {
                self.collect_modified_idents_in_expression(&call.callee, out);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_modified_idents_in_expression(e, out);
                    }
                }
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_modified_idents_in_expression(&c.object, out);
                self.collect_modified_idents_in_expression(&c.expression, out);
            }
            Expression::StaticMemberExpression(s) => {
                self.collect_modified_idents_in_expression(&s.object, out);
            }
            Expression::PrivateFieldExpression(p) => {
                self.collect_modified_idents_in_expression(&p.object, out);
            }
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_modified_idents_in_expression(e, out);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_modified_idents_in_expression(&bin.left, out);
                self.collect_modified_idents_in_expression(&bin.right, out);
            }
            Expression::LogicalExpression(log) => {
                self.collect_modified_idents_in_expression(&log.left, out);
                self.collect_modified_idents_in_expression(&log.right, out);
            }
            Expression::UnaryExpression(un) => {
                self.collect_modified_idents_in_expression(&un.argument, out);
            }
            Expression::ConditionalExpression(c) => {
                self.collect_modified_idents_in_expression(&c.test, out);
                self.collect_modified_idents_in_expression(&c.consequent, out);
                self.collect_modified_idents_in_expression(&c.alternate, out);
            }
            Expression::ParenthesizedExpression(p) => self.collect_modified_idents_in_expression(&p.expression, out),
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_modified_idents_in_expression(expr, out);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            self.collect_modified_idents_in_expression(&p.value, out);
                        }
                        ObjectPropertyKind::SpreadProperty(s) => {
                            self.collect_modified_idents_in_expression(&s.argument, out);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_candidates_from_statement_list(&self, stmts: &[Statement<'a>]) -> HashMap<(String, PropKey), Expression<'a>> {
        let mut candidates: HashMap<(String, PropKey), Expression<'a>> = HashMap::new();
        let mut member_assignment_count: HashMap<(String, PropKey), usize> = HashMap::new();
        let mut member_modified: HashSet<(String, PropKey)> = HashSet::new();

        for stmt in stmts {
            self.collect_candidates_from_statement(stmt, &mut candidates, &mut member_assignment_count, &mut member_modified);
        }

        // Only allow entries assigned exactly once, and never updated/modified otherwise.
        candidates.retain(|k, _| member_assignment_count.get(k).copied().unwrap_or(0) == 1 && !member_modified.contains(k));
        candidates
    }

    fn collect_candidates_from_statement(
        &self,
        stmt: &Statement<'a>,
        candidates: &mut HashMap<(String, PropKey), Expression<'a>>,
        member_assignment_count: &mut HashMap<(String, PropKey), usize>,
        member_modified: &mut HashSet<(String, PropKey)>,
    ) {
        match stmt {
            Statement::ExpressionStatement(es) => self.collect_candidates_from_expression(
                &es.expression,
                candidates,
                member_assignment_count,
                member_modified,
            ),
            Statement::ReturnStatement(rs) => {
                if let Some(arg) = rs.argument.as_ref() {
                    self.collect_candidates_from_expression(arg, candidates, member_assignment_count, member_modified);
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    if let Some(init) = decl.init.as_ref() {
                        self.collect_candidates_from_expression(init, candidates, member_assignment_count, member_modified);
                    }
                }
            }
            Statement::BlockStatement(block) => {
                for s in &block.body {
                    self.collect_candidates_from_statement(s, candidates, member_assignment_count, member_modified);
                }
            }
            Statement::IfStatement(ifstmt) => {
                self.collect_candidates_from_expression(&ifstmt.test, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_statement(&ifstmt.consequent, candidates, member_assignment_count, member_modified);
                if let Some(alt) = ifstmt.alternate.as_ref() {
                    self.collect_candidates_from_statement(alt, candidates, member_assignment_count, member_modified);
                }
            }
            Statement::WhileStatement(ws) => {
                self.collect_candidates_from_expression(&ws.test, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_statement(&ws.body, candidates, member_assignment_count, member_modified);
            }
            Statement::DoWhileStatement(ws) => {
                self.collect_candidates_from_statement(&ws.body, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&ws.test, candidates, member_assignment_count, member_modified);
            }
            Statement::ForStatement(fs) => {
                if let Some(init) = fs.init.as_ref() {
                    if let Some(e) = init.as_expression() {
                        self.collect_candidates_from_expression(e, candidates, member_assignment_count, member_modified);
                    }
                }
                if let Some(test) = fs.test.as_ref() {
                    self.collect_candidates_from_expression(test, candidates, member_assignment_count, member_modified);
                }
                if let Some(update) = fs.update.as_ref() {
                    self.collect_candidates_from_expression(update, candidates, member_assignment_count, member_modified);
                }
                self.collect_candidates_from_statement(&fs.body, candidates, member_assignment_count, member_modified);
            }
            Statement::ForInStatement(for_in) => {
                self.collect_candidates_from_expression(&for_in.right, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_statement(&for_in.body, candidates, member_assignment_count, member_modified);
            }
            Statement::ForOfStatement(for_of) => {
                self.collect_candidates_from_expression(&for_of.right, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_statement(&for_of.body, candidates, member_assignment_count, member_modified);
            }
            _ => {}
        }
    }

    fn collect_candidates_from_expression(
        &self,
        expr: &Expression<'a>,
        candidates: &mut HashMap<(String, PropKey), Expression<'a>>,
        member_assignment_count: &mut HashMap<(String, PropKey), usize>,
        member_modified: &mut HashSet<(String, PropKey)>,
    ) {
        match expr {
            Expression::AssignmentExpression(a) => {
                // Count/record assignments to the member key.
                if let AssignmentTarget::ComputedMemberExpression(mem) = &a.left {
                    if let Expression::Identifier(obj) = &mem.object {
                        if let Some(prop_key) = Self::prop_key_from_computed(mem) {
                            let key = (obj.name.as_str().to_string(), prop_key);
                            *member_assignment_count.entry(key.clone()).or_insert(0) += 1;

                            if Self::is_literal_expr(&a.right) {
                                candidates.entry(key).or_insert_with(|| a.right.clone_in(self.allocator));
                            }
                        }
                    }
                }
                if let AssignmentTarget::StaticMemberExpression(mem) = &a.left {
                    if let Expression::Identifier(obj) = &mem.object {
                        if let Some(prop_key) = Self::prop_key_from_static(mem) {
                            let key = (obj.name.as_str().to_string(), prop_key);
                            *member_assignment_count.entry(key.clone()).or_insert(0) += 1;

                            if Self::is_literal_expr(&a.right) {
                                candidates.entry(key).or_insert_with(|| a.right.clone_in(self.allocator));
                            }
                        }
                    }
                }

                self.collect_candidates_from_expression(&a.right, candidates, member_assignment_count, member_modified);
            }
            Expression::UpdateExpression(u) => {
                // Any update on member expression => don't resolve that member.
                match &u.argument {
                    SimpleAssignmentTarget::ComputedMemberExpression(mem) => {
                        let Expression::Identifier(obj) = &mem.object else { return };
                        let Some(prop_key) = Self::prop_key_from_computed(mem) else { return };
                        member_modified.insert((obj.name.as_str().to_string(), prop_key));
                    }
                    SimpleAssignmentTarget::StaticMemberExpression(mem) => {
                        let Expression::Identifier(obj) = &mem.object else { return };
                        let Some(prop_key) = Self::prop_key_from_static(mem) else { return };
                        member_modified.insert((obj.name.as_str().to_string(), prop_key));
                    }
                    _ => {}
                }
            }
            Expression::CallExpression(call) => {
                self.collect_candidates_from_expression(&call.callee, candidates, member_assignment_count, member_modified);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_candidates_from_expression(e, candidates, member_assignment_count, member_modified);
                    }
                }
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_candidates_from_expression(&c.object, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&c.expression, candidates, member_assignment_count, member_modified);
            }
            Expression::StaticMemberExpression(s) => {
                self.collect_candidates_from_expression(&s.object, candidates, member_assignment_count, member_modified);
            }
            Expression::PrivateFieldExpression(p) => {
                self.collect_candidates_from_expression(&p.object, candidates, member_assignment_count, member_modified);
            }
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_candidates_from_expression(e, candidates, member_assignment_count, member_modified);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_candidates_from_expression(&bin.left, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&bin.right, candidates, member_assignment_count, member_modified);
            }
            Expression::LogicalExpression(log) => {
                self.collect_candidates_from_expression(&log.left, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&log.right, candidates, member_assignment_count, member_modified);
            }
            Expression::UnaryExpression(un) => {
                self.collect_candidates_from_expression(&un.argument, candidates, member_assignment_count, member_modified);
            }
            Expression::ConditionalExpression(c) => {
                self.collect_candidates_from_expression(&c.test, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&c.consequent, candidates, member_assignment_count, member_modified);
                self.collect_candidates_from_expression(&c.alternate, candidates, member_assignment_count, member_modified);
            }
            Expression::ParenthesizedExpression(p) => {
                self.collect_candidates_from_expression(&p.expression, candidates, member_assignment_count, member_modified);
            }
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_candidates_from_expression(expr, candidates, member_assignment_count, member_modified);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            self.collect_candidates_from_expression(&p.value, candidates, member_assignment_count, member_modified);
                        }
                        ObjectPropertyKind::SpreadProperty(s) => {
                            self.collect_candidates_from_expression(&s.argument, candidates, member_assignment_count, member_modified);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn load_map_for_statement_list(&mut self, stmts: &[Statement<'a>]) {
        let mut candidates = self.collect_candidates_from_statement_list(stmts);
        let modified_ids = self.collect_modified_idents_in_statement_list(stmts);

        // If the object binding itself is modified in this statement list, skip.
        candidates.retain(|(obj, _), _| !modified_ids.contains(obj));
        self.map = candidates;
    }

    fn try_replace_member_expression(&mut self, it: &Expression<'a>) -> Option<Expression<'a>> {
        let (obj, prop) = Self::key_from_member_expr(it)?;
        let replacement = self.map.get(&(obj, prop))?;
        Some(replacement.clone_in(self.allocator))
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

    fn visit_assignment_expression(&mut self, it: &mut AssignmentExpression<'a>) {
        // Don't rewrite the assignment target `obj.prop`.
        let prev = self.modified;

        match &mut it.left {
            AssignmentTarget::ComputedMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.expression);
            }
            AssignmentTarget::StaticMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
            }
            _ => {
                oxc_ast_visit::walk_mut::walk_assignment_target(self, &mut it.left);
            }
        }

        self.modified = prev;
        oxc_ast_visit::walk_mut::walk_expression(self, &mut it.right);
    }

    fn visit_update_expression(&mut self, it: &mut UpdateExpression<'a>) {
        // Don't rewrite update targets `obj.prop++`.
        match &mut it.argument {
            SimpleAssignmentTarget::ComputedMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.expression);
            }
            SimpleAssignmentTarget::StaticMemberExpression(mem) => {
                oxc_ast_visit::walk_mut::walk_expression(self, &mut mem.object);
            }
            _ => {
                oxc_ast_visit::walk_mut::walk_simple_assignment_target(self, &mut it.argument);
            }
        }
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if matches!(it, Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_)) {
            if let Some(repl) = self.try_replace_member_expression(it) {
                *it = repl;
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
