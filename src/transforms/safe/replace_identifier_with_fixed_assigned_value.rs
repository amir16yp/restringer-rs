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
        let mut v = Visitor {
            allocator: ctx.allocator,
            map: HashMap::new(),
            modified: false,
            in_object_shorthand: false,
            in_computed_member_property: false,
            in_call_callee: false,
        };
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
    // Conservative: do not inline identifiers used as computed member keys: `obj[key]`.
    // This matches the JS transform's behavior which only resolves computed access when
    // the property is a direct literal.
    in_computed_member_property: bool,

    // Do not inline identifiers when they are used as the callee: `fnVar()`.
    in_call_callee: bool,
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

    fn collect_candidates_not_assigned_at_declaration(
        &self,
        stmts: &[Statement<'a>],
    ) -> HashMap<String, Expression<'a>> {
        // Port of JS module `replaceIdentifierWithFixedValueNotAssignedAtDeclaration`.
        // Pattern: `let a; a = 3; ... use a ...` where the assignment is the only write
        // and RHS is a literal.

        let mut declared_uninit: HashSet<String> = HashSet::new();
        let mut assignment_count: HashMap<String, usize> = HashMap::new();
        let mut assignment_literal: HashMap<String, Expression<'a>> = HashMap::new();

        let mut disqualified: HashSet<String> = HashSet::new();
        let mut used_as_for_iterator: HashSet<String> = HashSet::new();
        let mut used_in_conditional: HashSet<String> = HashSet::new();

        // 1) Collect `let a;` style declarations.
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue; };
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue; };
                if decl.init.is_none() {
                    declared_uninit.insert(binding.name.as_str().to_string());
                }
            }
        }

        // 2) Scan statement list for writes/unsafe contexts.
        for stmt in stmts {
            self.collect_used_in_conditional_from_statement(stmt, &mut used_in_conditional);

            match stmt {
                Statement::ForInStatement(for_in) => {
                    if let Some(target) = for_in.left.as_assignment_target() {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                            used_as_for_iterator.insert(id.name.as_str().to_string());
                        }
                    }
                }
                Statement::ForOfStatement(for_of) => {
                    if let Some(target) = for_of.left.as_assignment_target() {
                        if let AssignmentTarget::AssignmentTargetIdentifier(id) = target {
                            used_as_for_iterator.insert(id.name.as_str().to_string());
                        }
                    }
                }
                _ => {}
            }

            self.collect_writes_and_single_literal_assignment(stmt, &mut assignment_count, &mut assignment_literal, &mut disqualified);
        }

        let mut out = HashMap::new();
        for name in declared_uninit {
            if disqualified.contains(&name)
                || used_as_for_iterator.contains(&name)
                || used_in_conditional.contains(&name)
            {
                continue;
            }

            let Some(count) = assignment_count.get(&name) else { continue; };
            if *count != 1 {
                continue;
            }
            let Some(lit) = assignment_literal.get(&name) else { continue; };
            out.insert(name, lit.clone_in(self.allocator));
        }

        out
    }

    fn collect_used_in_conditional_from_statement(&self, stmt: &Statement<'a>, out: &mut HashSet<String>) {
        match stmt {
            Statement::ExpressionStatement(es) => self.collect_used_in_conditional_from_expression(&es.expression, out),
            Statement::ReturnStatement(rs) => {
                if let Some(arg) = rs.argument.as_ref() {
                    self.collect_used_in_conditional_from_expression(arg, out);
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    if let Some(init) = decl.init.as_ref() {
                        self.collect_used_in_conditional_from_expression(init, out);
                    }
                }
            }
            Statement::IfStatement(ifstmt) => {
                self.collect_used_in_conditional_from_expression(&ifstmt.test, out);
                self.collect_used_in_conditional_from_statement(&ifstmt.consequent, out);
                if let Some(alt) = ifstmt.alternate.as_ref() {
                    self.collect_used_in_conditional_from_statement(alt, out);
                }
            }
            Statement::BlockStatement(block) => {
                for s in &block.body {
                    self.collect_used_in_conditional_from_statement(s, out);
                }
            }
            Statement::ForStatement(fs) => {
                if let Some(init) = fs.init.as_ref().and_then(|i| i.as_expression()) {
                    self.collect_used_in_conditional_from_expression(init, out);
                }
                if let Some(test) = fs.test.as_ref() {
                    self.collect_used_in_conditional_from_expression(test, out);
                }
                if let Some(update) = fs.update.as_ref() {
                    self.collect_used_in_conditional_from_expression(update, out);
                }
                self.collect_used_in_conditional_from_statement(&fs.body, out);
            }
            Statement::WhileStatement(ws) => {
                self.collect_used_in_conditional_from_expression(&ws.test, out);
                self.collect_used_in_conditional_from_statement(&ws.body, out);
            }
            Statement::DoWhileStatement(ws) => {
                self.collect_used_in_conditional_from_statement(&ws.body, out);
                self.collect_used_in_conditional_from_expression(&ws.test, out);
            }
            Statement::ForInStatement(for_in) => {
                self.collect_used_in_conditional_from_expression(&for_in.right, out);
                self.collect_used_in_conditional_from_statement(&for_in.body, out);
            }
            Statement::ForOfStatement(for_of) => {
                self.collect_used_in_conditional_from_expression(&for_of.right, out);
                self.collect_used_in_conditional_from_statement(&for_of.body, out);
            }
            _ => {}
        }
    }

    fn collect_used_in_conditional_from_expression(&self, expr: &Expression<'a>, out: &mut HashSet<String>) {
        match expr {
            Expression::ConditionalExpression(c) => {
                self.collect_all_identifier_names_in_expression(&c.test, out);
                self.collect_all_identifier_names_in_expression(&c.consequent, out);
                self.collect_all_identifier_names_in_expression(&c.alternate, out);

                // Also traverse nested conditionals.
                self.collect_used_in_conditional_from_expression(&c.test, out);
                self.collect_used_in_conditional_from_expression(&c.consequent, out);
                self.collect_used_in_conditional_from_expression(&c.alternate, out);
            }
            Expression::ParenthesizedExpression(p) => self.collect_used_in_conditional_from_expression(&p.expression, out),
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_used_in_conditional_from_expression(e, out);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_used_in_conditional_from_expression(&bin.left, out);
                self.collect_used_in_conditional_from_expression(&bin.right, out);
            }
            Expression::LogicalExpression(log) => {
                self.collect_used_in_conditional_from_expression(&log.left, out);
                self.collect_used_in_conditional_from_expression(&log.right, out);
            }
            Expression::UnaryExpression(un) => self.collect_used_in_conditional_from_expression(&un.argument, out),
            Expression::CallExpression(call) => {
                self.collect_used_in_conditional_from_expression(&call.callee, out);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_used_in_conditional_from_expression(e, out);
                    }
                }
            }
            Expression::AssignmentExpression(a) => {
                self.collect_used_in_conditional_from_expression(&a.right, out);
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_used_in_conditional_from_expression(&c.object, out);
                self.collect_used_in_conditional_from_expression(&c.expression, out);
            }
            Expression::StaticMemberExpression(s) => self.collect_used_in_conditional_from_expression(&s.object, out),
            Expression::PrivateFieldExpression(p) => self.collect_used_in_conditional_from_expression(&p.object, out),
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_used_in_conditional_from_expression(expr, out);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => self.collect_used_in_conditional_from_expression(&p.value, out),
                        ObjectPropertyKind::SpreadProperty(s) => self.collect_used_in_conditional_from_expression(&s.argument, out),
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_all_identifier_names_in_expression(&self, expr: &Expression<'a>, out: &mut HashSet<String>) {
        match expr {
            Expression::Identifier(idref) => {
                out.insert(idref.name.as_str().to_string());
            }
            Expression::ParenthesizedExpression(p) => self.collect_all_identifier_names_in_expression(&p.expression, out),
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_all_identifier_names_in_expression(e, out);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_all_identifier_names_in_expression(&bin.left, out);
                self.collect_all_identifier_names_in_expression(&bin.right, out);
            }
            Expression::LogicalExpression(log) => {
                self.collect_all_identifier_names_in_expression(&log.left, out);
                self.collect_all_identifier_names_in_expression(&log.right, out);
            }
            Expression::UnaryExpression(un) => self.collect_all_identifier_names_in_expression(&un.argument, out),
            Expression::ConditionalExpression(c) => {
                self.collect_all_identifier_names_in_expression(&c.test, out);
                self.collect_all_identifier_names_in_expression(&c.consequent, out);
                self.collect_all_identifier_names_in_expression(&c.alternate, out);
            }
            Expression::CallExpression(call) => {
                self.collect_all_identifier_names_in_expression(&call.callee, out);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_all_identifier_names_in_expression(e, out);
                    }
                }
            }
            Expression::AssignmentExpression(a) => {
                self.collect_all_identifier_names_in_expression(&a.right, out);
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_all_identifier_names_in_expression(&c.object, out);
                self.collect_all_identifier_names_in_expression(&c.expression, out);
            }
            Expression::StaticMemberExpression(s) => self.collect_all_identifier_names_in_expression(&s.object, out),
            Expression::PrivateFieldExpression(p) => self.collect_all_identifier_names_in_expression(&p.object, out),
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_all_identifier_names_in_expression(expr, out);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => self.collect_all_identifier_names_in_expression(&p.value, out),
                        ObjectPropertyKind::SpreadProperty(s) => self.collect_all_identifier_names_in_expression(&s.argument, out),
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_writes_and_single_literal_assignment(
        &self,
        stmt: &Statement<'a>,
        assignment_count: &mut HashMap<String, usize>,
        assignment_literal: &mut HashMap<String, Expression<'a>>,
        disqualified: &mut HashSet<String>,
    ) {
        match stmt {
            Statement::ExpressionStatement(es) => {
                self.collect_writes_and_single_literal_assignment_expr(&es.expression, assignment_count, assignment_literal, disqualified)
            }
            Statement::ReturnStatement(rs) => {
                if let Some(arg) = rs.argument.as_ref() {
                    self.collect_writes_and_single_literal_assignment_expr(arg, assignment_count, assignment_literal, disqualified);
                }
            }
            Statement::IfStatement(ifstmt) => {
                self.collect_writes_and_single_literal_assignment_expr(&ifstmt.test, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment(&ifstmt.consequent, assignment_count, assignment_literal, disqualified);
                if let Some(alt) = ifstmt.alternate.as_ref() {
                    self.collect_writes_and_single_literal_assignment(alt, assignment_count, assignment_literal, disqualified);
                }
            }
            Statement::BlockStatement(block) => {
                for s in &block.body {
                    self.collect_writes_and_single_literal_assignment(s, assignment_count, assignment_literal, disqualified);
                }
            }
            Statement::ForStatement(fs) => {
                if let Some(init) = fs.init.as_ref().and_then(|i| i.as_expression()) {
                    self.collect_writes_and_single_literal_assignment_expr(init, assignment_count, assignment_literal, disqualified);
                }
                if let Some(test) = fs.test.as_ref() {
                    self.collect_writes_and_single_literal_assignment_expr(test, assignment_count, assignment_literal, disqualified);
                }
                if let Some(update) = fs.update.as_ref() {
                    self.collect_writes_and_single_literal_assignment_expr(update, assignment_count, assignment_literal, disqualified);
                }
                self.collect_writes_and_single_literal_assignment(&fs.body, assignment_count, assignment_literal, disqualified);
            }
            Statement::ForInStatement(for_in) => {
                self.collect_writes_and_single_literal_assignment_expr(&for_in.right, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment(&for_in.body, assignment_count, assignment_literal, disqualified);
            }
            Statement::ForOfStatement(for_of) => {
                self.collect_writes_and_single_literal_assignment_expr(&for_of.right, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment(&for_of.body, assignment_count, assignment_literal, disqualified);
            }
            Statement::WhileStatement(ws) => {
                self.collect_writes_and_single_literal_assignment_expr(&ws.test, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment(&ws.body, assignment_count, assignment_literal, disqualified);
            }
            Statement::DoWhileStatement(ws) => {
                self.collect_writes_and_single_literal_assignment(&ws.body, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&ws.test, assignment_count, assignment_literal, disqualified);
            }
            Statement::VariableDeclaration(var_decl) => {
                for decl in &var_decl.declarations {
                    if let Some(init) = decl.init.as_ref() {
                        self.collect_writes_and_single_literal_assignment_expr(init, assignment_count, assignment_literal, disqualified);
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_writes_and_single_literal_assignment_expr(
        &self,
        expr: &Expression<'a>,
        assignment_count: &mut HashMap<String, usize>,
        assignment_literal: &mut HashMap<String, Expression<'a>>,
        disqualified: &mut HashSet<String>,
    ) {
        match expr {
            Expression::AssignmentExpression(a) => {
                if let AssignmentTarget::AssignmentTargetIdentifier(id) = &a.left {
                    let name = id.name.as_str().to_string();
                    *assignment_count.entry(name.clone()).or_insert(0) += 1;

                    if Self::is_literal_expr(&a.right) {
                        // Keep the RHS literal from the (first) assignment.
                        assignment_literal.entry(name).or_insert_with(|| a.right.clone_in(self.allocator));
                    } else {
                        disqualified.insert(name);
                    }
                }

                self.collect_writes_and_single_literal_assignment_expr(&a.right, assignment_count, assignment_literal, disqualified);
            }
            Expression::UpdateExpression(u) => {
                if let SimpleAssignmentTarget::AssignmentTargetIdentifier(id) = &u.argument {
                    disqualified.insert(id.name.as_str().to_string());
                }
            }
            Expression::ConditionalExpression(c) => {
                self.collect_writes_and_single_literal_assignment_expr(&c.test, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&c.consequent, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&c.alternate, assignment_count, assignment_literal, disqualified);
            }
            Expression::CallExpression(call) => {
                self.collect_writes_and_single_literal_assignment_expr(&call.callee, assignment_count, assignment_literal, disqualified);
                for a in &call.arguments {
                    if let Some(e) = a.as_expression() {
                        self.collect_writes_and_single_literal_assignment_expr(e, assignment_count, assignment_literal, disqualified);
                    }
                }
            }
            Expression::SequenceExpression(seq) => {
                for e in &seq.expressions {
                    self.collect_writes_and_single_literal_assignment_expr(e, assignment_count, assignment_literal, disqualified);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.collect_writes_and_single_literal_assignment_expr(&bin.left, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&bin.right, assignment_count, assignment_literal, disqualified);
            }
            Expression::LogicalExpression(log) => {
                self.collect_writes_and_single_literal_assignment_expr(&log.left, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&log.right, assignment_count, assignment_literal, disqualified);
            }
            Expression::UnaryExpression(un) => {
                self.collect_writes_and_single_literal_assignment_expr(&un.argument, assignment_count, assignment_literal, disqualified);
            }
            Expression::ParenthesizedExpression(p) => {
                self.collect_writes_and_single_literal_assignment_expr(&p.expression, assignment_count, assignment_literal, disqualified)
            }
            Expression::ComputedMemberExpression(c) => {
                self.collect_writes_and_single_literal_assignment_expr(&c.object, assignment_count, assignment_literal, disqualified);
                self.collect_writes_and_single_literal_assignment_expr(&c.expression, assignment_count, assignment_literal, disqualified);
            }
            Expression::StaticMemberExpression(s) => {
                self.collect_writes_and_single_literal_assignment_expr(&s.object, assignment_count, assignment_literal, disqualified);
            }
            Expression::PrivateFieldExpression(p) => {
                self.collect_writes_and_single_literal_assignment_expr(&p.object, assignment_count, assignment_literal, disqualified);
            }
            Expression::ArrayExpression(arr) => {
                for el in &arr.elements {
                    if let Some(expr) = el.as_expression() {
                        self.collect_writes_and_single_literal_assignment_expr(expr, assignment_count, assignment_literal, disqualified);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::ObjectProperty(p) => {
                            self.collect_writes_and_single_literal_assignment_expr(&p.value, assignment_count, assignment_literal, disqualified);
                        }
                        ObjectPropertyKind::SpreadProperty(s) => {
                            self.collect_writes_and_single_literal_assignment_expr(&s.argument, assignment_count, assignment_literal, disqualified);
                        }
                    }
                }
            }
            _ => {}
        }
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
        // Case 1: `const a = 3; ...` — only safe if the name is never modified (assigned/updated/etc).
        let mut candidates_assigned_at_decl = self.collect_candidates_from_statement_list(stmts);
        let modified = self.collect_modified_names_in_statement_list(stmts);
        candidates_assigned_at_decl.retain(|k, _| !modified.contains(k));

        // Case 2: `let a; a = 3; ...` — allow the single assignment itself.
        // Safety is handled inside `collect_candidates_not_assigned_at_declaration`.
        let candidates_not_assigned_at_decl = self.collect_candidates_not_assigned_at_declaration(stmts);

        let mut combined = candidates_assigned_at_decl;
        combined.extend(candidates_not_assigned_at_decl);
        self.map = combined;
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
        if !self.in_object_shorthand && !self.in_computed_member_property && !self.in_call_callee {
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

    fn visit_call_expression(&mut self, it: &mut CallExpression<'a>) {
        // Do not inline `fnVar` when it is used as the callee.
        let prev = self.in_call_callee;
        self.in_call_callee = true;
        self.visit_expression(&mut it.callee);
        self.in_call_callee = prev;

        for arg in &mut it.arguments {
            if let Some(expr) = arg.as_expression_mut() {
                self.visit_expression(expr);
            }
        }
    }

    fn visit_computed_member_expression(&mut self, it: &mut ComputedMemberExpression<'a>) {
        // Walk object normally; but don't replace identifiers used as computed keys.
        self.visit_expression(&mut it.object);
        let prev = self.in_computed_member_property;
        self.in_computed_member_property = true;
        self.visit_expression(&mut it.expression);
        self.in_computed_member_property = prev;
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
