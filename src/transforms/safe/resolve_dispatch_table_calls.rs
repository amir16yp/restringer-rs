use std::collections::HashMap;

use oxc_allocator::Box as ArenaBox;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_span::GetSpan;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct ResolveDispatchTableCalls;

impl Transform for ResolveDispatchTableCalls {
    fn name(&self) -> &'static str {
        "resolveDispatchTableCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor { allocator: ctx.allocator, tables: HashMap::new(), modified: false };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Key {
    Str(String),
    Num(i64),
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    // table identifier name -> (key -> target function identifier name)
    tables: HashMap<String, HashMap<Key, String>>,
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

    fn make_ident_expr(&self, span: oxc_span::Span, name: &str) -> Expression<'a> {
        let name = self.allocator.alloc_str(name);
        Expression::Identifier(ArenaBox::new_in(
            IdentifierReference { span, name: name.into(), reference_id: None.into() },
            self.allocator,
        ))
    }

    fn key_from_member_property(&self, mem: &Expression<'a>) -> Option<(String, Key)> {
        match mem {
            Expression::StaticMemberExpression(s) => {
                let Expression::Identifier(obj) = self.unwrap_parens(&s.object) else { return None };
                Some((obj.name.as_str().to_string(), Key::Str(s.property.name.as_str().to_string())))
            }
            Expression::ComputedMemberExpression(c) => {
                let Expression::Identifier(obj) = self.unwrap_parens(&c.object) else { return None };
                let key = match self.unwrap_parens(&c.expression) {
                    Expression::StringLiteral(s) => Key::Str(s.value.as_str().to_string()),
                    Expression::NumericLiteral(n) => {
                        let v = n.value;
                        if !v.is_finite() || v.fract() != 0.0 {
                            return None;
                        }
                        if v < (i64::MIN as f64) || v > (i64::MAX as f64) {
                            return None;
                        }
                        Key::Num(v as i64)
                    }
                    _ => return None,
                };
                Some((obj.name.as_str().to_string(), key))
            }
            _ => None,
        }
    }

    fn try_collect_table_from_declarator(&mut self, decl: &VariableDeclarator<'a>) {
        let BindingPattern::BindingIdentifier(binding) = &decl.id else { return };
        let Some(init) = decl.init.as_ref() else { return };

        let table_name = binding.name.as_str().to_string();

        match self.unwrap_parens(init) {
            Expression::ObjectExpression(obj) => {
                let mut map: HashMap<Key, String> = HashMap::new();
                for prop in &obj.properties {
                    let ObjectPropertyKind::ObjectProperty(p) = prop else {
                        return;
                    };
                    let key = match &p.key {
                        PropertyKey::StaticIdentifier(ident) => Key::Str(ident.name.as_str().to_string()),
                        PropertyKey::StringLiteral(s) => Key::Str(s.value.as_str().to_string()),
                        PropertyKey::NumericLiteral(n) => {
                            let v = n.value;
                            if !v.is_finite() || v.fract() != 0.0 {
                                return;
                            }
                            if v < (i64::MIN as f64) || v > (i64::MAX as f64) {
                                return;
                            }
                            Key::Num(v as i64)
                        }
                        _ => return,
                    };

                    let Expression::Identifier(v) = self.unwrap_parens(&p.value) else {
                        return;
                    };
                    map.insert(key, v.name.as_str().to_string());
                }

                if !map.is_empty() {
                    self.tables.insert(table_name, map);
                }
            }
            Expression::ArrayExpression(arr) => {
                let mut map: HashMap<Key, String> = HashMap::new();
                for (i, el) in arr.elements.iter().enumerate() {
                    let Some(expr) = el.as_expression() else { return };
                    let Expression::Identifier(id) = self.unwrap_parens(expr) else { return };
                    map.insert(Key::Num(i as i64), id.name.as_str().to_string());
                }
                if !map.is_empty() {
                    self.tables.insert(table_name, map);
                }
            }
            _ => {}
        }
    }

    fn collect_tables_from_statement_list(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            let Statement::VariableDeclaration(var_decl) = stmt else { continue };
            if var_decl.kind != VariableDeclarationKind::Const {
                continue;
            }
            for decl in &var_decl.declarations {
                self.try_collect_table_from_declarator(decl);
            }
        }
    }

    fn resolve_dispatch_call(&self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        if call.optional {
            return None;
        }

        let (table_name, key) = self.key_from_member_property(&call.callee)?;
        let table = self.tables.get(&table_name)?;
        let target = table.get(&key)?;

        Some(self.make_ident_expr(call.callee.span(), target))
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_tables_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        let prev = std::mem::take(&mut self.tables);
        self.collect_tables_from_statement_list(&it.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, it);
        self.tables = prev;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev = std::mem::take(&mut self.tables);
        self.collect_tables_from_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, it);
        self.tables = prev;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if let Some(new_callee) = self.resolve_dispatch_call(call) {
                call.callee = new_callee;
                self.modified = true;
                // continue walking to allow downstream simplifications inside args
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let prev = std::mem::take(&mut self.tables);
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.tables = prev;
    }
}
