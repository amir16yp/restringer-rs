use std::collections::HashMap;

use oxc_allocator::{Box as ArenaBox, CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

const MAX_ROTATION_STEPS: usize = 50_000;

pub struct ResolveStringArrayDecoderCalls;

impl Transform for ResolveStringArrayDecoderCalls {
    fn name(&self) -> &'static str {
        "resolveStringArrayDecoderCalls"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = Visitor {
            allocator: ctx.allocator,
            array_factories: HashMap::new(),
            decoder_functions: HashMap::new(),
            rotated_arrays: HashMap::new(),
            modified: false,
        };
        v.visit_program(program);
        v.modified
    }
}

#[derive(Clone)]
struct DecoderInfo {
    offset: i64,
    factory_name: String,
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,

    // function name -> array literal elements (cloned into arena)
    array_factories: HashMap<String, ArenaVec<'a, ArrayExpressionElement<'a>>>,

    // decoder function name -> { offset, factory function name }
    decoder_functions: HashMap<String, DecoderInfo>,

    // factory function name -> resolved (possibly rotated) elements
    rotated_arrays: HashMap<String, ArenaVec<'a, ArrayExpressionElement<'a>>>,

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

    fn collect_factories_and_decoders_from_statement_list(&mut self, stmts: &[Statement<'a>]) {
        for stmt in stmts {
            match stmt {
                Statement::FunctionDeclaration(func) => {
                    self.try_collect_array_factory(func);
                    self.try_collect_decoder_function(func);
                }
                Statement::VariableDeclaration(var) => {
                    self.try_collect_array_factory_from_var(var);
                }
                _ => {}
            }
        }
    }

    fn try_collect_array_factory(&mut self, func: &oxc_allocator::Box<'a, Function<'a>>) {
        let f = &**func;
        let Some(id) = f.id.as_ref() else { return };
        let Some(body) = f.body.as_ref() else { return };
        if body.statements.len() != 1 {
            return;
        }
        let Statement::ReturnStatement(ret) = &body.statements[0] else { return };
        let Some(arg) = ret.argument.as_ref() else { return };
        let Expression::ArrayExpression(arr) = self.unwrap_parens(arg) else { return };

        self.array_factories
            .insert(id.name.as_str().to_string(), arr.elements.clone_in(self.allocator));
    }

    fn try_collect_array_factory_from_var(&mut self, var: &oxc_allocator::Box<'a, VariableDeclaration<'a>>) {
        // Support patterns like: `const _0x2047 = () => [ ... ];`
        for decl in &var.declarations {
            let BindingPattern::BindingIdentifier(binding) = &decl.id else { continue };
            let Some(init) = decl.init.as_ref() else { continue };
            match self.unwrap_parens(init) {
                Expression::ArrowFunctionExpression(arrow) => {
                    if arrow.params.items.len() != 0 {
                        continue;
                    }
                    if arrow.body.statements.len() != 1 {
                        continue;
                    }
                    let Statement::ReturnStatement(ret) = &arrow.body.statements[0] else { continue };
                    let Some(arg) = ret.argument.as_ref() else { continue };
                    let Expression::ArrayExpression(arr) = self.unwrap_parens(arg) else { continue };
                    self.array_factories
                        .insert(binding.name.as_str().to_string(), arr.elements.clone_in(self.allocator));
                }
                _ => {}
            }
        }
    }

    fn parse_int_literal(expr: &Expression<'a>) -> Option<i64> {
        match expr {
            Expression::NumericLiteral(lit) => {
                if !lit.value.is_finite() || lit.value.fract() != 0.0 {
                    return None;
                }
                Some(lit.value as i64)
            }
            Expression::UnaryExpression(un) => {
                if un.operator != UnaryOperator::UnaryNegation {
                    return None;
                }
                let v = Self::parse_int_literal(&un.argument)?;
                Some(-v)
            }
            Expression::ParenthesizedExpression(p) => Self::parse_int_literal(&p.expression),
            _ => None,
        }
    }

    fn try_collect_decoder_function(&mut self, func: &oxc_allocator::Box<'a, Function<'a>>) {
        // Match:
        // function dec(a,b){ a=a-<offset>; const arr=factory(); return arr[a]; }
        // or with `let arr = factory();`.
        let f = &**func;
        let Some(id) = f.id.as_ref() else { return };
        if f.params.items.is_empty() {
            return;
        }
        let BindingPattern::BindingIdentifier(first_param) = &f.params.items[0].pattern else { return };
        let idx_name = first_param.name.as_str();

        let Some(body) = f.body.as_ref() else { return };
        if body.statements.len() < 2 {
            return;
        }

        // 1) Extract `idx = idx - OFFSET`.
        let Statement::ExpressionStatement(expr_stmt) = &body.statements[0] else { return };
        let Expression::AssignmentExpression(assign) = self.unwrap_parens(&expr_stmt.expression) else { return };
        if assign.operator != AssignmentOperator::Assign {
            return;
        }
        let AssignmentTarget::AssignmentTargetIdentifier(lhs_id) = &assign.left else { return };
        if lhs_id.name.as_str() != idx_name {
            return;
        }
        let Expression::BinaryExpression(bin) = self.unwrap_parens(&assign.right) else { return };
        if bin.operator != BinaryOperator::Subtraction {
            return;
        }
        let Expression::Identifier(left_id) = self.unwrap_parens(&bin.left) else { return };
        if left_id.name.as_str() != idx_name {
            return;
        }
        let Some(offset) = Self::parse_int_literal(self.unwrap_parens(&bin.right)) else { return };

        // 2) Find `const tmp = factory();`
        let mut factory_name: Option<String> = None;
        for stmt in &body.statements[1..] {
            if let Statement::VariableDeclaration(var_decl) = stmt {
                for decl in &var_decl.declarations {
                    let Some(init) = decl.init.as_ref() else { continue };
                    let Expression::CallExpression(call) = self.unwrap_parens(init) else { continue };
                    if !call.arguments.is_empty() {
                        continue;
                    }
                    let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else { continue };
                    let name = callee_id.name.as_str().to_string();
                    if self.array_factories.contains_key(&name) {
                        factory_name = Some(name);
                        break;
                    }
                }
            }
            if factory_name.is_some() {
                break;
            }
        }
        let Some(factory_name) = factory_name else { return };

        self.decoder_functions.insert(id.name.as_str().to_string(), DecoderInfo { offset, factory_name });
    }

    fn make_string_literal_expr(&self, span: oxc_span::Span, s: &str) -> Expression<'a> {
        let s = self.allocator.alloc_str(s);
        Expression::StringLiteral(ArenaBox::new_in(
            StringLiteral { span, value: s.into(), raw: None, lone_surrogates: false },
            self.allocator,
        ))
    }

    fn materialize_factory_array(&mut self, factory: &str) -> Option<ArenaVec<'a, ArrayExpressionElement<'a>>> {
        if let Some(v) = self.rotated_arrays.get(factory) {
            return Some(v.clone_in(self.allocator));
        }
        let base = self.array_factories.get(factory)?;
        Some(base.clone_in(self.allocator))
    }

    fn rotate_left_once(elements: &mut ArenaVec<'a, ArrayExpressionElement<'a>>) {
        if elements.len() <= 1 {
            return;
        }
        let first = elements.remove(0);
        elements.push(first);
    }

    fn eval_expr_numeric(
        &self,
        expr: &Expression<'a>,
        decoder_name: &str,
        offset: i64,
        elements: &ArenaVec<'a, ArrayExpressionElement<'a>>,
    ) -> Option<f64> {
        match self.unwrap_parens(expr) {
            Expression::NumericLiteral(lit) => Some(lit.value),
            Expression::UnaryExpression(un) => {
                let v = self.eval_expr_numeric(&un.argument, decoder_name, offset, elements)?;
                match un.operator {
                    UnaryOperator::UnaryNegation => Some(-v),
                    UnaryOperator::UnaryPlus => Some(v),
                    _ => None,
                }
            }
            Expression::BinaryExpression(bin) => {
                let l = self.eval_expr_numeric(&bin.left, decoder_name, offset, elements)?;
                let r = self.eval_expr_numeric(&bin.right, decoder_name, offset, elements)?;
                match bin.operator {
                    BinaryOperator::Addition => Some(l + r),
                    BinaryOperator::Subtraction => Some(l - r),
                    BinaryOperator::Multiplication => Some(l * r),
                    BinaryOperator::Division => Some(l / r),
                    _ => None,
                }
            }
            Expression::CallExpression(call) => {
                // parseInt(dec(<num>))
                let Expression::Identifier(callee) = self.unwrap_parens(&call.callee) else { return None };
                if callee.name.as_str() != "parseInt" {
                    return None;
                }
                let arg0 = call.arguments.first()?.as_expression()?;
                let s = self.eval_decoder_call_to_string(arg0, decoder_name, offset, elements)?;
                s.parse::<f64>().ok()
            }
            _ => None,
        }
    }

    fn eval_decoder_call_to_string(
        &self,
        expr: &Expression<'a>,
        decoder_name: &str,
        offset: i64,
        elements: &ArenaVec<'a, ArrayExpressionElement<'a>>,
    ) -> Option<String> {
        let Expression::CallExpression(call) = self.unwrap_parens(expr) else { return None };
        let Expression::Identifier(callee) = self.unwrap_parens(&call.callee) else { return None };
        if callee.name.as_str() != decoder_name {
            return None;
        }
        let arg0 = call.arguments.first()?.as_expression()?;
        let idx_raw = Self::parse_int_literal(self.unwrap_parens(arg0))?;
        let idx = idx_raw - offset;
        if idx < 0 {
            return None;
        }
        let idx = idx as usize;
        let el = elements.get(idx)?;
        let expr = el.as_expression()?;
        match self.unwrap_parens(expr) {
            Expression::StringLiteral(s) => Some(s.value.as_str().to_string()),
            Expression::NumericLiteral(n) => Some(n.value.to_string()),
            _ => None,
        }
    }

    fn try_resolve_rotator_iife_in_statement_list(&mut self, stmts: &[Statement<'a>]) {
        // Very specific matcher for javascript-obfuscator style:
        // (function(factory, target){ const dec = <decoder>; const arr = factory(); while(!![]){ try { const x = <expr>; if (x===target) break; arr.push(arr.shift()); } catch(e){ arr.push(arr.shift()); } } })(_0x2047, 123);

        for stmt in stmts {
            let Statement::ExpressionStatement(es) = stmt else { continue };
            let Expression::CallExpression(call) = self.unwrap_parens(&es.expression) else { continue };

            if call.arguments.len() < 2 {
                continue;
            }
            let Some(factory_arg) = call.arguments[0].as_expression() else { continue };
            let Some(target_arg) = call.arguments[1].as_expression() else { continue };
            let Expression::Identifier(factory_id) = self.unwrap_parens(factory_arg) else { continue };
            let Some(target) = Self::parse_int_literal(self.unwrap_parens(target_arg)) else { continue };

            let factory_name = factory_id.name.as_str().to_string();
            if !self.array_factories.contains_key(&factory_name) {
                continue;
            }

            // callee must be function/arrow
            let callee = self.unwrap_parens(&call.callee);
            let (params, body_stmts) = match callee {
                Expression::FunctionExpression(func) => {
                    let Some(body) = func.body.as_ref() else { continue };
                    (&func.params.items, body.statements.as_slice())
                }
                Expression::ArrowFunctionExpression(arrow) => (&arrow.params.items, arrow.body.statements.as_slice()),
                _ => continue,
            };
            if params.len() < 2 {
                continue;
            }

            // find `const dec = <decoder>` and `const arr = factory()`
            let mut decoder_name: Option<String> = None;
            let mut arr_var: Option<String> = None;
            let mut checksum_expr: Option<Expression<'a>> = None;

            for s in body_stmts {
                if let Statement::VariableDeclaration(var) = s {
                    for d in &var.declarations {
                        let BindingPattern::BindingIdentifier(binding) = &d.id else { continue };
                        let Some(init) = d.init.as_ref() else { continue };

                        // const dec = <identifier>
                        if let Expression::Identifier(id) = self.unwrap_parens(init) {
                            if self.decoder_functions.contains_key(id.name.as_str()) {
                                decoder_name = Some(id.name.as_str().to_string());
                            }
                        }

                        // const arr = factory()
                        if let Expression::CallExpression(c) = self.unwrap_parens(init) {
                            if !c.arguments.is_empty() {
                                continue;
                            }
                            if let Expression::Identifier(id) = self.unwrap_parens(&c.callee) {
                                if id.name.as_str() == factory_name {
                                    arr_var = Some(binding.name.as_str().to_string());
                                }
                            }
                        }
                    }
                }

                // Find checksum `const x = ...;` inside try block later; handled below.
            }

            let Some(decoder_name) = decoder_name else { continue };
            let Some(arr_var) = arr_var else { continue };
            let Some(dec_info) = self.decoder_functions.get(&decoder_name).cloned() else { continue };
            if dec_info.factory_name != factory_name {
                continue;
            }

            // find while loop and extract checksum expr & rotation operation presence
            for s in body_stmts {
                let Statement::WhileStatement(ws) = s else { continue };
                let Statement::BlockStatement(block) = &ws.body else { continue };

                // inside while: try { const tmp = <expr>; if(tmp===target) break; ... } catch(e){ ... }
                for inner in &block.body {
                    let Statement::TryStatement(ts) = inner else { continue };
                    let try_block = &ts.block;

                    // checksum expr: const <name> = <expr>;
                    for tstmt in &try_block.body {
                        if let Statement::VariableDeclaration(vd) = tstmt {
                            for decl in &vd.declarations {
                                let Some(init) = decl.init.as_ref() else { continue };
                                checksum_expr = Some(init.clone_in(self.allocator));
                                break;
                            }
                        }
                        if checksum_expr.is_some() {
                            break;
                        }
                    }

                    // require rotation expression somewhere: arr.push(arr.shift())
                    let mut has_rotation = false;
                    for tstmt in &try_block.body {
                        if self.contains_rotation_stmt(tstmt, &arr_var) {
                            has_rotation = true;
                            break;
                        }
                    }
                    if !has_rotation {
                        if let Some(handler) = ts.handler.as_ref() {
                            let catch_block = &handler.body;
                            for cstmt in &catch_block.body {
                                if self.contains_rotation_stmt(cstmt, &arr_var) {
                                    has_rotation = true;
                                    break;
                                }
                            }
                        }
                    }

                    if checksum_expr.is_some() && has_rotation {
                        break;
                    }
                }

                if checksum_expr.is_some() {
                    break;
                }
            }

            let Some(checksum_expr) = checksum_expr else { continue };

            // simulate rotation until checksum matches
            let mut elements = match self.materialize_factory_array(&factory_name) {
                Some(v) => v,
                None => continue,
            };
            let max_steps = elements.len().saturating_mul(3).min(MAX_ROTATION_STEPS).max(elements.len());

            let mut ok = false;
            for _ in 0..max_steps {
                let v = self.eval_expr_numeric(&checksum_expr, &decoder_name, dec_info.offset, &elements);
                if let Some(num) = v {
                    if num.is_finite() && (num as i64) == target {
                        ok = true;
                        break;
                    }
                }
                Self::rotate_left_once(&mut elements);
            }

            if ok {
                self.rotated_arrays.insert(factory_name.clone(), elements);
            }
        }
    }

    fn contains_rotation_stmt(&self, stmt: &Statement<'a>, arr_var: &str) -> bool {
        let Statement::ExpressionStatement(es) = stmt else { return false };
        let Expression::CallExpression(push_call) = self.unwrap_parens(&es.expression) else { return false };

        // arr.push(...)
        let Expression::StaticMemberExpression(push_mem) = self.unwrap_parens(&push_call.callee) else { return false };
        let Expression::Identifier(obj) = self.unwrap_parens(&push_mem.object) else { return false };
        if obj.name.as_str() != arr_var {
            return false;
        }
        if push_mem.property.name.as_str() != "push" {
            return false;
        }
        if push_call.arguments.len() != 1 {
            return false;
        }

        // argument must be arr.shift()
        let Some(arg0) = push_call.arguments[0].as_expression() else { return false };
        let Expression::CallExpression(shift_call) = self.unwrap_parens(arg0) else { return false };
        if !shift_call.arguments.is_empty() {
            return false;
        }
        let Expression::StaticMemberExpression(shift_mem) = self.unwrap_parens(&shift_call.callee) else { return false };
        let Expression::Identifier(shift_obj) = self.unwrap_parens(&shift_mem.object) else { return false };
        if shift_obj.name.as_str() != arr_var {
            return false;
        }
        shift_mem.property.name.as_str() == "shift"
    }

    fn resolve_decoder_call_expr(&mut self, call: &CallExpression<'a>) -> Option<Expression<'a>> {
        let Expression::Identifier(callee_id) = self.unwrap_parens(&call.callee) else { return None };
        let info = self.decoder_functions.get(callee_id.name.as_str())?;

        let arg0 = call.arguments.first()?.as_expression()?;
        let idx_raw = Self::parse_int_literal(self.unwrap_parens(arg0))?;
        let idx = idx_raw - info.offset;
        if idx < 0 {
            return None;
        }
        let idx = idx as usize;

        let elements = self.rotated_arrays.get(&info.factory_name).or_else(|| self.array_factories.get(&info.factory_name))?;
        let el = elements.get(idx)?;
        let expr = el.as_expression()?;

        // clone expression; for string literal we rewrite to literal with same span for nicer output.
        match self.unwrap_parens(expr) {
            Expression::StringLiteral(s) => Some(self.make_string_literal_expr(call.span, s.value.as_str())),
            _ => Some(expr.clone_in(self.allocator)),
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        self.collect_factories_and_decoders_from_statement_list(&it.body);
        self.try_resolve_rotator_iife_in_statement_list(&it.body);
        oxc_ast_visit::walk_mut::walk_program(self, it);
    }

    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        // New scope: only consider factories/decoders declared inside this body.
        let prev_factories = std::mem::take(&mut self.array_factories);
        let prev_decoders = std::mem::take(&mut self.decoder_functions);
        let prev_rotated = std::mem::take(&mut self.rotated_arrays);

        self.collect_factories_and_decoders_from_statement_list(&it.statements);
        self.try_resolve_rotator_iife_in_statement_list(&it.statements);

        oxc_ast_visit::walk_mut::walk_function_body(self, it);

        self.array_factories = prev_factories;
        self.decoder_functions = prev_decoders;
        self.rotated_arrays = prev_rotated;
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        let prev_factories = std::mem::take(&mut self.array_factories);
        let prev_decoders = std::mem::take(&mut self.decoder_functions);
        let prev_rotated = std::mem::take(&mut self.rotated_arrays);

        self.collect_factories_and_decoders_from_statement_list(&it.body);
        self.try_resolve_rotator_iife_in_statement_list(&it.body);

        oxc_ast_visit::walk_mut::walk_block_statement(self, it);

        self.array_factories = prev_factories;
        self.decoder_functions = prev_decoders;
        self.rotated_arrays = prev_rotated;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if let Some(repl) = self.resolve_decoder_call_expr(call) {
                *it = repl;
                self.modified = true;
                return;
            }
        }
        oxc_ast_visit::walk_mut::walk_expression(self, it);
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        // Do not let outer mappings rewrite inside the function header; reset at boundary.
        let prev_factories = std::mem::take(&mut self.array_factories);
        let prev_decoders = std::mem::take(&mut self.decoder_functions);
        let prev_rotated = std::mem::take(&mut self.rotated_arrays);

        oxc_ast_visit::walk_mut::walk_function(self, it, flags);

        self.array_factories = prev_factories;
        self.decoder_functions = prev_decoders;
        self.rotated_arrays = prev_rotated;
    }
}
