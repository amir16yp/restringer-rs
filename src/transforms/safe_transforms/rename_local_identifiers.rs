use std::collections::{HashMap, HashSet};

use oxc_allocator::Allocator;
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;
use oxc_str::Ident;
use oxc_syntax::scope::ScopeFlags;

use crate::{Transform, TransformCtx};

pub struct RenameLocalIdentifiers;

impl Transform for RenameLocalIdentifiers {
    fn name(&self) -> &'static str {
        "renameLocalIdentifiers"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut v = RenameVisitor {
            allocator: ctx.allocator,
            modified: false,
            stack: Vec::new(),
            used: HashSet::new(),
        };
        v.visit_program(program);
        v.modified
    }
}

struct RenameVisitor<'a> {
    allocator: &'a Allocator,
    modified: bool,
    stack: Vec<Scope>,
    used: HashSet<String>,
}

struct Scope {
    renames: HashMap<String, String>,
}

impl<'a> RenameVisitor<'a> {
    fn rename_ident(&mut self, ident: &mut Ident<'a>) {
        let name = ident.as_str();
        for scope in self.stack.iter().rev() {
            if let Some(new_name) = scope.renames.get(name) {
                if new_name != name {
                    let s = self.allocator.alloc_str(new_name);
                    *ident = Ident::from(s);
                    self.modified = true;
                }
                return;
            }
        }
    }

    fn add_param_rename(
        &mut self,
        pat: &BindingPattern<'_>,
        base: &str,
        renames: &mut HashMap<String, String>,
    ) {
        if let BindingPattern::BindingIdentifier(id) = pat {
            let old = id.name.as_str();
            if looks_obfuscated(old) && !renames.contains_key(old) {
                let new = make_unique(base, &mut self.used);
                self.used.insert(new.clone());
                renames.insert(old.to_string(), new);
            }
        }
    }
}

impl<'a> VisitMut<'a> for RenameVisitor<'a> {
    fn visit_program(&mut self, it: &mut Program<'a>) {
        let renames = collect_renames_for_scope(&it.body, &mut self.used, false);
        self.stack.push(Scope { renames });
        oxc_ast_visit::walk_mut::walk_program(self, it);
        self.stack.pop();
    }

    fn visit_function(&mut self, it: &mut Function<'a>, flags: ScopeFlags) {
        let mut renames = if let Some(body) = &it.body {
            collect_renames_for_scope(&body.statements, &mut self.used, true)
        } else {
            HashMap::new()
        };
        for param in &it.params.items {
            self.add_param_rename(&param.pattern, "arg", &mut renames);
        }
        self.stack.push(Scope { renames });
        oxc_ast_visit::walk_mut::walk_function(self, it, flags);
        self.stack.pop();
    }

    fn visit_arrow_function_expression(&mut self, it: &mut ArrowFunctionExpression<'a>) {
        let mut renames = collect_renames_for_scope(&it.body.statements, &mut self.used, true);
        for param in &it.params.items {
            self.add_param_rename(&param.pattern, "arg", &mut renames);
        }
        self.stack.push(Scope { renames });
        oxc_ast_visit::walk_mut::walk_arrow_function_expression(self, it);
        self.stack.pop();
    }

    fn visit_catch_clause(&mut self, it: &mut CatchClause<'a>) {
        let mut renames = HashMap::new();
        if let Some(param) = &it.param {
            self.add_param_rename(&param.pattern, "err", &mut renames);
        }
        self.stack.push(Scope { renames });
        oxc_ast_visit::walk_mut::walk_catch_clause(self, it);
        self.stack.pop();
    }

    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        self.rename_ident(&mut it.name);
    }

    fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
        self.rename_ident(&mut it.name);
    }
}

fn collect_renames_for_scope<'a>(
    stmts: &[Statement<'a>],
    used: &mut HashSet<String>,
    _is_function_scope: bool,
) -> HashMap<String, String> {
    let mut declared: HashSet<String> = HashSet::new();
    let mut candidates: Vec<(String, String)> = Vec::new();

    for stmt in stmts {
        collect_declared_names(stmt, &mut declared);
    }
    for d in &declared {
        used.insert(d.clone());
    }

    let mut push_pairs: Vec<(String, String)> = Vec::new();
    collect_push_pairs(stmts, &mut push_pairs);

    for stmt in stmts {
        if let Statement::VariableDeclaration(var_decl) = stmt {
            for decl in &var_decl.declarations {
                let BindingPattern::BindingIdentifier(binding) = &decl.id else {
                    continue;
                };
                let old_name = binding.name.as_str();
                if !looks_obfuscated(old_name) {
                    continue;
                }
                if let Some(init) = decl.init.as_ref() {
                    if let Some(suggested) = infer_name_from_expression(init) {
                        candidates.push((old_name.to_string(), suggested));
                    }
                }
                if let Some(suggested) = push_pairs
                    .iter()
                    .find(|(old, _)| old == old_name)
                    .map(|(_, name)| name.clone())
                {
                    candidates.push((old_name.to_string(), suggested));
                }
            }
        }
        if let Statement::FunctionDeclaration(func) = stmt {
            if let Some(id) = &func.id {
                let old_name = id.name.as_str();
                if looks_obfuscated(old_name) {
                    candidates.push((old_name.to_string(), "func".to_string()));
                }
            }
        }
        if let Statement::ClassDeclaration(class) = stmt {
            if let Some(id) = &class.id {
                let old_name = id.name.as_str();
                if looks_obfuscated(old_name) {
                    candidates.push((old_name.to_string(), "cls".to_string()));
                }
            }
        }
    }

    let mut renames: HashMap<String, String> = HashMap::new();

    for (old, new) in candidates {
        if renames.contains_key(&old) || !looks_obfuscated(&old) {
            continue;
        }
        let unique = make_unique(&new, used);
        used.insert(unique.clone());
        renames.insert(old, unique);
    }

    // Rename any remaining obfuscated local identifiers to generic names.
    for old in &declared {
        if renames.contains_key(old) || !looks_obfuscated(old) {
            continue;
        }
        let unique = make_unique("v", used);
        used.insert(unique.clone());
        renames.insert(old.clone(), unique);
    }

    renames
}

fn make_unique(base: &str, used: &HashSet<String>) -> String {
    if !used.contains(base) && is_valid_identifier(base) {
        return base.to_string();
    }
    let mut n = 2usize;
    loop {
        let candidate = format!("{}_{}", base, n);
        if !used.contains(&candidate) {
            return candidate;
        }
        n += 1;
    }
}

fn is_valid_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' && first != '$' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '$')
}

fn looks_obfuscated(name: &str) -> bool {
    if name.starts_with("_0x") {
        return true;
    }
    if name.chars().any(|c| c.is_ascii_digit())
        && name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit())
        && name.len() > 1
    {
        return true;
    }
    false
}

fn collect_declared_names<'a>(stmt: &Statement<'a>, out: &mut HashSet<String>) {
    match stmt {
        Statement::VariableDeclaration(var_decl) => {
            for decl in &var_decl.declarations {
                if let BindingPattern::BindingIdentifier(id) = &decl.id {
                    out.insert(id.name.as_str().to_string());
                }
            }
        }
        Statement::FunctionDeclaration(func) => {
            if let Some(id) = &func.id {
                out.insert(id.name.as_str().to_string());
            }
        }
        Statement::ClassDeclaration(class) => {
            if let Some(id) = &class.id {
                out.insert(id.name.as_str().to_string());
            }
        }
        Statement::BlockStatement(block) => {
            for s in &block.body {
                collect_declared_names(s, out);
            }
        }
        _ => {}
    }
}

fn collect_push_pairs<'a>(stmts: &[Statement<'a>], out: &mut Vec<(String, String)>) {
    for window in stmts.windows(2) {
        let Statement::ExpressionStatement(a) = &window[0] else {
            continue;
        };
        let Statement::ExpressionStatement(b) = &window[1] else {
            continue;
        };
        let Some((key_arr, key_lit)) = extract_push_literal(&a.expression) else {
            continue;
        };
        let Some((val_arr, val_id)) = extract_push_identifier(&b.expression) else {
            continue;
        };
        if key_arr == val_arr {
            continue;
        }
        let suggested = format!("{}Value", camelize(&key_lit));
        out.push((val_id, suggested));
    }
}

fn extract_push_literal<'a>(expr: &Expression<'a>) -> Option<(String, String)> {
    let Expression::CallExpression(call) = expr else {
        return None;
    };
    let callee = unwrap_parens(&call.callee);
    let (object, method) = match callee {
        Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
        Expression::ComputedMemberExpression(m) => {
            let name = match unwrap_parens(&m.expression) {
                Expression::StringLiteral(s) => s.value.as_str(),
                _ => return None,
            };
            (&m.object, name)
        }
        _ => return None,
    };
    if method != "push" {
        return None;
    }
    let Expression::Identifier(arr) = unwrap_parens(object) else {
        return None;
    };
    let arg = call.arguments.first()?.as_expression()?;
    let Expression::StringLiteral(s) = unwrap_parens(arg) else {
        return None;
    };
    Some((arr.name.as_str().to_string(), s.value.as_str().to_string()))
}

fn extract_push_identifier<'a>(expr: &Expression<'a>) -> Option<(String, String)> {
    let Expression::CallExpression(call) = expr else {
        return None;
    };
    let callee = unwrap_parens(&call.callee);
    let (object, method) = match callee {
        Expression::StaticMemberExpression(m) => (&m.object, m.property.name.as_str()),
        Expression::ComputedMemberExpression(m) => {
            let name = match unwrap_parens(&m.expression) {
                Expression::StringLiteral(s) => s.value.as_str(),
                _ => return None,
            };
            (&m.object, name)
        }
        _ => return None,
    };
    if method != "push" {
        return None;
    }
    let Expression::Identifier(arr) = unwrap_parens(object) else {
        return None;
    };
    let arg = call.arguments.first()?.as_expression()?;
    let Expression::Identifier(id) = unwrap_parens(arg) else {
        return None;
    };
    Some((arr.name.as_str().to_string(), id.name.as_str().to_string()))
}

fn infer_name_from_expression<'a>(expr: &Expression<'a>) -> Option<String> {
    match expr {
        Expression::StaticMemberExpression(m) => Some(camelize(m.property.name.as_str())),
        Expression::ComputedMemberExpression(m) => {
            let Expression::StringLiteral(s) = unwrap_parens(&m.expression) else {
                return None;
            };
            Some(camelize(s.value.as_str()))
        }
        Expression::CallExpression(call) => infer_name_from_call(call),
        _ => None,
    }
}

fn infer_name_from_call<'a>(call: &CallExpression<'a>) -> Option<String> {
    let callee = unwrap_parens(&call.callee);
    let method = match callee {
        Expression::StaticMemberExpression(m) => m.property.name.as_str(),
        Expression::ComputedMemberExpression(m) => match unwrap_parens(&m.expression) {
            Expression::StringLiteral(s) => s.value.as_str(),
            _ => return None,
        },
        _ => return None,
    };
    let first_arg = call.arguments.first()?.as_expression()?;

    match method {
        "getElementById" | "getElementsByName" => {
            let Expression::StringLiteral(s) = unwrap_parens(first_arg) else {
                return None;
            };
            Some(camelize(s.value.as_str()))
        }
        "querySelector" | "querySelectorAll" => {
            let Expression::StringLiteral(s) = unwrap_parens(first_arg) else {
                return None;
            };
            if let Some(name) = extract_name_from_selector(s.value.as_str()) {
                Some(name)
            } else {
                Some(camelize(s.value.as_str()))
            }
        }
        "getElementsByTagName" | "getElementsByClassName" => {
            let Expression::StringLiteral(s) = unwrap_parens(first_arg) else {
                return None;
            };
            Some(camelize(s.value.as_str()))
        }
        _ => None,
    }
}

fn extract_name_from_selector(selector: &str) -> Option<String> {
    if let Some(start) = selector.find("name=") {
        let rest = &selector[start + 5..];
        let quote = rest.chars().next()?;
        let close = rest[1..].find(quote)? + 1;
        let value = &rest[1..close];
        let inner = if let Some(open) = value.find('[') {
            let close_bracket = value.rfind(']')?;
            &value[open + 1..close_bracket]
        } else {
            value
        };
        return camelize_opt(inner);
    }
    if let Some(start) = selector.find("id=") {
        let rest = &selector[start + 3..];
        let quote = rest.chars().next()?;
        let close = rest[1..].find(quote)? + 1;
        let value = &rest[1..close];
        return camelize_opt(value);
    }
    None
}

fn camelize(s: &str) -> String {
    camelize_opt(s).unwrap_or_else(|| s.to_string())
}

fn camelize_opt(s: &str) -> Option<String> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    let mut parts: Vec<String> = Vec::new();
    let mut current = String::new();
    for ch in s.chars() {
        if ch.is_alphanumeric() {
            current.push(ch);
        } else if !current.is_empty() {
            parts.push(std::mem::take(&mut current));
        }
    }
    if !current.is_empty() {
        parts.push(current);
    }
    if parts.is_empty() {
        return None;
    }
    let mut result = parts[0].to_lowercase();
    for part in &parts[1..] {
        let mut chars = part.chars();
        if let Some(first) = chars.next() {
            result.push(first.to_uppercase().next().unwrap_or(first));
            for ch in chars {
                result.push(ch.to_lowercase().next().unwrap_or(ch));
            }
        }
    }
    if result
        .chars()
        .next()
        .map(|c| c.is_ascii_digit())
        .unwrap_or(false)
    {
        result.insert(0, '_');
    }
    Some(result)
}

fn unwrap_parens<'a, 'b>(mut expr: &'b Expression<'a>) -> &'b Expression<'a> {
    loop {
        match expr {
            Expression::ParenthesizedExpression(p) => expr = &p.expression,
            _ => return expr,
        }
    }
}
