use std::cell::Cell;
use std::collections::HashSet;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::*;
use oxc_ast_visit::Visit;
use oxc_codegen::{Codegen, CodegenOptions};
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::{GetSpanMut, SourceType, Span};
use oxc_syntax::node::NodeId;

/// Generates JS source code for a single statement.
pub fn statement_to_code(stmt: &Statement) -> String {
    let allocator = Allocator::default();
    let mut program = empty_program(&allocator);
    program.body.push(stmt.clone_in(&allocator));
    Codegen::new()
        .with_options(CodegenOptions::default())
        .build(&program)
        .code
}

/// Generates JS source code for an expression (without a trailing semicolon).
pub fn expression_to_code(expr: &Expression) -> String {
    let allocator = Allocator::default();
    let mut program = empty_program(&allocator);
    let cloned = expr.clone_in(&allocator);
    program
        .body
        .push(Statement::ExpressionStatement(oxc_allocator::Box::new_in(
            ExpressionStatement {
                node_id: Cell::new(NodeId::DUMMY),
                span: oxc_span::SPAN,
                expression: cloned,
            },
            &allocator,
        )));
    let mut code = Codegen::new()
        .with_options(CodegenOptions::default())
        .build(&program)
        .code;
    let trimmed = code.trim_end();
    code.truncate(trimmed.len());
    if code.ends_with(';') {
        code.pop();
    }
    code
}

fn empty_program(allocator: &Allocator) -> Program<'_> {
    Program {
        node_id: Cell::new(NodeId::DUMMY),
        span: oxc_span::SPAN,
        source_type: SourceType::mjs(),
        source_text: "",
        hashbang: None,
        directives: oxc_allocator::Vec::new_in(allocator),
        body: oxc_allocator::Vec::new_in(allocator),
        comments: oxc_allocator::Vec::new_in(allocator),
        scope_id: Cell::new(None),
    }
}

/// Parses `code` as a single JS expression allocated in `allocator`,
/// assigning `span` to the top-level node. Used to turn JSON eval results
/// (strings, numbers, booleans, arrays, null) back into AST literals.
pub fn parse_expression_in<'a>(
    allocator: &'a Allocator,
    code: &str,
    span: Span,
) -> Option<Expression<'a>> {
    let wrapped = allocator.alloc_str(&format!("({})", code));
    let ret = Parser::new(allocator, wrapped, SourceType::mjs()).parse();
    if !ret.errors.is_empty() {
        return None;
    }
    let mut program = ret.program;
    if program.body.len() != 1 {
        return None;
    }
    let stmt = program.body.pop()?;
    let mut expr = match stmt {
        Statement::ExpressionStatement(es) => es.unbox().expression,
        _ => return None,
    };
    // Unwrap the parentheses we added.
    if let Expression::ParenthesizedExpression(paren) = expr {
        expr = paren.unbox().expression;
    }
    *expr.span_mut() = span;
    Some(expr)
}

/// True if the expression is a simple literal value that can be safely
/// inlined as a function call argument: string/number/boolean/null,
/// or a unary +/- applied to a numeric literal.
pub fn is_static_literal(expr: &Expression) -> bool {
    match expr {
        Expression::StringLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NullLiteral(_) => true,
        Expression::UnaryExpression(un) => {
            matches!(
                un.operator,
                UnaryOperator::UnaryNegation | UnaryOperator::UnaryPlus
            ) && matches!(un.argument, Expression::NumericLiteral(_))
        }
        _ => false,
    }
}

/// True if every element of the array expression is a static literal.
pub fn is_static_literal_array(arr: &ArrayExpression) -> bool {
    arr.elements.iter().all(|elem| match elem {
        ArrayExpressionElement::SpreadElement(_) => false,
        ArrayExpressionElement::Elision(_) => true,
        _ => is_static_literal(elem.to_expression()),
    })
}

/// Identifier names that should never be resolved/evaluated because they
/// reference environment-dependent or side-effecting globals.
pub const SKIP_IDENTIFIERS: &[&str] = &[
    "window",
    "document",
    "location",
    "navigator",
    "fetch",
    "XMLHttpRequest",
    "WebSocket",
    "localStorage",
    "sessionStorage",
    "process",
    "require",
    "module",
    "exports",
    "globalThis",
    "self",
    "alert",
    "prompt",
    "confirm",
    "setTimeout",
    "setInterval",
    "setImmediate",
    "Date",
    "Math.random",
];

/// Property/method names that should not be resolved (environment-dependent
/// or commonly used for anti-debugging).
pub const SKIP_PROPERTIES: &[&str] = &[
    "test",
    "exec",
    "match",
    "matchAll",
    "random",
    "now",
    "getTime",
    "getTimezoneOffset",
    "toLocaleString",
    "toLocaleDateString",
    "toLocaleTimeString",
    "apply",
    "call",
    "bind",
];

/// Returns true if the given source snippet references any identifier that
/// indicates side effects or environment dependence.
pub fn contains_skip_word(code: &str) -> bool {
    const WORDS: &[&str] = &[
        "document",
        "window",
        "fetch",
        "XMLHttpRequest",
        "WebSocket",
        "location",
        "navigator",
        "localStorage",
        "sessionStorage",
        "require(",
        "process.",
        "Math.random",
        "Date.now",
        "new Date",
    ];
    WORDS.iter().any(|w| code.contains(w))
}

/// Collects the names of all identifier references inside a statement.
pub fn collect_referenced_idents(stmt: &Statement) -> HashSet<String> {
    let mut collector = IdentCollector {
        names: HashSet::new(),
    };
    collector.visit_statement(stmt);
    collector.names
}

/// Collects the names of all identifier references inside an expression.
pub fn collect_referenced_idents_expr(expr: &Expression) -> HashSet<String> {
    let mut collector = IdentCollector {
        names: HashSet::new(),
    };
    collector.visit_expression(expr);
    collector.names
}

struct IdentCollector {
    names: HashSet<String>,
}

impl<'a> Visit<'a> for IdentCollector {
    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        self.names.insert(it.name.to_string());
    }
}

/// JavaScript built-in globals that are safe to reference during evaluation.
pub const KNOWN_GLOBALS: &[&str] = &[
    "Array",
    "Boolean",
    "Date",
    "Error",
    "Function",
    "JSON",
    "Map",
    "Math",
    "Number",
    "Object",
    "RegExp",
    "Set",
    "String",
    "Symbol",
    "WeakMap",
    "WeakSet",
    "parseInt",
    "parseFloat",
    "isNaN",
    "isFinite",
    "atob",
    "btoa",
    "escape",
    "unescape",
    "encodeURI",
    "encodeURIComponent",
    "decodeURI",
    "decodeURIComponent",
    "undefined",
    "NaN",
    "Infinity",
    "console",
];

/// True if `name` is a known JavaScript built-in global.
pub fn is_known_global(name: &str) -> bool {
    KNOWN_GLOBALS.contains(&name)
}

/// True if the source snippet has any root-scope identifier references that are
/// not resolved to a declaration in the snippet itself and are not known safe
/// globals or skip identifiers. This is used to avoid spawning a JS engine for
/// code that is guaranteed to fail with ReferenceError/TypeError at runtime.
pub fn has_unresolved_references(source: &str, source_type: SourceType) -> bool {
    let allocator = Allocator::default();
    let parse_ret = Parser::new(&allocator, source, source_type).parse();
    if !parse_ret.errors.is_empty() {
        return true;
    }
    let semantic_ret = SemanticBuilder::new().build(&parse_ret.program);
    if !semantic_ret.errors.is_empty() {
        return true;
    }
    let unresolved = semantic_ret.semantic.scoping().root_unresolved_references();
    for name in unresolved.keys() {
        let name_str = name.as_str();
        if !is_known_global(name_str) && !SKIP_IDENTIFIERS.contains(&name_str) {
            return true;
        }
    }
    false
}

/// Computes the set of free identifier references for a single statement:
/// identifiers used in the statement that are not declared inside it and are
/// not known safe globals or skip identifiers.
pub fn free_identifier_references(stmt: &Statement) -> HashSet<String> {
    let allocator = Allocator::default();
    let mut program = empty_program(&allocator);
    program.body.push(stmt.clone_in(&allocator));

    let semantic_ret = SemanticBuilder::new().build(&program);
    if !semantic_ret.errors.is_empty() {
        return HashSet::new();
    }

    let mut free = HashSet::new();
    let unresolved = semantic_ret.semantic.scoping().root_unresolved_references();
    for name in unresolved.keys() {
        let name_str = name.as_str();
        if !is_known_global(name_str) && !SKIP_IDENTIFIERS.contains(&name_str) {
            free.insert(name_str.to_string());
        }
    }
    free
}

/// A polyfill prepended to evaluation snippets to provide browser globals
/// missing from QuickJS (atob/btoa).
pub const EVAL_PRELUDE: &str = r#"
var __B64A = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
if (typeof globalThis.atob === 'undefined') {
    globalThis.atob = function (input) {
        var str = String(input).replace(/=+$/, '');
        var output = '';
        for (var bc = 0, bs = 0, buffer, idx = 0; (buffer = str.charAt(idx++));) {
            buffer = __B64A.indexOf(buffer);
            if (buffer === -1) continue;
            bs = bc % 4 ? bs * 64 + buffer : buffer;
            if (bc++ % 4) output += String.fromCharCode(255 & (bs >> ((-2 * bc) & 6)));
        }
        return output;
    };
}
if (typeof globalThis.btoa === 'undefined') {
    globalThis.btoa = function (input) {
        var str = String(input);
        var output = '';
        for (var block, charCode, idx = 0, map = __B64A; str.charAt(idx | 0) || ((map = '='), idx % 1); output += map.charAt(63 & (block >> (8 - (idx % 1) * 8)))) {
            charCode = str.charCodeAt((idx += 3 / 4));
            if (charCode > 255) throw new Error('btoa: invalid character');
            block = (block << 8) | charCode;
        }
        return output;
    };
}
"#;
