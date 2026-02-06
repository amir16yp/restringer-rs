use restringer_rs::{DeobfuscateOptions, Restringer};

fn run(input: &str) -> String {
    let r = Restringer::default();
    let out = r
        .deobfuscate(
            input,
            DeobfuscateOptions {
                source_type: Some(oxc_span::SourceType::mjs()),
                ..DeobfuscateOptions::default()
            },
        )
        .unwrap();

    println!("==== INPUT ====\n{input}\n==== OUTPUT ====\n{}\n", out.code);
    out.code
}

#[test]
fn normalize_empty_statements_removes_standalone_semicolons() {
    let input = "const a = 1;;\nfunction f() { ; return 1;; }\n";
    let output = run(input);
    assert!(!output.contains(";;"));
    // Ensure function-body empty statements were removed as well.
    assert!(!output.contains("\n        ;\n"));
}

#[test]
fn normalize_empty_statements_preserves_control_flow_empty_bodies() {
    let input = "if (a); else;\nfor (;;);\nwhile (a);\n";
    let output = run(input);

    // OXC prints with trailing newline; just ensure the empty bodies remain.
    assert!(output.contains("if (a);"));
    assert!(output.contains("else;"));
    assert!(output.contains("for (;;);"));
    assert!(output.contains("while (a);"));
}

#[test]
fn remove_redundant_block_statements_flattens_nested_blocks() {
    let input = "{{{ const a = 1; }}}\n";
    let output = run(input);

    // Should not have nested braces anymore.
    assert!(output.contains("const a = 1"));
    assert!(!output.contains("{{"));
}

#[test]
fn rearrange_sequences_splits_return_sequence() {
    let input = "function f(){return a(), b(), c();}\n";
    let output = run(input);

    // Expect extracted calls before return.
    assert!(output.contains("a();"));
    assert!(output.contains("b();"));
    assert!(output.contains("return c();"));
}

#[test]
fn separate_chained_declarators_splits_variable_declaration() {
    let input = "const a = 1, b = 2;\n";
    let output = run(input);

    // Each declarator should become its own const.
    let const_count = output.matches("const ").count();
    assert!(const_count >= 2);
}

#[test]
fn resolve_redundant_logical_expressions_simplifies_if_test() {
    let input = "if (false && foo()) { a(); }\nif (true || bar()) { b(); }\n";
    let output = run(input);

    // false && foo() => false; true || bar() => true
    assert!(output.contains("if (false)"));
    assert!(output.contains("if (true)"));
}

#[test]
fn simplify_if_statements_removes_empty_branches() {
    let input = "if (a) ; else b();\nif (c) d(); else ;\nif (e) ;\n";
    let output = run(input);

    // if (a) ; else b() => if (!a) b();
    assert!(output.contains("if (!a)"));
    // if (c) d() else ; => if (c) d();
    assert!(output.contains("if (c)"));
    // if (e) ; => e; (or similar)
    assert!(output.contains("e"));
}

#[test]
fn normalize_computed_converts_bracket_string_member_access_to_dot() {
    let input = "console['log'](1);\n";
    let output = run(input);
    assert!(output.contains("console.log"));
}

#[test]
fn parse_template_literals_into_string_literals_converts_literal_templates() {
    let input = "const s = `hello ${'world'}!`;\n";
    let output = run(input);
    assert!(output.contains("hello world!"));
    assert!(!output.contains('`'));
}

#[test]
fn rearrange_switches_linearizes_deterministic_state_switch() {
    let input = "var state = 0;\nswitch (state) {\n  case 0: a(); state = 1; break;\n  case 1: b(); state = 2; break;\n  case 2: c(); break;\n}\n";
    let output = run(input);
    assert!(output.contains("a();"));
    assert!(output.contains("b();"));
    assert!(output.contains("c();"));
    assert!(!output.contains("switch"));
}

#[test]
fn unwrap_simple_operations_replaces_wrapper_calls_with_operations() {
    let input = "function add(a, b) { return a + b; }\nfunction neg(a) { return -a; }\nconst x = add(1, 2);\nconst y = neg(3);\n";
    let output = run(input);
    assert!(output.contains("1 + 2"));
    assert!(output.contains("-3"));
    assert!(!output.contains("add(1, 2)"));
    assert!(!output.contains("neg(3)"));
}

#[test]
fn unwrap_iifes_unwraps_initializer_return_iife() {
    let input = "const f = (() => { return x => x + 1; })();\n";
    let output = run(input);
    assert!(output.contains("const f ="));
    assert!(output.contains("=> x + 1"));
    assert!(!output.contains("})();"));
    assert!(!output.contains(")();"));
}

#[test]
fn unwrap_iifes_unwraps_statement_wrapper_iife() {
    let input = "(() => { a(); b(); })();\n";
    let output = run(input);
    assert!(output.contains("a();"));
    assert!(output.contains("b();"));
    assert!(!output.contains(")();"));
}

#[test]
fn unwrap_function_shells_unwraps_apply_arguments_forwarder() {
    let input = "function outer(x) { return function inner() { return x + 3; }.apply(this, arguments); }\n";
    let output = run(input);
    assert!(!output.contains(".apply(this, arguments)"));
    assert!(output.contains("return x + 3"));
}
