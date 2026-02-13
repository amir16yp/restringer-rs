use restringer_rs::{DeobfuscateOptions, Restringer};

fn run(input: &str) -> String {
    let r = Restringer::default();
    let out = r
        .deobfuscate(
            input,
            DeobfuscateOptions {
                source_type: Some(oxc_span::SourceType::mjs()),
                max_iterations: Some(3),
                ..DeobfuscateOptions::default()
            },
        )
        .unwrap();

    println!("==== INPUT ====\n{input}\n==== OUTPUT ====\n{}\n", out.code);
    out.code
}

#[test]
fn normalize_void0_rewrites_void0_to_undefined_when_safe() {
    let input = "if (void 0 !== a) { b(); }\n";
    let output = run(input);
    assert!(!output.contains("void 0"));
    assert!(output.contains("undefined !== a") || output.contains("undefined!==a"));
}

#[test]
fn normalize_void0_does_not_rewrite_if_undefined_is_shadowed() {
    let input = "function f(undefined){ return void 0; }\n";
    let output = run(input);
    assert!(output.contains("void 0"));
}

#[test]
fn simplify_double_negation_simplifies_in_if_test() {
    let input = "if (!!this.worldSeed) { a(); }\n";
    let output = run(input);
    assert!(!output.contains("!!this.worldSeed"));
    assert!(output.contains("if (this.worldSeed)") || output.contains("if(this.worldSeed)"));
}

#[test]
fn simplify_double_negation_does_not_simplify_in_value_context() {
    let input = "var x = !!a;\n";
    let output = run(input);
    assert!(output.contains("!!a"));
}

#[test]
fn unwrap_bind_null_literal_rewrites_simple_index_closure() {
    let input = "var e = {a: 1}; var r = 'a'; var f = function (t) { return e[t]; }.bind(null, r);\n";
    let output = run(input);
    assert!(!output.contains(".bind(null"));
    assert!(
        output.contains("function ()")
            || output.contains("function()")
            || output.contains("function ()")
    );
    assert!(output.contains("e[r]") || output.contains("e[r ]") || output.contains("e[r]"));
}

#[test]
fn unwrap_bind_null_literal_does_not_rewrite_when_this_used() {
    let input = "var r = 1; var f = function (t) { return this[t]; }.bind(null, r);\n";
    let output = run(input);
    assert!(output.contains(".bind(null"));
}

#[test]
fn simplify_jsfuck_booleans_rewrites_bang_empty_array_to_false() {
    let input = "if (![]) { a(); }\n";
    let output = run(input);
    assert!(!output.contains("![]"));
    // The pipeline may fold `if (false)` away entirely.
    assert!(!output.contains("a()"));
}

#[test]
fn simplify_jsfuck_booleans_rewrites_double_bang_empty_array_to_true() {
    let input = "if (!![]) { a(); }\n";
    let output = run(input);
    assert!(!output.contains("!![]"));
    // The pipeline may fold `if (true)` into the consequent.
    assert!(output.contains("a()"));
}

#[test]
fn fold_string_concatenation_folds_adjacent_string_literals() {
    let input = "var x = 'a' + 'b' + 'c';\n";
    let output = run(input);
    assert!(!output.contains("'a'+'b'"));
    assert!(output.contains("'abc'") || output.contains("\"abc\""));
}

#[test]
fn fold_string_concatenation_does_not_fold_when_non_string_involved() {
    let input = "var x = 'a' + b;\n";
    let output = run(input);
    assert!(
        output.contains("'a'+b")
            || output.contains("'a' + b")
            || output.contains("\"a\"+b")
            || output.contains("\"a\" + b")
    );
}
