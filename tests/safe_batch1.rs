use restringer_rs::{DeobfuscateOptions, Restringer};

fn run(input: &str) -> String {
    let r = Restringer::default();
    let out = r
        .deobfuscate(
            input,
            DeobfuscateOptions {
                source_type: Some(oxc_span::SourceType::mjs()),
                max_iterations: Some(1),
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

    // false && foo() => false (then eliminated by resolveDeterministicIfStatements)
    // true || bar() => true (then reduced to its consequent)
    assert!(output.contains("b();"));
    assert!(!output.contains("a();"));
    assert!(!output.contains("foo()"));
    assert!(!output.contains("bar()"));
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
fn resolve_deterministic_if_statements_resolves_literal_and_unary_conditions() {
    let input = "if (true) a(); else b();\nif (0) c(); else d();\nif (!0) e();\nif (undefined) f();\n";
    let output = run(input);

    assert!(output.contains("a();"));
    assert!(!output.contains("b();"));

    assert!(output.contains("d();"));
    assert!(!output.contains("c();"));

    assert!(output.contains("e();"));

    // if (undefined) f(); should be removed entirely.
    assert!(!output.contains("f();"));
    assert!(!output.contains("if (undefined)"));
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

#[test]
fn resolve_proxy_calls_rewrites_proxy_function_identifier_uses() {
    let input = "function proxy(a, b) { return target(a, b); }\nconst x = proxy(1, 2);\n";
    let output = run(input);

    assert!(output.contains("function proxy"));
    assert!(output.contains("target(1, 2)") || output.contains("target(1,2)"));
    assert!(!output.contains("proxy(1, 2)"));
}

#[test]
fn resolve_proxy_variables_rewrites_proxy_variable_identifier_uses() {
    let input = "const proxy = target;\nproxy(1);\n";
    let output = run(input);

    assert!(output.contains("target(1)") || output.contains("target(1 )") || output.contains("target(1);"));
    assert!(!output.contains("proxy(1)"));
}

#[test]
fn resolve_proxy_variables_drops_unused_proxy_declarator() {
    let input = "const proxy = target;\n";
    let output = run(input);

    assert!(!output.contains("const proxy"));
}

#[test]
fn resolve_proxy_references_rewrites_proxy_identifier_uses_to_member_expression() {
    let input = "const obj = { a: 1 };\nconst proxy = obj.a;\nconsole.log(proxy);\n";
    let output = run(input);

    assert!(output.contains("console.log(obj.a)") || output.contains("console.log(obj.a);") || output.contains("console.log(obj.a )"));
    assert!(!output.contains("console.log(proxy"));
}

#[test]
fn resolve_member_expression_references_to_array_index_replaces_large_array_index_reads() {
    let input = "const arr = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21];\nconst x = arr[0];\nconst y = arr[21];\n";
    let output = run(input);

    assert!(output.contains("const x = 0") || output.contains("const x=0"));
    assert!(output.contains("const y = 21") || output.contains("const y=21"));
    assert!(!output.contains("arr[0]"));
    assert!(!output.contains("arr[21]"));
}

#[test]
fn resolve_member_expression_references_to_array_index_skips_small_arrays() {
    let input = "const arr = [0,1,2];\nconst x = arr[0];\n";
    let output = run(input);

    assert!(output.contains("arr[0]"));
}

#[test]
fn resolve_member_expression_references_to_array_index_does_not_touch_assignment_lhs() {
    let input = "const arr = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21];\narr[0] = 99;\nconst x = arr[0];\n";
    let output = run(input);

    assert!(output.contains("arr[0] = 99") || output.contains("arr[0]=99"));
    assert!(output.contains("const x = 0") || output.contains("const x=0"));
}

#[test]
fn simplify_calls_rewrites_call_this_to_direct_call() {
    let input = "function f(a,b){return a+b;}\nf.call(this, 1, 2);\n";
    let output = run(input);
    // In the full safe pipeline this may further simplify to `1 + 2` via `unwrapSimpleOperations`.
    assert!(
        output.contains("f(1, 2)")
            || output.contains("f(1,2)")
            || output.contains("f(1 , 2)")
            || output.contains("1 + 2")
            || output.contains("1+2")
    );
    assert!(!output.contains(".call(this"));
}

#[test]
fn simplify_calls_rewrites_apply_this_array_to_direct_call() {
    let input = "function f(a,b){return a+b;}\nf.apply(this, [1, 2]);\n";
    let output = run(input);
    assert!(
        output.contains("f(1, 2)") || output.contains("f(1,2)") || output.contains("1 + 2") || output.contains("1+2")
    );
    assert!(!output.contains(".apply(this"));
}

#[test]
fn simplify_calls_keeps_non_this_context() {
    let input = "function f(a){return a;}\nf.call(obj, 1);\n";
    let output = run(input);
    assert!(output.contains("f.call(obj") || output.contains(".call(obj"));
}

#[test]
fn replace_call_expressions_with_unwrapped_identifier_unwraps_function_returning_identifier() {
    let input = "function a() { return String; }\na()('x');\n";
    let output = run(input);
    assert!(output.contains("String('x')") || output.contains("String(\"x\")"));
    assert!(!output.contains("a()('x')"));
}

#[test]
fn replace_call_expressions_with_unwrapped_identifier_unwraps_var_arrow_returning_identifier() {
    let input = "const b = () => btoa;\nb()('data');\n";
    let output = run(input);
    assert!(output.contains("btoa('data')") || output.contains("btoa(\"data\")"));
    assert!(!output.contains("b()('data')"));
}

#[test]
fn replace_call_expressions_with_unwrapped_identifier_unwraps_function_returning_parameterless_call() {
    let input = "function g() { return h; }\nfunction wrap() { return g(); }\nwrap()();\n";
    let output = run(input);
    assert!(!output.contains("wrap()();"));
    assert!(output.contains("g()()") || output.contains("h()") || output.contains("h();"));
}

#[test]
fn replace_eval_calls_with_literal_content_replaces_eval_expression() {
    let input = "console.log(eval('1 + 2'));\n";
    let output = run(input);
    assert!(!output.contains("eval("));
    assert!(output.contains("console.log(1 + 2)") || output.contains("console.log(1+2)"));
}

#[test]
fn replace_eval_calls_with_literal_content_replaces_eval_statement_with_block() {
    let input = "eval('a(); b();');\n";
    let output = run(input);
    assert!(!output.contains("eval("));
    assert!(output.contains("a();"));
    assert!(output.contains("b();"));
}

#[test]
fn replace_eval_calls_with_literal_content_handles_eval_as_callee() {
    let input = "eval('Function')('return 7;')();\n";
    let output = run(input);
    assert!(!output.contains("eval("));
    assert!(output.contains("Function(") || output.contains("function"));
}

#[test]
fn replace_new_func_calls_with_literal_content_replaces_expression() {
    let input = "const x = new Function('return 7')();\n";
    let output = run(input);
    assert!(!output.contains("new Function"));
    assert!(output.contains("const x = 7") || output.contains("const x=7"));
}

#[test]
fn replace_new_func_calls_with_literal_content_replaces_multiple_statements_with_block() {
    let input = "new Function('a(); b();')();\n";
    let output = run(input);
    assert!(!output.contains("new Function"));
    assert!(output.contains("a();"));
    assert!(output.contains("b();"));
}

#[test]
fn replace_new_func_calls_with_literal_content_replaces_expression_code_in_expression_context() {
    let input = "console.log(new Function('1 + 2')());\n";
    let output = run(input);
    assert!(!output.contains("new Function"));
    assert!(output.contains("console.log(1 + 2)") || output.contains("console.log(1+2)"));
}

#[test]
fn replace_function_shells_with_wrapped_value_replaces_calls_only() {
    let input = "function a(){return 42;}\nconst x = a();\nconst y = a;\n";
    let output = run(input);
    assert!(output.contains("const x = 42") || output.contains("const x=42"));
    // The reference `a` (not called) should remain.
    assert!(output.contains("const y = a") || output.contains("const y=a"));
}

#[test]
fn replace_function_shells_with_wrapped_value_iife_replaces_simple_iife() {
    let input = "const x = (function(){ return 7; })();\n";
    let output = run(input);
    assert!(output.contains("const x = 7") || output.contains("const x=7"));
    assert!(!output.contains("function(){"));
    assert!(!output.contains(")();"));
}

#[test]
fn resolve_function_constructor_calls_replaces_literal_constructor_call() {
    let input = "const f = Function.constructor(\"a\", \"b\", \"return a + b;\");\nconst x = f(1, 2);\n";
    let output = run(input);
    // Should contain an actual function expression instead of `.constructor(...)`
    assert!(!output.contains(".constructor("));
    assert!(output.contains("function") || output.contains("=>"));
    assert!(output.contains("return a + b") || output.contains("return a+b"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_replaces_number_literal_references() {
    let input = "const a = 3; const b = a * 2; console.log(b + a);\n";
    let output = run(input);
    assert!(output.contains("const a = 3") || output.contains("const a=3"));
    assert!(output.contains("const b = 3 * 2") || output.contains("const b=3*2") || output.contains("const b=3 * 2"));
    assert!(output.contains("console.log(b + 3)") || output.contains("console.log(b+3)"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_replaces_string_literal_references() {
    let input = "const msg = 'hello'; console.log(msg + ' world');\n";
    let output = run(input);
    assert!(output.contains("const msg"));
    assert!(
        output.contains("console.log('hello' + ' world')")
            || output.contains("console.log('hello'+' world')")
            || output.contains("console.log(\"hello\" + \" world\")")
            || output.contains("console.log(\"hello\"+\" world\")")
    );
}

#[test]
fn replace_identifier_with_fixed_assigned_value_does_not_replace_modified_variable() {
    let input = "let counter = 0; counter++; console.log(counter);\n";
    let output = run(input);
    assert!(output.contains("counter++") || output.contains("counter ++"));
    assert!(output.contains("console.log(counter)"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_does_not_replace_for_in_loop_lhs() {
    let input = "var a = 3; for (a in [1, 2]) console.log(a);\n";
    let output = run(input);
    assert!(output.contains("for (a in") || output.contains("for(a in"));
    assert!(output.contains("console.log(a)") || output.contains("console.log(a);"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_does_not_replace_object_shorthand() {
    let input = "const a = 3; const obj = { a }; console.log(obj.a);\n";
    let output = run(input);
    assert!(output.contains("{ a }") || output.contains("{a}"));
}
