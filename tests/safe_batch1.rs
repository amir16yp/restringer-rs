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
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_number_literal() {
    let input = "let a; a = 3; const b = a * 2; console.log(b + a);\n";
    let output = run(input);
    assert!(output.contains("a = 3"));
    assert!(output.contains("const b = 3 * 2"));
    assert!(output.contains("b + 3"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_string_literal() {
    let input = "let name; name = 'test'; alert(name);\n";
    let output = run(input);
    assert!(output.contains("name = \"test\"") || output.contains("name = 'test'"));
    assert!(output.contains("alert(\"test\")") || output.contains("alert('test')"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_boolean_literal() {
    let input = "let flag; flag = true; if (flag) console.log('yes');\n";
    let output = run(input);
    assert!(output.contains("flag = true"));
    // After inlining, downstream transforms may resolve `if (true)` into the consequent.
    assert!(output.contains("if (true)") || output.contains("console.log"));
    assert!(output.contains("console.log(\"yes\")") || output.contains("console.log('yes')"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_null_literal() {
    let input = "let value; value = null; console.log(value);\n";
    let output = run(input);
    assert!(output.contains("value = null"));
    assert!(output.contains("console.log(null)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_var_declaration() {
    let input = "var x; x = 42; console.log(x);\n";
    let output = run(input);
    assert!(output.contains("x = 42"));
    assert!(output.contains("console.log(42)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_replaces_multiple_references() {
    let input = "let count; count = 5; alert(count); console.log(count);\n";
    let output = run(input);
    assert!(output.contains("count = 5"));
    assert!(output.contains("alert(5)"));
    assert!(output.contains("console.log(5)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_for_in_iterator() {
    let input = "let a; a = 'prop'; for (a in obj) console.log(a);\n";
    let output = run(input);
    assert!(output.contains("for (a in obj)"));
    assert!(output.contains("console.log(a)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_for_of_iterator() {
    let input = "let item; item = 1; for (item of arr) console.log(item);\n";
    let output = run(input);
    assert!(output.contains("for (item of arr)"));
    assert!(output.contains("console.log(item)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_in_conditional_expression_context() {
    let input = "let a; b === c ? (a = 1) : (a = 2); console.log(a);\n";
    let output = run(input);
    assert!(output.contains("console.log(a)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_multiple_assignments() {
    let input = "let a; a = 1; a = 2; console.log(a);\n";
    let output = run(input);
    assert!(output.contains("console.log(a)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_non_literal_assignment() {
    let input = "let a; a = someFunction(); console.log(a);\n";
    let output = run(input);
    assert!(output.contains("console.log(a)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_function_callee() {
    let input = "let func; func = alert; func('hello');\n";
    let output = run(input);
    assert!(output.contains("func(\"hello\")") || output.contains("func('hello')"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_variable_with_initial_value() {
    let input = "let a = 1; a = 2; console.log(a);\n";
    let output = run(input);
    assert!(output.contains("console.log(a)"));
}

#[test]
fn replace_identifier_with_fixed_value_not_assigned_at_declaration_does_not_replace_when_references_are_modified() {
    let input = "let a; a = 1; a++; console.log(a);\n";
    let output = run(input);
    assert!(output.contains("console.log(a)"));
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
            || output.contains("console.log('hello world')")
            || output.contains("console.log(\"hello world\")")
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

#[test]
fn replace_identifier_with_fixed_assigned_value_not_assigned_at_declaration_replaces_number_literal() {
    let input = "let a; a = 3; const b = a * 2; console.log(b + a);\n";
    let output = run(input);
    assert!(output.contains("let a") || output.contains("let a;"));
    assert!(output.contains("a = 3") || output.contains("a=3"));
    assert!(output.contains("const b = 3 * 2") || output.contains("const b=3*2") || output.contains("const b=3 * 2"));
    assert!(output.contains("console.log(b + 3)") || output.contains("console.log(b+3)"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_not_assigned_at_declaration_replaces_string_literal() {
    let input = "let name; name = 'test'; alert(name);\n";
    let output = run(input);
    assert!(output.contains("name = 'test'") || output.contains("name=\"test\"") || output.contains("name= 'test'") || output.contains("name = \"test\""));
    assert!(output.contains("alert('test')") || output.contains("alert(\"test\")"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_not_assigned_at_declaration_does_not_replace_for_in_iterator() {
    let input = "let a; a = 'prop'; for (a in obj) console.log(a);\n";
    let output = run(input);
    assert!(output.contains("for (a in") || output.contains("for(a in"));
    assert!(output.contains("console.log(a)") || output.contains("console.log(a);"));
}

#[test]
fn replace_identifier_with_fixed_assigned_value_not_assigned_at_declaration_does_not_replace_multiple_assignments() {
    let input = "let a; a = 1; a = 2; console.log(a);\n";
    let output = run(input);
    assert!(output.contains("a = 1") || output.contains("a=1"));
    assert!(output.contains("a = 2") || output.contains("a=2"));
    assert!(output.contains("console.log(a)") || output.contains("console.log(a);"));
}

#[test]
fn resolve_member_expressions_with_direct_assignment_replaces_reads_with_literal() {
    let input = "function a() {} a.b = 3; a.c = '5'; console.log(a.b + a.c);\n";
    let output = run(input);
    assert!(output.contains("a.b = 3"));
    assert!(output.contains("a.c = '5'") || output.contains("a.c = \"5\""));
    assert!(output.contains("console.log(3 + '5')") || output.contains("console.log(3 + \"5\")"));
}

#[test]
fn resolve_member_expressions_with_direct_assignment_does_not_replace_when_property_is_updated() {
    let input = "const a = {}; a.b = 3; a.b++; console.log(a.b);\n";
    let output = run(input);
    assert!(output.contains("a.b++") || output.contains("a.b ++"));
    assert!(output.contains("console.log(a.b") || output.contains("console.log(a.b)"));
}

#[test]
fn resolve_member_expressions_with_direct_assignment_does_not_replace_non_literal_computed_property() {
    let input = "const a = {}; const k = 'b'; a[k] = 3; console.log(a[k]);\n";
    let output = run(input);
    assert!(output.contains("a[k] = 3") || output.contains("a[k]=3"));
    assert!(output.contains("console.log(a[k])") || output.contains("console.log(a[k])"));
}

#[test]
fn resolve_string_array_decoder_calls_inlines_simple_decoder_call() {
    let input = "function _0x2047(){return ['a','b','c'];}\nfunction _0x4274(x,y){x=x-1;const arr=_0x2047();return arr[x];}\nconst z=_0x4274(2);\n";
    let output = run(input);
    assert!(output.contains("const z = \"b\"") || output.contains("const z=\"b\"") || output.contains("const z = 'b'") || output.contains("const z='b'"));
    assert!(!output.contains("_0x4274(2"));
}

#[test]
fn resolve_dispatch_table_calls_rewrites_object_table_indirect_call() {
    let input = "function a(x){return x+1;}\nconst tbl = { add: a };\nconst y = tbl['add'](2);\n";
    let output = run(input);
    assert!(output.contains("const y = a(2)") || output.contains("const y=a(2)") || output.contains("const y = 2 + 1") || output.contains("const y=2+1"));
    assert!(!output.contains("tbl['add']"));
}

#[test]
fn resolve_dispatch_table_calls_rewrites_array_table_indirect_call() {
    let input = "function a(x){return x+1;}\nconst tbl = [a];\nconst y = tbl[0](2);\n";
    let output = run(input);
    assert!(output.contains("const y = a(2)") || output.contains("const y=a(2)") || output.contains("const y = 2 + 1") || output.contains("const y=2+1"));
    assert!(!output.contains("tbl[0](2"));
}

#[test]
fn replace_function_return_this_replaces_with_global_this() {
    let input = "const g = Function('return this')();\n";
    let output = run(input);
    assert!(output.contains("const g = globalThis") || output.contains("const g=globalThis"));
    assert!(!output.contains("Function(\"return this\")") && !output.contains("Function('return this')"));
}

#[test]
fn simplify_module_factory_call_rewrites_call_null_thisarg() {
    let input = "function f(a,b){return a+b;}\nf.call(null, 1, 2);\n";
    let output = run(input);
    assert!(!output.contains(".call(null"));
    assert!(output.contains("f(1, 2)") || output.contains("f(1,2)") || output.contains("1 + 2") || output.contains("1+2"));
}

#[test]
fn simplify_module_factory_call_rewrites_call_undefined_thisarg() {
    let input = "function f(a,b){return a+b;}\nf.call(undefined, 1, 2);\n";
    let output = run(input);
    assert!(!output.contains(".call(undefined"));
    assert!(output.contains("f(1, 2)") || output.contains("f(1,2)") || output.contains("1 + 2") || output.contains("1+2"));
}

#[test]
fn unwrap_webpack_bootstrap_unwraps_bang_iife() {
    let input = "!function(){\nconsole.log(1);\n}();\n";
    let output = run(input);
    assert!(!output.contains("!function"));
    assert!(output.contains("console.log"));
}

#[test]
fn unwrap_webpack_bootstrap_renames_require_fn_to_webpack_require() {
    let input = "!function(e){\nfunction r(i){ return i; }\nreturn r((r.s = 228));\n}([]);\n";
    let output = run(input);
    assert!(output.contains("__webpack_require__"));
    assert!(!output.contains("return r("));
}

#[test]
fn unwrap_webpack_bootstrap_renames_require_fn_for_plain_iife_entry_statement() {
    let input = "(function(e){function r(i){return i;}r((r.s=228));})([]);";
    let output = run(input);
    assert!(output.contains("__webpack_require__"));
    assert!(!output.contains("r((r.s"));
}

#[test]
fn unwrap_webpack_bootstrap_renames_nested_helper_references() {
    let input = "(function(e){function r(i){return i;}r.o=function(a,b){return a[b];};r.d=function(e,t,n){if(!r.o(e,t))e[t]=n;};r((r.s=1));})([]);";
    let output = run(input);
    assert!(output.contains("function __webpack_require__"));
    // Depending on simplification passes, helper assignments like `.d` / `.o` may be pruned
    // if they are not used outside the bootstrap.
    assert!(!output.contains("r.d"));
    assert!(!output.contains("!r.o"));
    assert!(!output.contains("r.d"));
}

#[test]
fn detect_webpack_bundle_normalizes_module_factory_param_names() {
    let input = "var m = {573:function(e,t,i){var x=i(1);t.a=x;}};";
    let output = run(input);
    assert!(output.contains("__webpack_require__(1)"));
    assert!(output.contains("exports.a"));
    assert!(output.contains("function _wp_mod_573"));
    assert!(output.contains("var m") && output.contains("573: _wp_mod_573"));
}

#[test]
fn simplify_babel_class_helpers_inlines_simple_descriptor_array_to_assignments() {
    let input = "function n(e,t){for(var i=0;i<t.length;i++){var n=t[i];Object.defineProperty(e,n.key,n);}}\nfunction C(){}\nn(C.prototype,[{key:\"m\",value:function(){return 1;}}]);\n";
    let output = run(input);
    assert!(!output.contains("Object.defineProperty(C.prototype"));
    assert!(
        output.contains("C.prototype.\"m\"")
            || output.contains("C.prototype.m")
            || output.contains("C.prototype[\"m\"]")
            || output.contains("C.prototype['m']")
    );
}

#[test]
fn simplify_babel_class_helpers_inlines_static_props() {
    let input = "function n(e,t){for(var i=0;i<t.length;i++){var n=t[i];Object.defineProperty(e,n.key,n);}}\nfunction C(){}\nn(C,[{key:\"x\",value:1}]);\n";
    let output = run(input);
    assert!(!output.contains("Object.defineProperty(C"));
    assert!(output.contains("C.x") || output.contains("C[\"x\"]") || output.contains("C['x']"));
}
