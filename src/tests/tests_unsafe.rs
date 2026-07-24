use crate::transforms::unsafe_transforms::eval_constant_expressions::EvalConstantExpressions;
use crate::transforms::unsafe_transforms::js_runtime::JsEvaluator;
use crate::transforms::unsafe_transforms::resolve_injected_prototype_method_calls::ResolveInjectedPrototypeMethodCalls;
use crate::transforms::unsafe_transforms::resolve_literal_iife_results::ResolveLiteralIifeResults;
use crate::transforms::unsafe_transforms::resolve_packed_eval_calls::ResolvePackedEvalCalls;
use crate::{DeobfuscateOptions, Restringer};

fn apply_module_to_code(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer
        .apply_module_to_code(code, transform, DeobfuscateOptions::default())
        .unwrap()
}

fn apply_module_to_code_looped(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer
        .apply_modules_to_code(code, vec![transform], DeobfuscateOptions::default())
        .unwrap()
}

fn assert_transform(transform_name: &str, input: &str, expected: &str, actual: &str) {
    println!("\n## Unsafe Transform: `{}`\n", transform_name);
    println!("### Input\n```javascript\n{}\n```\n", input);
    println!("### Expected\n```javascript\n{}\n```\n", expected);
    println!("### Actual\n```javascript\n{}\n```\n", actual);
    assert_eq!(actual, expected);
}

#[cfg(test)]
mod resolve_literal_iife_results {
    use super::*;

    #[test]
    fn resolves_nested_literal_iife_without_removing_its_scope() {
        let code =
            r#"if (ready) { const value = (function(value) { return btoa(value); })("A"); }"#;
        let expected = "if (ready) {\n\tconst value = \"QQ==\";\n}\n";
        let result = apply_module_to_code(code, Box::new(ResolveLiteralIifeResults::new()));
        assert_transform("ResolveLiteralIifeResults", code, expected, &result);
    }
}

#[cfg(test)]
mod resolve_packed_eval_calls {
    use super::*;

    #[test]
    fn resolves_packer_using_preceding_helper() {
        let code = "function decode(value) { return value; } eval(function(payload) { return decode(payload); }(\"const answer = 42;\"));";
        let expected = "function decode(value) {\n\treturn value;\n}\nconst answer = 42;\n";
        let result = apply_module_to_code(code, Box::new(ResolvePackedEvalCalls::new()));
        assert_transform("ResolvePackedEvalCalls", code, expected, &result);
    }

    #[test]
    fn resolves_nested_self_contained_packer() {
        let code = "if (true) eval(function(payload) { return payload; }(\"const answer = 42;\"));";
        let expected = "if (true) eval(\"const answer = 42;\");\n";
        let result = apply_module_to_code(code, Box::new(ResolvePackedEvalCalls::new()));
        assert_transform("ResolvePackedEvalCalls", code, expected, &result);
    }

    #[test]
    fn skips_packer_with_unresolved_reference() {
        let code = r#"if (true) eval(function(payload) { return unknownFunc(payload); }("const answer = 42;"));"#;
        let result = apply_module_to_code(code, Box::new(ResolvePackedEvalCalls::new()));
        assert!(
            result.contains("unknownFunc(payload)"),
            "expected packer with unresolved reference to stay unevaluated; got: {}",
            result
        );
    }
}

#[cfg(test)]
mod js_evaluator {
    use super::*;

    #[test]
    fn test_eval_string_concatenation() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("'hello' + ' ' + 'world'").unwrap();
        assert_eq!(result, "hello world");
    }

    #[test]
    fn test_eval_arithmetic() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_number("2 + 2 * 3").unwrap();
        assert_eq!(result, 8.0);
    }

    #[test]
    fn test_eval_boolean_logic() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_bool("true && false || true").unwrap();
        assert_eq!(result, true);
    }

    #[test]
    fn test_eval_array_operations() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("[1, 2, 3].join(',')").unwrap();
        assert_eq!(result, "1,2,3");
    }

    #[test]
    fn test_eval_string_methods() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("'hello'.toUpperCase()").unwrap();
        assert_eq!(result, "HELLO");
    }

    #[test]
    fn test_eval_complex_expression() {
        let evaluator = JsEvaluator::new();
        let result = evaluator
            .eval_to_number("Math.pow(2, 3) + Math.sqrt(16)")
            .unwrap();
        assert_eq!(result, 12.0);
    }

    #[test]
    fn test_eval_template_literals() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("`Hello ${'world'}`").unwrap();
        assert_eq!(result, "Hello world");
    }

    #[test]
    fn test_eval_object_access() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_number("({a: 42}).a").unwrap();
        assert_eq!(result, 42.0);
    }
}

#[cfg(test)]
mod eval_constant_expressions {
    use super::*;

    #[test]
    fn test_eval_simple_arithmetic() {
        let code = "const x = 2 + 3 * 4;";
        let expected = "const x = 14;\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_string_concatenation() {
        let code = "const msg = 'Hello' + ' ' + 'World';";
        let expected = "const msg = \"Hello World\";\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_boolean_expressions() {
        let code = "const flag = true && false || true;";
        let expected = "const flag = true;\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_array_literals() {
        let code = "const arr = [1, 2, 3];";
        let expected = "const arr = [\n\t1,\n\t2,\n\t3\n];\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_nested_operations() {
        let code = "const result = (10 + 5) * 2 - 3;";
        let expected = "const result = 27;\n";
        let result = apply_module_to_code_looped(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_unary_operators() {
        let code = "const neg = -42; const pos = +42;";
        let expected = "const neg = -42;\nconst pos = 42;\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_logical_not() {
        let code = "const val = !false;";
        let expected = "const val = true;\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_conditional_expression() {
        let code = "const val = true ? 'yes' : 'no';";
        let expected = "const val = \"yes\";\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_template_literal() {
        let code = "const msg = `Result: ${2 + 2}`;";
        let expected = "const msg = \"Result: 4\";\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_object_literal() {
        let code = "const obj = {a: 1, b: 2 + 3};";
        let expected = "const obj = {\n\ta: 1,\n\tb: 5\n};\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_do_not_eval_unsafe_code() {
        let code = "const x = someFunction();";
        let expected = "const x = someFunction();\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_do_not_eval_variables() {
        let code = "const x = a + b;";
        let expected = "const x = a + b;\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_mixed_safe_and_unsafe() {
        let code = "const safe = 2 + 3; const unsafe = someFunc();";
        let expected = "const safe = 5;\nconst unsafe = someFunc();\n";
        let result = apply_module_to_code(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }

    #[test]
    fn test_eval_iterative_simplification() {
        let code = "const x = 1 + 1; const y = x + 2;";
        let expected = "const x = 2;\nconst y = x + 2;\n";
        let result = apply_module_to_code_looped(code, Box::new(EvalConstantExpressions::new()));
        assert_transform("EvalConstantExpressions", code, expected, &result);
    }
}

#[cfg(test)]
mod resolve_member_expressions_local_references {
    use super::*;
    use crate::transforms::unsafe_transforms::resolve_member_expressions_local_references::ResolveMemberExpressionsLocalReferences;

    #[test]
    fn keeps_reads_of_mutated_function_properties() {
        let code = r#"var decode = function (key) { if (decode.cache === undefined) { decode.cache = {}; } var value = decode.cache[key]; decode.cache[key] = value; return value; }; decode("x");"#;
        let result = apply_module_to_code(
            code,
            Box::new(ResolveMemberExpressionsLocalReferences::new()),
        );
        assert!(result.contains("decode.cache[key]"), "got: {}", result);
        assert!(
            result.contains("decode.cache === undefined"),
            "got: {}",
            result
        );
    }
}

#[cfg(test)]
mod resolve_local_calls {
    use super::*;
    use crate::transforms::unsafe_transforms::resolve_local_calls::ResolveLocalCalls;

    #[test]
    fn test_resolve_local_calls_simple() {
        let code = "function add(a, b) { return a + b; } const res = add(5, 10);";
        let expected = "function add(a, b) {\n\treturn a + b;\n}\nconst res = 15;\n";
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert_transform("ResolveLocalCalls", code, expected, &result);
    }

    #[test]
    fn resolves_calls_with_large_static_array_dependencies() {
        let elements = (0..800)
            .map(|index| format!(r#""value-{index}""#))
            .collect::<Vec<_>>()
            .join(",");
        let code = format!(
            "var table = [{elements}]; function decode(index) {{ return table[index]; }} decode(799);"
        );
        assert!(code.len() > 5_000);
        let result = apply_module_to_code(&code, Box::new(ResolveLocalCalls::new()));
        assert!(result.contains(r#""value-799";"#), "got: {}", result);
        assert!(!result.contains("decode(799)"), "got: {}", result);
    }

    #[test]
    fn resolves_local_function_inside_iife() {
        let code = r#"
            (function() {
                function helper(x) { return x + 1; }
                const a = helper(2);
            })();
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("const a = 3"),
            "expected helper(2) to resolve to 3; got: {}",
            result
        );
    }

    #[test]
    fn inlines_partial_local_call() {
        use crate::transforms::unsafe_transforms::resolve_partial_local_calls::ResolvePartialLocalCalls;
        let code = r#"
            function addConstant(x) { return x + (2 * 3); }
            const y = addConstant(z);
        "#;
        let result = apply_module_to_code(code, Box::new(ResolvePartialLocalCalls));
        assert!(
            result.contains("const y = z + 2 * 3"),
            "expected partial inline; got: {}",
            result
        );
    }

    #[test]
    fn resolves_local_calls_to_variable_function_expressions() {
        let code = r#"
            var helper = function(x) { return x + 1; };
            const a = helper(2);
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("const a = 3"),
            "expected helper(2) to resolve to 3; got: {}",
            result
        );
    }

    #[test]
    fn skips_local_call_referencing_enclosing_parameter() {
        let code = r#"
            function helper(x) { return x + 1; }
            function outer(y) {
                return helper(y);
            }
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("helper(y)"),
            "expected helper(y) to stay unresolved because y is not in eval context; got: {}",
            result
        );
    }

    #[test]
    fn skips_eval_when_context_has_conflicting_declarations() {
        // Block-scoped function and a top-level const share a name. The transform
        // hoists the nested function into the eval context, which would create a
        // duplicate declaration syntax error. It should bail out safely.
        let code = r#"
            {
                function helper() { return 1; }
            }
            const helper = 2;
            helper();
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("const helper = 2"),
            "expected transform to leave code unchanged when context is invalid; got: {}",
            result
        );
        assert!(
            result.contains("helper()"),
            "expected helper() call to remain; got: {}",
            result
        );
    }

    #[test]
    fn skips_local_call_when_function_body_has_unresolved_free_reference() {
        let code = r#"
            function helper() { return unknownVar + 1; }
            const a = helper();
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("const a = helper()"),
            "expected helper() call to remain when body references unknownVar; got: {}",
            result
        );
    }

    #[test]
    fn resolves_local_call_when_function_body_uses_available_reference() {
        let code = r#"
            var table = ["x", "y"];
            function helper() { return table[0]; }
            const a = helper();
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert!(
            result.contains("const a = \"x\""),
            "expected helper() to resolve when table is in context; got: {}",
            result
        );
    }

    #[test]
    fn resolves_literal_calls_to_nested_mapper_functions() {
        let code = r#"
            if (true) {
                function mapper(value) {
                    return btoa(value).replace(new RegExp("Q", "g"), ".Y");
                }
                const result = mapper("A");
            }
        "#;
        let expected = "if (true) {\n\tfunction mapper(value) {\n\t\treturn btoa(value).replace(new RegExp(\"Q\", \"g\"), \".Y\");\n\t}\n\tconst result = \".Y.Y==\";\n}\n";
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert_transform("ResolveLocalCalls", code, expected, &result);
    }

    #[test]
    fn resolves_literal_calls_to_base64_map_replacers() {
        let code = r#"
            function psInstance(str) {
                str = btoa(str);
                var map = new Map([["Q", ".Y"], ["=", ".equal"]]);
                for (let pair of map) {
                    str = str.replace(new RegExp(pair[0], "g"), pair[1]);
                }
                return str;
            }
            const result = psInstance("A");
        "#;
        let expected = "function psInstance(str) {\n\tstr = btoa(str);\n\tvar map = new Map([[\"Q\", \".Y\"], [\"=\", \".equal\"]]);\n\tfor (let pair of map) {\n\t\tstr = str.replace(new RegExp(pair[0], \"g\"), pair[1]);\n\t}\n\treturn str;\n}\nconst result = \".Y.Y.equal.equal\";\n";
        let result = apply_module_to_code(code, Box::new(ResolveLocalCalls::new()));
        assert_transform("ResolveLocalCalls", code, expected, &result);
    }
}

#[cfg(test)]
mod resolve_augmented_function_wrapped_array_replacements {
    use super::*;
    use crate::transforms::unsafe_transforms::resolve_augmented_function_wrapped_array_replacements::ResolveAugmentedFunctionWrappedArrayReplacements;

    #[test]
    fn test_resolve_augmented_array_simple() {
        let code = r#"
            var arr = ["first", "second", "third"];
            (function(a, count) {
                a.push(a.shift());
            })(arr, 1);
        "#;
        let expected = "var arr = [\n\t\"second\",\n\t\"third\",\n\t\"first\"\n];\n";
        let result = apply_module_to_code(
            code,
            Box::new(ResolveAugmentedFunctionWrappedArrayReplacements::new()),
        );
        assert_transform(
            "ResolveAugmentedFunctionWrappedArrayReplacements",
            code,
            expected,
            &result,
        );
    }
}

#[cfg(test)]
mod resolve_builtin_calls {
    use super::*;
    use crate::transforms::unsafe_transforms::resolve_builtin_calls::ResolveBuiltinCalls;

    #[test]
    fn test_resolve_builtin_calls_simple() {
        let code = r#"
            const a = atob("SGVsbG8=");
            const b = "foo,bar".split(",");
            const c = [1, 2, 3].join("-");
        "#;
        let expected =
            "const a = \"Hello\";\nconst b = [\"foo\", \"bar\"];\nconst c = \"1-2-3\";\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinCalls::new()));
        assert_transform("ResolveBuiltinCalls", code, expected, &result);
    }

    #[test]
    fn resolves_nested_encoding_calls() {
        let code = r#"
            const a = decodeURIComponent(escape(atob("SGVsbG8gV29ybGQ=")));
            const b = btoa(unescape("Hello%20World"));
            const c = encodeURIComponent(decodeURIComponent("Hello%20World"));
        "#;
        let expected = "const a = \"Hello World\";\nconst b = \"SGVsbG8gV29ybGQ=\";\nconst c = \"Hello%20World\";\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinCalls::new()));
        assert_transform("ResolveBuiltinCalls", code, expected, &result);
    }

    #[test]
    fn resolves_math_and_object_calls() {
        let code = r#"
            const a = Math.max(1, 2);
            const b = Object.keys({a: 1, b: 2});
            const c = Array.isArray([]);
            const d = Number("42");
            const e = Boolean(0);
            const f = JSON.stringify({x: 1});
        "#;
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinCalls::new()));
        assert!(
            !result.contains("Math.max"),
            "expected Math.max to be resolved; got: {}",
            result
        );
        assert!(
            !result.contains("Object.keys"),
            "expected Object.keys to be resolved; got: {}",
            result
        );
        assert!(
            !result.contains("Array.isArray"),
            "expected Array.isArray to be resolved; got: {}",
            result
        );
        assert!(
            !result.contains("Number("),
            "expected Number to be resolved; got: {}",
            result
        );
        assert!(
            !result.contains("Boolean("),
            "expected Boolean to be resolved; got: {}",
            result
        );
        assert!(
            !result.contains("JSON.stringify"),
            "expected JSON.stringify to be resolved; got: {}",
            result
        );
        assert!(
            result.contains("const a = 2"),
            "expected a = 2; got: {}",
            result
        );
        assert!(
            result.contains("const c = true"),
            "expected c = true; got: {}",
            result
        );
        assert!(
            result.contains("const e = false"),
            "expected e = false; got: {}",
            result
        );
    }
}

#[cfg(test)]
mod ant_regression {
    use crate::{DeobfuscateOptions, Restringer};

    #[test]
    fn test_ant_js_does_not_lose_function_body() {
        let code = include_str!("../../restringer-js/tests/resources/ant.js");
        let restringer = Restringer::default();
        let result = restringer
            .deobfuscate(code, DeobfuscateOptions::default())
            .unwrap();
        assert!(result.modified);
        // The bug caused only the string-array declaration to be emitted.
        assert!(
            result.code.contains("function ant_main"),
            "expected ant_main to remain in deobfuscated output"
        );
        assert!(
            result.code.contains("window.ant_zero"),
            "expected window property assignments to remain in output"
        );
    }

    #[test]
    fn test_ds_js_resolves_rotated_decoder_array() {
        let code = include_str!("../../restringer-js/tests/resources/ds.js");
        let restringer = Restringer::default();
        let result = restringer
            .deobfuscate(code, DeobfuscateOptions::default())
            .unwrap();
        assert!(result.modified);
        assert!(
            !result.code.contains("_0x2cb1("),
            "expected all rotated decoder calls to be resolved"
        );
    }

    #[test]
    fn test_udu_js_resolves_string_array() {
        let code = include_str!("../../restringer-js/tests/resources/udu.js");
        let restringer = Restringer::default();
        let result = restringer
            .deobfuscate(code, DeobfuscateOptions::default())
            .unwrap();
        assert!(result.modified);
        assert!(
            !result.code.contains("_$_2b1a["),
            "expected all string-array references to be resolved"
        );
    }
}

#[cfg(test)]
mod resolve_injected_prototype_method_calls {
    use super::*;

    #[test]
    fn resolves_prototype_assignment_inside_nested_function() {
        let code = r#"(function () { function setup() { String.prototype.rot13 = function () { return "rot13-result"; }; } var secret = "abc"; setup(); console.log(secret.rot13()); })();"#;
        let result =
            apply_module_to_code(code, Box::new(ResolveInjectedPrototypeMethodCalls::new()));
        assert!(
            result.contains("\"rot13-result\""),
            "expected prototype method call to be resolved; got: {}",
            result
        );
    }
}
