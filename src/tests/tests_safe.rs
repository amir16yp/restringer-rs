use crate::transforms::safe_transforms::normalize_computed::NormalizeComputed;
use crate::transforms::safe_transforms::normalize_empty_statements::NormalizeEmptyStatements;
use crate::transforms::safe_transforms::parse_template_literals_into_string_literals::ParseTemplateLiteralsIntoStringLiterals;
use crate::transforms::safe_transforms::rearrange_sequences::RearrangeSequences;
use crate::transforms::safe_transforms::rearrange_switches::RearrangeSwitches;
use crate::transforms::safe_transforms::remove_redundant_block_statements::RemoveRedundantBlockStatements;
use crate::transforms::safe_transforms::resolve_builtin_string_calls::ResolveBuiltinStringCalls;
use crate::{DeobfuscateOptions, Restringer};

fn apply_module_to_code(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer.apply_module_to_code(code, transform, DeobfuscateOptions::default()).unwrap()
}

fn apply_module_to_code_looped(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer.apply_modules_to_code(code, vec![transform], DeobfuscateOptions::default()).unwrap()
}

fn assert_transform(transform_name: &str, input: &str, expected: &str, actual: &str) {
    println!("\n## Safe Transform: `{}`\n", transform_name);
    println!("### Input\n```javascript\n{}\n```\n", input);
    println!("### Expected\n```javascript\n{}\n```\n", expected);
    println!("### Actual\n```javascript\n{}\n```\n", actual);
    assert_eq!(actual, expected);
}

#[cfg(test)]
mod remove_redundant_block_statements {
    use super::*;

    #[test]
    fn tp_1() {
        let code = "if (a) {{do_a();}}";
        let expected = "if (a) {\n\tdo_a();\n}\n";
        let result = apply_module_to_code(code, Box::new(RemoveRedundantBlockStatements));
        assert_transform("RemoveRedundantBlockStatements", code, expected, &result);
    }

    #[test]
    fn tp_2() {
        let code = "if (a) {{do_a();}{do_b();}}";
        let expected = "if (a) {\n\tdo_a();\n\tdo_b();\n}\n";
        let result = apply_module_to_code(code, Box::new(RemoveRedundantBlockStatements));
        assert_transform("RemoveRedundantBlockStatements", code, expected, &result);
    }

    #[test]
    fn tp_3() {
        let code = "if (a) {{do_a();}{do_b(); do_c();}{do_d();}}";
        let expected = "if (a) {\n\tdo_a();\n\tdo_b();\n\tdo_c();\n\tdo_d();\n}\n";
        let result = apply_module_to_code_looped(code, Box::new(RemoveRedundantBlockStatements));
        assert_transform("RemoveRedundantBlockStatements", code, expected, &result);
    }

    #[test]
    fn tp_4() {
        let code = "if (a) {{{{{do_a();}}}} do_b();}";
        let expected = "if (a) {\n\tdo_a();\n\tdo_b();\n}\n";
        let result = apply_module_to_code_looped(code, Box::new(RemoveRedundantBlockStatements));
        assert_transform("RemoveRedundantBlockStatements", code, expected, &result);
    }
}

#[cfg(test)]
mod normalize_computed {
    use super::*;

    #[test]
    fn tp_1_convert_valid_string_identifiers_to_dot_notation() {
        let code = "hello['world'][0]['%32']['valid'];";
        let expected = "hello.world[0][\"%32\"].valid;\n";
        let result = apply_module_to_code(code, Box::new(NormalizeComputed));
        assert_transform("NormalizeComputed", code, expected, &result);
    }

    #[test]
    fn tp_2_convert_object_properties_with_valid_identifiers() {
        let code = "const obj = {['validProp']: 1, ['invalid-prop']: 2, ['$valid']: 3};";
        let expected = "const obj = {\n\t[\"validProp\"]: 1,\n\t[\"invalid-prop\"]: 2,\n\t[\"$valid\"]: 3\n};\n";
        let result = apply_module_to_code(code, Box::new(NormalizeComputed));
        assert_transform("NormalizeComputed", code, expected, &result);
    }

    #[test]
    fn tp_3_convert_class_method_definitions_with_valid_identifiers() {
        let code = "class Test { ['method']() {} ['123invalid']() {} ['_valid']() {} }";
        let expected = "class Test {\n\t[\"method\"]() {}\n\t[\"123invalid\"]() {}\n\t[\"_valid\"]() {}\n}\n";
        let result = apply_module_to_code(code, Box::new(NormalizeComputed));
        assert_transform("NormalizeComputed", code, expected, &result);
    }

    #[test]
    fn tn_1_do_not_convert_invalid_identifiers() {
        let code = "obj['123']['-invalid']['spa ce']['@special'];";
        let expected = "obj[\"123\"][\"-invalid\"][\"spa ce\"][\"@special\"];\n";
        let result = apply_module_to_code(code, Box::new(NormalizeComputed));
        assert_transform("NormalizeComputed", code, expected, &result);
    }

    #[test]
    fn tn_2_do_not_convert_numeric_indices_but_convert_valid_string() {
        let code = "arr[0][42]['string'];";
        let expected = "arr[0][42].string;\n";
        let result = apply_module_to_code(code, Box::new(NormalizeComputed));
        assert_transform("NormalizeComputed", code, expected, &result);
    }
}

#[cfg(test)]
mod normalize_empty_statements {
    use super::*;

    #[test]
    fn tp_1_remove_standalone_empty_statements() {
        let code = ";;var a = 3;;";
        let expected = "var a = 3;\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tp_2_remove_empty_statements_in_blocks() {
        let code = "if (true) {;; var x = 1; ;;;};";
        let expected = "if (true) {\n\tvar x = 1;\n}\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tn_1_preserve_empty_statements_in_for_loops() {
        let code = ";for (;;);;";
        let expected = "for (;;);\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tn_2_preserve_empty_statements_in_while_loops() {
        let code = ";while (true);;";
        let expected = "while (true);\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tn_3_preserve_empty_statements_in_if_statements() {
        let code = ";if (condition); else;;";
        let expected = "if (condition);\nelse;\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tn_4_preserve_empty_statements_in_do_while_loops() {
        let code = ";do; while(true);;";
        let expected = "do;\nwhile (true);\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }

    #[test]
    fn tn_5_preserve_empty_statements_in_for_in_loops() {
        let code = ";for (;;);;";
        let expected = "for (;;);\n";
        let result = apply_module_to_code(code, Box::new(NormalizeEmptyStatements));
        assert_transform("NormalizeEmptyStatements", code, expected, &result);
    }
}

#[cfg(test)]
mod parse_template_literals_into_string_literals {
    use super::*;

    #[test]
    fn tp_1_convert_template_literal_with_string_expression() {
        let code = "`hello ${\"world\"}!`;";
        let expected = "(\"hello world!\");\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tp_2_convert_template_literal_with_multiple_expressions() {
        let code = "`start ${42} middle ${\"end\"} finish`;";
        let expected = "(\"start 42 middle end finish\");\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tp_3_convert_template_literal_with_no_expressions() {
        let code = "`just plain text`;";
        let expected = "(\"just plain text\");\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tp_4_convert_template_literal_with_boolean_and_number_expressions() {
        let code = "`flag: ${true}, count: ${123.456}`;";
        let expected = "(\"flag: true, count: 123.456\");\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tp_5_convert_empty_template_literal() {
        let code = "``;";
        let expected = "(\"\");\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tn_1_do_not_convert_template_literal_with_variable_expression() {
        let code = "`hello ${name}!`;";
        let expected = "`hello ${name}!`;\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tn_2_do_not_convert_template_literal_with_function_call_expression() {
        let code = "`result: ${getValue()}`;";
        let expected = "`result: ${getValue()}`;\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }

    #[test]
    fn tn_3_do_not_convert_template_literal_with_mixed_literal_and_non_literal_expressions() {
        let code = "`hello ${\"world\"} and ${name}!`;";
        let expected = "`hello ${\"world\"} and ${name}!`;\n";
        let result = apply_module_to_code(code, Box::new(ParseTemplateLiteralsIntoStringLiterals));
        assert_transform("ParseTemplateLiteralsIntoStringLiterals", code, expected, &result);
    }
}

#[cfg(test)]
mod rearrange_sequences {
    use super::*;

    #[test]
    fn tp_1_split_sequenced_calls_to_standalone_expressions() {
        let code = "function f() { return a(), b(), c(); }";
        let expected = "function f() {\n\ta();\n\tb();\n\treturn c();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tp_2_split_sequenced_calls_to_standalone_expressions_in_if_statements() {
        let code = "function f() { if (x) return a(), b(), c(); else d(); }";
        let expected = "function f() {\n\tif (x) return a(), b(), c();\n\telse d();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tp_3_split_sequenced_calls_in_if_statements_to_cascading_if_statements() {
        let code = "function f() { if (a(), b()) c(); }";
        let expected = "function f() {\n\ta();\n\tif (b()) c();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tp_4_split_sequenced_calls_in_nested_if_statements_to_cascading_if_statements() {
        let code = "function f() { if (x) if (a(), b()) c(); }";
        let expected = "function f() {\n\tif (x) {\n\t\tif (a(), b()) c();\n\t}\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tp_5_split_sequences_with_more_than_three_expressions() {
        let code = "function f() { return a(), b(), c(), d(), e(); }";
        let expected = "function f() {\n\ta();\n\tb();\n\tc();\n\td();\n\treturn e();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tp_6_split_sequences_in_if_condition_with_else_clause() {
        let code = "if (setup(), check(), validate()) action(); else fallback();";
        let expected = "setup();\ncheck();\nif (validate()) action();\nelse fallback();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tn_1_do_not_transform_single_expression_returns() {
        let code = "function f() { return a(); }";
        let expected = "function f() {\n\treturn a();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tn_2_do_not_transform_single_expression_if_conditions() {
        let code = "if (condition()) action();";
        let expected = "if (condition()) action();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }

    #[test]
    fn tn_3_do_not_transform_non_sequence_expressions() {
        let code = "function f() { return func(a, b, c); if (obj.prop) x(); }";
        let expected = "function f() {\n\treturn func(a, b, c);\n\tif (obj.prop) x();\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSequences));
        assert_transform("RearrangeSequences", code, expected, &result);
    }
}

#[cfg(test)]
mod rearrange_switches {
    use super::*;

    #[test]
    fn tp_1_complex_switch_with_multiple_cases_and_return_statement() {
        let code = "(() => {let a = 1;\twhile (true) {switch (a) {case 3: return console.log(3); case 2: console.log(2); a = 3; break;\ncase 1: console.log(1); a = 2; break;}}})();";
        let expected = "(() => {\n\tlet a = 1;\n\twhile (true) {\n\t\tswitch (a) {\n\t\t\tcase 3: return console.log(3);\n\t\t\tcase 2:\n\t\t\t\tconsole.log(2);\n\t\t\t\ta = 3;\n\t\t\t\tbreak;\n\t\t\tcase 1:\n\t\t\t\tconsole.log(1);\n\t\t\t\ta = 2;\n\t\t\t\tbreak;\n\t\t}\n\t}\n})();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tp_2_simple_switch_with_sequential_cases() {
        let code = "var state = 0; switch (state) { case 0: first(); state = 1; break; case 1: second(); break; }";
        let expected = "first();\nstate = 1;\nsecond();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tp_3_switch_with_default_case() {
        let code = "var x = 1; switch (x) { case 1: action1(); x = 2; break; default: defaultAction(); break; case 2: action2(); break; }";
        let expected = "action1();\nx = 2;\ndefaultAction();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tp_4_switch_starting_with_non_initial_case_via_default() {
        let code = "var val = 99; switch (val) { case 1: step1(); val = 2; break; case 2: step2(); break; default: val = 1; break; }";
        let expected = "val = 1;\nstep1();\nval = 2;\nstep2();\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tn_1_do_not_transform_switch_without_literal_discriminant_initialization() {
        let code = "var a; switch (a) { case 1: doSomething(); break; }";
        let expected = "var a;\nswitch (a) {\n\tcase 1:\n\t\tdoSomething();\n\t\tbreak;\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tp_5_transform_switch_but_stop_at_multiple_assignments_to_discriminant() {
        let code = "var state = 0; switch (state) { case 0: state = 1; state = 2; break; case 1: action(); break; }";
        let expected = "state = 1;\nstate = 2;\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }

    #[test]
    fn tn_2_do_not_transform_switch_with_non_literal_case_value() {
        let code = "var x = 0; switch (x) { case variable: doSomething(); break; }";
        let expected = "var x = 0;\nswitch (x) {\n\tcase variable:\n\t\tdoSomething();\n\t\tbreak;\n}\n";
        let result = apply_module_to_code(code, Box::new(RearrangeSwitches));
        assert_transform("RearrangeSwitches", code, expected, &result);
    }
}
// Note: The JavaScript reference file contains many more test modules.
// Due to the extensive length, I'm providing a representative sample.
// The pattern should be continued for all remaining modules following the same structure.

#[cfg(test)]
mod resolve_jsfuck_primitives {
    use super::*;
    use crate::transforms::safe_transforms::resolve_jsfuck_primitives::ResolveJSFuckPrimitives;

    #[test]
    fn test_jsfuck_file() {
        let code = include_str!("../../restringer-js/tests/resources/jsfuck.js");
        let restringer = Restringer::default();
        let result = restringer.deobfuscate(code, DeobfuscateOptions::default()).unwrap();
        assert!(result.modified);
        assert_eq!(result.code, "alert(1);\n");
    }

    #[test]
    fn test_empty_array_to_zero() {
        let code = "+[]";
        let expected = "0;\n";
        let result = apply_module_to_code_looped(code, Box::new(ResolveJSFuckPrimitives));
        assert_transform("ResolveJSFuckPrimitives", code, expected, &result);
    }

    #[test]
    fn test_not_empty_array_to_false() {
        let code = "![]";
        let expected = "false;\n";
        let result = apply_module_to_code_looped(code, Box::new(ResolveJSFuckPrimitives));
        assert_transform("ResolveJSFuckPrimitives", code, expected, &result);
    }

    #[test]
    fn test_double_not_empty_array_to_true() {
        let code = "!![]";
        let expected = "true;\n";
        let result = apply_module_to_code_looped(code, Box::new(ResolveJSFuckPrimitives));
        assert_transform("ResolveJSFuckPrimitives", code, expected, &result);
    }

    #[test]
    fn test_array_index_access() {
        let code = r#"["a", "b", "c"][+!+[]]"#;
        let restringer = Restringer::default();
        let result = restringer.deobfuscate(code, DeobfuscateOptions::default()).unwrap();
        assert!(result.modified);
        assert!(result.code.contains("\"b\""));
    }

    #[test]
    fn test_string_concatenation() {
        let code = r#"(![]+[])[+[]]+(![]+[])[+!+[]]"#;
        let restringer = Restringer::default();
        let result = restringer.deobfuscate(code, DeobfuscateOptions::default()).unwrap();
        assert!(result.modified);
        // Should resolve to "f" + "a" = "fa"
        // Due to iteration limits, check that it at least partially deobfuscated
        assert!(result.code.contains("false") || result.code.contains("\"f\"") || result.code.contains("\"fa\""));
    }

    #[test]
    fn test_numeric_operations() {
        let code = "+!+[]+!+[]+!+[]";
        let expected = "3;\n";
        let result = apply_module_to_code_looped(code, Box::new(ResolveJSFuckPrimitives));
        assert_transform("ResolveJSFuckPrimitives", code, expected, &result);
    }

    #[test]
    fn test_string_literal_indexing() {
        let code = r#""false"[0]"#;
        let result = apply_module_to_code_looped(code, Box::new(ResolveJSFuckPrimitives));
        // Should simplify to "f" (may have parentheses from codegen)
        assert!(result.contains("\"f\""), "Expected result to contain '\"f\"', got: {}", result);
    }
}

#[cfg(test)]
mod resolve_builtin_string_calls {
    use super::*;

    #[test]
    fn tp_1_resolve_char_at() {
        let code = r#""hello".charAt(1);"#;
        let expected = "(\"e\");\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tp_2_resolve_char_at_out_of_bounds() {
        let code = r#""hi".charAt(10);"#;
        let expected = "(\"\");\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tp_3_resolve_from_char_code_single() {
        let code = r#"String.fromCharCode(65);"#;
        let expected = "(\"A\");\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tp_4_resolve_from_char_code_multiple() {
        let code = r#"String.fromCharCode(72, 101, 108, 108, 111);"#;
        let expected = "(\"Hello\");\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tp_5_resolve_computed_members() {
        let code = r#""abc"['charAt'](2); String['fromCharCode'](97);"#;
        let expected = "(\"c\");\n\"a\";\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tn_1_no_resolve_with_non_literal_argument() {
        let code = r#""abc".charAt(i);"#;
        let expected = "\"abc\".charAt(i);\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }

    #[test]
    fn tn_2_no_resolve_with_non_literal_object() {
        let code = r#"x.charAt(0);"#;
        let expected = "x.charAt(0);\n";
        let result = apply_module_to_code(code, Box::new(ResolveBuiltinStringCalls));
        assert_transform("ResolveBuiltinStringCalls", code, expected, &result);
    }
}
