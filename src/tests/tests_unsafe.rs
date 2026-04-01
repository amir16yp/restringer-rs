use crate::{DeobfuscateOptions, Restringer};
use crate::transforms::unsafe_transforms::eval_constant_expressions::EvalConstantExpressions;
use crate::transforms::unsafe_transforms::js_runtime::JsEvaluator;

fn apply_module_to_code(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer.apply_module_to_code(code, transform, DeobfuscateOptions::default()).unwrap()
}

fn apply_module_to_code_looped(code: &str, transform: Box<dyn crate::Transform>) -> String {
    let restringer = Restringer::default();
    restringer.apply_modules_to_code(code, vec![transform], DeobfuscateOptions::default()).unwrap()
}

fn assert_transform(transform_name: &str, input: &str, expected: &str, actual: &str) {
    println!("\n## Unsafe Transform: `{}`\n", transform_name);
    println!("### Input\n```javascript\n{}\n```\n", input);
    println!("### Expected\n```javascript\n{}\n```\n", expected);
    println!("### Actual\n```javascript\n{}\n```\n", actual);
    assert_eq!(actual, expected);
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
        let result = evaluator.eval_to_number("Math.pow(2, 3) + Math.sqrt(16)").unwrap();
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
