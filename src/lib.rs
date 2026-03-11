use std::{path::PathBuf, time::{Duration, Instant}};

use oxc_allocator::Allocator;
use oxc_codegen::{Codegen, CodegenOptions, CodegenReturn};
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

mod transforms;

#[cfg(test)]
mod tests {
    use super::*;

    fn run_one_transform<T: Transform>(transform: T, source_text: &str) -> (bool, String, String) {
        let allocator = Allocator::default();
        let source_type = SourceType::mjs();
        let parse_ret = Parser::new(&allocator, source_text, source_type)
            .with_options(ParseOptions { parse_regular_expression: true, ..ParseOptions::default() })
            .parse();
        assert!(parse_ret.errors.is_empty(), "parse failed");

        let mut program = parse_ret.program;
        let mut ctx = TransformCtx { allocator: &allocator, source_text, source_type };

        let CodegenReturn { code: before, .. } = Codegen::new().build(&program);
        let modified = transform.run(&mut ctx, &mut program);

        let CodegenReturn { code: after, .. } = Codegen::new().build(&program);

        println!("=== {} ===\n-- before --\n{}\n\n-- after --\n{}\n", transform.name(), before, after);
        (modified, before, after)
    }

    #[test]
    fn normalize_void0_example() {
        let (modified, before, after) =
            run_one_transform(transforms::safe::normalize_void0::NormalizeVoid0, "var x = void 0;");
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("undefined"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn simplify_double_negation_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::simplify_double_negation::SimplifyDoubleNegation,
            "if (!!x) { a(); }",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(
            after.contains("if (x") || after.contains("if(x"),
            "before:\n{before}\n\nafter:\n{after}\n"
        );
    }

    #[test]
    fn fold_string_concatenation_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::fold_string_concatenation::FoldStringConcatenation,
            "var s = 'a' + 'b';",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("ab"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn parse_template_literals_into_string_literals_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::parse_template_literals_into_string_literals::ParseTemplateLiteralsIntoStringLiterals,
            "var s = `ab`;",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("ab"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn normalize_computed_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::normalize_computed::NormalizeComputed,
            "var o = { a: 1 }; o['a'];",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("o.a"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn normalize_empty_statements_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::normalize_empty_statements::NormalizeEmptyStatements,
            ";;a();;;",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(!after.contains(";;;"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn rearrange_sequences_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::rearrange_sequences::RearrangeSequences,
            "if (a(), b()) { c(); }",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a();") || after.contains("a()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("if (b()") || after.contains("if(b()"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn separate_chained_declarators_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::separate_chained_declarators::SeparateChainedDeclarators,
            "var a=1,b=2;",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("var a"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("var b"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn replace_sequences_with_expressions_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::replace_sequences_with_expressions::ReplaceSequencesWithExpressions,
            "a(), b();",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("b()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(!after.contains(","), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn resolve_deterministic_if_statements_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::resolve_deterministic_if_statements::ResolveDeterministicIfStatements,
            "if (true) { a(); } else { b(); }",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(!after.contains("b()"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn simplify_jsfuck_booleans_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::simplify_jsfuck_booleans::SimplifyJsFuckBooleans,
            "if (![]) { a(); }",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("false"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn fold_string_from_char_codes_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::fold_string_from_char_codes::FoldStringFromCharCodes,
            "var s = String.fromCharCode(97, 98);",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("ab"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn resolve_function_constructor_calls_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::resolve_function_constructor_calls::ResolveFunctionConstructorCalls,
            "var f = Function.constructor('a', 'return a+1');",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("function"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("return"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn replace_function_return_this_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::replace_function_return_this::ReplaceFunctionReturnThis,
            "var x = Function('return this')();",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("globalThis"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn unwrap_iifes_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::unwrap_iifes::UnwrapIIFEs,
            "(function(){a();})();",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a()"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(!after.contains("function"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn unwrap_iife_returning_identifier_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::unwrap_iife_returning_identifier::UnwrapIifeReturningIdentifier,
            "var x=(function(){return y})();",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("var x") && after.contains("y"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn unwrap_bind_null_literal_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::unwrap_bind_null_literal::UnwrapBindNullLiteral,
            "var f=function(x){return x}.bind(null,1); f();",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("1"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn normalize_webpack_require_var_to_const_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::normalize_webpack_require_var_to_const::NormalizeWebpackRequireVarToConst,
            "var a = __webpack_require__(0);",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("const"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn resolve_proxy_variables_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::resolve_proxy_variables::ResolveProxyVariables,
            "const b = a; b;",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a"), "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("a;"), "before:\n{before}\n\nafter:\n{after}\n");
    }

    #[test]
    fn resolve_member_expression_references_to_array_index_example() {
        let (modified, before, after) = run_one_transform(
            transforms::safe::resolve_member_expression_references_to_array_index::ResolveMemberExpressionReferencesToArrayIndex,
            "const a=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]; a[20];",
        );
        assert!(modified, "before:\n{before}\n\nafter:\n{after}\n");
        assert!(after.contains("20"), "before:\n{before}\n\nafter:\n{after}\n");
    }
}

pub struct Restringer {
    normalize: bool,
    max_iterations: usize,
    parse_options: ParseOptions,
    codegen_options: CodegenOptions,
    safe_transforms: Vec<Box<dyn Transform>>,
    unsafe_transforms: Vec<Box<dyn Transform>>,
}

impl Default for Restringer {
    fn default() -> Self {
        Self {
            normalize: true,
            max_iterations: 100,
            parse_options: ParseOptions { parse_regular_expression: true, ..ParseOptions::default() },
            codegen_options: CodegenOptions::default(),
            safe_transforms: vec![
                Box::new(transforms::safe::rearrange_sequences::RearrangeSequences),
                Box::new(transforms::safe::separate_chained_declarators::SeparateChainedDeclarators),
                Box::new(transforms::safe::rearrange_switches::RearrangeSwitches),
                Box::new(transforms::safe::resolve_proxy_variables::ResolveProxyVariables),
                Box::new(transforms::safe::resolve_proxy_calls::ResolveProxyCalls),
                Box::new(transforms::safe::resolve_proxy_references::ResolveProxyReferences),
                Box::new(transforms::safe::resolve_string_array_decoder_calls::ResolveStringArrayDecoderCalls),
                Box::new(transforms::safe::resolve_dispatch_table_calls::ResolveDispatchTableCalls),
                Box::new(transforms::safe::normalize_webpack_require_var_to_const::NormalizeWebpackRequireVarToConst),
                Box::new(transforms::safe::replace_function_return_this::ReplaceFunctionReturnThis),
                Box::new(
                    transforms::safe::resolve_member_expression_references_to_array_index::ResolveMemberExpressionReferencesToArrayIndex,
                ),
                Box::new(
                    transforms::safe::resolve_member_expressions_with_direct_assignment::ResolveMemberExpressionsWithDirectAssignment,
                ),
                Box::new(transforms::safe::normalize_computed::NormalizeComputed),
                Box::new(transforms::safe::normalize_empty_statements::NormalizeEmptyStatements),
                Box::new(transforms::safe::normalize_void0::NormalizeVoid0),
                Box::new(transforms::safe::simplify_jsfuck_booleans::SimplifyJsFuckBooleans),
                Box::new(transforms::safe::simplify_double_negation::SimplifyDoubleNegation),
                Box::new(transforms::safe::remove_redundant_block_statements::RemoveRedundantBlockStatements),
                Box::new(transforms::safe::resolve_redundant_logical_expressions::ResolveRedundantLogicalExpressions),
                Box::new(transforms::safe::unwrap_simple_operations::UnwrapSimpleOperations),
                Box::new(transforms::safe::fold_string_concatenation::FoldStringConcatenation),
                Box::new(transforms::safe::fold_string_from_char_codes::FoldStringFromCharCodes),
                Box::new(
                    transforms::safe::parse_template_literals_into_string_literals::ParseTemplateLiteralsIntoStringLiterals,
                ),
                Box::new(transforms::safe::resolve_deterministic_if_statements::ResolveDeterministicIfStatements),
                Box::new(
                    transforms::safe::replace_boolean_expressions_with_if::ReplaceBooleanExpressionsWithIf,
                ),
                Box::new(
                    transforms::safe::replace_sequences_with_expressions::ReplaceSequencesWithExpressions,
                ),
                Box::new(transforms::safe::resolve_function_constructor_calls::ResolveFunctionConstructorCalls),
                Box::new(transforms::safe::simplify_module_factory_call::SimplifyModuleFactoryCall),
                Box::new(transforms::safe::unwrap_bind_null_literal::UnwrapBindNullLiteral),
                Box::new(transforms::safe::simplify_calls::SimplifyCalls),
                Box::new(
                    transforms::safe::replace_call_expressions_with_unwrapped_identifier::ReplaceCallExpressionsWithUnwrappedIdentifier,
                ),
                Box::new(
                    transforms::safe::replace_eval_calls_with_literal_content::ReplaceEvalCallsWithLiteralContent,
                ),
                Box::new(
                    transforms::safe::replace_new_func_calls_with_literal_content::ReplaceNewFuncCallsWithLiteralContent,
                ),
                Box::new(transforms::safe::replace_identifier_with_fixed_assigned_value::ReplaceIdentifierWithFixedAssignedValue),
                Box::new(transforms::safe::unwrap_iife_returning_identifier::UnwrapIifeReturningIdentifier),
                Box::new(transforms::safe::simplify_babel_class_helpers::SimplifyBabelClassHelpers),
                Box::new(transforms::safe::simplify_babel_spread_helper::SimplifyBabelSpreadHelper),
                Box::new(transforms::safe::simplify_babel_es5_class_to_class::SimplifyBabelEs5ClassToClass),
                Box::new(
                    transforms::safe::replace_identifier_with_fixed_value_not_assigned_at_declaration::ReplaceIdentifierWithFixedValueNotAssignedAtDeclaration,
                ),
                Box::new(
                    transforms::safe::replace_function_shells_with_wrapped_value::ReplaceFunctionShellsWithWrappedValue,
                ),
                Box::new(
                    transforms::safe::replace_function_shells_with_wrapped_value_iife::ReplaceFunctionShellsWithWrappedValueIIFE,
                ),
                Box::new(transforms::safe::unwrap_function_shells::UnwrapFunctionShells),
                Box::new(transforms::safe::unwrap_iifes::UnwrapIIFEs),
                Box::new(transforms::safe::simplify_if_statements::SimplifyIfStatements),
            ],
            unsafe_transforms: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeobfuscateOptions {
    pub clean: bool,
    pub run_unsafe: bool,
    pub max_iterations: Option<usize>,
    pub timeout: Option<Duration>,
    pub source_type: Option<SourceType>,
    pub filename_for_source_type: Option<PathBuf>,
}

impl Default for DeobfuscateOptions {
    fn default() -> Self {
        Self {
            clean: false,
            run_unsafe: false,
            max_iterations: None,
            timeout: None,
            source_type: None,
            filename_for_source_type: None,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidSourceType { path: PathBuf, message: String },
    ParseFailed,
    Timeout,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidSourceType { path, message } => {
                write!(f, "Failed to determine source type for {}: {}", path.display(), message)
            }
            Error::ParseFailed => write!(f, "Parsing failed"),
            Error::Timeout => write!(f, "Operation timed out"),
        }
    }
}

impl std::error::Error for Error {}

pub struct DeobfuscateResult {
    pub modified: bool,
    pub code: String,
}

pub trait Transform {
    fn name(&self) -> &'static str;

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut oxc_ast::ast::Program<'a>) -> bool;
}

pub struct TransformCtx<'a> {
    pub allocator: &'a Allocator,
    pub source_text: &'a str,
    pub source_type: SourceType,
}

impl Restringer {
    pub fn set_max_iterations(&mut self, max_iterations: usize) {
        self.max_iterations = max_iterations;
    }

    pub fn max_iterations(&self) -> usize {
        self.max_iterations
    }

    pub fn deobfuscate(&self, source_text: &str, opts: DeobfuscateOptions) -> Result<DeobfuscateResult, Error> {
        let allocator = Allocator::default();
        let start = Instant::now();

        let mut check_timeout = || -> Result<(), Error> {
            if let Some(timeout) = opts.timeout {
                if start.elapsed() >= timeout {
                    return Err(Error::Timeout);
                }
            }
            Ok(())
        };

        let source_type = if let Some(st) = opts.source_type {
            st
        } else if let Some(path) = opts.filename_for_source_type.as_ref() {
            SourceType::from_path(path)
                .map_err(|e| Error::InvalidSourceType { path: path.clone(), message: e.to_string() })?
        } else {
            SourceType::mjs()
        };

        let max_iterations = opts.max_iterations.unwrap_or(self.max_iterations);

        check_timeout()?;

        let parse_ret = Parser::new(&allocator, source_text, source_type)
            .with_options(self.parse_options)
            .parse();

        if !parse_ret.errors.is_empty() {
            return Err(Error::ParseFailed);
        }

        let mut program = parse_ret.program;
        let mut modified_any = false;

        // Milestone 2: implement loop semantics, even if there are no transforms yet.
        let mut modified_this_round;
        loop {
            check_timeout()?;
            modified_this_round = false;

            // Phase 1: apply safe transforms iteratively up to max_iterations.
            if !self.safe_transforms.is_empty() {
                for _ in 0..max_iterations {
                    check_timeout()?;
                    let mut modified_iter = false;
                    let mut ctx = TransformCtx { allocator: &allocator, source_text, source_type };
                    for t in &self.safe_transforms {
                        check_timeout()?;
                        if t.run(&mut ctx, &mut program) {
                            modified_iter = true;
                        }
                    }
                    if modified_iter {
                        modified_this_round = true;
                        modified_any = true;
                    } else {
                        break;
                    }
                }
            }

            // Phase 2: apply unsafe transforms exactly once.
            if opts.run_unsafe && !self.unsafe_transforms.is_empty() {
                check_timeout()?;
                let mut modified_iter = false;
                let mut ctx = TransformCtx { allocator: &allocator, source_text, source_type };
                for t in &self.unsafe_transforms {
                    check_timeout()?;
                    if t.run(&mut ctx, &mut program) {
                        modified_iter = true;
                    }
                }
                if modified_iter {
                    modified_this_round = true;
                    modified_any = true;
                }
            }

            if !modified_this_round {
                break;
            }
        }

        // TODO (later milestones): clean pass + normalize pass.
        let _ = opts.clean;
        let _ = self.normalize;

        check_timeout()?;

        let CodegenReturn { code, .. } = Codegen::new()
            .with_options(self.codegen_options.clone())
            .with_source_text(source_text)
            .build(&program);

        Ok(DeobfuscateResult { modified: modified_any, code })
    }
}
