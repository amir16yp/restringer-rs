use std::path::PathBuf;

use oxc_allocator::Allocator;
use oxc_codegen::{Codegen, CodegenOptions, CodegenReturn};
use oxc_parser::{ParseOptions, Parser};
use oxc_span::SourceType;

mod transforms;

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
            max_iterations: 50,
            parse_options: ParseOptions { parse_regular_expression: true, ..ParseOptions::default() },
            codegen_options: CodegenOptions::default(),
            safe_transforms: vec![
                Box::new(transforms::safe::rearrange_sequences::RearrangeSequences),
                Box::new(transforms::safe::separate_chained_declarators::SeparateChainedDeclarators),
                Box::new(transforms::safe::rearrange_switches::RearrangeSwitches),
                Box::new(transforms::safe::normalize_computed::NormalizeComputed),
                Box::new(transforms::safe::normalize_empty_statements::NormalizeEmptyStatements),
                Box::new(transforms::safe::remove_redundant_block_statements::RemoveRedundantBlockStatements),
                Box::new(transforms::safe::resolve_redundant_logical_expressions::ResolveRedundantLogicalExpressions),
                Box::new(transforms::safe::unwrap_simple_operations::UnwrapSimpleOperations),
                Box::new(
                    transforms::safe::parse_template_literals_into_string_literals::ParseTemplateLiteralsIntoStringLiterals,
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
    pub source_type: Option<SourceType>,
    pub filename_for_source_type: Option<PathBuf>,
}

impl Default for DeobfuscateOptions {
    fn default() -> Self {
        Self {
            clean: false,
            run_unsafe: false,
            max_iterations: None,
            source_type: None,
            filename_for_source_type: None,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    InvalidSourceType { path: PathBuf, message: String },
    ParseFailed,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidSourceType { path, message } => {
                write!(f, "Failed to determine source type for {}: {}", path.display(), message)
            }
            Error::ParseFailed => write!(f, "Parsing failed"),
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

        let source_type = if let Some(st) = opts.source_type {
            st
        } else if let Some(path) = opts.filename_for_source_type.as_ref() {
            SourceType::from_path(path)
                .map_err(|e| Error::InvalidSourceType { path: path.clone(), message: e.to_string() })?
        } else {
            SourceType::mjs()
        };

        let max_iterations = opts.max_iterations.unwrap_or(self.max_iterations);

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
            modified_this_round = false;

            // Phase 1: apply safe transforms iteratively up to max_iterations.
            if !self.safe_transforms.is_empty() {
                for _ in 0..max_iterations {
                    let mut modified_iter = false;
                    for t in &self.safe_transforms {
                        let mut ctx = TransformCtx { allocator: &allocator, source_text, source_type };
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
                let mut modified_iter = false;
                for t in &self.unsafe_transforms {
                    let mut ctx = TransformCtx { allocator: &allocator, source_text, source_type };
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

        let CodegenReturn { code, .. } = Codegen::new()
            .with_options(self.codegen_options.clone())
            .with_source_text(source_text)
            .build(&program);

        Ok(DeobfuscateResult { modified: modified_any, code })
    }
}
