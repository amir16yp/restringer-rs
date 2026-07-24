use oxc_parser::Parser;
use regex::Regex;

use super::helpers::EVAL_PRELUDE;
use super::js_runtime::JsEvaluator;
use super::unsafe_transform::UnsafeTransform;
use crate::{Transform, TransformCtx};

pub struct ResolveCaesarPlus {
    evaluator: JsEvaluator,
}

impl ResolveCaesarPlus {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
}

impl Default for ResolveCaesarPlus {
    fn default() -> Self {
        Self::new()
    }
}

impl Transform for ResolveCaesarPlus {
    fn name(&self) -> &'static str {
        "resolveCaesarPlus"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut oxc_ast::ast::Program<'a>) -> bool {
        let source = ctx.source_text;

        // The Caesar+ outer layer builds an inner layer string using DOM APIs,
        // base64-decodes it, assigns it to an object's toString method, and then
        // triggers execution by concatenating the object with a string.
        // We locate the final assignment/concatenation line and the inner-layer
        // variable/expression so we can evaluate the IIFE and extract the payload.
        let final_assignment_re = Regex::new(
            r#"(?ms)(\w+)\s*\[.*?\]\s*=.*?\((\w+)\).*?=\s*(\w+)\s*\+\s*['\"]"#,
        )
        .unwrap();
        let Some(final_caps) = final_assignment_re.captures(source) else {
            return false;
        };
        let assigned_var = final_caps.get(1).map(|m| m.as_str()).unwrap_or("");
        let concat_var = final_caps.get(3).map(|m| m.as_str()).unwrap_or("");
        if assigned_var.is_empty() || assigned_var != concat_var {
            return false;
        }
        let final_match = final_caps.get(0).unwrap();

        let tail = &source[final_match.start()..];
        let inner_var_re = Regex::new(r#"\(((?:\w+\()+\w+\)*)\)"#).unwrap();
        let Some(inner_match) = inner_var_re.captures(tail) else {
            return false;
        };
        let inner_var = inner_match.get(0).map(|m| m.as_str()).unwrap_or("");
        if inner_var.is_empty() {
            return false;
        }

        // Replace the tail of the script so the IIFE returns the inner layer's
        // string instead of executing it.
        let modified = format!(
            "{}return {}.toString();}})();",
            &source[..final_match.start()],
            inner_var
        );

        let eval_code = format!(
            "{}\nvar window = globalThis;\nvar document = {{ createElement: function() {{ return {{ innerHTML: '' }}; }} }};\nvar Buffer = {{ from: function(str, enc) {{ return {{ toString: function() {{ return atob(str); }} }}; }} }};\n{}",
            EVAL_PRELUDE, modified
        );

        let inner_source = match self.evaluator.eval_to_string(&eval_code) {
            Ok(s) => s,
            Err(_) => return false,
        };

        // Allocate the extracted source in the same allocator so the parsed
        // program has the required lifetime.
        let inner_source_ref = ctx.allocator.alloc_str(&inner_source);
        let parse_ret = Parser::new(ctx.allocator, inner_source_ref, ctx.source_type).parse();
        if !parse_ret.errors.is_empty() {
            return false;
        }
        if parse_ret.program.body.is_empty() {
            return false;
        }

        *program = parse_ret.program;
        true
    }
}

impl UnsafeTransform for ResolveCaesarPlus {
    fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}
