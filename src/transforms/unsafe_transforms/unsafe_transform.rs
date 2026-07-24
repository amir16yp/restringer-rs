use super::js_runtime::JsEvaluator;
use crate::Transform;
use oxc_ast::ast::*;

pub trait UnsafeTransform: Transform {
    fn evaluator(&self) -> &JsEvaluator;
    
    fn eval_expression_to_string(&self, expr: &Expression) -> Result<String, String> {
        let code = self.expression_to_code(expr)?;
        self.evaluator().eval_to_string(&code)
    }
    
    fn eval_expression_to_number(&self, expr: &Expression) -> Result<f64, String> {
        let code = self.expression_to_code(expr)?;
        self.evaluator().eval_to_number(&code)
    }
    
    fn eval_expression_to_bool(&self, expr: &Expression) -> Result<bool, String> {
        let code = self.expression_to_code(expr)?;
        self.evaluator().eval_to_bool(&code)
    }
    
    fn expression_to_code(&self, expr: &Expression) -> Result<String, String> {
        use oxc_codegen::{Codegen, CodegenOptions};
        use oxc_allocator::Allocator;
        
        let allocator = Allocator::default();
        let mut program = Program {
            node_id: std::cell::Cell::new(oxc_syntax::node::NodeId::DUMMY),
            span: oxc_span::SPAN,
            source_type: oxc_span::SourceType::mjs(),
            source_text: "",
            hashbang: None,
            directives: oxc_allocator::Vec::new_in(&allocator),
            body: oxc_allocator::Vec::new_in(&allocator),
            comments: oxc_allocator::Vec::new_in(&allocator),
            scope_id: std::cell::Cell::new(None),
        };
        
        use oxc_allocator::CloneIn;
        let cloned_expr = expr.clone_in(&allocator);
        
        program.body.push(Statement::ExpressionStatement(
            oxc_allocator::Box::new_in(
                ExpressionStatement {
                    node_id: std::cell::Cell::new(oxc_syntax::node::NodeId::DUMMY),
                    span: oxc_span::SPAN,
                    expression: cloned_expr,
                },
                &allocator,
            )
        ));
        
        let codegen_result = Codegen::new()
            .with_options(CodegenOptions::default())
            .build(&program);

        let mut code = codegen_result.code;
        let trimmed = code.trim_end();
        code.truncate(trimmed.len());
        if code.ends_with(';') {
            code.pop();
        }

        Ok(code)
    }
}

pub struct UnsafeTransformHelper {
    evaluator: JsEvaluator,
}

impl UnsafeTransformHelper {
    pub fn new() -> Self {
        Self {
            evaluator: JsEvaluator::new(),
        }
    }
    
    pub fn evaluator(&self) -> &JsEvaluator {
        &self.evaluator
    }
}

impl Default for UnsafeTransformHelper {
    fn default() -> Self {
        Self::new()
    }
}
