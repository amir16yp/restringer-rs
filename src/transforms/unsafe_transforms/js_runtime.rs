use rquickjs::{Context, Runtime};
use std::sync::Mutex;

pub struct JsEvaluator {
    runtime: Mutex<Runtime>,
}

impl JsEvaluator {
    pub fn new() -> Self {
        let runtime = Runtime::new().expect("Failed to create QuickJS runtime");
        
        Self {
            runtime: Mutex::new(runtime),
        }
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;
        
        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;
            
            if result.is_string() {
                result.as_string()
                    .and_then(|s| s.to_string().ok())
                    .ok_or_else(|| "Failed to convert to string".to_string())
            } else if result.is_number() {
                Ok(result.as_number().unwrap_or(0.0).to_string())
            } else if result.is_bool() {
                Ok(result.as_bool().unwrap_or(false).to_string())
            } else if result.is_null() {
                Ok("null".to_string())
            } else if result.is_undefined() {
                Ok("undefined".to_string())
            } else {
                ctx.json_stringify(result)
                    .ok()
                    .and_then(|v| v)
                    .and_then(|s| s.to_string().ok())
                    .ok_or_else(|| "Failed to stringify result".to_string())
            }
        })
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;
        
        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;
            
            if result.is_number() {
                Ok(result.as_number().unwrap_or(0.0))
            } else {
                Err(format!("Result is not a number"))
            }
        })
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;
        
        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;
            
            if result.is_bool() {
                Ok(result.as_bool().unwrap_or(false))
            } else {
                Err(format!("Result is not a boolean"))
            }
        })
    }
}

impl Default for JsEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_string() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("'hello' + ' ' + 'world'").unwrap();
        assert_eq!(result, "hello world");
    }

    #[test]
    fn test_eval_number() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_number("2 + 2").unwrap();
        assert_eq!(result, 4.0);
    }

    #[test]
    fn test_eval_bool() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_bool("true && false").unwrap();
        assert_eq!(result, false);
    }

    #[test]
    fn test_eval_complex_expression() {
        let evaluator = JsEvaluator::new();
        let result = evaluator.eval_to_string("[1,2,3].map(x => x * 2).join(',')").unwrap();
        assert_eq!(result, "2,4,6");
    }
}
