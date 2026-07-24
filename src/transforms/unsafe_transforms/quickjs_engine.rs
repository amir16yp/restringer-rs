use std::sync::Mutex;
use rquickjs::{Context, Runtime};

pub struct QuickJsEngine {
    runtime: Mutex<Runtime>,
}

impl QuickJsEngine {
    pub fn new() -> Self {
        let runtime = Runtime::new().expect("Failed to create QuickJS runtime");
        Self { runtime: Mutex::new(runtime) }
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code).map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            if result.is_string() {
                result.as_string().and_then(|s| s.to_string().ok()).ok_or_else(|| "Failed to convert to string".to_string())
            } else if result.is_number() {
                Ok(result.as_number().unwrap_or(0.0).to_string())
            } else if result.is_bool() {
                Ok(result.as_bool().unwrap_or(false).to_string())
            } else if result.is_null() {
                Ok("null".to_string())
            } else if result.is_undefined() {
                Ok("undefined".to_string())
            } else {
                ctx.json_stringify(result).ok().flatten().and_then(|s| s.to_string().ok()).ok_or_else(|| "Failed to stringify result".to_string())
            }
        })
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code).map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            if result.is_number() {
                Ok(result.as_number().unwrap_or(0.0))
            } else {
                Err("Result is not a number".to_string())
            }
        })
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code).map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            if result.is_bool() {
                Ok(result.as_bool().unwrap_or(false))
            } else {
                Err("Result is not a boolean".to_string())
            }
        })
    }

    pub fn eval_to_json(&self, code: &str) -> Result<String, String> {
        let runtime = self.runtime.lock().unwrap();
        let context = Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code).map_err(|e| {
                let detail = ctx.catch();
                format!("JavaScript evaluation error: {} ({:?})", e, detail)
            })?;

            if result.is_undefined() {
                return Err("Result is undefined".to_string());
            }
            if result.is_function() {
                return Err("Result is a function".to_string());
            }

            ctx.json_stringify(result)
                .map_err(|e| format!("Failed to stringify result: {}", e))?
                .and_then(|s| s.to_string().ok())
                .ok_or_else(|| "Failed to stringify result".to_string())
        })
    }
}
