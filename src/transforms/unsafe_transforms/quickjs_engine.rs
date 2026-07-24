use rquickjs::{Context, Runtime};
use std::sync::Mutex;

use super::engine::log_eval_result;

pub struct QuickJsEngine {
    runtime: Mutex<Runtime>,
}

impl QuickJsEngine {
    pub fn new() -> Self {
        let runtime = Runtime::new().expect("Failed to create QuickJS runtime");
        Self {
            runtime: Mutex::new(runtime),
        }
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        let runtime = self.runtime.lock().unwrap();
        let context =
            Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx
                .eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            let value = if result.is_string() {
                result
                    .as_string()
                    .and_then(|s| s.to_string().ok())
                    .ok_or_else(|| "Failed to convert to string".to_string())?
            } else if result.is_number() {
                result.as_number().unwrap_or(0.0).to_string()
            } else if result.is_bool() {
                result.as_bool().unwrap_or(false).to_string()
            } else if result.is_null() {
                "null".to_string()
            } else if result.is_undefined() {
                "undefined".to_string()
            } else {
                ctx.json_stringify(result)
                    .ok()
                    .flatten()
                    .and_then(|s| s.to_string().ok())
                    .ok_or_else(|| "Failed to stringify result".to_string())?
            };
            log_eval_result("quickjs", "eval_to_string", code, &Ok(value.clone()));
            Ok(value)
        })
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        let runtime = self.runtime.lock().unwrap();
        let context =
            Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx
                .eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            if result.is_number() {
                let value = result.as_number().unwrap_or(0.0);
                log_eval_result("quickjs", "eval_to_number", code, &Ok(value.to_string()));
                Ok(value)
            } else {
                let err = "Result is not a number".to_string();
                log_eval_result("quickjs", "eval_to_number", code, &Err(err.clone()));
                Err(err)
            }
        })
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        let runtime = self.runtime.lock().unwrap();
        let context =
            Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx
                .eval(code)
                .map_err(|e| format!("JavaScript evaluation error: {}", e))?;

            if result.is_bool() {
                let value = result.as_bool().unwrap_or(false);
                log_eval_result("quickjs", "eval_to_bool", code, &Ok(value.to_string()));
                Ok(value)
            } else {
                let err = "Result is not a boolean".to_string();
                log_eval_result("quickjs", "eval_to_bool", code, &Err(err.clone()));
                Err(err)
            }
        })
    }

    pub fn eval_to_json(&self, code: &str) -> Result<String, String> {
        let runtime = self.runtime.lock().unwrap();
        let context =
            Context::full(&runtime).map_err(|e| format!("Failed to create context: {}", e))?;

        context.with(|ctx| {
            let result: rquickjs::Value = ctx.eval(code).map_err(|e| {
                let detail = ctx.catch();
                let err = format!("JavaScript evaluation error: {} ({:?})", e, detail);
                log_eval_result("quickjs", "eval_to_json", code, &Err(err.clone()));
                err
            })?;

            if result.is_undefined() {
                let err = "Result is undefined".to_string();
                log_eval_result("quickjs", "eval_to_json", code, &Err(err.clone()));
                return Err(err);
            }
            if result.is_function() {
                // Get the function source via Function.prototype.toString()
                let to_string: rquickjs::Function = ctx
                    .eval("(function(f) { return f.toString(); })")
                    .map_err(|e| format!("Failed to create toString helper: {}", e))?;
                let src: String = to_string
                    .call((result,))
                    .map_err(|e| format!("Failed to call toString on function: {}", e))?;
                if src.contains("[native code]") {
                    let err = "Result is a native function".to_string();
                    log_eval_result("quickjs", "eval_to_json", code, &Err(err.clone()));
                    return Err(err);
                }
                log_eval_result("quickjs", "eval_to_json", code, &Ok(src.clone()));
                return Ok(src);
            }

            let value = ctx
                .json_stringify(result)
                .map_err(|e| format!("Failed to stringify result: {}", e))?
                .and_then(|s| s.to_string().ok())
                .ok_or_else(|| "Failed to stringify result".to_string())?;
            log_eval_result("quickjs", "eval_to_json", code, &Ok(value.clone()));
            Ok(value)
        })
    }
}
