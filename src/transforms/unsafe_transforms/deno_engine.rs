use std::io::Write;
use std::process::{Command, Stdio};

use super::engine::log_eval_result;

pub struct DenoEngine;

impl DenoEngine {
    fn run(&self, script: &str) -> Result<String, String> {
        let mut child = Command::new("deno")
            .args(["run", "--ext=js", "-"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("Failed to spawn Deno process: {}", e))?;

        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| "Failed to open Deno stdin".to_string())?;
        stdin
            .write_all(script.as_bytes())
            .map_err(|e| format!("Failed to write Deno script: {}", e))?;
        drop(stdin);

        let output = child
            .wait_with_output()
            .map_err(|e| format!("Failed to read Deno output: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("Deno eval failed: {}", stderr.trim()));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(trim_newline(&stdout).to_string())
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        let script = format!(
            "const __v = globalThis.eval({}); console.log(String(__v));",
            escape_js_string(code)
        );
        let result = self.run(&script);
        log_eval_result("deno", "eval_to_string", code, &result);
        result
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        let script = format!(
            "const __v = globalThis.eval({}); if (typeof __v !== 'number') {{ throw new Error('not a number'); }} console.log(JSON.stringify(__v));",
            escape_js_string(code)
        );
        let output = self.run(&script);
        log_eval_result("deno", "eval_to_number", code, &output);
        output?
            .parse::<f64>()
            .map_err(|e| format!("Failed to parse Deno number output: {}", e))
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        let script = format!(
            "const __v = globalThis.eval({}); if (typeof __v !== 'boolean') {{ throw new Error('not a boolean'); }} console.log(JSON.stringify(__v));",
            escape_js_string(code)
        );
        let output = self.run(&script);
        log_eval_result("deno", "eval_to_bool", code, &output);
        output?
            .parse::<bool>()
            .map_err(|e| format!("Failed to parse Deno boolean output: {}", e))
    }

    pub fn eval_to_json(&self, code: &str) -> Result<String, String> {
        let script = format!(
            concat!(
                "const __v = globalThis.eval({});",
                " if (typeof __v === 'undefined') {{ throw new Error('undefined result'); }}",
                " if (typeof __v === 'function') {{",
                "   const __src = __v.toString();",
                "   if (__src.includes('[native code]')) {{ throw new Error('native function result'); }}",
                "   console.log(__src);",
                " }} else if (typeof __v === 'object' && __v !== null && !Array.isArray(__v) && Object.getPrototypeOf(__v) !== Object.prototype) {{",
                "   throw new Error('non-plain object result');",
                " }} else {{",
                "   console.log(JSON.stringify(__v));",
                " }}",
            ),
            escape_js_string(code)
        );
        let output = self.run(&script);
        log_eval_result("deno", "eval_to_json", code, &output);
        let output = output?;
        if output.is_empty() {
            return Err("Deno produced empty output".to_string());
        }
        Ok(output)
    }
}

fn escape_js_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000c}' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn trim_newline(s: &str) -> &str {
    s.strip_suffix("\r\n")
        .or_else(|| s.strip_suffix("\n"))
        .unwrap_or(s)
}
