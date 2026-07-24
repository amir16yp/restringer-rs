use std::process::Command;

use super::engine::is_eval_verbose;

pub struct DenoEngine;

impl DenoEngine {
    fn run(&self, script: &str) -> Result<String, String> {
        let output = Command::new("deno")
            .args(["eval", "--ext=js", script])
            .output()
            .map_err(|e| format!("Failed to spawn Deno process: {}", e))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("Deno eval failed: {}", stderr.trim()));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(trim_newline(&stdout).to_string())
    }

    pub fn eval_to_string(&self, code: &str) -> Result<String, String> {
        if is_eval_verbose() {
            eprintln!("[verbose] deno eval: {}", code);
        }
        let script = format!(
            "const __v = globalThis.eval({}); console.log(String(__v));",
            escape_js_string(code)
        );
        let result = self.run(&script)?;
        if is_eval_verbose() {
            eprintln!("[verbose] deno result: {}", result);
        }
        Ok(result)
    }

    pub fn eval_to_number(&self, code: &str) -> Result<f64, String> {
        if is_eval_verbose() {
            eprintln!("[verbose] deno eval: {}", code);
        }
        let script = format!(
            "const __v = globalThis.eval({}); if (typeof __v !== 'number') {{ throw new Error('not a number'); }} console.log(JSON.stringify(__v));",
            escape_js_string(code)
        );
        let output = self.run(&script)?;
        if is_eval_verbose() {
            eprintln!("[verbose] deno result: {}", output);
        }
        output.parse::<f64>().map_err(|e| format!("Failed to parse Deno number output {:?}: {}", output, e))
    }

    pub fn eval_to_bool(&self, code: &str) -> Result<bool, String> {
        if is_eval_verbose() {
            eprintln!("[verbose] deno eval: {}", code);
        }
        let script = format!(
            "const __v = globalThis.eval({}); if (typeof __v !== 'boolean') {{ throw new Error('not a boolean'); }} console.log(JSON.stringify(__v));",
            escape_js_string(code)
        );
        let output = self.run(&script)?;
        if is_eval_verbose() {
            eprintln!("[verbose] deno result: {}", output);
        }
        output.parse::<bool>().map_err(|e| format!("Failed to parse Deno boolean output {:?}: {}", output, e))
    }

    pub fn eval_to_json(&self, code: &str) -> Result<String, String> {
        if is_eval_verbose() {
            eprintln!("[verbose] deno eval: {}", code);
        }
        let script = format!(
            "const __v = globalThis.eval({}); if (typeof __v === 'undefined') {{ throw new Error('undefined result'); }} if (typeof __v === 'function') {{ throw new Error('function result'); }} console.log(JSON.stringify(__v));",
            escape_js_string(code)
        );
        let output = self.run(&script)?;
        if is_eval_verbose() {
            eprintln!("[verbose] deno result: {}", output);
        }
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
    s.strip_suffix("\r\n").or_else(|| s.strip_suffix("\n")).unwrap_or(s)
}
