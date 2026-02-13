# Obvious leftover obfuscation/deobfuscation opportunities (from `tests/app_deobf_pretty.js`)

This file documents patterns that still appear in `tests/app_deobf_pretty.js` and look like good candidates for additional (safe) transforms.

## 1) `eval()` used as dynamic identifier selection (local scope)

### Example

Around line ~16851:

- `activeArray = eval("array" + state)`
- With in-scope bindings like `array0`, `array1`, ... `array6`.

### Why it’s interesting

- This is a classic “dynamic name lookup” pattern.
- It is *usually* unnecessary and can often be rewritten without executing arbitrary code.
- It can also block downstream transforms because it hides actual references.

### Suggested safe transform

- **Name**: `replace_eval_dynamic_identifier_lookup`
- **When**:
  - `eval(arg)` where `arg` is a deterministic string expression like `"prefix" + <expr>` or template literal `` `prefix${expr}` ``.
  - The produced identifier(s) are known to exist in the same lexical scope (or can be proven to exist).
- **Rewrite ideas**:
  - If the set of targets is finite and declared nearby, rewrite to an object lookup:
    - `const _map = { array0, array1, array2, array3, array4, array5, array6 }; activeArray = _map["array" + state];`
  - Or rewrite to a `switch(state)` assigning `activeArray`.

### Notes about existing transforms

You already have:

- `replace_eval_calls_with_literal_content.rs` (covers `eval("...")` where the argument is a literal)

…but this case is *not* a literal, so it won’t trigger.

## 2) `new Function(<string>)()` / `Function(<string>)` with constructed source strings

### Examples

- Around line ~7206:
  - `return new Function(o)();`
  - where `o` is built via concatenation and `.join("")`.

- Also appears elsewhere as:
  - `Function(e)` in polyfills (e.g., `setImmediate` fallback)
  - `new Function("CTOR_LIST", "ORDER", code)(...)`

### Why it’s interesting

- Runtime code generation is frequently used in obfuscation, but it also appears in performance libraries.
- If the generated source is **fully deterministic**, it can usually be evaluated safely at transform-time.

### Suggested safe transform(s)

- **Name**: `fold_string_builder_into_literal`
  - Extend string folding beyond `"a" + "b"`:
    - `["a", x, "b"].join("")` when all parts are literals
    - `"...".concat("...")` when args are literals

- **Name**: `replace_new_function_with_literal_content_deeper`
  - Detect `new Function(<expr>)` / `Function(<expr>)` where `<expr>` becomes a string literal after constant folding.
  - Then reuse the existing logic you already have for function-constructor resolution.

### Notes about existing transforms

You already have multiple relevant transforms:

- `replace_new_func_calls_with_literal_content.rs`
- `resolve_function_constructor_calls.rs`

However, in the observed cases the source is often constructed via multiple concatenations / joins, and may not become a direct literal with current folding.

## 3) “Hidden string” / byte-array-to-string conversions (`fromCharCode`, `charCodeAt`)

### Examples

`String.fromCharCode(...)` and `charCodeAt(...)` appear frequently (e.g., around ~647, ~742, ~1056, etc.).

### Why it’s interesting

- In obfuscation, this pattern often appears as:
  - `String.fromCharCode(…numeric ops…)` building short strings
  - `arr.map(x => x ^ key)` style decode loops feeding `fromCharCode`

In this file, many hits are from Buffer/polyfill code, but it’s still a useful target if you want to deobfuscate *small deterministic sequences*.

### Suggested safe transform

- **Name**: `fold_string_from_char_codes`
- **When**:
  - `String.fromCharCode(<numbers...>)` where all args are numeric literals
  - `String.fromCharCode.apply(String, <array-literal-of-numbers>)`
  - `String.fromCharCode(...[<numbers>])`
- **Rewrite**:
  - Replace with a string literal.

### Notes about existing transforms

I didn’t see a dedicated transform for `fromCharCode` folding in `src/transforms/safe/`.

## Non-findings (good to know)

In quick scans/greps I did **not** see the classic control-flow-flattening signature:

- `while (true) { switch (...) { case ...: ... continue; } }`
- `"x|y|z".split("|")` dispatcher arrays

So the remaining “obvious” wins in this test artifact are mainly around **dynamic code execution** (`eval` / `Function`) and **more aggressive constant folding** for string-building.
