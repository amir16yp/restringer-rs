# Rust Restringer – Missing Unsafe Transforms

## Context

This is a Rust port (`src/`) of the JavaScript deobfuscator `restringer/` (Node.js).
The JS version defines transforms in `restringer/src/modules/safe/` and `restringer/src/modules/unsafe/`.
The Rust port has 33 safe transforms in `src/transforms/safe_transforms/` (all ported, mostly correct)
but only **1 unsafe transform**: `src/transforms/unsafe_transforms/eval_constant_expressions.rs`.

The JS version has **12 unsafe modules**. 6 of them are critical gaps that cause the Rust version
to produce far worse output than JS on the real-world test samples in `src/tests/resources/`.

Evidence: run `cargo test test_deobfuscate_all_samples` to produce `test_output/*_deobf.js`,
then compare with `restringer/tests/resources/*.js-deob.js` (the JS ground truth).

---

## How unsafe transforms work in Rust

- Trait: `src/transforms/unsafe_transforms/unsafe_transform.rs` → `UnsafeTransform`
- JS runtime wrapper: `src/transforms/unsafe_transforms/js_runtime.rs` → `JsEvaluator`
  - Evaluates JS snippets via Deno/V8 (or similar). Use existing `JsEvaluator::eval_expression` etc.
- Register new transforms in `src/lib.rs` `unsafe_transforms: vec![...]` (see existing entry).
- Each transform implements `Transform` trait from `src/lib.rs`.
- The pipeline loops safe+unsafe transforms until no more changes, up to `max_iterations`.
- See `eval_constant_expressions.rs` as the reference implementation pattern.

---

## Transforms to implement (ordered by impact)

### 1. `resolve_local_calls` — **HIGHEST PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_local_calls.rs`

**What it does:** Resolves calls to locally-defined decryptor functions where all arguments
are literal values. Replaces the call expression with the return value as a literal.

**Pattern (from `newFunc.js`, `obfuscator.io.js`, `udu.js`):**
```js
// Before
function e(n, a) { var r = t(); return (e = function(t, e) { return r[t -= 494]; })(n, a); }
e(522)  // → "searchParams"
e(503)  // → "data-fiikfu"

// Also: _yb("0x1e") → "chain"  (obfuscator.io.js)
```

**Algorithm:**
1. Find all `CallExpression` nodes where the callee is a local `Identifier`.
2. Look up the function declaration/expression for that identifier in scope.
3. If ALL arguments are literals (string/number), evaluate the call via `JsEvaluator`
   by synthesizing a self-contained JS snippet: the function body + call with literal args.
4. If the result is a string/number literal, replace the `CallExpression` with that literal.
5. Guard: do not replace if the function has side effects beyond returning a value
   (i.e., skip functions that modify DOM, make HTTP requests, etc. — but a simple heuristic:
   only replace if the entire function source is under ~500 chars and contains no `document`,
   `window`, `fetch`, `XMLHttpRequest`, `eval` references).

**JS reference:** `restringer/src/modules/unsafe/resolveLocalCalls.js`

---

### 2. `resolve_augmented_function_wrapped_array_replacements` — **HIGH PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_augmented_function_wrapped_array_replacements.rs`

**What it does:** Handles the pattern where a plain array is scrambled by an immediately-invoked
array-rotation IIFE, then a decryptor function indexes into it. After detecting the pattern,
executes the rotation to produce the final array, rewrites the array declaration with the
rotated values, and removes the rotation IIFE.

**Pattern (from `newFunc.js`, `obfuscator.io.js`):**
```js
// Array declaration
function t() { var e = ["364LQAOhD", "iframe", ...]; return (t = function(){ return e; })(); }

// Rotation IIFE
(function(t, n) {
  for (var a = e, r = t();;) try {
    if (472109 === parseInt(a(497)) / 1 * ...) break;
    r.push(r.shift());
  } catch(t) { r.push(r.shift()); }
})(t);
```

**Algorithm:**
1. Find a top-level IIFE of the form `(function(arr, count) { while/for loop with push+shift })(arrRef, N)`.
2. Extract the target array (resolve `arrRef` to its declaration).
3. Extract the rotation count `N` (must be a numeric literal or simple expression).
4. Compute the rotated array by running `N` rounds of `arr.push(arr.shift())`.
5. Replace the array declaration with the rotated version.
6. Remove the rotation IIFE statement.

**JS reference:** `restringer/src/modules/unsafe/resolveAugmentedFunctionWrappedArrayReplacements.js`

---

### 3. `resolve_builtin_calls` — **HIGH PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_builtin_calls.rs`

**What it does:** Evaluates calls to known pure JS builtins where all arguments are literals.

**Builtins to handle (with literal-only args):**
- `atob("base64string")` → decoded string literal
- `btoa("plaintext")` → base64 string literal
- `String.fromCharCode(72, 101, 108, 108, 111)` → `"Hello"`
- `"string".split("")` → array literal of characters
- `"hello world".replace("world", "universe")` → `"hello universe"`
- `"hello world".replace(/world/, "universe")` → `"hello universe"` (literal regex)
- `[...].join("")` / `[...].join("x")` where array is all literals → string literal
- `"string".indexOf("sub")` → number literal
- `"string".charAt(N)` → string literal
- `"string".substring(a, b)` / `.slice(a, b)` → string literal
- `parseInt("42", 10)` → number literal
- `decodeURIComponent("encoded")` → string literal

**Guard:** Skip if the builtin name is shadowed by a local variable/function declaration.

**JS reference:** `restringer/src/modules/unsafe/resolveBuiltinCalls.js`

---

### 4. `resolve_eval_calls_on_non_literals` — **HIGH PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_eval_calls_on_non_literals.rs`

**What it does:** Resolves `eval(expr)` where `expr` is NOT already a string literal
(literal strings are already handled by the safe transform `replace_eval_calls_with_literal_content`).

**Pattern (from `evalOxd.js`):**
```js
// Before — Ox$ is a local decryptor that returns a string of JS code
eval(Ox$("...obfuscated...", 1632567))
// After (once Ox$ args are literals and can be resolved):
// First resolveLocalCalls makes Ox$(...) → "var x = ...; function y() {...}..."
// Then this transform takes eval("var x = ...") → inlines the parsed code
```

**Algorithm:**
1. Find `CallExpression` where callee is `Identifier("eval")` with exactly 1 argument.
2. Skip if argument is already a `StringLiteral` (handled by safe transform).
3. Try to evaluate the argument expression to a string (via `JsEvaluator` or by checking
   if previous transforms have reduced it to a literal).
4. If the result is a valid string, parse it as JS, and replace the `eval(...)` expression
   statement with the parsed AST nodes.

**JS reference:** `restringer/src/modules/unsafe/resolveEvalCallsOnNonLiterals.js`

---

### 5. `resolve_function_to_array` — **MEDIUM PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_function_to_array.rs`

**What it does:** Replaces calls to a function that always returns an array literal with
the array literal itself (when the call result is only used via array index access).

**Pattern:**
```js
// Before
function getArr() { return ['one', 'two', 'three']; }
let arr = getArr();
const x = arr[0]; // only array-index usage

// After
function getArr() { return ['one', 'two', 'three']; }
let arr = ['one', 'two', 'three'];
const x = arr[0];
```

**Guard:** Only replace if ALL usages of the declared variable are either:
- Array index access (`arr[N]`)
- Property access (`arr.length`)
- Not used at all

**JS reference:** `restringer/src/modules/unsafe/resolveFunctionToArray.js`

---

### 6. `resolve_injected_prototype_method_calls` — **MEDIUM PRIORITY**

**File:** `src/transforms/unsafe_transforms/resolve_injected_prototype_method_calls.rs`

**What it does:** Resolves calls on string/number literals where the method was injected
into the prototype earlier in the same file.

**Pattern (from `prototypeCalls.js`):**
```js
// Before
String.prototype.secret = function() { return 'secret ' + this; };
'hello'.secret();  // → 'secret hello'

// After
String.prototype.secret = function() { return 'secret ' + this; };
'secret hello';
```

**Algorithm:**
1. Scan for `AssignmentExpression` of the form `<Type>.prototype.<method> = function() { ... }`.
2. Store the method body keyed by `(Type, method)`.
3. Find `CallExpression` of the form `<literal>.<method>()` where `<literal>` matches `<Type>`.
4. Evaluate via `JsEvaluator` using the stored function body with `this` bound to `<literal>`.
5. Replace with the result literal.

**JS reference:** `restringer/src/modules/unsafe/resolveInjectedPrototypeMethodCalls.js`

---

### 7. Fix `resolve_jsfuck_primitives` for `[].flat` pattern — **MEDIUM PRIORITY**

**File:** `src/transforms/safe_transforms/resolve_jsfuck_primitives.rs` (existing, fix needed)

**Current state:** Rust produces 689 bytes. JS ground truth is `alert(1);`.

**Missing pattern:** Modern JSFuck uses `[].flat` instead of `[]+[]` to get `"function flat() { [native code] }"`.
The existing transform handles `![]→false`, `+[]→0`, `!![]→true` but does not handle:
- `[].flat` → `function flat() { [native code] }`
- `([][...] + [])` → `"undefined"`
- The full alphabet extraction from `[].flat + []`

**Fix:** Add resolution rules for `[].flat`, `[][Symbol.iterator]` and the resulting
string indexing patterns. Or alternatively move JSFuck to `EvalConstantExpressions` /
a new unsafe transform that calls `JsEvaluator.eval` on the entire expression.

---

## File cross-reference: which transform fixes which test file

| Test file | JS deob size | Rust deob size | Primary missing transforms |
|---|---|---|---|
| `jsfuck.js` | 9 bytes | 689 bytes | Fix #7 (`resolve_jsfuck_primitives` `[].flat`) |
| `newFunc.js` | 2060 bytes | 2205 bytes | #1 `resolveLocalCalls`, #2 array augmentation, #3 builtins (`atob`) |
| `caesar.js` | 4002 bytes | 20628 bytes | #4 `resolveEvalCallsOnNonLiterals` (base64→eval chain) |
| `obfuscator.io.js` | 5170 bytes | 7781 bytes | #1 `resolveLocalCalls`, #2 array augmentation |
| `udu.js` | 7643 bytes | 6765 bytes | #1 `resolveLocalCalls` (array values not decoded by IIFE) |
| `evalOxd.js` | 13499 bytes | 11917 bytes | #4 `resolveEvalCallsOnNonLiterals` |
| `ant.js` | 10000 bytes | 8723 bytes | #1 `resolveLocalCalls` (1 remaining `_0x1ad7[N]`) |
| `localProxies.js` | 34126 bytes | 42649 bytes | #1 `resolveLocalCalls`, #3 builtins |
| `hunter.js` | 6173 bytes | **missing** | Not in `src/tests/resources/` — add the file |

---

## How to add a new unsafe transform

1. Create `src/transforms/unsafe_transforms/<name>.rs`.
2. Implement `Transform` + `UnsafeTransform` traits (see `eval_constant_expressions.rs`).
3. Add `pub mod <name>;` to `src/transforms/unsafe_transforms/mod.rs`.
4. Add `Box::new(<Type>::new())` to `unsafe_transforms: vec![...]` in `src/lib.rs`.
5. Write unit tests in `src/tests/tests_unsafe.rs` following the existing pattern in
   `src/tests/tests_safe.rs`.

---

## Do NOT change

- Safe transforms in `src/transforms/safe_transforms/` — they are correct.
- `src/tests/test_samples.rs` — it writes `test_output/*_deobf.js`, used for verification.
- `restringer/` JS source — read-only reference.
