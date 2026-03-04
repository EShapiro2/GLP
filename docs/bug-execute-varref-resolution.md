# Bug Report: `execute/2` VarRef Resolution Failure

**Date**: 2026-03-04
**Severity**: High — `execute/2` is unusable for any predicate that takes variable arguments
**Status**: Open (workaround available via body kernels)
**Discovered during**: Bonds Phase 4b implementation (time-aware redemption)

---

## Summary

The `execute/2` instruction, which calls registered system predicates (e.g., `current_time`, `evaluate`), fails at runtime because the compiled `VarRef` addresses do not match the runtime variable storage. The Execute handler's `_resolveHeadVarRefs` and `_dereferenceForExecute` resolve VarRef addresses to wrong variables — for example, dereferencing `VarRef(0)` intended for `Now` instead resolves to the `ActorChannel` or other unrelated variable.

---

## Reproduction

### Minimal case (from untyped boot file)

```glp
test_now :-
    execute('current_time', [T]),
    send_to_user_tagged(test, notify(time(T?))).
```

**Expected**: T is bound to the current epoch milliseconds; output shows `tagged(test, notify(time(1772577...)))`.

**Actual**: Either:
- `RangeError (length): Invalid value: Valid value range is empty: 0` (when no other variables exist), or
- T resolves to a completely wrong variable (e.g., the first argument of the calling clause), producing garbage output or suspension.

### Case from Bonds Phase 4b (typed actor)

```glp
alice_p4b_wait_credit1(T, [credit_proposed(bob, 5, 0, Ref)|In], [accept_credit(bob, Ref?)|Out?]) :-
    execute('current_time', [Now]) |  %% <-- BUG HERE
    T := Now? + 500,
    ...
```

**Observed debug output**:
```
[EXECUTE] Direct args: [Var@0]
[EXECUTE] Processing arg: Var@0 (VarRef)
[EXECUTE] After HEAD resolve: Var@<some addr>
[DEREF] VarRef: Var@<addr> (addr=<addr>, isReader=true)
[DEREF] Bound to: <value of ActorChannel, not Now>
```

The `VarRef(0)` compiled for `Now` instead resolves to whatever is at clauseVars[0] — which is the head's first variable, not the `Now` variable from the `execute` call.

---

## Root Cause Analysis

### How body kernels work (correctly)

Body kernels (e.g., `'_now'(T)`) are compiled as regular procedure calls via the `Spawn` instruction. The compiler generates `PutVariable`/`PutValue` instructions before the `Spawn`, which correctly place arguments into `cx.argSlots`. The Spawn handler then reads `cx.argSlots`:

```dart
// runner.dart:2808-2818 (body kernel execution)
final kernel = cx.rt.bodyKernels.lookup(procName, op.arity);
if (kernel != null) {
    final args = <Object?>[];
    for (int i = 0; i < op.arity; i++) {
        args.add(cx.argSlots[i]);    // <-- uses standard argument slots
    }
    final result = kernel(cx.rt, args);
    ...
}
```

The `PutVariable` instruction (runner.dart:2547-2621) correctly resolves variable indices through `cx.clauseVars`, creates/finds the correct heap addresses, and stores properly-addressed `VarRef`s in `cx.argSlots`. This is the standard, well-tested argument-passing path used by all procedure calls.

### How `execute/2` works (incorrectly)

The `execute/2` instruction bypasses the standard argument-passing path entirely. Instead:

1. **Compilation** (`codegen.dart:543-583`): `_generateExecuteCall` calls `_termToValue` for each argument. For variables, `_termToValue` returns `VarRef(varInfo.registerIndex!)` — using the *compiler's register index*, not a heap address.

2. **The compiled instruction** (`opcodes.dart:280-288`): The `Execute` opcode stores these raw `VarRef` objects directly in `op.args`:
   ```dart
   class Execute implements Op {
     final String predicateName;
     final List<Object?> args;   // <-- contains VarRef(registerIndex), not heap addrs
   }
   ```

3. **Runtime resolution** (`runner.dart:3807-3819`): The Execute handler tries to resolve these VarRefs through two helper functions:

   - `_resolveHeadVarRefs(arg, cx)` (line 4265): Checks `cx.clauseVars[term.addr]` — but `term.addr` is a *register index* from compilation, not a clauseVars key. If `cx.clauseVars` happens to contain an entry at that index (because head processing stored something there), it returns the *wrong* variable.

   - `_dereferenceForExecute(resolved, rt, cx)` (line 4304): Further dereferences the (already wrong) result through the heap.

### The fundamental mismatch

The compiler stores `VarRef(registerIndex)` in the Execute instruction's args. At runtime, `_resolveHeadVarRefs` interprets `registerIndex` as a `clauseVars` key. But `clauseVars` is populated by head-matching instructions (`GetVariable`, `HeadVariable`, etc.), which use *their own* indexing scheme. The register index from `_termToValue` may or may not coincide with the clauseVars key for the intended variable.

In practice:
- `VarRef(0)` resolves to `clauseVars[0]`, which is typically the first head variable (e.g., `ActorChannel` or `Id`), not the variable named in the `execute` call.
- For a fresh variable like `Now` that does not appear in the clause head, there is no clauseVars entry at all, so `_resolveHeadVarRefs` returns the raw `VarRef(registerIndex)` unchanged — and `_dereferenceForExecute` tries to use `registerIndex` as a heap address, which is also wrong.

### Additional issue: dead code after suspend

There is also unreachable code in the Execute handler (runner.dart:3841-3843):

```dart
pc = _suspendAndFailMulti(cx, call.suspendedReaders, pc); continue;
pc++;        // <-- unreachable
continue;    // <-- unreachable
```

The `continue` after `_suspendAndFailMulti` makes the subsequent two lines dead code. This is not the cause of the VarRef bug but indicates the handler was not thoroughly reviewed.

---

## Workaround

Use body kernels instead of `execute/2`. Body kernels go through the standard `PutVariable` → `Spawn` argument-passing path, which handles variable addressing correctly.

**Instead of**:
```glp
execute('current_time', [T])
```

**Use**:
```glp
'_now'(T)
```

The `'_now'(T)` body kernel is registered in `body_kernels.dart` (line 91) and correctly binds T to the current epoch milliseconds.

For cases where `execute` is needed from typed code (where the type checker doesn't recognise body kernels), compute the value in an untyped boot file and pass it as a `Constant?` parameter.

---

## Affected System Predicates

All system predicates registered in `system_predicates_impl.dart` are affected, since they are all invoked through the same `Execute` instruction:

- `current_time/1`
- `evaluate/2`
- `unique_id/1`
- `variable_name/1`
- `copy_term/2`
- `file_read/2`, `file_write/2`, `file_exists/1`
- `file_open/3`, `file_close/1`, `file_read_handle/2`, `file_write_handle/2`
- `directory_list/2`
- `write/1`, `nl/0`, `read/1`
- `link/1`, `load_module/1`
- `distribute_stream/2`, `copy_term_multi/3`

Any system predicate that takes variable arguments (rather than only constants) will fail.

---

## Suggested Fix Direction

The Execute handler should use the same argument-passing mechanism as body kernels. Two possible approaches:

### Option A: Compile `execute/2` to PutVariable + Spawn

Treat `execute('predname', [args...])` as equivalent to `'predname'(args...)` at the compiler level. Generate `PutVariable`/`PutValue` instructions for each argument, then a `Spawn` to a synthetic label that the runtime maps to the system predicate. This reuses the entire standard argument-passing path.

### Option B: Fix the Execute handler's VarRef resolution

Replace `_resolveHeadVarRefs` and `_dereferenceForExecute` with logic that correctly maps the compiler's register indices to runtime heap addresses. This requires understanding the exact mapping between `varInfo.registerIndex` (from the compiler's `VariableTable`) and the runtime's `clauseVars` / heap addressing. This approach is more fragile because it duplicates the complex variable-resolution logic that `PutVariable` already handles correctly.

Option A is strongly preferred — it eliminates the separate resolution path entirely and guarantees correctness by construction.

---

## Files Involved

| File | Role |
|------|------|
| `lib/compiler/codegen.dart` (lines 543-630) | `_generateExecuteCall` and `_termToValue` — compiles `execute/2` with raw `VarRef(registerIndex)` |
| `lib/bytecode/opcodes.dart` (lines 280-288) | `Execute` opcode class — stores args as `List<Object?>` containing VarRefs |
| `lib/bytecode/runner.dart` (lines 3789-3844) | Execute handler — calls `_resolveHeadVarRefs` and `_dereferenceForExecute` |
| `lib/bytecode/runner.dart` (lines 4265-4300) | `_resolveHeadVarRefs` — incorrect clauseVars lookup |
| `lib/bytecode/runner.dart` (lines 4302-4360) | `_dereferenceForExecute` — heap dereference on wrong addresses |
| `lib/runtime/system_predicates_impl.dart` | System predicate implementations (correct, but never reached with correct args) |
| `lib/runtime/body_kernels.dart` | Body kernel registry and implementations (working alternative path) |
| `lib/bytecode/runner.dart` (lines 2796-2828) | Body kernel execution via Spawn (correct path, for comparison) |
