# Dead Code Removal: Module Boundary Bypass Paths

**Date**: 2026-03-12
**Author**: Claude Chat
**Status**: Ready for Claude Code execution

---

## Context

The dynamic dispatch chain is now working (428/428 tests). Several legacy code paths bypass module boundaries — they predate the dynamic dispatch system and are now dead or deprecated. This task removes them.

---

## Item 1: Remove `_generateSelectProcedure()` — DEAD CODE

`_activate` now dispatches directly to the target procedure (bypassing `_select/1` clause execution). The compiler still generates `_select/1` bytecode but nothing executes it.

### Critical dependency: Replace auto-activation trigger

Currently, `loadSource()` and `activateDynamicModule()` use `program.labels.containsKey('_select/1')` to detect whether a module has exports. When we stop generating `_select/1`, we need a replacement.

**Solution:** Add a `hasExports` field to `ModuleInfo`. Set it from the parsed module's `procDeclarations` (check if any `ProcDecl` has `exported == true`). Use `moduleInfo.hasExports` instead of the `_select/1` label check.

**Files to change:**

1. `lib/engine/glp_engine.dart`:
   - Add `final bool hasExports;` to `ModuleInfo`
   - In `_extractModuleInfo()`, compute `hasExports` from source: check if any `exported procedure` declarations exist (the same regex or AST data already available)
   - In `loadSource()`, replace `if (program.labels.containsKey('_select/1'))` with `if (moduleInfo.hasExports)`
   - In `activateDynamicModule()`, replace `if (!moduleProg.labels.containsKey('_select/1'))` with a check on `moduleInfo.hasExports`

2. `lib/compiler/compiler.dart`:
   - Delete `_generateSelectProcedure()` method entirely
   - Remove the Phase 2.6 block in `compileWithMetadata()` that calls it (the `selectProc` variable, the `programForAnalyzer` conditional)
   - Just pass `ast` directly to the analyzer instead of `programForAnalyzer`

3. `test/dynamic_dispatch_test.dart`:
   - Remove Tests 1 and 2 (`_select/1` label tests) — we no longer generate `_select/1`
   - Renumber remaining tests if needed. The end-to-end tests (double/triple/add_ten/unknown goal) are the important ones.

---

## Item 2: Remove Distribute fallback path — DEAD CODE

In `lib/bytecode/runner.dart`, the `Distribute` handler has two branches:
1. If `glpChannel != null` → route via GLP channel (the live path)
2. `else` → spawn goal directly via `ReplModuleContext.combinedProgram` (the legacy fallback)

Since `loadSource()` auto-activates modules with exports, a GLP channel always exists for any module that has exported procedures. The fallback is dead.

**Files to change:**
- `lib/bytecode/runner.dart`: In the `Distribute` handler, within the `if (cx.moduleContext is ReplModuleContext)` block, after checking `glpChannel != null`:
  - Remove the entire `else` branch (the one that finds `entryPC` in `combinedProgram` and spawns directly)
  - Keep only the GLP channel path
  - If `glpChannel` is null, just log a warning (module not activated)

---

## Item 3: Remove `execute/2` — UNUSED FEATURE

`execute/2` allows calling any procedure by name with dynamic arguments, bypassing module boundaries entirely. No GLP program or test uses it.

**Files to change:**

1. `lib/bytecode/opcodes.dart`:
   - Remove `class Execute implements Op`
   - Remove `class SetClauseVar implements Op`

2. `lib/compiler/codegen.dart`:
   - Remove `_generateExecuteCall()` method
   - Remove the `execute/2` special case in `_generateBody()` (the `if (goal.functor == 'execute' && goal.arity == 2)` block)
   - Remove `_termToValue()` helper (only used by `_generateExecuteCall`)

3. `lib/bytecode/runner.dart`:
   - Search for `Execute` handler in `runWithStatus()` and remove it
   - Search for `SetClauseVar` handler and remove it

4. `lib/compiler/ast.dart` (if relevant): Check if there's an `ExecuteGoal` AST node — if so, remove it

---

## Item 4: REPL `combinedProgram` — NOTE FOR FUTURE

**DO NOT change this yet.** The REPL's `combinedProgram` merges all loaded programs into one flat bytecode blob, allowing any procedure from any loaded file to be called directly. This bypasses `exported procedure` access control.

For now, this is the only way the REPL can run goals. Enforcing module boundaries at the REPL level requires a design change (selecting a "current module", restricting goal resolution to exports + current module's local procedures). Defer to a separate task.

Add a TODO comment in `GlpEngine.combinedProgram`:
```dart
/// Get the combined bytecode program from all loaded sources.
///
/// TODO: This merges all modules into one flat program, bypassing
/// `exported procedure` access control. Goals typed at the REPL can
/// call any procedure from any loaded module, not just exported ones.
/// A future revision should enforce module boundaries by restricting
/// goal resolution to the current module's local procedures plus
/// exports of activated modules.
BytecodeProgram get combinedProgram {
```

---

## Execution order

1. Pull main, run `bash test/run_all_tests.sh` — expect 428/428
2. Item 1: Remove `_generateSelectProcedure()`, add `hasExports` to ModuleInfo, update triggers, update Dart tests
3. Item 2: Remove Distribute fallback
4. Item 3: Remove `execute/2`
5. Item 4: Add TODO comment to `combinedProgram`
6. Run all tests (REPL + Dart). Fix any breakage.
7. Commit and push

---

## What NOT to change

- Do NOT remove `reduce/2` generation from `analyzer.dart` — kept for future metaprogramming
- Do NOT change `combinedProgram` behavior — noted for future revision only
- Do NOT remove the Transmit handler — it's the live dynamic RPC path
- Do NOT remove the Distribute GLP channel path — that's the live path
- Do NOT modify any `.glp` program files
