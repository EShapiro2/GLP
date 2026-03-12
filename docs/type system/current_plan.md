# Current Plan: Dead Code Removal — COMPLETE

Started: 2026-03-12
Completed: 2026-03-12

## Steps
- [x] 1. Baseline tests (428/428)
- [x] 2. Remove `_generateSelectProcedure()`, replace auto-activation trigger with `ModuleInfo.hasExports`
- [x] 3. Remove Distribute fallback path in runner.dart
- [x] 4. Remove `execute/2` (Execute, SetClauseVar opcodes, codegen, runner handlers)
- [x] 5. Add TODO to `combinedProgram` re: future module boundary enforcement
- [x] 6. 428/428 REPL tests, 5/5 Dart tests. Committed and pushed.

## Net result: -259 lines

## What was removed
1. `_generateSelectProcedure()` — dead since `_activate` bypasses `_select/1`
2. Distribute fallback — dead since `loadSource()` auto-activates modules with exports
3. `execute/2` — unused feature (zero programs, zero tests)
4. Associated helpers: `_termToValue`, `_resolveHeadVarRefs`, `_dereferenceForExecute`

## What remains (by design)
- `reduce/2` generation — kept for future metaprogramming (FCP heritage)
- `combinedProgram` — TODO noted, deferred to future module boundary enforcement
- Transmit handler — live path for dynamic RPC
- Distribute GLP channel path — live path for static RPC
