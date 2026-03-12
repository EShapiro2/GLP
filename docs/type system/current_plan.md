# Current Plan: Dead Code Removal — COMPLETE

Started: 2026-03-12
Completed: 2026-03-12

## Steps
- [x] 1. Baseline tests (428/428)
- [x] 2. Remove `_generateSelectProcedure()` from compiler.dart, add `hasExports` to ModuleInfo, update Dart tests
- [x] 3. Remove Distribute fallback path in runner.dart
- [x] 4. Remove `execute/2` (opcodes, codegen, runner) + helper functions
- [x] 5. Add TODO to `combinedProgram` re: future module boundary enforcement
- [x] 6. Run all tests (428/428 REPL, 5/5 Dart), commit and push

## What was removed
- `_generateSelectProcedure()` — compiler no longer generates `_select/1` bytecode
- `_select/1` label check replaced by `ModuleInfo.hasExports` (regex on source)
- Distribute fallback (direct spawn via combinedProgram) — only GLP channel path remains
- `Execute` and `SetClauseVar` opcodes
- `_generateExecuteCall()` and `_termToValue()` from codegen
- Execute/SetClauseVar handlers from runner
- `_resolveHeadVarRefs()` and `_dereferenceForExecute()` helper functions from runner

## Context

Full instructions: `docs/modules/dead-code-removal-instructions.md`
