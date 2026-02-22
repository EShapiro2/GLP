# Module System Phase 1: Syntax — Handover Report

**Date:** 2026-02-22
**Author:** Claude session (claude/module-phase1 branch)
**Status:** Completed

---

## Summary

Phase 1 of the module system is complete. Three procedure declaration kinds are
now supported: `procedure` (private), `exported procedure` (public), and
`imported procedure` (cross-module dependency). Old `-export`/`-import` syntax
is rejected with clear error messages. Imported procedures support `#`-separated
module paths and qualified type references. All existing tests pass; no regressions.

---

## Completed Work

### 1. New test file

- `glp_runtime/test/module/module_syntax_v2_test.dart` (14 tests)
  - `exported procedure` parsing (4 tests)
  - `imported procedure` parsing (5 tests): module path, deep path, ancestor scope,
    qualified types, declaration-only (no clauses)
  - Rejection of `-export`/`-import` (2 tests)
  - `-module(name).` still works (1 test)
  - `Module # Goal` still works (1 test)
  - Type-only file (no procedures) parses (1 test)

### 2. AST changes

- `glp_runtime/lib/analysis/type_checker/type_ast.dart`
  - `ProcDecl` gains three visibility-related fields:
    - `bool exported` (default `false`)
    - `bool imported` (default `false`)
    - `String? modulePath` (default `null`) — for imported procedures, the module
      path (e.g., `'social'` or `'ui#actors'`). `null` means ancestor scope.
  - Added `qualifiedName` getter for `path#name` representation
  - Updated `toString()` to show visibility prefix and qualified name

- `glp_runtime/lib/compiler/ast.dart`
  - Removed `ExportDeclaration`, `ImportDeclaration`, `ProcRef` classes
  - Removed `exports` and `imports` fields from `Module` class
  - Added `exportedSignatures` getter that derives from `ProcDecl.exported`

### 3. Parser changes

- `glp_runtime/lib/compiler/parser.dart`
  - `_parseProcDeclaration()` handles three prefixes: `exported`, `imported`, none
  - For `imported procedure`: parses `#`-separated path, last component is procedure
    name, rest is module path
  - `_parseProcArgType()` handles qualified type references: `social#AgentChannel?`
    lexes as `ATOM(social)` `HASH` `VARIABLE(AgentChannel)` `QUESTION`, produces
    `TypeRef('social#AgentChannel', isInput: true)`
  - Imported procedures are declaration-only — no `pendingProcDecl` is set, so no
    clauses are expected after them
  - `exported` and `imported` are recognized only before `procedure`, not reserved words
  - `-export(...)` emits: "Use 'exported procedure' instead."
  - `-import(...)` emits: "Use 'imported procedure' instead."
  - Removed `_parseProcRefList`, `_parseProcRef`, `_parseAtomList` methods

### 4. Runtime changes

- `glp_runtime/lib/runtime/module_loader.dart`
  - Export extraction uses `module.exportedSignatures`
  - Backwards compatibility: if no procedure has `exported=true`, all are exported
  - Imports set to empty list (cross-module calls auto-detected from `Module # Goal`)

- `glp_runtime/lib/runtime/module_runtime.dart`
  - Removed debug print for imports

- `glp_runtime/lib/engine/glp_engine.dart`
  - Removed regex-based `-import([...])` parsing

### 5. Existing tests updated

- `glp_runtime/test/module/module_parser_test.dart` — updated source strings
- `glp_runtime/test/module/module_integration_test.dart` — updated source strings and arities
- `glp_runtime/test/module/module_e2e_test.dart` — updated source strings, removed `-import` assertion
- `glp_runtime/test/module/files/*.glp` — updated 4 on-disk test fixtures

---

## Current State

### Test Status

| Suite | Baseline | After Phase 1 |
|-------|----------|---------------|
| Dart Unit Tests | 286 passed, 5 skipped, 15 failed | 293 passed, 5 skipped, 15 failed |
| REPL Tests | 326/326 | 326/326 |

The 15 Dart failures are all pre-existing (multiagent, archive, arithmetic, srsw, engine, meta-interpreter). The +7 net new tests come from 14 new syntax v2 tests minus consolidated old tests.

### Known Issues

- The meta-interpreter e2e test (`meta.glp`) fails with `Undefined variable: _Module` — this is a **pre-existing** codegen issue with don't-care variables in fact clauses, unrelated to Phase 1.
- Commit `3c9733f` inadvertently included unrelated file changes (docs, multiagent, programs) that were staged at commit time. The module-system-related changes in that commit are the parser/AST/runtime files only.

---

## Commits on `claude/module-phase1`

1. `e460a29` — test(modules): add Phase 1 syntax tests for exported procedure (red)
2. `70b5cc1` — refactor(ast): add exported field to procedure, remove export/import nodes
3. `3c9733f` — feat(parser): implement exported procedure syntax, reject old export/import
4. `1516d7b` — fix(tests): update existing module tests to new exported procedure syntax
5. `0ce994b` — docs: add Phase 1 module syntax handover report
6. `696ccb5` — test(modules): add imported procedure syntax tests (red)
7. `93d53d8` — refactor(ast): add imported and modulePath fields to ProcDecl
8. `d0b050e` — feat(parser): implement imported procedure syntax with module paths

---

## Next Steps

1. **Phase 2: Hierarchy and scoping** — requires design review before implementation
2. **`_Module` codegen bug** — the `_` prefix don't-care variable in fact clauses causes "Undefined variable" in codegen. Not blocking but should be fixed separately.

---

## Files Changed (Phase 1 relevant only)

**New:**
- `glp_runtime/test/module/module_syntax_v2_test.dart`

**Modified (implementation):**
- `glp_runtime/lib/analysis/type_checker/type_ast.dart`
- `glp_runtime/lib/compiler/ast.dart`
- `glp_runtime/lib/compiler/parser.dart`
- `glp_runtime/lib/engine/glp_engine.dart`
- `glp_runtime/lib/runtime/module_loader.dart`
- `glp_runtime/lib/runtime/module_runtime.dart`

**Modified (tests):**
- `glp_runtime/test/module/module_parser_test.dart`
- `glp_runtime/test/module/module_integration_test.dart`
- `glp_runtime/test/module/module_e2e_test.dart`
- `glp_runtime/test/module/files/math.glp`
- `glp_runtime/test/module/files/main.glp`
- `glp_runtime/test/module/files/math_rules.glp`
- `glp_runtime/test/module/files/meta.glp`

---

## Notes for Next Session

- `exported` and `imported` are not reserved words. They are recognized only when followed by `procedure` at the declaration level. In all other contexts they are regular atoms.
- `ProcDecl` has three visibility states: plain (`exported=false, imported=false`), exported (`exported=true`), imported (`imported=true`). Both `exported` and `imported` being true is semantically invalid (enforced by convention, not runtime check).
- For `imported procedure social#agent(...)`, the `modulePath` is `'social'` and `name` is `'agent'`. For `imported procedure ui#actors#render(...)`, `modulePath` is `'ui#actors'` and `name` is `'render'`. For `imported procedure merge(...)` (ancestor scope), `modulePath` is `null`.
- Qualified type references in procedure arguments (`social#AgentChannel?`) are stored as `TypeRef('social#AgentChannel', isInput: true)`. The `#` is part of the type name string.
- `LoadedModule` (runtime) still has `exports` and `imports` fields — these are populated from the AST's `exportedSignatures` and are independent of the removed AST fields.
- The backwards-compatibility path (no `exported procedure` → export all) is in both `module_loader.dart` and test helper `compileModule()`.
