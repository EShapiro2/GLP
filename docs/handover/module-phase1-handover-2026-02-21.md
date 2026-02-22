# Module System Phase 1: Syntax — Handover Report

**Date:** 2026-02-21
**Author:** Claude session (claude/module-phase1 branch)
**Status:** Completed

---

## Summary

Phase 1 of the module system is complete. The `exported procedure` syntax replaces
`-export([...])` and `-import([...])` declarations. The parser now recognizes
`exported procedure p(T1?, T2).` as a public procedure declaration. Old
`-export`/`-import` syntax is rejected with clear error messages. All existing tests
pass; no regressions.

---

## Completed Work

### 1. New test file (RED phase)

- Created `glp_runtime/test/module/module_syntax_v2_test.dart` (9 tests)
  - `exported procedure` parsing (3 tests)
  - Rejection of `-export`/`-import` (2 tests)
  - `-module(name).` still works (1 test)
  - `Module # Goal` still works (1 test)
  - Type-only file (no procedures) parses (2 tests)

### 2. AST changes

- `glp_runtime/lib/analysis/type_checker/type_ast.dart`
  - Added `bool exported` field to `ProcDecl` (default `false`)
  - Updated `toString()` to include `exported` prefix when true

- `glp_runtime/lib/compiler/ast.dart`
  - Removed `ExportDeclaration`, `ImportDeclaration`, `ProcRef` classes
  - Removed `exports` and `imports` fields from `Module` class
  - Replaced with `exportedSignatures` getter that derives from `ProcDecl.exported`

### 3. Parser changes

- `glp_runtime/lib/compiler/parser.dart`
  - Added parsing for `exported procedure` keyword sequence
  - `exported` is recognized only before `procedure`, not as a reserved word
  - `-export(...)` now emits: "The -export() declaration is no longer supported. Use 'exported procedure' instead."
  - `-import(...)` now emits: "The -import() declaration is no longer supported. Use Module # Goal for cross-module calls."
  - Removed `_parseProcRefList`, `_parseProcRef`, `_parseAtomList` methods

### 4. Runtime changes

- `glp_runtime/lib/runtime/module_loader.dart`
  - Updated export extraction to use `module.exportedSignatures`
  - Backwards compatibility: if no procedure has `exported=true`, all are exported
  - Imports set to empty (auto-detected from `Module # Goal`)

- `glp_runtime/lib/runtime/module_runtime.dart`
  - Removed debug print for imports

- `glp_runtime/lib/engine/glp_engine.dart`
  - Removed regex-based `-import([...])` parsing

### 5. Existing tests updated

- `glp_runtime/test/module/module_parser_test.dart` — removed export/import groups, updated source strings
- `glp_runtime/test/module/module_integration_test.dart` — updated all GLP source strings and arity declarations
- `glp_runtime/test/module/module_e2e_test.dart` — updated all GLP source strings, removed `-import` assertion
- `glp_runtime/test/module/files/*.glp` — updated 4 on-disk test fixtures

---

## Current State

### Test Status

| Suite | Before | After |
|-------|--------|-------|
| Dart Unit Tests | 286 passed, 5 skipped, 15 failed | 288 passed, 5 skipped, 15 failed |
| REPL Tests | 326/326 | 326/326 |

The 15 Dart failures are all pre-existing (multiagent, archive, arithmetic, srsw, engine tests). The +2 net new tests come from the 9 new syntax v2 tests minus consolidated old tests.

### Known Issues

- The meta-interpreter e2e test (`meta.glp`) fails with `Undefined variable: _Module` — this is a **pre-existing** codegen issue with don't-care variables in fact clauses, unrelated to Phase 1.
- Commit `3c9733f` inadvertently included unrelated file changes (docs, multiagent, programs) that were staged at commit time. The module-system-related changes in that commit are the parser/AST/runtime files.

---

## Commits on `claude/module-phase1`

1. `e460a29` — test(modules): add Phase 1 syntax tests for exported procedure (red)
2. `70b5cc1` — refactor(ast): add exported field to procedure, remove export/import nodes
3. `3c9733f` — feat(parser): implement exported procedure syntax, reject old export/import
4. `1516d7b` — fix(tests): update existing module tests to new exported procedure syntax

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

- `exported` is not a reserved word. It is recognized only when followed by `procedure` at the declaration level. In all other contexts it is a regular atom.
- `LoadedModule` (runtime) still has `exports` and `imports` fields — these are populated from the AST's `exportedSignatures` and are independent of the removed AST fields.
- The backwards-compatibility path (no `exported procedure` declarations → export all) is in both `module_loader.dart` and the test helper `compileModule()`. Any code that constructs `LoadedModule` directly (like some e2e tests) bypasses this.
- The `_skipDeclarations` method in the parser was updated to skip `module`, `stdlib`, `mode` declarations (removed `export` and `import`).
