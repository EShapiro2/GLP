# Module System Phase 2: Hierarchy and Scoping — Handover Report

**Date:** 2026-02-22
**Author:** Claude session (claude/module-phase1 branch)
**Status:** Completed

---

## Summary

Phase 2 of the module system is complete. The compiler can now discover
`self.glp` files in the directory hierarchy, assemble a type scope from
ancestor definitions, and pass it through the compilation pipeline. Child
definitions shadow parent definitions. Sibling modules are isolated — they
share only what `self.glp` provides. The prelude remains the root ancestor
of all type chains. All existing tests pass; no regressions.

---

## Completed Work

### 1. New test file

- `glp_runtime/test/module/module_hierarchy_test.dart` (13 tests)
  - `self.glp` chain discovery (4 tests): basic chain, empty chain, skipped
    intermediate, target-is-self.glp
  - Type scope assembly from ancestor chain (2 tests): single ancestor,
    multiple ancestor levels
  - Shadowing (2 tests): child self.glp shadows parent, module's own type
    shadows ancestor
  - Sibling isolation (1 test): siblings don't see each other's types
  - Type-only self.glp (1 test): self.glp with only type definitions works
  - Prelude as root ancestor (1 test): prelude types and procedures always
    available
  - Procedure declarations from ancestors (2 tests): exported and plain
    procedure declarations visible to descendants

### 2. Chain discovery

- `glp_runtime/lib/runtime/module_hierarchy.dart` (new file)
  - `discoverSelfChain(targetFile, rootDir)` — walks from target file's
    directory up to root, collecting `self.glp` files at each level
  - Returns paths in root-first order (outermost ancestor first)
  - If target IS `self.glp`, only includes ancestors above it
  - Skips missing intermediate `self.glp` files
  - Returns empty list if no `self.glp` exists anywhere in the chain

### 3. Type scope assembly

- `glp_runtime/lib/runtime/module_hierarchy.dart`
  - `assembleTypeScope(chain, module)` — builds `TypeEnvironment` by layering:
    1. Prelude (via `buildPreludeEnvironment()`)
    2. Each `self.glp` in chain order (root first)
    3. Target module's own definitions
  - Shadowing: later entries overwrite earlier ones (Map semantics)
  - Both `TypeDef` and `ProcDecl` entries from ancestors are included

### 4. Pipeline integration

- `glp_runtime/lib/analysis/type_checker/type_environment_builder.dart`
  - `buildTypeEnvironment(module, {ancestorScope})` — new optional parameter
  - When `ancestorScope` is provided, uses it as base instead of just prelude
  - When `ancestorScope` is provided, skips redefinition checks (ancestors
    are allowed to shadow prelude types)
  - Existing callers unaffected (parameter defaults to `null`)

- `glp_runtime/lib/analysis/type_checker/type_checker.dart`
  - `checkModule(module, {transformedProcedures, ancestorScope})` — new optional
    parameter, passed through to `buildTypeEnvironment`
  - Existing callers in `compiler.dart` and `glp_engine.dart` unaffected

---

## Current State

### Test Status

| Suite | Baseline (Phase 2 start) | After Phase 2 |
|-------|--------------------------|---------------|
| Dart Unit Tests | 293 passed, 5 skipped, 15 failed | 306 passed, 5 skipped, 15 failed |
| REPL Tests | 326/326 | 326/326 |

The 15 Dart failures are all pre-existing (multiagent, archive, arithmetic,
srsw, engine, meta-interpreter). The +13 net new tests are the hierarchy
tests.

### Known Issues

- The `_Module` codegen bug (pre-existing) — don't-care variable in fact
  clauses causes "Undefined variable" in codegen. Unrelated to Phase 2.
- The `ModuleLoader` (runtime) does not yet use the hierarchy functions.
  It currently loads modules in flat mode. Integration with the loader
  requires Phase 3 (cross-module type checking).

---

## Commits on `claude/module-phase1` (Phase 2 only)

1. `5f275bd` — test(modules): add Phase 2 hierarchy and scoping tests (red)
2. `9fb7216` — feat(modules): implement self.glp chain discovery
3. `998b3a1` — feat(modules): implement type scope assembly from self.glp chain
4. `050d083` — feat(modules): integrate hierarchy scoping into compilation pipeline

---

## Next Steps

1. **Phase 3: Cross-module type checking** — requires design review before implementation
2. **ModuleLoader integration** — the loader should use `discoverSelfChain` and `assembleTypeScope` when compiling modules from files
3. **`_Module` codegen bug** — pre-existing, not blocking, should be fixed separately

---

## Files Changed (Phase 2 relevant only)

**New:**
- `glp_runtime/lib/runtime/module_hierarchy.dart`
- `glp_runtime/test/module/module_hierarchy_test.dart`

**Modified (implementation):**
- `glp_runtime/lib/analysis/type_checker/type_environment_builder.dart`
- `glp_runtime/lib/analysis/type_checker/type_checker.dart`

---

## Notes for Next Session

- `discoverSelfChain` and `assembleTypeScope` are pure functions — they take
  paths and AST nodes, return results. No side effects, easy to test.
- The chain discovery uses `File.existsSync()` for synchronous file checks.
  If async is preferred later, the API can be changed.
- `assembleTypeScope` parses each `self.glp` synchronously with
  `readAsStringSync()`. This is acceptable for compilation but could be made
  async if needed for IDE integration.
- When `ancestorScope` is provided to `buildTypeEnvironment`, redefinition
  checking is disabled (`checkRedefinitions: false`). This is because ancestor
  scoping explicitly allows shadowing prelude types — a child `self.glp` may
  redefine `Channel`, `Constant`, etc.
- The `_buildScopeFromModule` helper in `module_hierarchy.dart` is simpler
  than `_buildEnvironmentFromModule` in `type_environment_builder.dart` — it
  does not check redefinitions, does not resolve aliases, and does not mark
  builtins. Alias resolution happens after all scopes are assembled, in
  `buildTypeEnvironment`.
- Sibling isolation is enforced by design: `discoverSelfChain` only looks for
  `self.glp` files, and `assembleTypeScope` only processes the chain and the
  target module. Sibling `.glp` files are never read.
- The pipeline integration is backwards-compatible. All existing call sites
  (`compiler.dart`, `glp_engine.dart`, `checkSource()`) pass no
  `ancestorScope`, so they get the old behavior (prelude only).
