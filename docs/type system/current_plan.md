# Current Plan: Project Loader Fix + Module Boundary Enforcement Phase 2

Updated: 2026-03-14

## Completed

- [x] Phase 0: Baseline
- [x] Phase 1: Add `exported` to REPL-callable procedures (commit 696cc259)
- [x] Infrastructure goal fix (commit b262e28e). Serve goals excluded from scheduler status.
- [x] Infrastructure goal propagation in Spawn + Dart test expectations (commit 53da7a42). 354/0/5 Dart, 461/461 REPL.

## Remaining — Two tasks in order

### Task 1: Fix project loader bug (trailing slash + buildPreludeEnvironment)

Read `docs/bugs/project-loader-fix-instructions.md` for context. Two changes needed:

**Change A** — `glp_runtime/lib/runtime/module_hierarchy.dart`, function `discoverSelfChain`:
Normalize trailing slashes when comparing paths. The bug: `currentNorm` has no trailing slash but `rootNorm` does, so `startsWith` fails and the chain is always empty for top-level modules. Fix: strip trailing slashes from both `currentNorm` and `rootNorm` before comparing.

**Change B** — `glp_runtime/lib/compiler/project_linker.dart`, function `_buildAncestorScope`:
1. Add `import '../analysis/type_checker/type_environment_builder.dart';` at the top
2. Change `var env = TypeEnvironment({}, {});` to `var env = buildPreludeEnvironment();`  
3. Remove `rootSelfGlpPath` from the `fullChain` list (prelude already handles root self.glp). The loop should just iterate over `chain`, not `fullChain`.

**Verify**: `echo -e '../programs/bonds_v2/\n:quit' | dart run bin/glp_repl.dart` should load without `UnknownTypeError`. Then run both test suites.

**Commit**: Stage only `module_hierarchy.dart` and `project_linker.dart`. Message: "Fix project loader: normalize paths + use buildPreludeEnvironment"

### Task 2: Phase 2 module boundary enforcement

Read `docs/infra/module-boundary-phase2.md`. Implement, add negative test, verify both suites pass.

REPL target: 461/461. Dart target: 354 pass, 0 failures, 5 skipped.
