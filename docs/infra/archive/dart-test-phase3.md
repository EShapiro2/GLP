# Phase 3: Fix Remaining Individual Test Failures

## What this phase does

Fixes the remaining failures (expect ~17) across ~10 files. Each test needs to be read, diagnosed, and fixed individually. These are stale test expectations, not runtime bugs — the 428/428 REPL suite proves the runtime works.

## Method

For each file below: read the test, run it, read the error, fix the test (not the runtime). If a test is truly dead (tests removed functionality with no replacement), archive it.

## Files

- `test/analysis/type_checker/moded_head_test.dart` (2 failures) — Inline `Channel`/`DiffList` type definitions use old monomorphic forms. Update to match current parameterized definitions, or update expected assertions.

- `test/analysis/type_checker/well_typed_clause_test.dart` (1 failure) — Same: stale inline type definitions.

- `test/bytecode/arithmetic_test.dart` (1 failure) — Tests `:=` execution. Read the error — likely a small API/setup change.

- `test/heap/arithmetic_pointer_test.dart` (1 failure) — Same `:=` test, pointer variant.

- `test/engine/glp_engine_test.dart` (2 failures) — GlpEngine constructor now requires `rootSelfGlpPath`. Add it to test setup. Auto-activation on load may cause side effects — read the error.

- `test/compiler/project_linker_test.dart` (2 failures) — Tests `discoverProject` on `cssg_modules/`. Module count or directory structure may have changed. Update expected values.

- `test/module/module_hierarchy_test.dart` (1 failure) — Tests prelude type visibility. Type names changed with parameterized types. Update expected type names.

- `test/multiagent/output_kernel_test.dart` (2 failures) — Loads GLP source via GlpEngine. Likely needs `rootSelfGlpPath` in constructor.

- `test/multiagent/ui_mediator_test.dart` (3 failures) — Loads real `.glp` files. Likely needs `rootSelfGlpPath` or path updates.

- `test/srsw_test.dart` (2 failures) — Tests anonymous variable compilation. Read the error — likely a constructor or API change.

## Work pattern

For each file:
1. Run it: `dart test test/path/to/file.dart`
2. Read the error output
3. Read the test source
4. Fix the test
5. Re-run to confirm
6. Commit: `git add -A && git commit -m "Fix <test file description>"`

## Final verification

After all files are fixed:

```bash
cd glp_runtime && dart test
```
Target: 0 failures.

```bash
cd /Users/udi/Grassroots/GLP && bash test/run_all_tests.sh
```
Target: 428/428.

## Commit

```bash
git add -A && git commit -m "Dart test suite clean: 0 failures"
```

Push and provide merge instructions to user.
