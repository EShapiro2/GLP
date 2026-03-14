# Handover: Dart Test Suite Cleanup (All 53 Failures)

## Context

`dart test` in `glp_runtime/` shows 53 failures across 21 files. All caused by recent intentional changes: `_select/1` removal, parameterized types refactoring, and dynamic dispatch rewrite. The 428/428 REPL tests pass — the runtime works. These are stale Dart unit/integration tests.

## Category A: Archive dead `_select/1` tests (15 failures, 7 files)

We intentionally removed `_select/1` generation. These tests check for it.

Move to `test/archive/`:
```
test/compiler/select_dispatch_test.dart          (2 failures)
test/runtime/activate_kernel_test.dart           (3 failures)
test/runtime/cssg_glp_dispatch_test.dart         (1 failure)
test/runtime/serve_test.dart                     (2 failures)
test/runtime/rpc_routing_test.dart               (1 failure)
test/module/cssn_modules_test.dart               (3 failures)
test/module/social_graph_sim_modules_test.dart    (3 failures)
```

Create `glp_runtime/dart_test.yaml` to exclude archived tests:
```yaml
paths:
  - test/
exclude_paths:
  - test/archive/
```

This also fixes **Category C** (5 failures from files already in `test/archive/`).

## Category B: Isolate crashes — missing rootSelfGlpPath (16 failures, 3 files)

**Root cause:** `BootConfig.rootSelfGlpPath` defaults to `''`. Tests never set it. Inside each isolate, `GlpEngine` can't find `programs/self.glp` → no parameterized type templates → `UnknownTypeError: Stream?` → crash.

**Fix:** In each test, after `loader.load(source)`, add:
```dart
config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
```

Files to fix:
```
test/multiagent/multiagent_glp_test.dart         (12 failures)
test/multiagent/isolate_manager_test.dart         (3 failures)
test/multiagent/multiagent_modules_test.dart      (1 failure)
```

For `multiagent_glp_test.dart`, add the line in the `runGlpTest` helper after `config = loader.load(source)`.

For `isolate_manager_test.dart`, add after each `loader.load(source)` call. The first test has inline source with `procedure agent_init(_?, Channel?).` — change `Channel?` to `_?` because bare `Channel` without parameters no longer exists as a monomorphic type.

**Important:** These tests set `strictTypes = false` on the engine inside the isolate. After fixing rootSelfGlpPath, the tests will pass but print type warnings about undefined `send_to_net/1` etc. This is acceptable for now — the mad predicates are loaded at runtime but not visible to the type checker. Do NOT change `strictTypes` to `true` — that's a separate issue.

After fixing, run each file individually to verify:
```bash
cd glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
dart test test/multiagent/isolate_manager_test.dart
dart test test/multiagent/multiagent_modules_test.dart
```

## Category D: Type checker unit tests (3 failures, 2 files)

**Root cause:** Tests construct `Channel` and `DiffList` type definitions inline. After parameterized types refactoring, these definitions changed structure.

Files:
```
test/analysis/type_checker/moded_head_test.dart        (2 failures)
test/analysis/type_checker/well_typed_clause_test.dart  (1 failure)
```

**Fix:** Read each failing test. The test builds a `TypeEnvironment` with inline type definitions like `Channel ::= ch(Stream, Stream?).` These are the old monomorphic definitions. Either update the inline definitions to match current parameterized forms, or update the expected assertions to match what the type checker now produces. The type checker logic itself is correct (428 REPL tests prove it) — only the test expectations are stale.

## Category E: Stale tests / API changes (14 failures, 8 files)

For each file, read the test, understand the error, and fix:

- `test/engine/glp_engine_test.dart` (2) — Inline source uses `_?` types. The engine now auto-activates modules with exports, which may cause side effects. Read the error and fix test setup.

- `test/compiler/project_linker_test.dart` (2) — Tests `discoverProject` on `cssg_modules/`. The project directory structure or module count may have changed. Update expected values.

- `test/module/module_hierarchy_test.dart` (1) — Tests that prelude types are visible. The prelude type names changed with parameterized types. Update expected type names.

- `test/bytecode/arithmetic_test.dart` (1) — Tests `:=` execution. Read the error — likely a small setup issue.

- `test/heap/arithmetic_pointer_test.dart` (1) — Same `:=` test, pointer architecture variant.

- `test/multiagent/output_kernel_test.dart` (2) — Uses `GlpEngine` to load source. Likely needs `rootSelfGlpPath` in constructor.

- `test/multiagent/ui_mediator_test.dart` (3) — Loads real `.glp` files from `typed_book/social_graph/`. These files changed during parameterized types refactoring. May need path or content updates.

- `test/srsw_test.dart` (2) — Tests anonymous variable compilation. Read the error — likely a constructor or API change.

## Execution order

1. Category A + C: Archive files, create `dart_test.yaml`
2. Category B: Fix rootSelfGlpPath in 3 files
3. Run `dart test`, count remaining failures (should be ~17)
4. Category D: Fix 2 type checker test files
5. Category E: Fix 8 files individually
6. Run `dart test` — target: 0 failures
7. Run `bash test/run_all_tests.sh` — must be 428/428
8. Single commit: `git add -A && git commit -m "Clean Dart test suite: archive dead tests, fix remaining failures" && git push origin main`

## What NOT to do

- Do NOT modify any `.glp` program files
- Do NOT modify runtime or compiler code (unless you find a genuine bug exposed by a test)
- Do NOT use `@Skip` annotations
- Do NOT change `strictTypes` in isolate tests
- Do NOT start any work beyond this cleanup
