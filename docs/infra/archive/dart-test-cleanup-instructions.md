# Dart Test Suite Cleanup — Instructions for Claude Code

**Date**: 2026-03-13
**Author**: Claude Chat
**Status**: Ready for execution

---

## Required reading

Read `claude.md` and `docs/DISCIPLINE.md` first.

## Overview

53 Dart test failures across 21 files. Grouped by root cause below. Fix in order.

---

## Fix 1: Isolate tests — missing rootSelfGlpPath (16 failures, 3 files)

### Root cause

`BootConfig.rootSelfGlpPath` defaults to `''`. The isolate tests never set it. Inside each isolate, `GlpEngine` can't find `programs/self.glp`, so the prelude (parameterized type templates `Stream`, `Channel`, etc.) is never loaded. When `enableMadGLP()` loads mad predicates containing `send_to_net(Stream(_)?)`, the type checker can't expand `Stream(_)` → `UnknownTypeError: Stream?` → isolate crash.

### Files to fix

- `test/multiagent/multiagent_glp_test.dart` (12 failures)
- `test/multiagent/isolate_manager_test.dart` (3 failures)
- `test/multiagent/multiagent_modules_test.dart` (1 failure)

### Fix

In each test, after `loader.load(source)`, add:

```dart
config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
```

For `multiagent_glp_test.dart`, this goes in the `runGlpTest` helper:
```dart
final config = loader.load(source);
config.rootSelfGlpPath = File('../programs/self.glp').absolute.path;
```

For `isolate_manager_test.dart`, same pattern in each test.

For `multiagent_modules_test.dart`, same pattern.

### Verify

Run just these test files:
```bash
cd glp_runtime
dart test test/multiagent/multiagent_glp_test.dart
dart test test/multiagent/isolate_manager_test.dart
dart test test/multiagent/multiagent_modules_test.dart
```

---

## Fix 2: Archive `_select/1` tests (8 failures, 4 files)

### Root cause

We intentionally removed `_select/1` generation. These tests explicitly check for it.

### Files to archive (move to `test/archive/`)

- `test/compiler/select_dispatch_test.dart` (2 failures)
- `test/runtime/activate_kernel_test.dart` (3 failures)
- `test/runtime/cssg_glp_dispatch_test.dart` (1 failure)
- `test/runtime/serve_test.dart` (2 failures)

These are all replaced by `test/dynamic_dispatch_test.dart` (5 tests, all passing).

### Fix

```bash
mv test/compiler/select_dispatch_test.dart test/archive/
mv test/runtime/activate_kernel_test.dart test/archive/
mv test/runtime/cssg_glp_dispatch_test.dart test/archive/
mv test/runtime/serve_test.dart test/archive/
```

---

## Fix 3: Archive module integration tests with `_select/1` checks (6 failures, 2 files)

### Root cause

These test both project discovery AND check for `_select/1` in dynamic linking. The project discovery tests may be salvageable but the `_select/1` tests are dead. Since these are large integration tests that duplicate REPL coverage, archive them.

### Files to archive

- `test/module/cssn_modules_test.dart` (3 failures)
- `test/module/social_graph_sim_modules_test.dart` (3 failures)

---

## Fix 4: Exclude `test/archive/` from test discovery (5 failures, 3 files)

### Root cause

Files in `test/archive/` are still discovered by `dart test`.

### Fix

Create or update `dart_test.yaml` in `glp_runtime/`:

```yaml
filename: "test/{**_test,**_test/**}.dart"
exclude_tags:
  - archived
```

Actually, simpler: just ensure archived files don't end in `_test.dart`. Rename them:
```bash
cd test/archive
for f in *_test.dart; do mv "$f" "${f%.dart}.dart.archived"; done
```

Or create `dart_test.yaml` with:
```yaml
paths:
  - test/
exclude_paths:
  - test/archive/
```

Choose whichever approach works. Verify with `dart test --list` that archive files don't appear.

---

## Fix 5: RPC routing test (1 failure)

### File: `test/runtime/rpc_routing_test.dart`

Read the test. If it tests the old Distribute fallback path (which we removed), archive it. If it tests the GLP channel routing path (which is live), fix it.

---

## Fix 6: Remaining individual failures (17 failures, 10 files)

For each of these, read the test, understand what it tests, and either fix or archive:

- `test/analysis/type_checker/moded_head_test.dart` (2) — Channel/DiffList dual structure. These test core type system logic. Read the failing assertions and update to match the current parameterized type definitions.
- `test/analysis/type_checker/well_typed_clause_test.dart` (1) — dual types. Same approach.
- `test/bytecode/arithmetic_test.dart` (1) — `:=` test. Likely a small API change.
- `test/heap/arithmetic_pointer_test.dart` (1) — same `:=` test, pointer architecture.
- `test/engine/glp_engine_test.dart` (2) — GlpEngine basic tests. Read the error, fix the test setup.
- `test/compiler/project_linker_test.dart` (2) — project discovery for cssg_modules. May need path fix or param expansion in the test pipeline.
- `test/module/module_hierarchy_test.dart` (1) — prelude visibility. Likely needs updated type expectations.
- `test/multiagent/output_kernel_test.dart` (2) — loads GLP source via GlpEngine. Likely needs rootSelfGlpPath or similar fix.
- `test/multiagent/ui_mediator_test.dart` (3) — loads real .glp files. Likely needs rootSelfGlpPath.
- `test/srsw_test.dart` (2) — anonymous variable compilation. Read the error, fix.

---

## Execution order

1. Fix 1 (isolates) — biggest impact, single root cause
2. Fixes 2–4 (archive dead tests) — removes noise
3. Run `dart test` to see what's left
4. Fix 5–6 individually
5. Run `dart test` — target: 0 failures
6. Run `bash test/run_all_tests.sh` — must still be 428/428
7. Commit

---

## What NOT to do

- Do NOT modify any `.glp` program files
- Do NOT modify runtime/compiler code unless you find a genuine bug
- Do NOT add `@Skip` annotations — either fix or archive
- Do NOT add new features or refactor — cleanup only
