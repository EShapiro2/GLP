# Phase 1: Archive Dead Tests

## What this phase does

Moves test files that check for intentionally-removed `_select/1` generation into `test/archive/`, and creates `dart_test.yaml` to exclude that directory from test discovery.

## Steps

### 1. Create archive directory (if needed)

```bash
mkdir -p test/archive
```

### 2. Move dead `_select/1` tests (8 failures, 4 files)

```bash
mv test/compiler/select_dispatch_test.dart test/archive/
mv test/runtime/activate_kernel_test.dart test/archive/
mv test/runtime/cssg_glp_dispatch_test.dart test/archive/
mv test/runtime/serve_test.dart test/archive/
```

### 3. Move dead module integration tests (6 failures, 2 files)

These duplicate REPL coverage and check for `_select/1` in dynamic linking.

```bash
mv test/module/cssn_modules_test.dart test/archive/
mv test/module/social_graph_sim_modules_test.dart test/archive/
```

### 4. Move dead RPC routing test (1 failure)

Read `test/runtime/rpc_routing_test.dart` first. If it tests the old Distribute fallback path (which was removed), archive it. If it tests the live GLP channel routing path, do NOT archive — leave for Phase 3.

### 5. Exclude archive from test discovery

Create `glp_runtime/dart_test.yaml`:

```yaml
paths:
  - test/
exclude_paths:
  - test/archive/
```

### 6. Verify

Run `dart test` and confirm the archived tests no longer appear. Count remaining failures — should drop by 15–20.

### 7. Commit

```bash
git add -A && git commit -m "Archive dead _select/1 and module integration tests"
```

Then proceed to Phase 2: read `docs/infra/dart-test-phase2.md`.
