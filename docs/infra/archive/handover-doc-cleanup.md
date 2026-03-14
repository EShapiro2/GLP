# Handover: Documentation Cleanup + Dart Test Triage

## What to do

Two tasks, in order.

### Task 1: Replace "prelude" in the manual

Follow `docs/infra/replace-prelude-instructions.md` exactly. Six replacements in `docs/typed-glp-manual.md`. Verify with `grep -n -i "prelude" docs/typed-glp-manual.md` — must return zero matches. Also verify `grep -n -i "prelude" docs/glp-cheat-sheet.md` — should already be zero.

Commit: `git add -A && git commit -m "Replace prelude with root self.glp in manual"`

### Task 2: Archive dead Dart tests

The Dart test suite has 53 failures. Many are from tests that check for `_select/1` which was intentionally removed. Archive these files by moving them to `glp_runtime/test/archive/`:

```
test/compiler/select_dispatch_test.dart
test/runtime/activate_kernel_test.dart
test/runtime/cssg_glp_dispatch_test.dart
test/runtime/serve_test.dart
test/module/cssn_modules_test.dart
test/module/social_graph_sim_modules_test.dart
```

Then ensure `test/archive/` files don't run. Create `glp_runtime/dart_test.yaml` if it doesn't exist:
```yaml
paths:
  - test/
exclude_paths:
  - test/archive/
```

Verify: `cd glp_runtime && dart test test/archive/ 2>&1 | head -5` should show no tests running from archive.

Verify REPL tests still pass: `bash test/run_all_tests.sh` — must be 428/428.

Commit: `git add -A && git commit -m "Archive dead _select/1 tests, exclude test/archive from dart test"`

## What NOT to do

- Do not modify any `.glp` files
- Do not modify any runtime or compiler code
- Do not fix the remaining ~30 Dart test failures — that's a separate task
- Do not start any other work
