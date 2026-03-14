# Current Plan: Dart Test Suite Cleanup

Started: 2026-03-13

## Goal

Zero Dart test failures. Currently 53 failures across 21 files. The runtime is correct (428/428 REPL tests pass) — these are stale unit tests.

## Phases

Work in phases. **Read only the current phase file — do not read ahead.**

- [ ] **Phase 0: Baseline** — Run `dart test` and `bash test/run_all_tests.sh`. Commit.
- [ ] **Phase 1** — Read `docs/infra/dart-test-phase1.md`. Archive dead tests. Commit.
- [ ] **Phase 2** — Read `docs/infra/dart-test-phase2.md`. Fix isolate rootSelfGlpPath. Commit.
- [ ] **Phase 3** — Read `docs/infra/dart-test-phase3.md`. Fix remaining individual tests. Commit.
- [ ] **Final** — `dart test` = 0 failures, `run_all_tests.sh` = 428/428. Push.

## Constraints

- Do NOT modify any `.glp` program files
- Do NOT modify runtime or compiler code (unless a test exposes a genuine bug)
- Do NOT add `@Skip` annotations — either fix or archive
