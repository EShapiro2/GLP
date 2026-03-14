# Current Plan: Module Boundary Enforcement

Started: 2026-03-14

## Goal

Enforce the spec (typed-glp-manual.md §19.3): `procedure` is module-local, only `exported procedure` is callable from outside. The REPL currently bypasses this via `combinedProgram`.

## Phases

Work in phases. **Read only the current phase file — do not read ahead.**

- [ ] **Phase 0: Baseline** — Run `bash test/run_all_tests.sh` (428/428) and `dart test` (0 failures). Commit.
- [ ] **Phase 1** — Read `docs/infra/module-boundary-phase1.md`. Add `exported` to all REPL-callable procedures. Commit.
- [ ] **Phase 2** — Read `docs/infra/module-boundary-phase2.md`. Enforce boundaries in `combinedProgram`. Add negative test. Commit.
- [ ] **Final** — Both test suites green. Push.

## Constraints

- Do NOT modify runtime/compiler logic in Phase 1 — declaration changes only
- Phase 1 must pass all tests before starting Phase 2
