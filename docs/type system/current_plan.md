# Current Plan: Dynamic Module Dispatch — Test & Debug

Started: 2026-03-12

## Steps
- [x] 1. Baseline tests (424/424)
- [x] 2. Write Dart integration tests (test/dynamic_dispatch_test.dart) — 8 tests
- [x] 3. Run compilation tests (Tests 1-3) — passed immediately
- [x] 4. Run end-to-end dispatch test (Test 4) — found bug, fixed
- [x] 5. Fix: _activate now dispatches directly to procedure (bypasses _select clause execution)
- [ ] 6. Add REPL tests (Section J) ← CURRENT
- [ ] 7. Update claude.md, final test suite, commit and push

## Bug Found & Fixed

_activate previously routed goals through _select/1 clause execution. This failed for procedures with output parameters: _select's body call passed all args as readers, causing Reader x Reader failure when the target clause also had a reader at the output position. Fixed by having _activate extract functor/arity from the goal term and spawn the target procedure directly, preserving writer/reader polarity.

## Context

All 5 phases of dynamic dispatch are implemented and tested. 8 Dart integration tests pass (compilation, serve/2, end-to-end dispatch with output params, fallback). 424 REPL tests pass. 8 CSSG GLP dispatch tests pass.

Full instructions: `docs/modules/dynamic-dispatch-claude-code-instructions.md`
Test GLP files: `programs/tests/dynamic_dispatch/` (math_service.glp, private_only.glp, single_export.glp)
