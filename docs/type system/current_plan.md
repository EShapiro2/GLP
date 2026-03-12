# Current Plan: Dynamic Module Dispatch — Test & Debug

Started: 2026-03-12

## Steps
- [ ] 1. Baseline tests (399/399), commit ← CURRENT
- [ ] 2. Write Dart integration tests (test/dynamic_dispatch_test.dart)
- [ ] 3. Run compilation tests (Tests 1-3) — should pass immediately
- [ ] 4. Run end-to-end dispatch test (Test 4) — expect failures, debug
- [ ] 5. Fix whatever is broken in the existing implementation
- [ ] 6. Add REPL tests (Section J)
- [ ] 7. Update claude.md, final test suite, commit and push

## Context

All 5 phases of dynamic dispatch are implemented in the codebase but untested end-to-end. The code exists in compiler.dart (_select/1 generation), body_kernels.dart (_activate), glp_engine.dart (serve/2), glp_activation.dart (activateModule), and runner.dart (Distribute/Transmit handlers). Task is to validate, test, and fix.

Full instructions: `docs/modules/dynamic-dispatch-claude-code-instructions.md`
Test GLP files: `programs/tests/dynamic_dispatch/` (math_service.glp, private_only.glp, single_export.glp)
