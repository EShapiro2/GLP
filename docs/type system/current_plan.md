# Current Plan: Dynamic Module Dispatch

Started: 2026-03-11

## Steps
- [x] 1. Baseline tests (424/424)
- [x] 2. Phase 1: Compiler generates `_select/1` for modules with exports
- [x] 3. Phase 3: `serve/2` system predicate (embedded GLP source)
- [x] 4. Integration: test activation + dispatch via Dart API (8/8 CSSG dispatch tests)
- [x] 6. Auto-activation in `loadSource` for modules with exports
- [ ] 5. Phase 5: `_rpc/2` body kernel + codegen for `M # goal(...)` ← CURRENT
- [ ] 7. REPL tests (Section J), final test suite

## Context

Implement dynamic module dispatch per `docs/type system/dynamic-module-dispatch.md`. The `_activate` body kernel and `activateModule()` are already done. `_select/1` generation (compiler), `serve/2` (GLP system predicate), and auto-activation are now done. Remaining: RPC routing (`M # goal` sends on module channel) and REPL integration.

Full instructions: `docs/modules/dynamic-dispatch-claude-code-instructions.md`
Test GLP files: `programs/tests/dynamic_dispatch/` (math_service.glp, private_only.glp, single_export.glp)
