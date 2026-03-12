# Current Plan: Dynamic Module Dispatch — REPL Integration

Started: 2026-03-12

## Steps
- [x] 1. Baseline tests (424/424), commit
- [x] 2. Write Dart integration tests — 8 tests pass
- [x] 3. Fix _activate bug (direct dispatch, bypass _select)
- [x] 4. All Dart tests pass, 424 REPL tests pass
- [x] 5. Add activateDynamicModule() to GlpEngine
- [x] 6. Add :activate REPL command
- [x] 7. Add REPL tests (Section L) — 4 tests, 428/428 total
- [x] 8. Final test suite, commit and push

## Completed

All steps done. Dynamic module dispatch works end-to-end:
- Dart integration tests: 8/8
- REPL tests: 428/428 (including 4 new Section L tests)

### Key fixes during REPL integration
- **Module context**: Fixed `_extractModuleInfo` to extract imported module names from `imported procedure Module#Proc(...)` declarations. Without this, the Distribute opcode had no module context.
- **Stdlib merge**: Module bytecode must be merged with stdlib (`__root_self__`) before activation, so dispatched procedures can find `:=/2` and other stdlib labels.
- **Idempotent activation**: `activateDynamicModule()` skips if the module is already activated (loadSource auto-activates modules with exports).

## Context

Full instructions: `docs/modules/dynamic-dispatch-repl-instructions.md`
Test GLP files: `programs/tests/dynamic_dispatch/` (math_service.glp, dispatch_client.glp)
