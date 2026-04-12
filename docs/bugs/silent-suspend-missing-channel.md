# Bug: M # goal(...) silently suspends when module M has no active channel

**Date**: 2026-03-14
**Status**: Logged, not yet fixed

## Description

When a clause body contains `M # goal(...)` and module M is loaded but was never activated (no `exported procedure` declarations, or loaded individually without auto-activation), the cross-module call silently suspends instead of failing.

## Expected behavior

Per the runtime spec (`no_more_clauses`): if all clauses are exhausted and U is empty, the goal fails definitively. Since there is no channel for M and nothing will ever provide one, the goal should fail — not suspend.

## Reproduction

Load a module's files individually (not as a project directory) where the module lacks `exported procedure` declarations. Call a procedure that contains `M # goal(...)`. The call suspends silently; the REPL reports "succeeds" but the cross-module call never executed.

## Separate from

The `combinedProgram` module boundary enforcement (tracked in `docs/type system/current_plan.md`). That is about which procedures the REPL can call as top-level goals. This bug is about what happens inside clause bodies when a cross-module dispatch target doesn't exist.
