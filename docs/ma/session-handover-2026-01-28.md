# maGLP Isolate Boot - Session Handover

**Date:** 2026-01-28
**Status:** In Progress
**Branch:** `main` (plus uncommitted work on `claude/maglp-dart-isolate-retest-YOrJX`)

---

## Overview

This session implemented the `@` operator for declarative isolate spawning in maGLP. The infrastructure is working — 2 of 3 tests pass. One test (3-agent friend-introduction) needs debugging.

---

## What Was Built

### Spec
- **`docs/ma/isolate-boot-spec.md`** (v0.4) — Defines `@` operator syntax and Dart runtime behavior

### GLP Boot Files
- **`programs/typed_book/social_graph/play_alice_bob_charlie_boot.glp`** — Production boot (for real UI)
- **`programs/typed_book/social_graph/play_alice_bob_charlie_test_boot.glp`** — Test boot with GLP actors
- **`programs/typed_book/social_graph/ping_pong_test_boot.glp`** — Minimal 2-agent test ✓
- **`programs/typed_book/social_graph/cold_call_test_boot.glp`** — Shared variable test ✓
- **`programs/typed_book/social_graph/friend_intro_test_boot.glp`** — 3-agent relay test ✗

### Dart Infrastructure
- **`glp_runtime/lib/multiagent/boot_loader.dart`** — Parses boot clause, extracts `SpawnDirective`s
- **`glp_runtime/lib/multiagent/isolate_manager.dart`** — Spawns isolates, routes messages

### Tests
- **`glp_runtime/test/multiagent/boot_loader_test.dart`**
- **`glp_runtime/test/multiagent/isolate_manager_test.dart`**
- **`glp_runtime/test/multiagent/play_with_actors_test.dart`**

---

## Test Status

| Test | Status | Description |
|------|--------|-------------|
| ping-pong | ✓ Pass | Basic message exchange |
| cold-call | ✓ Pass | Shared response variable |
| friend-intro | ✗ Fail | Bob doesn't relay to Charlie |
| full play | ✗ Fail | Blocked by friend-intro issue |

---

## Current Bug

**Friend-introduction test:** Bob receives message from Alice but doesn't forward to Charlie.

**Detailed handover for debugging:** `docs/ma/handover-friend-intro-debug-2026-01-28.md`

---

## Key Fixes Made This Session

1. **PE variable renaming:** `_PE` → `PE_` (variables starting with `_` are anonymous)
2. **Deferred callbacks:** `applySigmaHatFCP` fires `onBind` after all bindings complete
3. **GoalRef equality:** Added `==` and `hashCode` for suspension cleanup
4. **IRMA enqueue fix:** All reactivation paths use `enqueueReactivatedGoal()`
5. **Boot clause stripping:** BootLoader removes `@` syntax before compilation

---

## Files to Read First

1. **`claude.md`** — Project instructions
2. **`docs/DISCIPLINE.md`** — Development standards
3. **`docs/ma/handover-friend-intro-debug-2026-01-28.md`** — Specific debugging handover
4. **`docs/ma/isolate-boot-spec.md`** — The `@` operator spec

---

## How to Run Tests

```bash
# All multiagent tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/

# Just isolate manager tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/isolate_manager_test.dart

# Full REPL tests (baseline)
cd /Users/udi/Grassroots/GLP && bash test/full_run_repl_tests.sh
```

---

## Next Steps

1. **Debug friend-intro test** — Why doesn't Bob relay to Charlie?
2. **Once friend-intro passes** — Full play with actors should work
3. **Then** — Flutter UI integration

---

## Architecture Summary

```
boot :- agent(alice, ch(_?,_), ch(_?,_))@alice, ...

        ↓ BootLoader parses

SpawnDirective(agentId: 'alice', goalFunctor: 'agent')

        ↓ IsolateManager spawns

[Isolate: alice]              [Isolate: bob]
  GlpRuntime                    GlpRuntime
  IrmaContext                   IrmaContext
  agent/3 goal                  agent/3 goal
       ↓                             ↓
    NetOut → Dart Router → NetIn
```

- Each agent runs in its own Dart isolate
- IRMA monitors NetOut, serializes messages
- Dart router delivers to destination isolate
- IRMA deserializes into NetIn

---

## Branch Notes

- `main` has all infrastructure merged
- `claude/maglp-dart-isolate-retest-YOrJX` may have uncommitted debug changes
- Check `git status` before starting work
