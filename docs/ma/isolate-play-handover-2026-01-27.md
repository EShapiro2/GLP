# Isolate Play Alice-Bob-Charlie Handover

**Date:** 2026-01-27
**Status:** Blocked

---

## Summary

IRMA Network Transaction infrastructure is complete and working. The full `play_alice_bob_charlie.glp` test across three isolates is blocked on a channel construction issue.

---

## What's Working

### IRMA Infrastructure (All Passing)
- **Cold-call protocol** across isolates (`isolate_cold_call_test.dart`)
- **Friend-mediated introduction** with 3-agent routing (`isolate_friend_introduction_test.dart`)
- **Bidirectional message flow** through creator-as-hub
- **Network Transaction** serialization/deserialization
- **Variable Table** with VarKey composite keys

### Test Baseline
- 146/161 multiagent tests passing
- 222/223 REPL tests passing

---

## What's Blocked

### Full Play Test (`isolate_play_alice_bob_charlie_test.dart`)

Goals fail immediately at head pattern matching:
```
[bob] Spawned agent_init
[bob] Spawned bob_actor
[bob] All goals completed  ← Immediate failure
```

**Root cause:** When spawning GLP goals from Dart with channel arguments like `ch(X?, Y)`, the manually-constructed `StructTerm` doesn't match what GLP's `new_channel/2` creates. The head pattern matching fails because the reader/writer structure isn't correct.

**The issue is in test harness construction, not IRMA implementation.**

---

## Next Task

Fix the channel term construction in `isolate_play_alice_bob_charlie_test.dart` so that:
1. Channels passed to `agent_init/3` match the expected `ch(Writer, Reader?)` pattern
2. Channels passed to `*_actor/1` match the reversed `ch(Reader?, Writer)` pattern

Options:
1. Run `new_channel/2` as a GLP goal first to create properly structured channels
2. Understand exact heap cell structure and replicate in Dart
3. Simplify the test protocol to avoid complex channel arguments

---

## Key Files

- **Blocked test:** `glp_runtime/test/multiagent/isolate_play_alice_bob_charlie_test.dart`
- **Working tests:** `isolate_cold_call_test.dart`, `isolate_friend_introduction_test.dart`
- **Program:** `programs/typed_book/social_graph/play_alice_bob_charlie.glp`
