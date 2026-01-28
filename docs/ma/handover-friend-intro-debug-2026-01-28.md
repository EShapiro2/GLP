# Handover: Friend-Introduction Test Debugging

**Date:** 2026-01-28
**Status:** In Progress
**Branch:** `claude/maglp-dart-isolate-retest-YOrJX`

---

## Summary

The maGLP isolate boot infrastructure is working. Two tests pass (ping-pong, cold-call). The 3-agent friend-introduction test fails: Bob receives Alice's message but doesn't forward to Charlie.

---

## What's Working

| Test | File | Status |
|------|------|--------|
| Ping-pong | `ping_pong_test_boot.glp` | ✓ Pass |
| Cold-call | `cold_call_test_boot.glp` | ✓ Pass |
| Friend-intro | `friend_intro_test_boot.glp` | ✗ Fail |

The IRMA infrastructure correctly:
- Routes network messages between isolates
- Serializes/deserializes terms
- Handles shared variables (response variable in cold-call)
- Fires callbacks when variables are bound

---

## The Bug

**Symptom:** Bob receives `msg(alice, forward(charlie, hello_charlie))` but doesn't relay to Charlie.

**Location:** Bob's `bob_relay` clause in `friend_intro_test_boot.glp`

**Possible causes:**
1. Message structure mismatch (IRMA wraps messages differently than clause expects)
2. Guard failure preventing clause match
3. NetOut stream tail not being tracked for subsequent writes

---

## Files to Read

### GLP Program
```
/Users/udi/Grassroots/GLP/programs/typed_book/social_graph/friend_intro_test_boot.glp
```

### Dart Test
```
/Users/udi/Grassroots/GLP/glp_runtime/test/multiagent/isolate_manager_test.dart
```
Look for the `friend_intro_test_boot.glp` test case.

### IRMA Context (message handling)
```
/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/irma_context.dart
```
Key methods: `handleNetworkMessage()`, `flushMessages()`

### Isolate Manager (routing)
```
/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/isolate_manager.dart
```
Key: `_agentIsolateEntry()`, `_routeNetworkMessage()`

### Boot Loader
```
/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/boot_loader.dart
```

### Spec
```
/Users/udi/Grassroots/GLP/docs/ma/isolate-boot-spec.md
```

---

## How to Run Tests

```bash
# Run all isolate manager tests
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/isolate_manager_test.dart

# Run with verbose output
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/isolate_manager_test.dart --reporter expanded
```

---

## Debugging Steps

### 1. Examine Bob's clause

Read `friend_intro_test_boot.glp` and check:
- What pattern does `bob_relay` expect on NetIn?
- What does it write to NetOut?
- Are there guards that could fail?

### 2. Add tracing

In `isolate_manager.dart` `_agentIsolateEntry()`, add logging to see:
```dart
// After ctx.handleNetworkMessage():
print('[$agentId] NetIn after handle: ${runtime.heap.derefAddr(netInWriter)}');

// Before ctx.flushMessages():
print('[$agentId] NetOut before flush: ${runtime.heap.derefAddr(netOutWriter)}');
```

### 3. Check message wrapping

When IRMA delivers a network message, it binds to NetIn as:
```
[ReceivedTerm | Tail]
```

Verify Bob's clause matches this structure.

### 4. Check output stream tracking

If Bob writes to NetOut, IRMA needs to:
1. Detect the binding
2. Extract the message
3. Track the new tail for subsequent messages

---

## Key Insight from Previous Session

The deferred callback fix was critical: `applySigmaHatFCP` now fires `onBind` callbacks **after** all bindings complete. This fixed the cold-call test where the response variable binding wasn't being detected.

The friend-intro failure may be a different issue — possibly clause matching rather than IRMA routing.

---

## Test Baseline

Before making changes, verify:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime && dart test test/multiagent/isolate_manager_test.dart
```

Expected: ping-pong and cold-call pass, friend-intro fails.

---

## Branch Status

Branch `claude/maglp-dart-isolate-retest-YOrJX` has uncommitted changes to `friend_intro_test_boot.glp`. 

To see status:
```bash
cd /Users/udi/Grassroots/GLP && git status
```

---

## Contact

This work continues the maGLP isolate boot implementation. See:
- `docs/ma/isolate-boot-spec.md` — the `@` operator spec
- `docs/ma/irmaGLP-spec.md` — IRMA message routing spec
