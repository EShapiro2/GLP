# Bug Report: Missing `onBind` Callbacks in Receive Handlers

**Date**: 2026-02-09
**Status**: Open
**Severity**: Critical — blocks all inter-agent communication that involves response variables or nested variables
**File**: `glp_runtime/lib/multiagent/mad_context.dart`

---

## Summary

`registerGlobalSendSpawns()` registers `GlobalSendGoal` objects in the `GlobalSendRegistry`, but does **not** set up `heap.onBind` callbacks on the corresponding writer addresses. Without these callbacks, when a writer is bound by the GLP runtime (via goal reduction), the `onWriterBound` method is never called, the `global_send` goal never fires, and the assignment message is never sent to the remote agent.

The `send()` method (line 497–504) correctly does both:

```dart
// In send() — CORRECT:
globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));
runtime.heap.onBind(spawn.readerAddr, (Term value) {
    onWriterBound(spawn.readerAddr, value);
});
```

But `registerGlobalSendSpawns()` (line 173–178) only does the first part:

```dart
// In registerGlobalSendSpawns() — MISSING onBind:
void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    globalSendRegistry.registerSpawns(spawns);
    // NO heap.onBind callback setup!
}
```

---

## Affected Code Paths

All three Receive transaction handlers call `registerGlobalSendSpawns()` after `localize()`:

1. **`_handleSerializerAssignment`** (line 258) — cold-call receive
2. **`_handleWriterAssignment`** (line 323) — `_w(p,i)` assignment with i > 0
3. **`_handleReaderAssignment`** (line 371) — `_r(p,i)` assignment

Additionally, `handleMadAssignmentWithGlobalNames` (line 414) has the same issue.

---

## Concrete Failure Scenario: Cold-Call with Response Variable

### Protocol

Alice cold-calls Bob with `msg(bob, intro(alice, Resp))`, where `Resp` is an unbound writer that Bob should bind with a response.

### What Happens

1. **Alice sends**: `_send` kernel fires. `globalize()` processes the reader `Resp?` — creates a GlobalizeEntry `(Resp_writer, bob)` at index 1 in Alice's table, replaces `Resp?` with `_r(alice, 1)`. No spawn needed for readers (correct per spec).

2. **Bob receives**: `_handleSerializerAssignment` localizes `_r(alice, 1)`:
   - `localize()` creates fresh pair `(Z_bob_w, Z_bob_r)`, replaces `_r(alice, 1)` with `Z_bob_w` (the writer), and produces spawn: `GlobalSendSpawn(readerAddr=Z_bob_r, globalName=_r(alice,1), destAgent=alice)`.
   - `registerGlobalSendSpawns([spawn])` registers the goal in `GlobalSendRegistry`.
   - **Bug**: No `heap.onBind(Z_bob_w, ...)` callback is set up.

3. **Bob binds the response**: Bob's agent processes the cold-call and binds `Z_bob_w := accept(ch(...))`. The heap changes the writer cell to ValueTag. But since no `onBind` callback exists, `onWriterBound` is never called.

4. **Result**: The `global_send` goal watching `Z_bob_r` never fires. The response `_r(alice, 1) := accept(...)` is never sent back to Alice. Alice's `inject_msg` waits forever on `Resp?`.

### What Should Happen

At step 2, after registering the goal, an `onBind` callback should be set up on `Z_bob_w` (the writer whose reader `Z_bob_r` is watched by the goal). When Bob binds `Z_bob_w`, the callback fires `onWriterBound(Z_bob_r, value)`, which finds the registered `GlobalSendGoal`, globalizes the value, and queues the assignment message to Alice.

---

## Note on `readerAddr` Semantics

The `GlobalSendSpawn.readerAddr` field names the **reader** that the `global_send` goal watches. In the heap model, the writer and reader of a pair share the same address (the writer address), with the reader being an `RoTag` cell pointing to it. The `onBind` callback must be registered on the **writer** address (which is what `spawn.readerAddr` actually points to in practice, since `TermVar.pairedReaderAddr` returns the same address). This should be verified during the fix to ensure the right address is used.

---

## Fix

Add `heap.onBind` callbacks in `registerGlobalSendSpawns()`:

```dart
void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    for (final spawn in spawns) {
        globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));
        runtime.heap.onBind(spawn.readerAddr, (Term value) {
            onWriterBound(spawn.readerAddr, value);
        });
    }
}
```

This matches what `send()` already does at lines 497–504. The same pattern — register goal, set up `onBind` — must be used everywhere goals are created.

---

## Related

- **Spec reference**: madGLP-spec.md Section 5.2 (Localize spawns `global_send` goals for `_r` names)
- **Spec reference**: madGLP-spec.md Section 4 (`global_send` fires when its reader becomes known)
- **Working code path**: `send()` at lines 497–504 correctly registers both goal and callback
