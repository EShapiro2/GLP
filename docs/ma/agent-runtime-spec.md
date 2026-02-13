# AgentRuntime Execution Spec

**Status: DRAFT — 2026-02-13**

This document specifies when and how the `AgentRuntime` (Flutter UI mode) drives GLP execution. It contrasts with the headless `IsolateManager` model and identifies gaps.

---

## 1. Two Execution Models

### 1.1 Headless (IsolateManager)

Each agent runs in a separate Dart isolate. A **tick loop** drives execution:

```
while not done:
    receive Tick message
    scheduler.drainWithStatus()    // run all runnable goals
    ctx.flushMessages()            // send queued outbound messages
    report status (running / suspended / completed)
```

The tick is an external clock sent by the `IsolateManager` at fixed intervals (default 50ms). Every tick, every agent drains its goal queue and flushes. The tick loop is the **only driver** of execution.

When a `NetworkMsg` (assignment) arrives between ticks, `handleMadAssignment` binds the local writer and calls `runtime.heap.bindVariable()`. The heap's `bindVariable` returns a list of **activations** — GoalRefs that were suspended on the now-bound reader. These are enqueued into the goal queue. On the **next tick**, those goals run.

Key property: **no execution happens during message receipt.** All execution happens in the tick handler.

### 1.2 Visual UI (AgentRuntime)

Each agent runs in a separate **Dart isolate** spawned by the coordinator (single-window architecture). There is **no tick loop.** Execution is event-driven, triggered by three events:

| Event | Entry point | What happens |
|-------|-------------|-------------|
| Initialization | `initialize()` | Compiles program, starts `agent_init/3`, runs until quiescent |
| User input | `injectUserInput(text)` | Parses term, injects into UserIn stream, runs until quiescent |
| Network message | `onMadMessageReceived(from, payload)` | Deserializes, handles assignment/message, runs until quiescent |

The absence of a tick loop means: **if execution is not explicitly triggered after each state change, goals can be stranded.**

---

## 2. Activation Points (When Goals Become Runnable)

A goal becomes runnable when data it was waiting for arrives. There are exactly four mechanisms:

### 2.1 Stream extension (InputInjector.inject)

When user input or a network message term is injected into a stream, the `InputInjector` binds the stream's current writer to `[Term | FreshTail?]`. The `bindVariable` call returns activations — goals that were suspended reading from that stream. The caller enqueues these activations.

**Current code (correct):**
```dart
final activations = _userInput!.inject(term);
for (final goal in activations) {
  _runtime!.gq.enqueue(goal);
}
```

### 2.2 MAD assignment (handleMadAssignment)

When a remote agent sends an assignment (`_w(p,i) := T` or `_r(p,i) := T`), `MadContext.handleMadAssignment` localizes the value and calls `runtime.heap.bindVariable(writerAddr, localizedValue)`. This returns activations. `MadContext` enqueues them via `runtime.enqueueReactivatedGoal()`.

**Current code (correct inside MadContext):** The `_handleSerializerAssignment`, `_handleWriterAssignment`, and `_handleReaderAssignment` methods all call `bindVariable` and enqueue the returned activations.

### 2.3 global_send firing (onWriterBound callback)

When a local writer is bound (during GLP execution), the heap's `onBind` callback fires `MadContext.onWriterBound`, which checks if a `global_send` goal was watching that writer's reader. If so, the message is globalized and queued to `M_p`. This does **not** produce new runnable goals directly — it produces outbound messages, which are picked up by `flushMessages()`.

### 2.4 Scheduler suspension reactivation (_reactivateSuspendedGoals)

**This is the problematic mechanism.** `AgentRuntime._reactivateSuspendedGoals()` scans `runtime.suspended` for goals blocked on readers that are now bound (`heap.isBound(readerId)`), and re-enqueues them.

**Problem:** This mechanism is redundant with 2.2, which already enqueues activations returned by `bindVariable`. The `suspended` map and `bindVariable`'s activation list track the same thing via different paths. Double-enqueuing may explain the duplicate messages observed.

---

## 3. The Run-Until-Quiescent Loop

After each trigger event, `AgentRuntime` runs `_runUntilQuiescent()`:

```
for round in 0..19:
    drain scheduler (run all goals in GQ, up to 1000 cycles)
    processSuspension (currently a no-op in madGLP push model)
    flushMessages (send all queued outbound messages via coordinator)
    if no goals ran AND nothing flushed: break
```

**Purpose of the loop:** A single drain may produce outbound messages (via global_send firing during execution). Flushing those messages doesn't directly produce new local goals, but within the same agent, one goal's output may enable another goal. The loop handles cascading local work.

**What the loop does NOT handle:** Cross-agent round-trips. When agent A sends a message to agent B, agent B processes it and may send a response back to agent A. This response arrives as a new `onMadMessageReceived` event, which triggers a new `_runUntilQuiescent` cycle. Each leg of the round-trip is a separate event-driven cycle.

---

## 4. Cross-Agent Message Flow

```
Agent A (isolate)           Coordinator (main isolate)   Agent B (isolate)
─────────────────           ──────────────────────────   ─────────────────
goal runs
 → global_send fires
 → message queued to M_p
flushMessages()
 → onMessageReady callback
 → send via SendPort
                            routes to B via IsolateRouter
                            → DeliverMad to B's SendPort
                                                        onMadMessageReceived()
                                                         → handleMadAssignment()
                                                         → bindVariable() → activations
                                                         → _runUntilQuiescent()
                                                          → goals run
                                                          → may produce response
                                                          → flushMessages()
                                                          → send via SendPort back
                            routes to A via IsolateRouter
                            → DeliverMad to A's SendPort
onMadMessageReceived()
 → handleMadAssignment()
 → _runUntilQuiescent()
  → goals run
```

Each arrow from coordinator to agent window is an asynchronous method channel call. The event-driven model handles this correctly **as long as** each `onMadMessageReceived` triggers `_runUntilQuiescent`.

---

## 5. Comparison: Headless vs UI Activation

| Mechanism | Headless (IsolateManager) | UI (AgentRuntime) |
|-----------|--------------------------|-------------------|
| Stream extension | `bindVariable` returns activations; next tick runs them | `bindVariable` returns activations; enqueued, `_runUntilQuiescent` runs them immediately |
| MAD assignment | `bindVariable` inside MadContext enqueues activations; next tick runs them | Same, **plus** `_reactivateSuspendedGoals` scans again (REDUNDANT) |
| global_send fire | `onBind` callback queues to M_p; next tick flushes | `onBind` callback queues to M_p; loop flushes in same cycle |
| Cross-agent | Tick loop naturally retries; all agents tick together | Event-driven: each message receipt triggers a new cycle |

The headless model's tick loop is simpler: it retries unconditionally. The UI model must be precise about triggering execution after each state change.

---

## 6. Known Bugs and Their Root Causes

### 6.1 Duplicate messages — FIXED (2026-02-12)

**Symptom:** Every SEND_MAD appeared twice in the trace log.

**Root cause:** `_reactivateSuspendedGoals()` in `agent_runtime.dart` re-enqueued goals that `MadContext.handleMadAssignment` had already enqueued via `bindVariable`'s returned activations.

**Fix applied:** Removed `_reactivateSuspendedGoals()` method and its call site from `agent_runtime.dart`. `MadContext` already handles reactivation correctly via `runtime.enqueueReactivatedGoal()`.

### 6.2 Send not delivering in Flutter UI

**Symptom:** `send(bob, hello_bob)` from Alice does not produce `received(alice, hello_bob)` on Bob in the visual Flutter UI. Steps 1-2 (connect + decision → connected) work correctly.

**Status:** Under investigation. The headless tests also had this issue masked by the loadSource filename collision bug (agents were crashing silently). Now that the filename bug is fixed, the headless protocol may work correctly but needs verification.

### 6.3 Headless tests don't distinguish success from failure

**Symptom:** `isolate_manager_test.dart` tests 2-3 pass even when agents crash, because `Done` messages are added to `_completed` regardless of `msg.success`.

**Status:** Known issue. The filename collision fix (section 5.3) was the root cause of the crashes. With the fix applied, agents no longer crash and the runtime ERROR messages are gone. The tests should still be hardened to check success status.

---

## 7. Applied and Proposed Fixes

### 7.1 Remove _reactivateSuspendedGoals — DONE

Deleted `_reactivateSuspendedGoals()` method and its call site from `agent_runtime.dart`.

### 7.2 Fix loadSource filename collisions — DONE (2026-02-13)

`loadSource()` without `filename:` defaults to key `'_source_'`. Multiple calls overwrite each other in `_loadedPrograms`. Fixed by passing unique filenames (`'shared_$i'`, `'program'`). This was the root cause of the `ERROR: Spawn could not find procedure label: agent/4` messages in the headless tests.

### 7.2b Fix source concatenation — DONE (2026-02-13)

Flutter app concatenated GLP files with `sources.join('\n')` and passed as single string. Parser failed on second file's `-mode(system)`. Fixed by changing `glpSource: String` to `glpSources: List<String>` in `AgentRuntime`, `InitAgent`, and `main.dart`.

### 7.2c GlpEngine constructor loads stdlib — DONE (2026-02-13)

Made stdlib loading mandatory in `GlpEngine({required String stdlibDir})`. All three paths (REPL, IsolateManager, AgentRuntime) now use the same initialization. `enableMadGLP()` loads madPredicates internally.

### 7.3 Fix send delivery in Flutter UI — TODO

Investigate why `send(bob, hello_bob)` from Alice does not deliver to Bob. The message should flow: Alice agent → global_send → MadContext → onMessageReady → coordinator → route to Bob → Bob's onMadMessageReceived → inject into NetIn → agent/4 matches text clause → send_to_user → `received(alice, hello_bob)`.

### 7.4 Harden headless tests — TODO

Change `IsolateManager._completed` to track success separately from failure. Add `allSucceeded` check. Update tests 2-3 to assert success, not just completion.

---

## 8. Invariants

1. **Every state change that may unblock a goal MUST be followed by `_runUntilQuiescent()`.** The three entry points (initialize, user input, network message) all satisfy this.

2. **A goal must never be enqueued twice.** The `suspended` map and `bindVariable`'s activations list are two views of the same information. Only one path should enqueue.

3. **`flushMessages()` must be called after every drain.** The run loop handles this.

4. **Cross-agent communication is asynchronous.** Each leg is a separate event cycle. The run loop does not need to handle multi-hop round-trips within a single call.
