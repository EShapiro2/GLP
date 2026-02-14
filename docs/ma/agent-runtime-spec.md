# Agent Execution Spec

**Status: DRAFT — 2026-02-14**

This document specifies how agents execute in both headless (IsolateManager) and visual UI (AgentRuntime) modes. Both modes use the same event-driven execution model.

---

## 1. Execution Model (Unified)

Each agent runs in a separate Dart isolate. Execution is **event-driven**: agents drain their goal queue and flush outgoing messages in response to events. There is no external clock or tick loop.

```
await for message on receivePort:
    handle message (Start, NetworkMsg, or UIEvent)
    scheduler.drainWithStatus()    // run all runnable goals
    ctx.flushMessages()            // send queued outbound messages
```

Three event types trigger execution:

| Event | What happens |
|-------|-------------|
| `Start` | Initial drain+flush after boot. Kicks off the agent's goal. |
| `NetworkMsg` | Deserialize assignment, bind variables (activating suspended goals), drain+flush. |
| `UIEvent` | Inject user input into stream (activating suspended goals), drain+flush. |

Each event is handled fully (drain+flush) before the next event is processed. The messages produced by flush are routed by the `IsolateManager` (headless) or coordinator (UI) to destination agents, where they arrive as new `NetworkMsg` events. This chain of events drives the entire protocol forward — no polling or periodic triggering is needed.

### 1.1 Headless (IsolateManager)

The `IsolateManager` spawns agent isolates, sends `Start` to each, and routes `NetworkMsg` between them. There is no tick loop. Events (incoming messages) drive execution.

### 1.2 Visual UI (AgentRuntime)

The coordinator spawns agent isolates and routes messages between them, same as headless. Additionally, user input from the Flutter UI arrives as `UIEvent` messages.

### 1.3 Termination

Agents do not detect or report their own termination. The caller (test harness, Flutter app) shuts down the isolates externally when done. This is the same in both modes — the Flutter app kills isolates when the user closes the window; the test harness kills isolates after observing expected behavior or after a timeout.

---

## 2. Activation Points (When Goals Become Runnable)

A goal becomes runnable when data it was waiting for arrives. There are exactly three mechanisms:

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

---

## 3. The Drain-Flush Cycle

After each event, the agent runs a drain-flush cycle:

```
drain scheduler (run all goals in GQ until quiescent)
flushMessages (send all queued outbound messages)
```

**Purpose:** A single drain may produce outbound messages (via global_send firing during execution). Flushing sends those messages to other agents. Within the same agent, one goal's output may enable another goal, but that is handled within the drain itself (the scheduler keeps running until the goal queue is empty or all remaining goals are suspended).

**What the cycle does NOT handle:** Cross-agent round-trips. When agent A sends a message to agent B, agent B processes it and may send a response back to agent A. This response arrives as a new `NetworkMsg` event at agent A, which triggers a new drain-flush cycle. Each leg of the round-trip is a separate event.

---

## 4. Cross-Agent Message Flow

```
Agent A (isolate)           Router (main isolate)        Agent B (isolate)
─────────────────           ─────────────────────        ─────────────────
goal runs
 → global_send fires
 → message queued to M_p
flushMessages()
 → onMessageReady callback
 → send via SendPort
                            routes to B
                            → NetworkMsg to B's SendPort
                                                        receives NetworkMsg
                                                         → handleMadAssignment()
                                                         → bindVariable() → activations
                                                         → drain+flush
                                                          → goals run
                                                          → may produce response
                                                          → flushMessages()
                                                          → send via SendPort back
                            routes to A
                            → NetworkMsg to A's SendPort
receives NetworkMsg
 → handleMadAssignment()
 → drain+flush
  → goals run
```

The router is the `IsolateManager` in headless mode and the coordinator in UI mode. Both perform the same function: receive a `NetworkMsg` from one agent and forward it to the destination agent's `SendPort`.

---

## 5. Known Bugs and Their Root Causes

### 5.1 Duplicate messages — FIXED (2026-02-12)

**Symptom:** Every SEND_MAD appeared twice in the trace log.

**Root cause:** `_reactivateSuspendedGoals()` in `agent_runtime.dart` re-enqueued goals that `MadContext.handleMadAssignment` had already enqueued via `bindVariable`'s returned activations.

**Fix applied:** Removed `_reactivateSuspendedGoals()` method and its call site from `agent_runtime.dart`. `MadContext` already handles reactivation correctly via `runtime.enqueueReactivatedGoal()`.

### 5.2 Mediator pending list stores readers instead of writers — IN PROGRESS (2026-02-14)

**Symptom:** Both dGLP and madGLP with-mediator tests suspend at `bob_ui_wait_alice_msg`. Bob accepts Alice's cold-call and gets `connected(alice)`, but Alice never receives `connected(bob)` because the cold-call response variable binding does not propagate back.

**Root cause:** `typed_ui_mediator.glp` stored `Resp?` (reader) in the pending list instead of `Resp` (writer). When `lookup_response` retrieved the variable and `bind_response` bound it, the binding was on a reader copy — it did not propagate back to Alice's original writer variable that `inject_msg` was waiting on.

**Fix applied (code logic):** Inverted modes on `Resp` and `Ch` in the storage clauses. Removed `response()`/`channel()` wrappers (the pending values are opaque). Merged `lookup_response`/`lookup_channel` into `lookup_pending`.

**Remaining issue (typechecking):** The `ui_mediator` clauses now typecheck. But `lookup_pending` does not — it needs to extract a writer from a pending list that is declared as `PendingList?` (reader). The type `PendingEntry ::= pending(ReqId, _?)` describes the intended structure but `lookup_pending`'s procedure declaration and clause modes need to be reconciled. The pending list is not a stream — it is a finite data structure (escrow table) passed by value. The type system cannot currently express "a reader list containing writer entries" without further work.

### 5.3 Premature death detection removed — DONE (2026-02-13)

**Symptom:** Headless tests appeared to pass but agents were not actually running the protocol. An unspecified "idle tick" heuristic declared agents dead after 2 idle ticks, before any messages arrived.

**Fix applied:** Removed the death detection code from `_agentIsolateEntry`. Agents do not self-terminate; termination is external.

### 5.4 Tick loop removed — DONE (2026-02-13)

**Symptom:** The headless model used an external tick loop (polling) that is not present in the paper or the UI model. This created an unnecessary difference between the two execution modes.

**Fix applied:** Replaced tick-driven execution with event-driven execution. Both headless and UI modes now use the same model: drain+flush on `Start` and on each incoming `NetworkMsg`.

---

## 6. Applied and Proposed Fixes

### 6.1 Remove _reactivateSuspendedGoals — DONE

Deleted `_reactivateSuspendedGoals()` method and its call site from `agent_runtime.dart`.

### 6.2 Fix loadSource filename collisions — DONE (2026-02-13)

`loadSource()` without `filename:` defaults to key `'_source_'`. Multiple calls overwrite each other in `_loadedPrograms`. Fixed by passing unique filenames (`'shared_$i'`, `'program'`). This was the root cause of the `ERROR: Spawn could not find procedure label: agent/4` messages in the headless tests.

### 6.2b Fix source concatenation — DONE (2026-02-13)

Flutter app concatenated GLP files with `sources.join('\n')` and passed as single string. Parser failed on second file's `-mode(system)`. Fixed by changing `glpSource: String` to `glpSources: List<String>` in `AgentRuntime`, `InitAgent`, and `main.dart`.

### 6.2c GlpEngine constructor loads stdlib — DONE (2026-02-13)

Made stdlib loading mandatory in `GlpEngine({required String stdlibDir})`. All three paths (REPL, IsolateManager, AgentRuntime) now use the same initialization. `enableMadGLP()` loads madPredicates internally.

### 6.3 Remove tick loop and death detection — DONE (2026-02-13)

Replaced tick-driven headless execution with event-driven execution. Removed `Tick` message type, tick timer, death detection, and self-termination (`Done` message). Both headless and UI modes now use the same event-driven model.

### 6.4 Fix mediator pending list modes — IN PROGRESS (2026-02-14)

Code logic fixed: writers stored instead of readers, wrappers removed, lookup unified. Blocked on typechecking `lookup_pending` — needs design discussion on how to type a list that carries writer entries in a reader context.

### 6.5 Fix send delivery in Flutter UI — TODO

Investigate why `send(bob, hello_bob)` from Alice does not deliver to Bob. Likely the same mediator bug (5.2) — once the mediator is fixed, re-test.

---

## 7. Invariants

1. **Every event that may unblock a goal MUST be followed by a drain-flush cycle.** The three event types (Start, NetworkMsg, UIEvent) all satisfy this.

2. **A goal must never be enqueued twice.** `bindVariable`'s returned activations are the single path for re-enqueuing suspended goals.

3. **`flushMessages()` must be called after every drain.** The drain-flush cycle handles this.

4. **Cross-agent communication is asynchronous.** Each leg is a separate event. The drain-flush cycle does not need to handle multi-hop round-trips within a single event.

5. **Agents do not self-terminate.** Termination is external — the caller shuts down isolates.
