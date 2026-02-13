# AgentRuntime Execution Spec

**Status: DRAFT — 2026-02-12**

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

Each agent runs in a separate **window process** (not an isolate — uses `desktop_multi_window`). There is **no tick loop.** Execution is event-driven, triggered by three events:

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
Agent A                     Coordinator                 Agent B
────────                    ───────────                 ────────
goal runs
 → global_send fires
 → message queued to M_p
flushMessages()
 → onMessageReady callback
 → send_mad via DesktopMultiWindow
                            routes to B's window
                            → deliver_mad
                                                        onMadMessageReceived()
                                                         → handleMadAssignment()
                                                         → bindVariable() → activations
                                                         → _runUntilQuiescent()
                                                          → goals run
                                                          → may produce response
                                                          → flushMessages()
                                                          → send_mad back
                            routes to A's window
                            → deliver_mad
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

### 6.2 Introduction protocol — GLP fix applied, runtime bug remains

**Symptom:** After `introduce` + `accept_intro`, `connected()` messages appear but `send(charlie, hi)` from Bob does not deliver to Charlie.

**GLP fix applied (2026-02-13):** Three edits to `social_agent.glp`:

1. **Removed `otherwise` from `inject_msg`** — The second clause of `inject_msg` had `otherwise` in its guard, but the first clause uses `known()` which suspends (never fails), so `otherwise` was dead code.

2. **Replaced `accept_intro` clause in agent/4** — The old clause called `handle_intro_accept(Ch?, ...)` which blocked the entire agent/4 recursion on Outs threading until the channel was known. The new clause decomposes the channel directly in the head pattern:
```
agent(Id, [msg('_user', Id1,
    accept_intro(Other, ch(FIn, FOut?)))
    |UserIn], NetIn, Outs) :-
    Id? =?= Id1?, ground(Other?) |
    add_output(Other?, FOut, Outs?, Outs1),
    lookup_send('_user',
        msg(agent, '_user', connected(Other?)),
        Outs1?, Outs2),
    merge(NetIn?, FIn?, NetIn1),
    agent(Id?, UserIn?, NetIn1?, Outs2?).
```

3. **Removed dead code `handle_intro_accept`** — No longer needed.

**Remaining runtime bug:** The `accept_intro` clause's head pattern `ch(FIn, FOut?)` requires the channel variable to be known (bound to a `ch(...)` structure) at the time agent/4 tries to reduce. The channel arrives from `lookup_pending`, which retrieves it from the pending list. Both `lookup_pending` and agent/4 are spawned from the same ui_mediator reduction.

Trace analysis (2026-02-13) shows:
- `lookup_pending` commits and binds the channel writer (e.g., W343 := `ch(...)`)
- `send` writes `msg(_user, bob, accept_intro(charlie, Ch?))` to the agent's UserIn stream
- agent/4 is reactivated by the UserIn extension and tries to match `accept_intro(charlie, Ch?)` against `accept_intro(Other, ch(FIn, FOut?))`
- agent/4 **suspends** because `Ch?` appears unbound, even though `lookup_pending` already bound the underlying writer

The bug appears to be a runtime issue with how the channel variable's binding propagates through the variable chain. Specifically: `Ch` is allocated as a fresh SRSW pair by the ui_mediator body phase. The writer goes to `lookup_pending` (which binds it), and the paired reader goes into the `accept_intro(Other, Ch?)` message inside `send`. When agent/4 tries to dereference the reader, the writer should already be bound, but the goal suspends as if it were unbound. The suspension is never re-activated because the writer was already bound before the suspension was registered.

**Investigation status:** Debug logging confirmed that `clauseVars` correctly maps the same register index for both occurrences of `Ch` in the ui_mediator clause, and the `SetVariable` instruction correctly derives the paired reader from the writer. The remaining hypothesis is a timing/ordering issue: agent/4's suspension on the channel reader may be registered after `lookup_pending` has already bound the writer, causing the activation to be missed. The `bindWriter` activation mechanism walks the writer's suspension list, but if no suspensions exist yet (because agent/4 hasn't suspended on it yet), no activation occurs. When agent/4 later suspends, it registers on an already-bound writer, but `suspendOnWriter` may not check whether the writer is already bound.

**Next step:** Check `suspendOnWriter` — if the writer is already bound (ValueTag), the suspension should not be registered at all; instead, the goal should be immediately re-enqueued. If `suspendOnWriter` silently stores the suspension on a bound writer, the goal will never wake.

### 6.3 Focus/typing issue

**Symptom:** Cannot type in agent window after switching to another window and back.

**Root cause:** `desktop_multi_window` spawned windows lose keyboard focus when the macOS window manager deactivates them. The current `FocusNode.requestFocus()` calls are insufficient because they only fire on specific events (init, send, tap), not on window re-activation.

---

## 7. Applied and Proposed Fixes

### 7.1 Remove _reactivateSuspendedGoals — DONE

Deleted `_reactivateSuspendedGoals()` method and its call site from `agent_runtime.dart`.

### 7.2 Fix accept_intro GLP logic — DONE

Replaced the `accept_intro` clause in `social_agent.glp` to inline channel decomposition in agent/4's head pattern. Removed dead `handle_intro_accept` predicate and fixed `inject_msg` `otherwise` bug.

### 7.3 Fix channel binding propagation — TODO

Investigate `suspendOnWriter` and `_suspendOnVariable` in `heap_fcp.dart` / `suspend_ops.dart`. If the writer is already bound when the suspension is registered, the goal should be immediately re-enqueued rather than stored as a suspension. This is the likely fix for the remaining introduce protocol bug.

### 7.4 Fix window focus

Listen for window activation events and request focus on the input field when the window regains focus.

---

## 8. Invariants

1. **Every state change that may unblock a goal MUST be followed by `_runUntilQuiescent()`.** The three entry points (initialize, user input, network message) all satisfy this.

2. **A goal must never be enqueued twice.** The `suspended` map and `bindVariable`'s activations list are two views of the same information. Only one path should enqueue.

3. **`flushMessages()` must be called after every drain.** The run loop handles this.

4. **Cross-agent communication is asynchronous.** Each leg is a separate event cycle. The run loop does not need to handle multi-hop round-trips within a single call.
