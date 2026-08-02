# madGLP Introduction Bug — Continuation Report

**Date: 2026-02-21**

---

## Status

The introduction step in madGLP stalls for both the headless UI-mediator test and the Flutter app. The root cause is identified: `_handleReaderAssignment` in `mad_context.dart` fails with `bindWriter called on non-writer cell`.

---

## What passes

All pre-existing headless tests pass (72 tests, 5 skipped, 0 failures):

- **social_graph headless test** (`isolate_manager_test.dart` test 2, "runs full play with actor scripts (no UI)"): Uses `play_madglp_boot.glp` + `typed_actors.glp` + `typed_social_agent.glp`. Passes with **no errors**. However, this test does not reach the introduction step — Alice suspends at `alice_wait_bob_connected` and the test just waits 5 seconds then shuts down without assertions. So this test does not exercise the introduction path.

- **UI mediator headless test** (`isolate_manager_test.dart` test 3, "runs full play with UI mediator and UI actors"): Uses `play_ui_madglp_boot.glp` + `typed_social_agent.glp` + `typed_ui_mediator.glp` + `typed_ui_actors.glp` — the exact same GLP files as the Flutter app. This test **passes** (no assertion failures), but only because it also just waits 5 seconds and asserts nothing about introduction completion. The trace log shows two `ERROR` lines.

---

## The bug

When Bob introduces Alice to Charlie (or vice versa), Bob sends the introduction channel to each side. The channel arrives as a nested global name inside a `_r(bob, i)` assignment. `_handleReaderAssignment` localizes the global name, creating a fresh variable pair and storing the writer address in a `LocalizeEntry`. Later, when the actual channel value arrives as `_r(bob, 9) := ch(...)`, `_handleReaderAssignment` looks up the `LocalizeEntry`, finds the stored writer address, and calls `runtime.heap.bindVariable(entry.writerAddr, localizedValue)`. This fails because the cell at that address is no longer a writer — it has `CellTag.ValueTag`.

### Exact trace (Charlie's side)

```
Assignment: _r(bob, 8) := intro(alice, _r(bob, 9))
  → Localizing _r(bob, 9): allocates fresh pair, writer=340, reader=341
  → LocalizeEntry(writerAddr=340, bob, 9) created
  → Localized value = intro(alice, Var@341)
  → Binds writer 338 to intro(alice, Var@341)  ← succeeds, reader 341 is now embedded in the term

Assignment: _r(bob, 9) := ch(_r(bob, 10), _w(bob, 11))
  → Looks up LocalizeEntry for (bob, 9), finds writerAddr=340
  → Tries to bind writer 340
  → ERROR: cell 340 is CellTag.ValueTag, not a writer
```

Between the two assignments, the GLP program's drain cycle runs. During that drain, the agent processes the `intro(alice, Var@341)` term. Reader 341 is the reader of the pair (340, 341). By the time the second assignment arrives, cell 340 has been converted from a writer to a value cell.

### Same bug on Alice's side

```
Assignment: _r(bob, 16) := intro(charlie, _r(bob, 17))
  → LocalizeEntry(writerAddr=328, bob, 17)

Assignment: _r(bob, 17) := ch(_r(bob, 18), _w(bob, 19))
  → Tries to bind writer 328
  → ERROR: cell 328 is CellTag.ValueTag
```

### Why social_graph tests don't hit this

The social_graph actors pass raw channel variables directly (`decision(yes, From?, Resp?)`). There is no UI mediator wrapping channels in `PendingValue`. The introduction path in social_graph is not reached in the headless test anyway (the test suspends early and doesn't assert completion).

---

## What was fixed in this session (unrelated to the introduction bug)

1. **Spec violation in `AgentRuntime._runUntilQuiescent`**: Had a 20-round loop with `drainAsyncWithStatus(maxCycles: 1000)` and a `processSuspension` call. Replaced with spec-compliant single `drainWithStatus()` + `flushMessages()`. All tests still pass.

2. **`mad_cold_call_isolate_test.dart` failure**: Test globalized a reader instead of a writer (per spec Section 10.2). Fixed by changing `TermVar.reader(r, writerAddr: w)` to `TermVar.writer(w, readerAddr: r)`.

---

## How to reproduce

### Expose the bug (headless, no Flutter needed)

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "runs full play with UI mediator" 2>&1 | grep ERROR
```

Expected output (two errors, one for Charlie and one for Alice):
```
[charlie] ERROR handling assignment: Bad state: bindWriter called on non-writer cell at 340 (tag: CellTag.ValueTag)
[alice] ERROR handling assignment: Bad state: bindWriter called on non-writer cell at 328 (tag: CellTag.ValueTag)
```

The test "passes" because it has no assertions about introduction completion — it just waits 5 seconds.

### Confirm non-buggy headless test still works

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test test/multiagent/isolate_manager_test.dart -n "runs full play with actor scripts" 2>&1 | grep ERROR
```

Expected: no output (no errors). This test uses the social_graph GLP files without a UI mediator. It does not reach the introduction step, so it does not hit the bug.

### Run all tests to confirm nothing else is broken

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
dart test 2>&1 | tail -5
```

Expected: 72 passed, 5 skipped, 0 failures.

---

## Files to read

### The bug site
- **`/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/mad_context.dart`** — `_handleReaderAssignment` (around line 373). This is where `bindVariable(entry.writerAddr, ...)` fails.
- **`/Users/udi/Grassroots/GLP/glp_runtime/lib/multiagent/global_writers_table.dart`** — `LocalizeEntry` and `addLocalizeEntry`. Stores the `writerAddr` that later becomes stale.

### The GLP code exercising the bug path
- **`/Users/udi/Grassroots/GLP/programs/typed_book/cssg/typed_social_agent.glp`** — `accept_intro` clause destructures `channel(ch(FIn, FOut?))`.
- **`/Users/udi/Grassroots/GLP/programs/typed_book/cssg/typed_ui_mediator.glp`** — Stores intro channels as `channel(Ch)` in pending list, retrieves on `accept_intro`.

### The non-buggy comparison path
- **`/Users/udi/Grassroots/GLP/programs/tests/agent_roundtrip/typed_actors.glp`** — Passes raw `Ch` variable directly, no `PendingValue` wrapper.
- **`/Users/udi/Grassroots/GLP/programs/tests/agent_roundtrip/play_madglp_boot.glp`** — Boot file for social_graph headless test.

### Test and spec
- **`/Users/udi/Grassroots/GLP/glp_runtime/test/multiagent/isolate_manager_test.dart`** — Contains both tests: test 2 (social_graph, no UI, no errors) and test 3 (UI mediator, errors).
- **`/Users/udi/Grassroots/GLP/docs/ma/agent-runtime-spec.md`** — Execution model spec (drain-flush cycle, activation points).

---

## Next steps

Diagnose why cell 340 (a fresh writer allocated by `localize`) is already bound to a value by the time the corresponding `_r(bob, 9)` assignment arrives. Something between allocation and the assignment converts this writer cell to a value cell.
