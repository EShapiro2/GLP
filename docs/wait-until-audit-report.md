# `wait_until` Audit Report

**Date**: 2026-03-05
**Requested by**: Udi
**Context**: During escrow design for Grassroots Bonds, discovered that `wait_until` fails instead of suspending when time has not passed. Udi states this is an error — it should suspend.

---

## 1. Paper (Source of Truth)

**File**: `/Users/udi/Grassroots/GLP-ICLP-2026/sections/appendix-guards.tex`

The paper's Appendix E "Guards and System Predicates" defines:

```latex
\verb|wait(D)| suspends for \verb|D| milliseconds then succeeds.
\verb|wait_until(T)| succeeds if the current time is at or past timestamp~\verb|T|
(Unix milliseconds), and fails otherwise. Neither guard can be negated.
```

**Verdict**: The paper says `wait_until` **fails** when time hasn't passed. Per Udi, this is **wrong in the paper** — it should suspend until the time arrives, then succeed.

---

## 2. Spec

**File**: `/Users/udi/Grassroots/GLP/docs/guards-reference.md`

```
### ✅ `wait_until(Timestamp)`
**Test if absolute time has passed**

**Semantics**:
- Success: current time (milliseconds since epoch) ≥ Timestamp
- Fail: current time < Timestamp
- Timestamp is non-number: fail
- Timestamp is unbound reader: suspend (handled by caller)

**Note**: Unlike `wait`, this guard does NOT suspend when the time has
not passed — it fails. The caller must arrange for the goal to be retried
later if needed.

**Non-Negatable**: Time-based control flow guard.
```

**Verdict**: The spec explicitly states `wait_until` fails and does NOT suspend. It faithfully reflects the paper but is **wrong for the same reason**. The "Note" even calls out the fail-not-suspend distinction from `wait` as an intentional design choice.

---

## 3. Prelude (Type System)

**File**: `/Users/udi/Grassroots/GLP/glp_runtime/lib/analysis/type_checker/prelude.dart`

```dart
// Time guards
procedure wait(Number?).
procedure wait_until(Number?).
```

Both are declared as builtin procedures in `predefinedProcedureNames` and `builtinProcedures` (`'wait/1'`, `'wait_until/1'`). Both are listed as non-negatable time guards.

**Verdict**: Type declarations are correct — no change needed here.

---

## 4. Runtime Implementation

**File**: `/Users/udi/Grassroots/GLP/glp_runtime/lib/bytecode/runner.dart`

In method `_evaluateGuard`:

```dart
case 'wait_until':
    // wait_until(Timestamp) - Test if absolute time has passed
    // Semantics:
    // - Unbound Timestamp: handled by caller (suspend on reader)
    // - Non-number: fail
    // - current time >= Timestamp: succeed
    // - current time < Timestamp: FAIL (not suspend!)
    if (args.isEmpty) return GuardResult.failure;
    final timestamp = evaluateNumeric(args[0]);
    if (timestamp == null) return GuardResult.failure;
    final now = DateTime.now().millisecondsSinceEpoch;
    return now >= timestamp ? GuardResult.success : GuardResult.failure;
```

**Verdict**: Code returns `GuardResult.failure` when time hasn't passed. **Wrong** — should suspend (like `wait` does) using timer + reader/writer pair.

For comparison, the `wait` guard in the same method correctly suspends:

```dart
case 'wait':
    // Duration > 0: create reader/writer pair, start timer, suspend on reader
    ...
    final (writerAddr, readerAddr) = cx.rt.heap.allocateVariable();
    cx.rt.setWaitReader(cx.goalId, readerAddr);
    cx.rt.incrementPendingTimers();
    Timer(Duration(milliseconds: duration.toInt()), () {
      final reactivated = cx.rt.heap.bindWriterConst(writerAddr, 0);
      for (final goalRef in reactivated) {
        cx.rt.enqueueReactivatedGoal(goalRef);
      }
      cx.rt.decrementPendingTimers();
    });
    cx.U.add(readerAddr);
    return GuardResult.failure;
```

The `wait` guard creates a timer, allocates a reader/writer pair, adds the reader to the suspension set `U`, returns `GuardResult.failure` (which triggers suspension since `U` is non-empty). When the timer fires, it binds the writer, which reactivates the goal via the ROQ.

---

## 5. Correct Semantics (per Udi)

`wait_until(T)` should:
- **Succeed** if current time ≥ T (time has passed)
- **Suspend** if current time < T (time has NOT passed) — using a timer that fires at time T, same mechanism as `wait`
- **Suspend** if T is unbound reader (waiting for T to be bound)
- **Fail** if T is non-number

The implementation should be similar to `wait` but with an absolute deadline rather than relative duration:

```dart
case 'wait_until':
    if (args.isEmpty) return GuardResult.failure;
    final timestamp = evaluateNumeric(args[0]);
    if (timestamp == null) return GuardResult.failure;
    final now = DateTime.now().millisecondsSinceEpoch;
    if (now >= timestamp) return GuardResult.success;

    // Time hasn't passed — SUSPEND until it does (same mechanism as wait)
    final remaining = timestamp.toInt() - now;
    // ... timer + reader/writer pair + suspend ...
```

---

## 6. Code That Depends on the Current (Wrong) Behavior

### `select_bonds_min_maturity` in `bond_agent.glp`

```prolog
select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    wait_until(MinMat?) |
    select_any_bonds(K?, Hs?, Sel, Rem, Got).

select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    otherwise |
    select_bonds_min_maturity_strict(MinMat?, K?, Hs?, Sel, Rem, Got).
```

This relies on `wait_until` **failing** (not suspending) so that `otherwise` fires when time hasn't passed. If `wait_until` suspends instead, the first clause would suspend, `otherwise` would not fire (it fires only when all previous clauses *fail*, not when they *suspend*), and the goal would hang until the time passes — which is the wrong behavior for this use case.

**Fix needed**: This code must be restructured. Instead of `wait_until`/`otherwise`, it needs a two-valued time test (is-past vs is-future). Options:
1. A new guard `time_passed(T)` that truly has two-valued semantics (success/fail, no suspend)
2. Comparing T against `now` using arithmetic: compute `now(Now)`, then guard `T? =< Now?` for the mature case and `otherwise` for the immature case
3. A helper that checks time without blocking

The cleanest option is (2): use `now` (body predicate) + arithmetic comparison. But this requires restructuring since you can't call `now` in a guard — it's a body predicate. However you CAN use `wait_until(0)` to always succeed, then use arithmetic comparison guards on the timestamp.

Actually, the simplest restructuring: the caller (the `agent` clause that calls `select_bonds_min_maturity`) could compute the current time first and pass it as a parameter, then `select_bonds_min_maturity` uses a simple arithmetic comparison `MinMat? =< CurrentTime?` instead of `wait_until`.

### All existing plays (fplay1–fplay9)

Plays 1–4 and 5–9 use maturity 0 exclusively (except play4b). `wait_until(0)` succeeds regardless (current time >> 0), so fixing `wait_until` to suspend instead of fail would NOT break these plays.

### Play 4b (`bond_actors.glp`, `bond_boot.glp`)

Play 4b uses `wait_until` with a future maturity T = now + 500ms. The first redemption happens before T — the `otherwise` clause fires because `wait_until(T)` fails. After `wait(500)`, the second redemption happens after T — `wait_until(T)` succeeds.

**If `wait_until` is fixed to suspend instead of fail**: The first redemption would hang (suspend) instead of falling through to `otherwise`. Play 4b would break.

---

## 7. Summary of Required Changes

| Artifact | Location | Change Needed |
|----------|----------|---------------|
| Paper | `GLP-ICLP-2026/sections/appendix-guards.tex` | Change "fails otherwise" to "suspends until time T" |
| Spec | `GLP/docs/guards-reference.md` | Rewrite `wait_until` semantics: suspend, not fail |
| Code | `GLP/glp_runtime/lib/bytecode/runner.dart` | Implement timer-based suspension (like `wait`) |
| Bond agent | `GLP/programs/typed_book/bonds/bond_agent.glp` | Restructure `select_bonds_min_maturity` to not rely on fail |
| Bond play 4b | `GLP/programs/typed_book/bonds/bond_actors.glp` | May need adjustment depending on restructuring |

---

## 8. FCP Reference Analysis

**FCP has no `wait` or `wait_until` guards.** Timer functionality in FCP/Logix is entirely stream-based:

- **C level** (`timer.c`): A device connects Unix signals (SIGALRM) to FCP stream writers via `set_select_entry`. Open a timer (`{open, real, Stream^}`), set intervals, and the C runtime binds the stream writer when the signal fires.
- **FCP level** (`timer.cp`): An FCP process reads from the timer stream. Suspension happens naturally — the reader is unbound until the device binds the writer. No special guard needed.
- **Guard table** (`guardtable.cp`): Lists ~70 FCP guards. No `wait`, `wait_until`, or any time-related guard. `info/2` can snapshot elapsed time but doesn't block.

So `wait` and `wait_until` are **GLP inventions** with no FCP precedent.

However, GLP's current `wait` guard already follows the correct FCP-inspired pattern translated to Dart:

1. Allocate a reader/writer pair on the heap
2. Start a Dart `Timer` for the duration
3. Add the reader to the suspension set `U`
4. Return `GuardResult.failure` (with non-empty `U`, this triggers suspension, not permanent failure)
5. When the timer fires: bind the writer → ROQ reactivates the goal
6. On resume: `wait` checks the reader is bound → succeeds

This is the Dart equivalent of FCP's C-level `set_select_entry` + signal handler binding the stream writer.

**The fix for `wait_until` is to use exactly this mechanism**, computing `remaining = max(0, timestamp - now)` for the timer duration instead of using a relative duration directly. On resume, `wait_until` re-checks `now >= timestamp` and succeeds.

---

## 9. Recommendation

Fix in this order:
1. Fix the paper (`appendix-guards.tex`) — change "fails otherwise" to "suspends until time T"
2. Fix the spec (`guards-reference.md`) — rewrite `wait_until` semantics
3. Fix the runtime (`runner.dart`) — implement timer-based suspension (like `wait`, with absolute deadline)
4. Restructure `select_bonds_min_maturity` — pass current time as parameter, use arithmetic comparison
5. Test all plays to confirm no regression
