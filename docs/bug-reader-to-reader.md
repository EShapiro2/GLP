# Bug: Reader-to-Reader Binding in Defined Guard

**Date**: 2026-02-21
**Status**: Fixed — commit 88ec6f7
**Severity**: Critical — reduction succeeds when it should fail

---

## Summary

When a goal passes a reader (`X?`) to a clause that also expects a reader (`Ch?`) via a defined guard, the runtime allows the reduction. Reader-to-reader term matching should fail (a writers substitution assigns only writers, so it cannot make two readers equal — CGLP paper Definition 5, term matching table Definition 10). Instead the runtime succeeds, producing an SRSW-violating resolvent.

---

## Spec Status

**Fixed.** Bytecode spec §12.2 Case 2 (`GetVariable(Xi, Ai, isReader: true)` when arg is reader) now says FAIL, consistent with §6.3 and the paper. The previous spec text incorrectly said "Reader-to-reader needs no conversion" and silently stored the value.

The CGLP paper now includes the term matching table (Appendix, Definition "Term Matching") showing Reader × Reader = fail explicitly, referenced from the body after the Writer MGU definition.

## Code Discrepancy

The runtime code does **not** match the revised spec. Two locations silently succeed instead of failing:

### 1. `GetVariable` reader mode, arg is reader (`runner.dart` ~line 1915)

```dart
} else if (arg is VarRef && cx.rt.heap.isReader(arg.addr)) {
    if (existing == null) {
      final wid = cx.rt.heap.tryWriterForReader(arg.addr);
      cx.clauseVars[varIndex] = wid ?? arg.addr;
    }
    // If existing != null, keep existing value
}
```

**Should:** soft-fail to next clause (reader × reader = fail).

### 2. `UnifyVariable` reader mode, READ mode, value is reader (`runner.dart` ~line 1669)

```dart
if (value is VarRef && cx.rt.heap.isReader(value.addr)) {
    final rid = value.addr;
    final wid = cx.rt.heap.tryWriterForReader(rid);
    cx.clauseVars[varIndex] = wid ?? rid;
    cx.S++;
}
```

**Should:** soft-fail to next clause (reader × reader = fail).

---

## Test File

`programs/tests/typed/test_befriend_intro_bug.glp`:

```prolog
-mode(system).

send(X, ch(In, [X?|Out?]), ch(In?, Out)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).

med(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user',
                befriend_intro(From, Other, Ch?)),
            AgentCh?, AgentCh1),
    ground(From?), ground(Other?), ground(N?) |
    send(befriend_intro(From?, Other?, req(N?)),
         UserCh?, UserCh1),
    N1 := N? + 1,
    med(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), channel(Ch)) | Ps?], N1?).
```

## Goal

```
med(charlie, ch([msg(agent, '_user', befriend_intro(bob, alice, X?)) | Xs], Y), ch(Us?, Vs), [], 2).
```

## REPL Output (verbatim)

```
med(charlie, ch([msg(agent, _user, befriend_intro(bob, alice, X1?)) | Xs], Y), ch(X2?, Vs), [], 2) :- send(befriend_intro(bob, alice, req(2)), ch(X2?, Vs), X3), :=/2(X4, +(2, 1)), med(charlie, ch(Xs?, Y), X5?, [pending(req(2), channel(X6)) | []], X7?)
send(befriend_intro(bob, alice, req(2)), ch(X2?, [befriend_intro(bob, alice, req(2)) | X8?]), ch(X2?, X9)) :- true
:=/2(3, +(2, 1)) :- true
med(charlie, ch(Xs?, Y), ch(X2?, X9), [pending(req(2), channel(X6)) | []], 3) → suspended
Xs = <unbound>
Y = <unbound>
Vs = [befriend_intro(bob, alice, req(2)) | X66]
→ suspended
```

## The Problem

The goal has `X?` (reader) inside `befriend_intro(bob, alice, X?)`. The clause guard has `Ch?` (reader) inside `receive(msg(agent, '_user', befriend_intro(From, Other, Ch?)), ...)`. This is reader-to-reader term matching — it should fail. The runtime allows it and the reduction succeeds.

## Impact

This bug is the root cause of the madGLP introduction crash (`bindWriter called on non-writer cell`). The `typed_ui_mediator.glp` `befriend_intro` clause receives a channel as a reader and stores its writer in a pending list. Because the reader-to-reader binding succeeds when it shouldn't, the writer ends up in two places in the resolvent, violating SRSW. When madGLP's Receive transaction later tries to bind the same writer via its LocalizeEntry, the writer has already been consumed, causing the crash.

## Reproducing

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
echo -e '../programs/tests/typed/test_befriend_intro_bug.glp\n:trace\nmed(charlie, ch([msg(agent, _user, befriend_intro(bob, alice, X?)) | Xs], Y), ch(Us?, Vs), [], 2).\n:quit' | dart run bin/glp_repl.dart
```

## Expected Behavior

The reduction should fail, because reader `X?` in the goal cannot be made equal to reader `Ch?` in the clause by a writers substitution (CGLP paper, Definition 5; term matching table, Reader × Reader = fail).
