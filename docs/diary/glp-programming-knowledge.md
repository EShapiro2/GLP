# GLP Programming Knowledge

Accumulated knowledge and techniques for GLP programming.

## Mode Inversion Rule

**When calling a predicate, the modes in the goal are the precise inverse of the modes in the head of the clause.**

This applies to all predicate calls, including defined guards.

### Example: Channel Creation

**Unit clause** (defines new_channel as a guard):
```prolog
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

Head modes: `ch(reader, writer), ch(reader, writer)`

**Correct call** (modes inverted):
```prolog
new_channel(ch(PQIn, PQOut?), ch(QPIn, QPOut?))
```

Call modes: `ch(writer, reader), ch(writer, reader)`

### Rationale

The head reader positions receive values from the caller (so caller provides writers). The head writer positions produce values for the caller (so caller provides readers to receive them).

### Common Error

```prolog
% WRONG - all writers in call
new_channel(ch(A, B), ch(C, D))

% CORRECT - modes inverted from head
new_channel(ch(A, B?), ch(C, D?))
```

---

## Channel Argument Mode Rule

**When a channel appears in a goal, its two arguments must be in inverse modes: `ch(In?, Out)` — one reader, one writer.**

- `In?` is the reader end (receive from channel)
- `Out` is the writer end (send to channel)

### Common Error

```prolog
% WRONG - both arguments are readers
ch(QPIn?, PQOut?)

% CORRECT - inverse modes
ch(QPIn?, PQOut)
```

This applies whenever a channel structure is passed as an argument.

---

## Guard-Position SRSW Rule

**Variables used in guard-position calls do NOT count toward SRSW checking. Only HEAD and BODY occurrences count.**

Each variable must have exactly **one writer occurrence in HEAD or BODY positions** (not in guards).

### Example: Channel Creation with SRSW

**Working pattern** (from friend_introduction.glp):
```prolog
social_graph(Id, [msg(user, Id?, introduce(P, Q))|In], Fs) :-
    ground(Id?), ground(P?), ground(Q?),
    new_channel(ch(PQ, QP?), ch(QP, PQ?)) |
    lookup_send(P?, msg(Id?, P?, intro(Q?, ch(QP?, PQ))), Fs?, Fs1),
    lookup_send(Q?, msg(Id?, Q?, intro(P?, ch(PQ?, QP))), Fs1?, Fs2),
    social_graph(Id?, In?, Fs2?).
```

**SRSW counting** (HEAD and BODY only):
- `PQ`: 1 writer in body (line 4: `ch(QP?, PQ)`), 1 reader in body (line 5: `ch(PQ?, ...)`) ✓
- `QP`: 1 writer in body (line 5: `ch(..., QP)`), 1 reader in body (line 4: `ch(QP?, ...)`) ✓

The guard call `new_channel(ch(PQ, QP?), ch(QP, PQ?))` does NOT count toward SRSW.

### Why This Matters

**Failed attempt** (QPIn only in guard):
```prolog
new_channel(ch(PQIn, PQOut?), ch(QPIn, QPOut?)) |
lookup_send(P?, msg(..., ch(QPIn?, PQOut?))), ...
```
Error: "QPIn has 0 writers" — because QPIn only appears in guard, not in HEAD/BODY.

**Key insight**: Variables from guard calls must also appear in HEAD or BODY to satisfy SRSW.

---

## Change Log

- 2025-12-18: Added Mode Inversion Rule based on friend_introduction.glp analysis
- 2025-12-18: Added Channel Argument Mode Rule
- 2025-12-18: Added Guard-Position SRSW Rule (critical finding for fixing SRSW violations)
