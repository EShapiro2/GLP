# Defined Guards: Book Examples Reference

## Status: IMPLEMENTATION COMPLETE

This document provides comprehensive before/after examples of defined guards for use in the GLP book. The partial evaluator automatically transforms clauses using defined guards into their expanded form at compile time.

---

## What Are Defined Guards?

A **defined guard** is a user-defined predicate that:
1. Has exactly **one clause** (unit clause)
2. Has **no guards**
3. Has **no body** (or body is just `true`)

When called in guard position, defined guards are **partially evaluated at compile time** — the guard call is removed and substitutions are applied to the entire clause.

---

## Unit Clause Definitions

### Channel Operations

```prolog
% Add message X to channel's output stream
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

% Remove message X from channel's input stream
receive(X?, ch([X|In], Out?), ch(In?, Out)).

% Create cross-linked channel pair
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

### Type Tests

```prolog
% Test for channel structure
channel(ch(_, _)).

% Test for pair structure
pair(p(_, _)).

% Test for wrapper structure
wrapper(w(_)).
```

---

## Example 1: `make_pair` — Channel Allocation

### With Defined Guard (Source Code)

```prolog
make_pair(C1?, C2?) :- new_channel(C1, C2) | true.
```

**SRSW Analysis:**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| C1 | guard `new_channel(C1, ...)` | head `C1?` | ✓ |
| C2 | guard `new_channel(..., C2)` | head `C2?` | ✓ |

### After Partial Evaluation (Compiled Form)

```prolog
make_pair(ch(X1?, X2), ch(X2?, X1)).
```

**SRSW Analysis:**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| X1 | arg2 `ch(..., X1)` | arg1 `X1?` | ✓ |
| X2 | arg1 `ch(..., X2)` | arg2 `X2?` | ✓ |

**Transformation Trace:**
1. Unit clause (renamed): `new_channel(ch(S1?, S2), ch(S2?, S1))`
2. Unify `C1` with `ch(S1?, S2)` → `{C1 → ch(S1?, S2)}`
3. Unify `C2` with `ch(S2?, S1)` → `{C2 → ch(S2?, S1)}`
4. Apply to head: `C1?` → `ch(S1?, S2)`, `C2?` → `ch(S2?, S1)`
5. Remove guard, body becomes empty fact

---

## Example 2: `relay_send` — Send on Channel

### With Defined Guard (Source Code)

```prolog
relay_send([X|In1], Out?, Ch) :-
    send(X?, Ch?, Ch1) |
    relay_send(In1?, Out, Ch1?).
relay_send([], [], ch([], [])).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| X | head `[X|In1]` | guard `X?` | ✓ |
| In1 | head `[X|In1]` | body `In1?` | ✓ |
| Out | body `Out` | head `Out?` | ✓ |
| Ch | caller | guard `Ch?` | ✓ |
| Ch1 | guard `send(..., Ch1)` | body `Ch1?` | ✓ |

### After Partial Evaluation (Compiled Form)

```prolog
relay_send([X|In1], Out?, ch(ChIn, [X?|ChOut?])) :-
    relay_send(In1?, Out, ch(ChIn?, ChOut)).
relay_send([], [], ch([], [])).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| X | head `[X|In1]` | head `[X?|ChOut?]` | ✓ |
| In1 | head `[X|In1]` | body `In1?` | ✓ |
| Out | body `Out` | head `Out?` | ✓ |
| ChIn | head `ch(ChIn, ...)` | body `ChIn?` | ✓ |
| ChOut | body `ch(..., ChOut)` | head `ChOut?` | ✓ |

**Transformation Trace:**
1. Unit clause (renamed): `send(S1, ch(S2, [S1?|S3?]), ch(S2?, S3))`
2. Unify `X?` with `S1` → `{S1 → X}`
3. Unify `Ch?` with `ch(S2, [S1?|S3?])` → `{Ch → ch(S2, [X?|S3?])}`
4. Unify `Ch1` with `ch(S2?, S3)` → `{Ch1 → ch(S2?, S3)}`
5. Apply to head: `Ch` becomes `ch(ChIn, [X?|ChOut?])`
6. Apply to body: `Ch1?` becomes `ch(ChIn?, ChOut)`

---

## Example 3: `relay_recv` — Receive from Channel

### With Defined Guard (Source Code)

```prolog
relay_recv(In?, [X?|Out1?], Ch) :-
    receive(X, Ch?, Ch1) |
    relay_recv(In, Out1, Ch1?).
relay_recv([], [], ch([], [])).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| In | body `In` | head `In?` | ✓ |
| X | guard `receive(X, ...)` | head `X?` | ✓ |
| Out1 | body `Out1` | head `Out1?` | ✓ |
| Ch | caller | guard `Ch?` | ✓ |
| Ch1 | guard `receive(..., Ch1)` | body `Ch1?` | ✓ |

### After Partial Evaluation (Compiled Form)

```prolog
relay_recv(In?, [X?|Out1?], ch([X|ChIn], ChOut?)) :-
    relay_recv(In, Out1, ch(ChIn?, ChOut)).
relay_recv([], [], ch([], [])).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| In | body `In` | head `In?` | ✓ |
| X | head `[X|ChIn]` | head `X?` | ✓ |
| Out1 | body `Out1` | head `Out1?` | ✓ |
| ChIn | body `ChIn?` | head `[X|ChIn]` | ✓ |
| ChOut | head `ChOut?` | body `ChOut` | ✓ |

**Transformation Trace:**
1. Unit clause (renamed): `receive(S1?, ch([S1|S2], S3?), ch(S2?, S3))`
2. Unify `X` with `S1?` → reader in unit, creates alias `{S1 → X}`
3. Unify `Ch?` with `ch([S1|S2], S3?)` → `{Ch → ch([X|S2], S3?)}`
4. Unify `Ch1` with `ch(S2?, S3)` → `{Ch1 → ch(S2?, S3)}`
5. Apply to head: `Ch` becomes `ch([X|ChIn], ChOut?)`
6. Apply to body: `Ch1?` becomes `ch(ChIn?, ChOut)`

---

## Example 4: `relay` — Full Bidirectional Relay

### With Defined Guard (Source Code)

```prolog
relay([X|In1], Out?, Ch) :-
    send(X?, Ch?, Ch1) |
    relay(In1?, Out, Ch1?).
relay(In?, [X?|Out1?], Ch) :-
    receive(X, Ch?, Ch1) |
    relay(In, Out1, Ch1?).
relay([], [], ch([], [])).
```

### After Partial Evaluation (Compiled Form)

```prolog
relay([X|In1], Out?, ch(ChIn, [X?|ChOut?])) :-
    relay(In1?, Out, ch(ChIn?, ChOut)).
relay(In?, [X?|Out1?], ch([X|ChIn], ChOut?)) :-
    relay(In, Out1, ch(ChIn?, ChOut)).
relay([], [], ch([], [])).
```

**Semantics:**
- Clause 1: When input stream has element `X`, send it to channel
- Clause 2: When channel has element `X`, output it to output stream
- Clause 3: Base case — both streams and channel exhausted

---

## Example 5: `bind_response` — Cold-Call Protocol

### With Defined Guard (Source Code)

```prolog
bind_response(yes, accept(RemoteCh?), LocalCh?) :-
    new_channel(LocalCh, RemoteCh) | true.
bind_response(no, no, none).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| RemoteCh | guard `new_channel(..., RemoteCh)` | head `RemoteCh?` | ✓ |
| LocalCh | guard `new_channel(LocalCh, ...)` | head `LocalCh?` | ✓ |

### After Partial Evaluation (Compiled Form)

```prolog
bind_response(yes, accept(ch(X2?, X1)), ch(X1?, X2)).
bind_response(no, no, none).
```

**SRSW Analysis (clause 1):**
| Variable | Writers | Readers | Valid |
|----------|---------|---------|-------|
| X1 | arg3 `ch(..., X2)` | arg2 `X1` inside accept | ✓ |
| X2 | arg2 `ch(X2?, ...)` inside accept | arg3 `X2?` | ✓ |

**Usage:**
- Caller invokes: `bind_response(yes, ResponseToSend, MyLocalChannel)`
- Returns: `ResponseToSend = accept(ch(TheirIn?, MyOut))` — sent to remote
- Returns: `MyLocalChannel = ch(MyIn?, TheirOut)` — kept locally
- The channels are cross-linked: what I write to `MyOut`, they read from `TheirIn`

---

## Example 6: `switch2x2` — Network Switch

### With Defined Guard (Source Code)

```prolog
% Route from In1 to Out1
switch2x2(In1, In2?, Out1, Out2?) :-
    receive(M, In1?, Ins1), send(M?, Out1?, Outs1) |
    switch2x2(Ins1?, In2, Outs1?, Out2).

% Route from In2 to Out1
switch2x2(In1?, In2, Out1, Out2?) :-
    receive(M, In2?, Ins2), send(M?, Out1?, Outs1) |
    switch2x2(In1, Ins2?, Outs1?, Out2).

% Route from In1 to Out2
switch2x2(In1, In2?, Out1?, Out2) :-
    receive(M, In1?, Ins1), send(M?, Out2?, Outs2) |
    switch2x2(Ins1?, In2, Out1, Outs2?).

% Route from In2 to Out2
switch2x2(In1?, In2, Out1?, Out2) :-
    receive(M, In2?, Ins2), send(M?, Out2?, Outs2) |
    switch2x2(In1, Ins2?, Out1, Outs2?).
```

### After Partial Evaluation (Compiled Form)

```prolog
% Route from In1 to Out1
switch2x2(ch([M|In1], Out1a?), In2?, ch(In1b, [M?|Out1b?]), Out2?) :-
    switch2x2(ch(In1?, Out1a), In2, ch(In1b?, Out1b), Out2).

% Route from In2 to Out1
switch2x2(In1?, ch([M|In2], Out2a?), ch(In1b, [M?|Out1b?]), Out2?) :-
    switch2x2(In1, ch(In2?, Out2a), ch(In1b?, Out1b), Out2).

% Route from In1 to Out2
switch2x2(ch([M|In1], Out1?), In2?, Out1?, ch(In2b, [M?|Out2b?])) :-
    switch2x2(ch(In1?, Out1), In2, Out1, ch(In2b?, Out2b)).

% Route from In2 to Out2
switch2x2(In1?, ch([M|In2], Out2a?), Out1?, ch(In2b, [M?|Out2b?])) :-
    switch2x2(In1, ch(In2?, Out2a), Out1, ch(In2b?, Out2b)).
```

**Semantics:**
- 2×2 switch routing messages between two input and two output channels
- Each clause handles one routing path
- Message `M` is received from one input and sent to one output
- Partial evaluation exposes the channel structure in clause heads

---

## Example 7: `channel` — Simple Type Test

### With Defined Guard (Source Code)

```prolog
channel(ch(_, _)).

test(X, ok) :- channel(X?) | true.
test(_, not_channel) :- otherwise | true.
```

### After Partial Evaluation (Compiled Form)

```prolog
test(ch(_, _), ok) :- true.
test(_, not_channel) :- otherwise | true.
```

**Semantics:**
- Tests if argument has channel structure `ch(_, _)`
- Underscores in unit clause stay as underscores (not renamed)

---

## Example 8: Friend Introduction — Double Channel Allocation

### With Defined Guard (Conceptual)

```prolog
introduce(P?, Q?, PCh?, QCh?) :-
    new_channel(PtoQCh, QtoQCh_remote),
    new_channel(QtoPCh, PtoPCh_remote) |
    PCh = ch(QtoPCh?, PtoQCh?),
    QCh = ch(PtoQCh?, QtoPCh?).
```

Note: This is conceptual — actual implementation threads variables differently.

### After Partial Evaluation (Compiled Form)

The cross-linked channels are constructed directly:

```prolog
introduce(P?, Q?, ch(X2?, X1), ch(X1?, X2)).
```

Where `X1` and `X2` form the cross-linked streams between P and Q.

---

## Writing Rules for Book Examples

### When to Use Defined Guards (Source Form)

1. **Clarity**: Defined guards make code more readable
2. **Abstraction**: Hide channel/data structure details
3. **Modularity**: Unit clauses can be reused across predicates

### SRSW Rules for Defined Guards

1. **Variables in guard structures must have writers**
   - `new_channel(Ch1, Ch2)` — both `Ch1` and `Ch2` are writers here

2. **Head readers must have paired writers**
   - `make_pair(C1?, C2?)` — readers in head need writers in guard

3. **Body variables follow normal SRSW**
   - Recursive calls typically have writers for accumulator outputs

### Common Patterns

| Pattern | Guard Form | Use Case |
|---------|------------|----------|
| Type test | `type(X?)` | Discriminate on structure |
| Channel send | `send(X?, Ch?, Ch1)` | Add to channel output |
| Channel receive | `receive(X, Ch?, Ch1)` | Take from channel input |
| Channel create | `new_channel(C1, C2)` | Allocate cross-linked pair |

---

## Compilation Errors

### Suspension Error (Cannot Reduce)

```
Error: Cannot reduce defined guard at compile time.
  Guard: channel(X?)
  Unbound reader 'X?' cannot be matched against pattern.
```

**Cause**: The guard argument is not bound by head or previous guards.

**Fix**: Ensure variables are bound before the defined guard.

### Failure Error (Dead Clause)

```
Error: Defined guard can never succeed.
  Guard: channel(foo)
  Constant 'foo' does not match pattern 'ch(_, _)'.
```

**Cause**: The guard argument can never match the unit clause pattern.

**Fix**: Remove the dead clause or fix the logic error.

---

## File Locations

| File | Purpose |
|------|---------|
| `lib/compiler/analyzer.dart` | Partial evaluator implementation (lines 409-991) |
| `docs/defined-guards-partial-evaluation.md` | Design specification |
| `glp/test_defined_guards_correct.glp` | Test cases |
| `glp/test_channel_guards.glp` | Channel operation tests |
| `AofGLP/lib/channels/channel_ops.glp` | Unit clause definitions |
| `AofGLP/lib/channels/relay.glp` | Relay with defined guards |
