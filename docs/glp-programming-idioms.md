# GLP Programming Idioms

**Version**: 1.2  
**Date**: 2026-01-24  
**Status**: ACTIVE

This document captures essential programming idioms and principles for writing correct GLP programs, particularly those involving the SRSW (Single-Reader Single-Writer) constraint and typed GLP.

---

## 1. Head-Body Variable Flow Principle

### 1.1 The Principle

In GLP, the SRSW constraint requires that each variable occurs exactly once as a writer and once as a reader. When data flows between a clause head and its body (as in recursive calls), this creates a specific pattern:

**Data flowing from head to body (input continuation):**
- Head uses a **writer** to receive the value
- Body uses the paired **reader** to consume the value

**Data flowing from body to head (output construction):**
- Head uses a **reader** (a "hole" to be filled)
- Body uses the paired **writer** to produce the value

### 1.2 Why This Works

- A **writer** in the head acts as an "input port" — it receives a value from the caller
- A **reader** in the body "consumes" the value received by its paired writer in the head
- A **reader** in the head acts as an "output port" — a hole that will be filled
- A **writer** in the body "produces" the value that fills its paired reader in the head

### 1.3 Example: Router Clause

Consider routing messages between Alice and Bob:

```prolog
Channel ::= ch(Stream, Stream?).
procedure router(Channel?, Channel?).

%% Alice sends response to Bob
router(ch([response(alice, X)|AliceIn], AliceOut?),
       ch(BobIn, [response(alice, X?)|BobOut?])) :-
    router(ch(AliceIn?, AliceOut), ch(BobIn?, BobOut)).
```

**Variable analysis:**

| Variable | Head Form | Body Form | Flow Direction |
|----------|-----------|-----------|----------------|
| `X`/`X?` | `X` (writer) | `X?` (reader) | Head→Head (message content) |
| `AliceIn`/`AliceIn?` | `AliceIn` (writer) | `AliceIn?` (reader) | Head→Body (input continuation) |
| `AliceOut`/`AliceOut?` | `AliceOut?` (reader) | `AliceOut` (writer) | Body→Head (output construction) |
| `BobIn`/`BobIn?` | `BobIn` (writer) | `BobIn?` (reader) | Head→Body (input continuation) |
| `BobOut`/`BobOut?` | `BobOut?` (reader) | `BobOut` (writer) | Body→Head (output construction) |

**Key insight**: The recursive call receives continuation streams via readers (`AliceIn?`, `BobIn?`) and produces output streams via writers (`AliceOut`, `BobOut`).

### 1.4 Common Mistake

The **wrong** approach puts readers in the head where writers should be, creating SRSW violations:

```prolog
%% WRONG: AliceOut? appears in both head and body
router(ch([response(alice, X)|AliceIn?], AliceOut),
       ch(BobIn?, [response(alice, X?)|BobOut?])) :-
    router(ch(AliceIn, AliceOut?), ch(BobIn, BobOut)).
```

This fails because:
- `AliceIn?` is a reader in the head
- `AliceIn` is a writer in the body
- But for head→body flow, we need writer in head, reader in body

### 1.5 The Rule of Thumb

For recursive clauses:

1. **Input streams**: Writer decomposes in head, reader passes tail to body
2. **Output streams**: Reader receives hole in head, writer fills/extends in body
3. **Message content**: Writer captures in head, reader uses in head (same clause)

---

## 2. Channel Type Convention

### 2.1 Recommended Definition

```prolog
Channel ::= ch(Stream, Stream?).
```

This means:
- Position 1 (`Stream`): output/produce mode — the channel owner writes here
- Position 2 (`Stream?`): input/consume mode — the channel owner reads here

### 2.2 When Consuming a Channel (Channel?)

When a procedure takes `Channel?` as input:
- Position 1 becomes `Stream?` (consume ↓) — reading from the channel
- Position 2 becomes `Stream` (produce ↑) — writing to the channel

This matches the natural intuition: `ch(In?, Out)` where you read from `In` and write to `Out`.

### 2.3 Example

```prolog
procedure agent(_?, Channel?).
agent(Id, ch(In, Out?)) :-
    process_input(In?),
    produce_output(Out).
```

The agent:
- Receives `Channel?` (inverted view)
- Reads from position 1 (`In?`)
- Writes to position 2 (`Out`)

---

## 3. New Channel Creation

### 3.1 The new_channel Pattern

```prolog
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

This creates two "ends" of a bidirectional channel:
- First channel: reads from `Xs?`, writes to `Ys`
- Second channel: reads from `Ys?`, writes to `Xs`

What one end writes, the other reads.

### 3.2 Usage

```prolog
new_channel(AliceCh, BobCh),
alice_process(AliceCh?),
bob_process(BobCh?).
```

Alice and Bob each receive `Channel?`, giving them the consumer's view of their channel end.

---

## 4. Avoid Assignment in Guards and Bodies

### 4.1 The Principle

The `=` predicate (assignment/unification) in guards or bodies is typically a sign of sloppy thinking. In GLP, bindings should flow through head patterns rather than explicit assignments.

**Bad pattern:**
```prolog
process(X, Out) :-
    Out = [X?|Rest].  %% Assignment in body - avoid this
```

**Good pattern:**
```prolog
process(X, [X?|Rest]) :-  %% Binding in head pattern
    true.
```

### 4.2 Example: lookup_send_step

**Before (with assignment):**
```prolog
lookup_send_step(Key, Msg, [(K, Out)|Rest], [(K?, Out1?)|Rest?]) :-
    Key? =?= K? |
    Out? = [Msg?|Out1].  %% BAD: explicit assignment
```

**After (binding in head):**
```prolog
lookup_send_step(Key, Msg, [(K, [Msg?|Out1])|Rest], [(K?, Out1?)|Rest?]) :-
    Key? =?= K? | true.
```

The second version expresses the logic directly in the head pattern:
- Input: `(K, [Msg?|Out1])` — the stored stream gets bound to `[Msg?|Out1]` where `Msg?` consumes the message and `Out1` is the fresh continuation writer
- Output: `(K?, Out1?)` — returns the key and the continuation reader

### 4.3 Why This Matters

1. **Clearer intent**: The head pattern shows exactly what structure is expected and produced
2. **Type checking**: The type checker can verify head patterns; body assignments may cause mode mismatches
3. **Efficiency**: Head patterns are matched directly; body assignments add extra reduction steps
4. **SRSW compliance**: Head patterns make variable occurrences explicit and easier to verify

### 4.4 When = Might Be Necessary

Rare cases where `=` is justified:
- Computed values that can't be expressed in patterns
- Conditional binding based on guard results
- Interoperability with external systems

But always ask: "Can I move this binding into the head pattern?" If yes, do it.

---

## 5. Store Writers, Not Readers, in Lookup Tables

### 5.1 The Problem

When building a lookup table of output streams for later use (e.g., sending messages to named destinations), you must store **writers**, not readers.

**Why?**
- A **writer** can produce values — you can bind it to a term like `[Msg|Rest]`
- A **reader** can only receive values — it waits for its paired writer to be bound

If you store a reader and later try to "send" by pattern-matching it against `[Msg|Rest]`, the match will **suspend** waiting for a value that never comes.

### 5.2 Wrong Approach: Storing Readers

```prolog
build_friends(UserOut, NetOut, [(user, UserOut?), (net, NetOut?)]).

agent_init(Id, ch(UserIn, UserOut), ch(NetIn, NetOut)) :-
    merge(UserIn?, NetIn?, In),
    build_friends(UserOut?, NetOut?, Fs),  %% Stores readers!
    agent(Id?, In?, Fs?).
```

This stores `UserOut?` and `NetOut?` (readers) in the list. When `lookup_send_step` later tries:

```prolog
lookup_send_step(Key, Msg, [(K, [Msg?|Out1])|Rest], ...) :-
```

The pattern `[Msg?|Out1]` cannot match an unbound reader — it suspends.

### 5.3 Correct Approach: Storing Writers

```prolog
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [(user, UserOut), (net, NetOut)]).  %% Stores writers!
```

**Mode analysis for `Channel? = ch(Stream?, Stream)`:**
- Position 1: `Stream?` (consume ↓) — so `UserIn` (writer) receives input
- Position 2: `Stream` (produce ↑) — so `UserOut?` (reader in head) pairs with `UserOut` (writer in body)

Now the body has `UserOut` and `NetOut` as **writers** that can be stored and later used to produce messages.

### 5.4 The Key Insight

When consuming `Channel?` with pattern `ch(In, Out?)`:
- `In` is a **writer** receiving the input stream → use `In?` to read
- `Out?` is a **reader** at a produce position → its paired **writer** `Out` is available to produce

This is an instance of the Head-Body Variable Flow Principle (Section 1): `Out?` in the head is a "hole" that the body can fill via its paired writer `Out`.

---

## 6. Summary Table

| Scenario | Head Variable | Body Variable | Explanation |
|----------|---------------|---------------|-------------|
| Input continues to recursive call | Writer `X` | Reader `X?` | Head receives, body consumes |
| Output constructed by recursive call | Reader `X?` | Writer `X` | Body produces, head receives |
| Value used within same clause | Writer `X` and Reader `X?` | — | Both in head, content transfer |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|  
| 1.2 | 2026-01-24 | Added Section 5: Store Writers, Not Readers, in Lookup Tables |
| 1.1 | 2026-01-24 | Added Section 4: Avoid Assignment in Guards and Bodies |
| 1.0 | 2026-01-24 | Initial version with Head-Body Variable Flow Principle |
