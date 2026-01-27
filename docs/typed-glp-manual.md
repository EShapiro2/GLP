# Typed GLP Manual

**Version**: 2.0  
**Date**: 2026-01-27  
**Status**: ACTIVE

This manual captures essential programming principles and advice for writing correct Typed GLP programs. It covers the SRSW (Single-Reader Single-Writer) constraint, type declarations, moding, and common pitfalls.

---

## 1. Procedure Declarations Must Match Data Flow

### 1.1 The Principle

A procedure declaration's argument modes must reflect the actual data flow semantics of the procedure. Getting this wrong causes type errors that may seem puzzling.

**The rule:** If a procedure **returns** a value in some argument position, that position must be an **output** (no `?`). If a procedure **receives** a value, that position must be an **input** (with `?`).

### 1.2 Example: lookup

Consider a `lookup` procedure that searches a list and returns a found value:

**Wrong declaration:**
```prolog
procedure lookup(String?, Integer?, PairList?, PairList).
```

This says arg 2 (`Integer?`) is an **input** — the caller provides the integer. But `lookup` **finds and returns** the integer, so it should be an output.

**Correct declaration:**
```prolog
procedure lookup(String?, Integer, PairList?, PairList).
```

Now arg 2 (`Integer`) is an **output** — the procedure produces it.

### 1.3 Why This Matters

With the wrong declaration `Integer?` (input), the type checker expects consume mode at that position. But the clause:

```prolog
lookup(Key, V?, [pair(K, V)|Rest], [pair(K?, V?)|Rest?]) :-
    Key? =?= K? | true.
```

Has `V?` (a reader) at arg 2. A reader at a consume position is correct for receiving input, but here we're **producing** output. The type checker reports a mode mismatch.

With the correct declaration `Integer` (output), arg 2 is in produce mode. A reader `V?` at a produce position is a "hole" to be filled — exactly right for returning a value.

### 1.4 The Lesson

Before writing clauses, think carefully about each argument:
- Does the **caller provide** this value? → Input (`Type?`)
- Does the **procedure produce** this value? → Output (`Type`)

---

## 2. Head-Body Variable Flow Principle

### 2.1 The Principle

In GLP, the SRSW constraint requires that each variable occurs exactly once as a writer and once as a reader. When data flows between a clause head and its body (as in recursive calls), this creates a specific pattern:

**Data flowing from head to body (input continuation):**
- Head uses a **writer** to receive the value
- Body uses the paired **reader** to consume the value

**Data flowing from body to head (output construction):**
- Head uses a **reader** (a "hole" to be filled)
- Body uses the paired **writer** to produce the value

### 2.2 Why This Works

- A **writer** in the head acts as an "input port" — it receives a value from the caller
- A **reader** in the body "consumes" the value received by its paired writer in the head
- A **reader** in the head acts as an "output port" — a hole that will be filled
- A **writer** in the body "produces" the value that fills its paired reader in the head

### 2.3 Example: Router Clause

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

### 2.4 The Rule of Thumb

For recursive clauses:

1. **Input streams**: Writer decomposes in head, reader passes tail to body
2. **Output streams**: Reader receives hole in head, writer fills/extends in body
3. **Message content**: Writer captures in head, reader uses in head (same clause)

---

## 3. SRSW Relaxation for Constant Types

### 3.1 The Rule

When a variable's type is a **constant type** (`Integer`, `Real`, `Number`, `String`, `Constant`), both the writer and reader may appear multiple times in the clause without violating SRSW.

This is because constant values contain no internal writers and can be safely duplicated.

### 3.2 Example

```prolog
Pair ::= pair(String, Integer).

lookup(Key, V?, [pair(K, V)|Rest], [pair(K?, V?)|Rest?]) :-
    Key? =?= K? | true.
```

Variable `V` has type `Integer`. The reader `V?` appears twice (arg 2 and arg 4). This is permitted because `Integer` is a constant type.

### 3.3 Guard-Based Relaxation

Alternatively, if a guard establishes that a variable is ground, multiple occurrences are also permitted:

```prolog
broadcast(Msg, Out1, Out2, Out3) :- ground(Msg?) |
    send(Msg?, Out1),
    send(Msg?, Out2),
    send(Msg?, Out3).
```

Guards that imply groundness include: `ground/1`, `integer/1`, `number/1`, `string/1`, `constant/1`, and arithmetic comparisons (`</2`, `>/2`, etc.).

---

## 4. Channel Type Convention

### 4.1 Recommended Definition

```prolog
Channel ::= ch(Stream, Stream?).
```

This means:
- Position 1 (`Stream`): output/produce mode — the channel owner writes here
- Position 2 (`Stream?`): input/consume mode — the channel owner reads here

### 4.2 When Consuming a Channel (Channel?)

When a procedure takes `Channel?` as input:
- Position 1 becomes `Stream?` (consume ↓) — reading from the channel
- Position 2 becomes `Stream` (produce ↑) — writing to the channel

This matches the natural intuition: `ch(In?, Out)` where you read from `In` and write to `Out`.

### 4.3 Example

```prolog
procedure agent(_?, Channel?).
agent(Id, ch(In, Out?)) :-
    process_input(In?),
    produce_output(Out).
```

The agent receives `Channel?` (inverted view), reads from position 1 (`In?`), and writes to position 2 (`Out`).

---

## 5. New Channel Creation

### 5.1 The new_channel Pattern

```prolog
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

This creates two "ends" of a bidirectional channel:
- First channel: reads from `Xs?`, writes to `Ys`
- Second channel: reads from `Ys?`, writes to `Xs`

What one end writes, the other reads.

### 5.2 Usage

```prolog
new_channel(AliceCh, BobCh),
alice_process(AliceCh?),
bob_process(BobCh?).
```

Alice and Bob each receive `Channel?`, giving them the consumer's view of their channel end.

---

## 6. Avoid Assignment in Guards and Bodies

### 6.1 The Principle

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

### 6.2 Why This Matters

1. **Clearer intent**: The head pattern shows exactly what structure is expected and produced
2. **Type checking**: The type checker can verify head patterns; body assignments may cause mode mismatches
3. **Efficiency**: Head patterns are matched directly; body assignments add extra reduction steps
4. **SRSW compliance**: Head patterns make variable occurrences explicit and easier to verify

---

## 7. Store Writers, Not Readers, in Lookup Tables

### 7.1 The Problem

When building a lookup table of output streams for later use (e.g., sending messages to named destinations), you must store **writers**, not readers.

**Why?**
- A **writer** can produce values — you can bind it to a term like `[Msg|Rest]`
- A **reader** can only receive values — it waits for its paired writer to be bound

If you store a reader and later try to "send" by pattern-matching it against `[Msg|Rest]`, the match will **suspend** waiting for a value that never comes.

### 7.2 Correct Approach

```prolog
agent_init(Id, ch(UserIn, UserOut?), ch(NetIn, NetOut?)) :-
    merge(UserIn?, NetIn?, In),
    agent(Id?, In?, [(user, UserOut), (net, NetOut)]).  %% Stores writers!
```

When consuming `Channel?` with pattern `ch(In, Out?)`:
- `In` is a **writer** receiving the input stream
- `Out?` is a **reader** at a produce position → its paired **writer** `Out` is available to produce

---

## 8. Single-Unit-Clause Procedures

### 8.1 What They Are

A **single-unit-clause procedure** is a regular procedure defined by exactly one clause with no guards and no body. These procedures can be called from either guard position (unfolded at compile time) or body position (executed at runtime).

Examples from the prelude:

```prolog
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

procedure send(_?, Channel?, Channel).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(_, Channel?, Channel).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

### 8.2 Guard Position Usage

When called in guard position, the partial evaluator unfolds the call at compile time:

```prolog
%% Original
play :- new_channel(AliceCh, BobCh) | alice(AliceCh?), bob(BobCh?).

%% After partial evaluation
play :- alice(ch(Xs?, Ys)?), bob(ch(Ys?, Xs)?).
```

---

## 9. Summary: Variable Flow Table

| Scenario | Head Variable | Body Variable | Explanation |
|----------|---------------|---------------|-------------|
| Input continues to recursive call | Writer `X` | Reader `X?` | Head receives, body consumes |
| Output constructed by recursive call | Reader `X?` | Writer `X` | Body produces, head receives |
| Value used within same clause | Writer `X` and Reader `X?` | — | Both in head, content transfer |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.0 | 2026-01-27 | Renamed to Typed GLP Manual; added Section 1 (Procedure Declarations Must Match Data Flow); added Section 3 (SRSW Relaxation for Constant Types); consolidated and reorganized |
| 1.3 | 2026-01-27 | Added Section 6: Unit Clause Procedures |
| 1.2 | 2026-01-24 | Added Section 5: Store Writers, Not Readers, in Lookup Tables |
| 1.1 | 2026-01-24 | Added Section 4: Avoid Assignment in Guards and Bodies |
| 1.0 | 2026-01-24 | Initial version with Head-Body Variable Flow Principle |
