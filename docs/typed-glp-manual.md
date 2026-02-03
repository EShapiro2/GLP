# Typed GLP Manual

**Version**: 2.3
**Date**: 2026-02-02
**Status**: ACTIVE

This manual captures essential programming principles and advice for writing correct Typed GLP programs. It covers the SRSW (Single-Reader Single-Writer) constraint, type declarations, moding, and common pitfalls.

---

## 0. Interactive Development Protocol

### 0.1 The "show me" Command

When the user says "show me", display ONLY:
1. Relevant type declaration(s)
2. Procedure declaration
3. The problematic clause(s)

No explanations, no analysis, no additional text. User will propose corrections.

### 0.2 GLP Code Modification Protocol

**🔴 CRITICAL:** Before modifying any `.glp` file:
1. Show the proposed change (old code → new code)
2. Wait for explicit user approval
3. Only then make the edit

Never modify GLP code without showing the user first.

### 0.3 Running GLP Code

Before running or tracing GLP code in the REPL:
1. Show the user which file will be loaded
2. Show the goal that will be executed
3. Wait for approval (or use pre-approved commands from settings)

---

## 1. SRSW Errors Must Be Corrected First

Before addressing type errors, ensure all SRSW (Single-Reader Single-Writer) violations are corrected. The type checker reports both SRSW violations and type errors, but they appear similar. SRSW errors manifest as "Variable mode mismatch" messages.

**How to identify SRSW errors:**
- A variable appears as a writer in multiple places
- A variable appears as a reader in multiple places (without constant-type or ground-guard relaxation)
- A writer/reader pair is not properly threaded through a clause

**Example SRSW error:**
```prolog
%% WRONG: AgentOut1 used as writer twice (in receive result and send result)
ui_relay(ch(AgentIn, AgentOut?), ch(ActorIn, ActorOut?)) :-
    receive(Msg, ch(AgentIn?, AgentOut), ch(AgentIn1, AgentOut1)),
    no_readers(Msg?) |
    send(Msg?, ch(ActorIn?, ActorOut), ch(ActorIn1, ActorOut1)),
    ui_relay(ch(AgentIn1?, AgentOut1?), ch(ActorIn1?, ActorOut1?)).
```

**Corrected:**
```prolog
%% CORRECT: Each variable has exactly one writer and one reader
ui_relay(ch(AgentIn, AgentOut?), ch(ActorIn, ActorOut?)) :-
    receive(Msg, ch(AgentIn?, AgentOut), ch(AgentIn1, AgentOut1?)),
    no_readers(Msg?) |
    send(Msg?, ch(ActorIn?, ActorOut), ch(ActorIn1, ActorOut1?)),
    ui_relay(ch(AgentIn1?, AgentOut1), ch(ActorIn1?, ActorOut1)).
```

The fix: `AgentOut1` and `ActorOut1` needed `?` in the channel outputs from `receive` and `send` (they are readers there, receiving the new output stream), and the writers go to the recursive call.

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

## 9. Anonymous Variables

### 9.1 Definition

An anonymous variable is any variable whose name begins with `_` (e.g., `_`, `_In?`, `_Out`). Anonymous writers may appear in the head, denoting a fresh writer with no paired reader, so that a value assigned to it is discarded. This provides a controlled exception to the SRSW restriction, allowing a process to abandon an input (e.g. an input stream) they are no longer interested in.

### 9.3 Examples

```prolog
%% Discard head and tail of list
second([_, X | _], X?).

%% Discard first output of bar
foo(X) :- bar(_Result, X?).

%% Named anonymous variables improve readability
process([msg(_From, _To, Content)|Rest], Out?) :-
    handle(Content?, Out),
    process(Rest?, Out?).
```

Using named anonymous variables like `_From` and `_To` documents what is being discarded, improving code readability while maintaining the same semantics as plain `_`.

### 9.4 Important Distinction

The symbols `_` and `_?` in **type definitions** are primitive type symbols meaning "any produced term" and "any consumed term" respectively. They are not variables and should not be confused with anonymous variables in program clauses.

```prolog
%% In type definition: _ is a type symbol
Stream ::= [] ; [_|Stream].

%% In clause: _ is an anonymous variable
second([_, X | _], X?).
```

---

## 10. Channel Creation vs Channel Reception

### 10.1 The Principle

A procedure declaration must accurately reflect whether channels are **created** by the procedure or **received** from the caller. This is a common source of mode errors.

**The rule:**
- If a procedure **creates** a channel internally (e.g., via `new_channel`), that argument must be an **output** (no `?`)
- If a procedure **receives** a channel from the caller, that argument must be an **input** (with `?`)

### 10.2 Wrong: Declaring Input but Creating Internally

```prolog
procedure agent_init(_?, Channel?, Channel?).

agent_init(Id, _DartUserCh, ch(NetIn, NetOut?)) :-
    ground(Id?), new_channel(ch(AgentIn, AgentOut?), ActorCh) |
    ui_agent_actor(Id?, ActorCh?),
    merge(AgentIn?, NetIn?, In),
    agent(Id?, In?, [friend(user, AgentOut), friend(net, NetOut)]).
```

This declares args 2 and 3 as `Channel?` (input), but the clause creates channels internally via `new_channel`. **You cannot declare a channel as input and create it internally at the same time.**

### 10.3 The Fix

If channels are created internally, declare them as outputs:

```prolog
procedure agent_init(_?, Channel, Channel).
```

Now the clause **produces** the channels rather than receiving them.

### 10.4 Diagnosing This Error

When you see `→ failed` on a clause that should match, check:

1. Does the procedure declaration say an argument is input (`Type?`)?
2. Does the clause create or construct that value internally?

If both are true, the declaration is wrong — change to output mode (`Type`).

### 10.5 Boot Clauses and External Channels

In madGLP boot scenarios, the Dart runtime may provide channels to agents. In this case:

```prolog
%% Channels come from Dart runtime
procedure agent_init(_?, Channel?, Channel?).

agent_init(Id, UserCh, NetCh) :-
    ground(Id?) |
    agent(Id?, UserCh?, NetCh?).
```

Here the channels truly are inputs — provided by the external runtime. The clause receives them and passes them along.

### 10.6 Hybrid Scenarios

If some channels come from outside and others are created internally:

```prolog
%% UserCh from outside, InternalCh created here
procedure agent_init(_?, Channel?, Channel).

agent_init(Id, UserCh, InternalCh) :-
    ground(Id?), new_channel(InternalCh, PeerCh) |
    peer_process(PeerCh?),
    agent(Id?, UserCh?, InternalCh?).
```

Each argument's mode reflects its actual data flow.

---

## 11. Summary: Variable Flow Table

| Scenario | Head Variable | Body Variable | Explanation |
|----------|---------------|---------------|-------------|
| Input continues to recursive call | Writer `X` | Reader `X?` | Head receives, body consumes |
| Output constructed by recursive call | Reader `X?` | Writer `X` | Body produces, head receives |
| Value used within same clause | Writer `X` and Reader `X?` | — | Both in head, content transfer |

---

## 12. Reserved Constants

Constants beginning with underscore (`'_...'`) are reserved for system use. User programs MUST NOT define or use such constants.

### System-Reserved Constants

| Constant | Purpose |
|----------|---------|
| `'_user'` | User input channel identifier |
| `'_net'` | Network output channel identifier |
| `'_w(p,i)'`, `'_r(p,i)'` | Global variable names in madGLP |

### Compiler Mode Directive

The compiler enforces this restriction in user mode (default). System code requiring these constants must declare `-mode(system).` at the top of the file:

```glp
-mode(system).  %% Allows use of reserved constants

%% Now '_user' and '_net' can be used
agent(Id, [msg('_user', Id, connect(Target))|In], NetIn, Outs) :-
    ...
```

**Rationale**: Reserved constants prevent naming collisions between user-defined identifiers and system channels. Without this restriction, a user could name an agent `user` or `net`, causing ambiguity with system channel identifiers.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.3 | 2026-02-02 | Added Section 12: Reserved Constants (underscore-prefixed constants reserved for system use, `-mode(system).` directive) |
| 2.2 | 2026-02-01 | Added Section 10: Channel Creation vs Channel Reception |
| 2.1 | 2026-01-28 | Added Section 9: Anonymous Variables (any variable starting with `_` is anonymous) |
| 2.0 | 2026-01-27 | Renamed to Typed GLP Manual; added Section 1 (Procedure Declarations Must Match Data Flow); added Section 3 (SRSW Relaxation for Constant Types); consolidated and reorganized |
| 1.3 | 2026-01-27 | Added Section 6: Unit Clause Procedures |
| 1.2 | 2026-01-24 | Added Section 5: Store Writers, Not Readers, in Lookup Tables |
| 1.1 | 2026-01-24 | Added Section 4: Avoid Assignment in Guards and Bodies |
| 1.0 | 2026-01-24 | Initial version with Head-Body Variable Flow Principle |
