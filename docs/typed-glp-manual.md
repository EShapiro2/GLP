# Typed GLP Manual

**Version**: 2.20
**Date**: 2026-06-24
**Status**: ACTIVE

This manual captures essential programming principles and advice for writing correct Typed GLP programs. It covers the SRSW (Single-Reader Single-Writer) constraint, type declarations, moding, modules, parameterized types, and common pitfalls.

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

## 2A. Determining Variable Modes in Clause Heads

### 2A.1 The Problem

Given a procedure declaration and its type definitions, what form—writer (`X`) or reader (`X?`)—should each variable take at each position in a clause head?  The answer is straightforward at top-level argument positions, but requires a precise compositional rule when structures with embedded `?` annotations appear in the head.

### 2A.2 The Rule

**Step 1: Determine the structural mode at each position.**

Start with the mode declared for the procedure argument:
- `Type?` in the procedure declaration → ↓ (consume)
- `Type` (no `?`) → ↑ (produce)

Traverse into the type structure.  Each `?` encountered in the type definition **flips** the mode: ↓ becomes ↑, ↑ becomes ↓.  Two flips cancel out.

**Step 2: Choose the variable form in the head.**

- At a ↓ (consume) position → use a **writer** (`X`): it captures the incoming value
- At a ↑ (produce) position → use a **reader** (`X?`): it is a hole to be filled

**Step 3: Body variables follow from SRSW.**

The body occurrence is the paired variable: if the head has `X` (writer), the body has `X?` (reader), and vice versa.

### 2A.3 Worked Example: Signaling Server

Consider a signaling server that coordinates reconnection between agents.  Agents send messages containing reply variables; the server stores these and later binds them when a matching request arrives.

**Type definitions:**

```prolog
SignalReply ::= punch(Constant) ; initiated.

SignalMsg ::= reconnect(Constant, Constant, Constant, SignalReply?)
            ; available(Constant, Constant, SignalReply?).

PendingEntry ::= needs(Constant, Constant, Constant, SignalReply?)
               ; ready(Constant, Constant, SignalReply?).
PendingList ::= [] ; [PendingEntry | PendingList].
```

The `SignalReply?` in `reconnect` and in `PendingEntry` means that these positions carry a reader reference to an unbound `SignalReply` writer—a reply variable that will be bound later.

**Receiving a message with a reply variable:**

```prolog
procedure signal_server(Stream(SignalMsg)?, PendingList?).

signal_server([reconnect(A, B, Proof, ReplyA?)|In], Pending) :-
    verify_friendship(Proof?, A?, B?),
    peer_address(A?, AddrA) |
    find_ready(A?, B?, AddrA?, ReplyA, Pending?, Pending1),
    signal_server(In?, Pending1?).
```

Mode analysis of arg 1 (`Stream(SignalMsg)?`, ↓):

| Position | Type path | Mode | Flips | Head variable |
|----------|-----------|------|-------|---------------|
| List element | `SignalMsg` within `Stream(SignalMsg)?` | ↓ | none | — |
| `reconnect` arg 1 | `Constant` within `SignalMsg` at ↓ | ↓ | none | `A` (writer) |
| `reconnect` arg 2 | `Constant` at ↓ | ↓ | none | `B` (writer) |
| `reconnect` arg 3 | `Constant` at ↓ | ↓ | none | `Proof` (writer) |
| `reconnect` arg 4 | `SignalReply?` at ↓ | **↑** | `?` flips ↓→↑ | `ReplyA?` (reader) |
| List tail | `Stream(SignalMsg)` at ↓ | ↓ | none | `In` (writer) |

The `?` on `SignalReply?` in the type definition flips the mode from ↓ to ↑ at arg 4 of `reconnect`.  Hence `ReplyA?` is a reader—a hole through which the server will later send a reply back to agent A.

In the body, `ReplyA` (writer, the SRSW pair) is passed to `find_ready` at its output position.

**Storing the reply variable in a pending list:**

```prolog
procedure find_ready(Constant?, Constant?, Constant?, SignalReply,
                     PendingList?, PendingList).

%% No match found — store A's request for later
find_ready(A, B, AddrA, ReplyA?, [], [needs(A?, B?, AddrA?, ReplyA)]).
```

Mode analysis of the head-constructed `needs(A?, B?, AddrA?, ReplyA)` in arg 6 (`PendingList`, ↑):

| Position | Type path | Mode | Flips | Head variable |
|----------|-----------|------|-------|---------------|
| `needs` arg 1 | `Constant` within `PendingEntry` at ↑ | ↑ | none | `A?` (reader) |
| `needs` arg 2 | `Constant` at ↑ | ↑ | none | `B?` (reader) |
| `needs` arg 3 | `Constant` at ↑ | ↑ | none | `AddrA?` (reader) |
| `needs` arg 4 | `SignalReply?` at ↑ | **↓** | `?` flips ↑→↓ | `ReplyA` (writer) |

The same `?` on `SignalReply?` now flips in the opposite direction: ↑→↓.  Hence `ReplyA` is a writer—it stores the reply variable in the pending list for later retrieval.

The variable pair in this clause: `ReplyA?` (reader at arg 4, ↑) and `ReplyA` (writer at arg 6 inside `needs`, ↓).  Both occur in the head—this is the head-to-head transfer from Section 2.3.

**Binding the reply variable:**

```prolog
procedure complete_rendezvous(Constant?, SignalReply, SignalReply).
complete_rendezvous(AddrA, initiated, punch(AddrA?)).
```

- Arg 1 (`Constant?`, ↓): `AddrA` writer — captures the address
- Arg 2 (`SignalReply`, ↑): `initiated` constant — produced as reply to A
- Arg 3 (`SignalReply`, ↑): `punch(AddrA?)` — produced as reply to B, with `AddrA?` (reader at ↑) filling in A's address

### 2A.4 Summary

The rule composes recursively: at any depth within a structure in the head, the mode is determined by starting from the argument's declared mode and flipping at each `?` in the type path.  Writers capture at ↓ positions, readers are holes at ↑ positions.

| Structural mode | Head variable form | Semantic role |
|-----------------|-------------------|---------------|
| ↓ (consume) | writer `X` | captures incoming value |
| ↑ (produce) | reader `X?` | hole to be filled |

This is the default for ordinary captures and constructions.  For forwarding a reader or writer through an output structure, the head form is determined by the forwarding direction; see §15 (reader-forwarding) and §15B (writer-forwarding) for the conventions and the role of `?` in the type definition.

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

### 3.4 Guard Occurrences and SRSW Counting

A reader `X?` appearing in a guard does **not** count toward the single-reader limit in the head+body.  Specifically, if `X?` occurs in a guard, its paired writer `X` must occur in the head, and `X?` may additionally occur once in the head+body.

This rule is stated in the Moded-Types paper (Remark on Guards and SRSW):

> Guard occurrences count toward SRSW satisfaction: if X? occurs in a guard, its paired writer X must occur in the head and X? may additionally occur once in the body.

**Example:**

```prolog
foo(X, Y?) :- known(X?) | bar(X?, Y).
```

Here `X?` appears twice in the clause — once in the guard (`known(X?)`) and once in the body (`bar(X?, Y)`).  The guard occurrence does not consume the single-reader allowance, so the single body occurrence is valid.  Note that `known/1` does not imply groundness, so this is not the ground-guard relaxation of Section 3.3 — it is the guard-occurrence rule.

Combined with Section 3.3: if a groundness-implying guard like `ground(X?)` is present, `X?` may appear multiple times in both guard and head+body positions.

---

### 3.5 Type Aliases Do Not Inherit Constant-Type Relaxation

A type alias of a primitive type, such as

```prolog
Agent ::= Constant.
Epoch ::= Integer.
```

is structurally equivalent to the primitive (Section 20.3 type identity is structural).  Aliases improve documentation: `stream_update(Agent, Epoch)` is clearer than `stream_update(Constant, Integer)`.

However, the SRSW relaxation of Section 3.1 (which permits multiple readers of `Constant`, `Integer`, `Number`, `String`, `Real`) does **not** automatically transfer through the alias.  A variable typed as `Agent` is treated by the SRSW checker as a non-constant-type variable; multi-reader use without a ground guard is rejected with:

> Reader variable X? occurs 2 times without ground guard or constant type

The workaround is an explicit ground guard (Section 3.3):

```prolog
broadcast(A, Out1, Out2) :- ground(A?) | send(A?, Out1), send(A?, Out2).
```

When designing aliases, either expect the explicit guard at use sites, or refer to the underlying primitive type directly in procedure declarations where multi-reader use is needed without a guard.

---

## 4. Channel Type Convention

### 4.1 Definition

```prolog
Channel(In, Out) ::= ch(In, Out?).
```

This means:
- Position 1 (`In`): output/produce mode — the channel owner writes here
- Position 2 (`Out?`): input/consume mode — the channel owner reads here

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
procedure new_channel(Channel(X, Y), Channel(Y, X)).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
```

This creates two "ends" of a bidirectional channel:
- First channel: reads from `Xs?`, writes to `Ys`
- Second channel: reads from `Ys?`, writes to `Xs`

The type parameters `X` and `Y` capture the asymmetry: what one end reads, the other writes.

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

The `=` predicate (assignment) in guards or bodies is typically a sign of sloppy thinking.  In GLP, bindings should flow through head patterns rather than explicit assignments.

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

## 8. Guards: What May Appear in a Guard

### 8.1 The Guard Rule

A guard expression must be **compile-time unfoldable**.  After partial evaluation, only the built-in three-valued operations may remain — `ground/1`, `known/1`, `=?=/2`, the arithmetic comparisons (`</2`, `>/2`, `=:=/2`, `=\=/2`, `=</2`, `>=/2`), the type tests (`integer/1`, `number/1`, `string/1`, `constant/1`, `compound/1`, `list/1`), and `otherwise`.  Anything else, the runtime cannot evaluate at guard time, and the guard is rejected at load with `Unknown guard predicate`.

The partial evaluator unfolds a call by substituting its body for the call site.  **A call is unfoldable iff its full call graph terminates at the built-ins above.**  In particular:

- **Recursive procedures are NOT unfoldable.**  Direct recursion (a procedure calling itself) or mutual recursion produces either an infinite expansion or a residual call — neither is admissible in a guard.  **No recursive procedure may appear in a guard.**
- **Single-unit-clause procedures (one clause, no body) are always unfoldable** — substitution is a one-step identity.  This is the *safe special case* the PE is guaranteed to handle (§8.2); it is convenience, not a separate language construct.
- **Multi-clause non-recursive procedures** are in principle unfoldable (enumerate clauses, substitute each), but the current PE accepts only the single-unit-clause case.  Until extended, treat any procedure with more than one clause as off-limits in guards.

**Practical implication.**  A precondition of the form "**∃ x ∈ list such that P(x)**" cannot be a guard, because list traversal is recursive.  Put such tests in the clause body via dispatch (a helper procedure with `otherwise` fallthrough), or restructure so the precondition is decided by head pattern matching against a known position.

### 8.2 Single-Unit-Clause Procedures: the Safe Special Case

A **single-unit-clause procedure** is a regular procedure defined by exactly one clause with no guards and no body.  These are the always-unfoldable case of §8.1 and the standard idiom for user-defined guards.  In general they are NOT expected to work as body predicates.

The root self.glp defines several single-unit-clause procedures.  The PE automatically includes them when processing any program, so user programs do not need to redefine them.  Multi-clause procedures defined in the root `self.glp` (e.g. `merge/3`) resolve through the ancestor scope chain like any ancestor `self.glp` definition, subject to innermost-first shadowing; unfolding applies only to single-unit-clause procedures, as an optimisation.  User programs may override a root self.glp unit clause by defining a procedure with the same name/arity.

Examples from root self.glp:

```prolog
procedure =(_?, _).
X? = X.

procedure new_channel(Channel(X, Y), Channel(Y, X)).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(X, Channel(Stream(X), Y)?, Channel(Stream(X), Y)).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

The `=` predicate performs assignment: the call `X = T` assigns the value `T` on the right to the writer `X` on the left.  Using `=` in clause bodies should be avoided where head construction suffices (Section 6).

### 8.3 Guard-Position Unfolding

When a single-unit-clause procedure is called in guard position, the partial evaluator substitutes its body in line:

```prolog
%% Original
play :- new_channel(AliceCh, BobCh) | alice(AliceCh?), bob(BobCh?).

%% After partial evaluation
play :- alice(ch(Xs?, Ys)?), bob(ch(Ys?, Xs)?).
```

The substitution leaves only built-ins (here: the implicit channel-creation bindings) and so satisfies the §8.1 rule.

---

## 9. Anonymous Variables

### 9.1 Definition

An anonymous variable is any variable whose name begins with `_` (e.g., `_`, `_In?`, `_Out`). Anonymous writers may appear in the head, denoting a fresh writer with no paired reader, so that a value assigned to it is discarded. This provides a controlled exception to the SRSW restriction, allowing a process to abandon an input (e.g. an input stream) they are no longer interested in.  Only anonymous writers are permitted in clause positions; `_?` (anonymous reader) is not allowed.

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
%% In type definition: _ is a type symbol (system builtins only)
procedure ground(_?).

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

## 13. Metainterpreter Pattern

### 13.1 Standard Metainterpreter

The standard metainterpreter pattern for running GLP goals with arithmetic:

```prolog
run(true).
run((A, B)) :- run(A?), run(B?).
run(A) :- otherwise | reduce(A?, B), run(B?).

% Handle := in metainterpreter
reduce((X?:=T), true) :- X:=T?.
```

This enables `run(factorial(5, F))` to work with arithmetic operations.

### 13.2 How It Works

1. `run(true)` - Base case: nothing to do
2. `run((A, B))` - Conjunction: run both parts
3. `run(A)` - Otherwise: reduce the goal and run the result

The `reduce/2` clause for `:=` handles arithmetic assignment by extracting the assignment and executing it.

---

## 14. Precise Typing via Renamed Procedures (OBSOLETE)

**This section is obsolete.** Parameterized types (Section 17) eliminate the need for renamed procedure copies. The root self.glp's generic procedures (`send`, `receive`, `new_channel`, `merge`) now have parameterized signatures, and the type checker infers precise types at each call site. Use the generic procedures directly.

### 14.1 Historical Context

Before parameterized types, generic procedures declared with monomorphic types (e.g., `procedure send(_?, Channel?, Channel)`) assigned generic output types in body position. To preserve precise types, modules defined renamed copies with precisely-typed signatures (e.g., `send_agent`, `send_user`, `merge_net_in`). The clause bodies were identical to the originals — only the name and type declaration differed.

### 14.2 Current Practice

With parameterized types, write:

```prolog
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).
```

The type checker instantiates `X` (and `Y`) from context. No renamed copies are needed.

All renamed copies (`send_agent`, `send_user`, `merge_net_in`, `new_friend_channel`, etc.) have been removed from the codebase as of Step 2.5 (March 2026).

---

## 15. Receiving and Forwarding Non-Ground Variables

### 15.1 The Pattern

When a process receives a message containing a non-ground variable (e.g., an unbound response variable) and needs to pass it to another concurrent process, use the standard writer/reader pattern:

```prolog
p(..., [msg(From, escrow_offer(Time, Result))|Rest], ...) :-
    ... |
    monitor(Result?, ...),
    p(..., Rest?, ...).
```

- `Result` (writer, no `?`) in the HEAD — receives the variable from the message
- `Result?` (reader) in the BODY — passes it to the monitoring process
- Total: 1 writer + 1 reader = SRSW satisfied

This is the same pattern as Section 2's "input continues to recursive call," applied to inter-process communication rather than recursion.

### 15.2 Common Mistake

Writing `Result?` (reader) in the head:

```prolog
%% WRONG — 2 readers, 0 writers
p(..., [msg(From, offer(Time, Result?))|Rest], ...) :-
    ... |
    monitor(Result?, ...).   %% SRSW violation!
```

This mistake often arises when the programmer sees `?` in the type definition (e.g., `offer(Constant, SomeType?)`) and assumes the clause variable must also carry `?`.

### 15.3 Existing Examples in the Codebase

The bond agent uses this pattern in every protocol:

- Cold-call: `intro(From, Resp?)` in the message → `Resp` (writer) in head, `Resp` forwarded as writer in outgoing befriend message
- Credit: `credit_propose(K, Maturity, Bonds, CreditResp?)` → `CreditResp` forwarded as writer to mediator
- Trade: `trade_propose(WantSpec, Bonds, TradeResp?)` → `TradeResp` forwarded as writer to mediator
- Escrow: `escrow_offer(Time, BenResult?)` → `BenResult` (writer) in head, `BenResult?` (reader) passed to inject

The escrow case is notable because the receiver calls `inject` directly rather than forwarding the writer to another message. The SRSW pattern is the same either way.

---

## 15B. Forwarding a Writer Through a Structure (dual of §15)

### 15B.1 The Convention

When a clause receives a structure carrying a stream variable and needs to forward the **writer** (not the reader) to a downstream consumer through an output structure, the convention is:

- the position in the type definition MUST carry `?`;
- the head form at the input arg is reader `X?`;
- the head form inside the output structure is writer `X` (no `?`).

This gives 1 reader + 1 writer across head args — SRSW-compliant.  The `?` on the type definition is what enables this: at an output (↑) position, the `?` flips the structural mode to ↓ inside, and the head form there is writer.  Without `?` the output structure conveys a reader, and the downstream consumer can only read.

### 15B.2 Worked Example: add_output

```prolog
OutputEntry ::= output(OutputKey, Stream?).
OutputsList ::= [] ; [OutputEntry | OutputsList].

procedure add_output(OutputKey?, Stream, OutputsList?, OutputsList).
add_output(Key, Out?, Outs, [output(Key?, Out) | Outs?]).
```

- Arg 2 (`Stream` at ↑): head form `Out?` (reader, captures from caller).
- Output arg 4, inside `output(...)`: position 2 is `Stream?` (type has `?`); at ↑ + `?` flip → ↓ → writer form.  Head form `Out` (no `?`), forwarding the writer.

The downstream consumer receives the OutputsList and binds the variable at position 2 by head pattern, writing to the stream:

```prolog
lookup_send_step(Key, Msg, [output(K, [Msg?|Out1?])|Rest],
                            [output(K?, Out1)|Rest?]) :-
    Key? =?= K? | true.
```

Here `lookup_send_step` writes the message and a fresh tail-writer at the head of the stream, exactly because the structure carried the writer forwarded by `add_output`.

### 15B.3 Reader-Forwarding vs Writer-Forwarding

| Forwarding | Type carries `?` | Input head form | Output head form |
|---|---|---|---|
| Reader (§15) | no | writer `X` | reader `X?` |
| Writer (§15B, this section) | yes | reader `X?` | writer `X` |

Both compose into SRSW pairs across head args.  The `?` in the type definition selects between them.

### 15B.4 Common Misconception

SRSW does not prevent writer-forwarding through a structure.  If a writer must be forwarded but the type lacks `?` at that position, add the `?`.  Do not redesign the architecture around the absence — the `?` is what enables the forwarding.

A practical sign of the misconception: arriving at a design where "the play does the wiring by hand" because "the substrate cannot forward writers through a list".  Re-examine the type definition first; the fix is almost always one `?`.

---

## 16. `?` in Type Definitions vs `?` on Clause Variables

### 16.1 The Distinction

The `?` symbol appears in two different contexts with different meanings:

- **In type definitions**: `?` describes the mode of data as it flows through the structure. For example, `credit_propose(Constant, Constant, BondList, CreditResponse?)` means the fourth position carries a reader reference — the data at that position is a reader pointing to an unbound writer.

- **On clause variables**: `?` marks the reader half of a variable in that clause. `X?` reads the value that `X` (the writer) provides.

These are independent. The `?` in a type definition does NOT constrain the clause variable to be a reader.

### 16.2 Why This Matters

When matching against `credit_propose(K, Maturity, Bonds, CreditResp?)` in a clause head:

```prolog
p(..., [msg(From, credit_propose(K, Maturity, Bonds, CreditResp?))|Rest], ...) :-
```

The `CreditResp?` extracts the reader from the message. But `CreditResp` (the writer half) is also available in the clause. If the body needs to forward this variable, it uses the writer `CreditResp`:

```prolog
    send_msg(credit_proposed(From?, K?, CreditResp, Bonds?), ...),
```

The type's `?` describes what's IN the data structure. The clause variable's `?` describes how you USE the variable in this particular clause.

### 16.3 Contrast with Section 9.4

Section 9.4 notes that `_` and `_?` in type definitions are type symbols (meaning "any type"), not anonymous variables. Section 16 generalizes this: ALL `?` annotations in type definitions describe data modes, not clause variable constraints.

### 16.4 Polarity Must Agree Across a Boundary the Checker Cannot Span

The checker compares a writer and its reader only when both occur in one program. When a channel crosses a boundary the checker does not span — two modules typed against different message definitions, or two agents — each side is checked against its own definition and the two are never compared. If the producer's slot carries no `?` (it forwards a reader) while the consumer's slot carries `?` (it expects to write), both sides type-check yet the channel stalls: a reader meets a reader and never commits. Make the slot polarity identical on both sides. This is the runtime boundary of the Moded-Types paper, §Type-Compatible Attestation Between Agents.

---

## 17. Parameterized Types

### 17.1 Overview

Parameterized types allow generic type definitions with type parameters. They are syntactic sugar: each use is expanded into a monomorphic type before type checking. This eliminates the need for renamed procedure copies (Section 14) when generic procedures are used in body position.

### 17.2 Defining Parameterized Types

Type parameters are uppercase identifiers in parentheses after the type name:

```prolog
Stream(X) ::= [] ; [X | Stream(X)].
Pair(A, B) ::= pair(A, B).
Channel(In, Out) ::= ch(In, Out?).
```

Mode annotations within the template (e.g., `Out?`) are preserved during expansion.

### 17.3 Parameterized Procedure Declarations

Use type parameters in procedure declarations to express uniform behaviour:

```prolog
procedure merge(Stream(X)?, Stream(X)?, Stream(X)).
procedure send(X?, Channel(Y, Stream(X))?, Channel(Y, Stream(X))).
procedure receive(X, Channel(Stream(X), Y)?, Channel(Stream(X), Y)).
procedure new_channel(Channel(X, Y), Channel(Y, X)).
```

The type parameter `X` is implicitly universally quantified. The type checker infers its binding from the call context by structural matching.

### 17.4 Using Parameterized Types

Use concrete instantiations in procedure declarations:

```prolog
AgentMsg ::= befriend(Constant, Response?) ; connected(Constant) ; rejected.

procedure agent_merge(Stream(AgentMsg)?, Stream(AgentMsg)?, Stream(AgentMsg)).
agent_merge(A, B, C) :- merge(A?, B?, C).
```

The call `merge(A?, B?, C)` matches `Stream(X)` against `Stream(AgentMsg)`, infers `X = AgentMsg`, and type-checks the body against the expanded monomorphic declaration.

### 17.5 In Module Declarations

Imported procedure declarations instantiate type parameters to the local message type:

```prolog
imported procedure merge(Stream(CounterCall)?, Stream(CounterCall)?, Stream(CounterCall)).
```

The defining module's parameterized declaration `merge(Stream(X)?, ...)` is instantiated at the importing site.

### 17.6 What Parameterized Types Replace

With parameterized types, the following Section 14 workarounds are no longer needed:

| Before (renamed copy) | After (parameterized) |
|-----------------------|-----------------------|
| `merge_agent(AgentStream?, ...)` | `merge(Stream(AgentMsg)?, ...)` |
| `send_agent(AgentMsg?, AgentChannel?, ...)` | `send(X?, Channel(Y, Stream(X))?, ...)` |
| `new_agent_channel(AgentChannel, ...)` | `new_channel(Channel(X,Y), ...)` |

The generic procedures `merge`, `send`, `new_channel` work directly with precise types through parameter inference.

### 17.7 Per-Instantiation Checking

A parameterized procedure has no type of its own; it is checked once per instantiation, the type checker matching each call's argument types against the declaration and checking the clauses at that instantiation. Three consequences:

- A parameterized procedure with no caller in its program is never instantiated, and so is not certified. If a parameterized export is used only from outside (no internal call), the program has a free type parameter, has no linked program, and is not compiled.
- A parameter-inspecting module — one that matches a concrete functor at a parameter position — loaded on its own is rejected: with the parameter free there is nothing to check it against. It is checked only when a program instantiates it.
- A cross-module `#` call instantiates the callee's parameters as a local call does; the whole-program check verifies the callee's clauses at every instantiation any call induces.

Formal account: Moded-Types paper, §Parameterised Procedure Declarations and §Modular Checking via Abstract Parameters.

---

## 18. Tight Typing Discipline

### 18.1 The Discipline

As a project discipline, we do not use `_` or `_?` in type definitions or procedure declarations.  All type definitions use concrete types or type parameters; all procedure declarations use concrete or parameterized types.

The GLP language continues to support `_` and `_?` as primitive types.  The discipline is a coding standard, not a language restriction.

### 18.2 Parameterized Types Replace Imprecise Types

Every local imprecise type definition should be replaced by a parameterized instantiation from root self.glp:

| Before (imprecise) | After (precise) |
|---|---|
| `MsgStream ::= [] ; [_ \| MsgStream].` | Remove; use `Stream(Msg)` |
| `Channel ::= ch(Stream, Stream?).` | Remove; use `Channel(In, Out)` |
| `DiffList ::= Stream \ Stream?.` | Remove; use `DiffList(X)` |
| `NonEmptyList ::= [_ \| Stream].` | Remove; use `OpenStream(X)` |
| `procedure merge(Stream?, Stream?, Stream).` | `procedure merge(Stream(X)?, Stream(X)?, Stream(X)).` |

### 18.3 Exceptions

Two categories of code may retain `_` and `_?`:

1. **System builtins** in `self.glp` that genuinely accept any term: `ground(_?)`, `=(_?, _)`, `=?=(_?, _?)`, `=..(_, Stream(_)?)`, `compound(_?)`, etc.  These are implemented by the runtime and have no meaningful type restriction.

2. **Meta-interpreters** that manipulate arbitrary terms (goals, clause representations).  Types like `DumpList ::= [] ; [_ | DumpList].` or `Chain ::= chain(Stream(_)?, Stream(_)).` are acceptable with a comment explaining why.  Meta-interpreter typing is deferred to future work.

### 18.4 `-mode(system)` Does Not Exempt From Type Checking

The `-mode(system)` directive permits calling kernel predicates and using reserved constants.  It does **not** exempt the module from type checking.  All modules, including system-mode modules, are fully type-checked.  Type definitions and procedure declarations in system-mode files must follow the tight typing discipline.

---

## 19. Modules

### 19.1 Overview

GLP programs are organized into modules. Each `.glp` file is a module. A module controls which procedures are visible to other modules through `exported procedure` and `imported procedure` declarations. Cross-module calls use the `M # goal(...)` syntax, following FCP conventions.

### 19.2 Module Name

A module's name is its filename without the `.glp` extension. To rename a module, rename the file.

### 19.3 Procedure Visibility

There are three kinds of procedure declarations:

**Local procedure** — visible only within this module:
```prolog
procedure helper(Integer?, Integer).
helper(X, Y?) :- Y := X? + 1.
```

**Exported procedure** — callable from other modules:
```prolog
exported procedure double(Integer?, Integer).
double(X, Y?) :- Y := X? * 2.
```

**Imported procedure** — declares a dependency on another module's export:
```prolog
imported procedure math_service#double(Integer?, Integer).
```

The `imported procedure` declaration enables type checking to verify the cross-module call locally, without parsing the target module. The types in the import must be compatible with the export's types.

### 19.4 Cross-Module Calls

To call an exported procedure from another module, use the `#` operator:

```prolog
test_double(X, Y?) :- math_service # double(X?, Y).
```

Under static linking (§19.7) the linker resolves `math_service # double(X?, Y)` at compile time to a local call to `math_service`'s `double`, entering the target directory through its `self.glp` (Moded-Types paper, §Static Linking). The qualifier is a single child directory or module file; multi-segment paths are future work. Routing the call through a GLP channel at runtime instead — dynamic linking — is retired (§19.7).

### 19.5 Complete Example

**Service module** (`math_service.glp`):
```prolog
exported procedure double(Integer?, Integer).
double(X, Y?) :- Y := X? * 2.

exported procedure triple(Integer?, Integer).
triple(X, Y?) :- Y := X? * 3.
```

**Client module** (`client.glp`):
```prolog
imported procedure math_service#double(Integer?, Integer).
imported procedure math_service#triple(Integer?, Integer).

exported procedure compute(Integer?, Integer).
compute(X, Y?) :- math_service # double(X?, Y).
```

### 19.6 The `self.glp` Scope Chain

Each directory may contain a `self.glp` file that defines types and procedures visible to all modules in that directory and its subdirectories. This follows FCP's `self.cp` convention.

```
program/
  self.glp               — shared types (AgentMsg, Response, etc.)
  agent.glp              — sees program/self.glp
  mediator.glp           — sees program/self.glp
  ui/
    self.glp             — UI-specific types
    actors.glp           — sees ui/self.glp + program/self.glp
```

A module sees every type and procedure of every ancestor scope. When a name is defined at more than one level, the nearer definition shadows the farther one, and a module's own definition shadows every ancestor's, for types and procedures alike.

A directory's `self.glp` is also the directory's interface: it declares which of the directory's procedures are exported, and a cross-module call into the directory resolves only to a procedure its `self.glp` exports — defined there directly, or forwarded to the module that defines it (`p :- m#p`). A program's entry points are the root `self.glp`'s exports (§19.7; Moded-Types paper, §Design, §External access).

The root `programs/self.glp` defines all predefined types (`Stream(X)`, `Channel(In, Out)`, `DiffList(X)`, `OpenStream(X)`) and all predefined procedure declarations (`merge`, `send`, `receive`, `new_channel`, etc.). Every module sees root self.glp automatically.

### 19.7 Static Linking

A Typed GLP module is **self-contained** if it has no cross-module `M#p` call and no uninstantiated type parameter. A Typed GLP program is either a self-contained module or a directory with a `self.glp`. Every program is compiled by static linking, one module or many: linked within its filesystem context, type-checked, compiled if it passes type checking, and run. The linker resolves every `M # goal(...)` call at compile time to a local call, flattening the modules into one program. Load a program by its directory:
```
GLP> social_graph/
✓ Loaded program: social_graph/
```

A program's entry points are the root `self.glp`'s exported procedures — each defined there or forwarded to the module that defines it (§19.4). A single-module program exports all its procedures, so every one is an entry point. The unit of compilation and execution is the whole program, and only a well-typed program carries the soundness guarantee (Moded-Types paper, §Static Linking, §External access).

Static linking is fully type-checked — the goal, every clause, every cross-module instantiation. **Discipline:** trace a runtime bug by first recompiling the whole program and re-checking it as one; most mode bugs are whole-program type errors invisible to a single-module check.

**Dynamic linking is retired.** A runtime dynamic-dispatch mechanism — routing cross-module calls through GLP channels at runtime — is present in the implementation but retired: it cannot be typed without runtime type-checking, which GLP avoids. Static linking is the model in force. Dynamic activation will return through attestation among validated modules, needing no runtime type-checking (Moded-Types paper, §Type-Compatible Attestation Between Agents), and the present mechanism will be removed once that lands.

### 19.8 Module Design Guidelines

1. **Declare all dependencies.** Every cross-module call must have a corresponding `imported procedure` declaration. This makes the module self-contained for type checking.

2. **Export the minimal interface.** Only mark procedures as `exported` if they are intended to be called from other modules. Internal helper procedures should use plain `procedure`.

3. **Types flow through declarations.** A procedure declaration implicitly carries the types it references. You do not need to export types separately — they are carried by the `exported procedure` declaration.

4. **Matching modes matter.** The `imported procedure` declaration's argument modes must match the `exported procedure` declaration's modes. If the export says `double(Integer?, Integer)`, the import must say the same.

### 19.9 The `-expose` Directive

A `self.glp` may **expose** another module, lifting that module's `exported` procedures (and the types their signatures carry) into the directory's scope — visible unqualified to the whole subtree, as if defined in the `self.glp`:

```glp
%% programs/social/self.glp
-expose(lib#streams).      %% tag_stream/3, broadcast/3, … now visible below

%% programs/social/graph/agent.glp — calls them unqualified, no import needed:
agent(Id, In, Outs) :- broadcast(Msg?, Outs?, Outs1), ...
```

Exposure sits at the exposing directory's level in the ancestor chain, so innermost-first shadowing applies: a module's own definition (or a nearer `self.glp`) beats an exposed one, and an exposed name beats outer scopes. Exposing two modules that contribute the same name/arity at one level is a compile-time error. Unlike `#`/`imported procedure` (which qualify each call site), `-expose` makes the names ambient for the subtree — use it for shared utility modules, not for ordinary cross-module calls.

---

## 20. Type Union

### 20.1 The Rule

An alternative in a type definition may be a type name. This provides type union: all alternatives of the named type are inherited. The top-level functors across all alternatives (including inherited ones) must be distinct.

```prolog
AgentContent ::= connected(Constant) ; rejected.
FriendContent ::= friend_connected(Constant).

OutputContent ::= AgentContent ; FriendContent.
```

`OutputContent` inherits `connected/1` and `rejected/0` from `AgentContent`, and `friend_connected/1` from `FriendContent`. All functors are distinct, so this is valid.

### 20.2 Invalid: Overlapping Functors

```prolog
A ::= msg(String).
B ::= msg(Integer).
C ::= A ; B.          %% INVALID: msg/1 appears in both A and B
```

### 20.3 Structural Type Identity

Type identity is structural. Two independently defined types with the same alternatives are compatible, regardless of name or defining module.

---

## 21. Initial Goals Are Type-Checked

Every goal posted at the REPL (or at boot) is type-checked before it runs. A goal is checked as a *body goal* — condition 2 of the well-typed-clause definition (each unit goal, as a produced moded term, well-typed by the program's types) plus the body-body variable-pair rule — and an ill-typed goal is rejected with a specific error and never runs. It is checked against one program's types; it may be conjunctive, but every unit goal is checked against that one program.

Consequences:

- A query variable is a body variable and obeys §2A. At an output (produce) position it is a writer the run fills and you then read — the normal way to obtain a result. At an input (consume) position a bare query variable is a writer where a reader is wanted, and is rejected.
- `X = foo` does not type-check: `=` is declared `=(_?, _)`, so the left argument is consumed and `X` there is a writer at a consume position. Write `foo = X` — the value is consumed, `X` is produced — which binds `X`.
- A goal calling an undeclared procedure is rejected as undeclared, not run and failed.

Formal definition: Moded-Types paper, §Type-Compatible Attestation Between Agents, and the well-typed-goal definition in the semantics section.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.20 | 2026-06-24 | §19.7: defined self-contained module (no `M#p`, no uninstantiated type parameter) and program (self-contained module or directory with `self.glp`); uniform pipeline (link, typecheck, compile if it passes, run) for one module or many; removed the multi-module-only framing and the development-aid note. |
| 2.19 | 2026-06-24 | §19.6: stated the general scope-chain shadowing rule (nearer definition shadows farther, module's own shadows all ancestors', for types and procedures), previously only in §8.2 and §19.9. |
| 2.18 | 2026-06-23 | §19 aligned to the paper: cross-module calls resolve at link time to local calls (§19.4); `self.glp` is the directory interface and entry points are the root `self.glp`'s exports (§19.6); §19.7 retitled Static Linking, single-module exports all; dynamic linking retired (present but unsupported, to return via attestation); removed the dynamic-dispatch REPL workflow, renumbering §19.9/§19.10 to §19.8/§19.9. |
| 2.17 | 2026-06-23 | Removed the `-module` directive: a module's name is its filename (§19.2, §19.5). |
| 2.16 | 2026-06-22 | Type/module updates: §17.7 (per-instantiation checking; uninstantiated parametric exports not certified); §19.7 (program is the unit of compilation and execution; dynamic dispatch not yet soundly typed; recompile-whole-program-first discipline); §21 (initial goals type-checked as body goals; `foo = X` not `X = foo`); §16.4 (polarity must agree across boundaries the checker cannot span) |
| 2.15 | 2026-06-10 | Added §19.10: the `-expose` directive (lift another module's exports into a `self.glp`'s subtree scope; innermost-first shadowing; collision is an error) |
| 2.14 | 2026-06-10 | §8.2: multi-clause root `self.glp` procedures (e.g. `merge/3`) resolve through the ancestor scope chain, subject to innermost-first shadowing; unfolding is a single-unit-clause optimisation, not the resolution mechanism (A3 module-system amendment) |
| 2.13 | 2026-05-24 | Rewrote Section 8 (Guards: What May Appear in a Guard): added §8.1 explicit guard rule (compile-time unfoldability; no recursion; multi-clause off-limits until PE extended), renumbered single-unit-clause material to §8.2/§8.3 as the safe special case rather than a separate construct |
| 2.12 | 2026-05-23 | Added Section 15B: Forwarding a Writer Through a Structure (dual of §15, role of `?` in type definitions at output positions); added cross-reference at end of §2A.4 summary |
| 2.11 | 2026-04-12 | Added Section 3.4: Guard Occurrences and SRSW Counting (guard reader occurrences don't count toward single-reader limit) |
| 2.10 | 2026-03-14 | Added Section 20: Type Union (type names as alternatives, disjoint functor requirement, structural identity) |
| 2.9 | 2026-03-12 | Added Section 19: Modules (declarations, exports/imports, `#` syntax, self.glp chain, REPL workflow) |
| 2.8 | 2026-03-11 | Added Section 18: Tight Typing Discipline; corrected send/receive Channel arity to 2-arg form |
| 2.7 | 2026-03-11 | Section 14 marked obsolete — renamed procedure copies removed, replaced by parameterized types |
| 2.6 | 2026-03-06 | Added Section 17: Parameterized Types; updated Section 14.6 |
| 2.5 | 2026-03-06 | Added Section 15: Receiving and Forwarding Non-Ground Variables; Added Section 16: `?` in Type Definitions vs `?` on Clause Variables |
| 2.4 | 2026-02-21 | Added Section 14: Precise Typing via Renamed Procedures (workaround for absent parametric types) |
| 2.3 | 2026-02-02 | Added Section 12: Reserved Constants (underscore-prefixed constants reserved for system use, `-mode(system).` directive) |
| 2.2 | 2026-02-01 | Added Section 10: Channel Creation vs Channel Reception |
| 2.1 | 2026-01-28 | Added Section 9: Anonymous Variables (any variable starting with `_` is anonymous) |
| 2.0 | 2026-01-27 | Renamed to Typed GLP Manual; added Section 1 (Procedure Declarations Must Match Data Flow); added Section 3 (SRSW Relaxation for Constant Types); consolidated and reorganized |
| 1.3 | 2026-01-27 | Added Section 6: Unit Clause Procedures |
| 1.2 | 2026-01-24 | Added Section 5: Store Writers, Not Readers, in Lookup Tables |
| 1.1 | 2026-01-24 | Added Section 4: Avoid Assignment in Guards and Bodies |
| 1.0 | 2026-01-24 | Initial version with Head-Body Variable Flow Principle |
