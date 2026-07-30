# GLP Programming Cheat Sheet for Claude Code

**READ THIS BEFORE WRITING ANY GLP CODE.**

GLP is NOT Prolog. If you find yourself writing Prolog-style code, STOP.

## 1. The Single Most Important Rule

**Writer-mode outputs are constructed in CLAUSE HEADS, never via `=` in the body.**

WRONG (Prolog):
```prolog
foo(Input, Output) :- Output = computed(Input?).
```

RIGHT (GLP):
```prolog
foo(Input, computed(Input?)).
```

Or with a guard:
```prolog
foo(Input, computed(Input?)) :- ground(Input?) | true.
```

## 2. Writer vs Reader

Every variable has two halves: writer `X` and reader `X?`.
- Writer = output, can be bound once
- Reader = input, suspends until writer binds it

In procedure declarations:
- `BondList?` = reader (input to this procedure)
- `BondList` = writer (output from this procedure) 
- The `?` marks the reader side

## 2.5 Variable Modes at Nested Positions in the Head

When a clause head contains structures with embedded `?` in the type definition (e.g., reply variables), use this rule:

**Step 1 — Determine structural mode:** Start with the argument's declared mode (`Type?` → ↓, `Type` → ↑). Each `?` in the type definition path **flips** the mode.

**Step 2 — Choose variable form:** ↓ → writer (`X`), ↑ → reader (`X?`).

**Step 3 — Body:** SRSW pair of the head occurrence.

Example with reply variables:

```prolog
SignalReply ::= punch(Constant) ; initiated.
SignalMsg ::= reconnect(Constant, Constant, Constant, SignalReply?).

procedure signal_server(Stream(SignalMsg)?, PendingList?).
signal_server([reconnect(A, B, Proof, ReplyA?)|In], Pending) :- ...
%%                       ↓  ↓  ↓     ↑        ↓
%%                       W  W  W     R        W
```

Arg 1 is `Stream(SignalMsg)?` → ↓. Inside `reconnect` at ↓, the first three `Constant` positions stay ↓ → writers. But `SignalReply?` has a `?` that flips ↓→↑ → `ReplyA?` is a **reader** (hole for the reply).

Storing the reply variable in an output list:

```prolog
PendingEntry ::= needs(Constant, Constant, Constant, SignalReply?).

procedure find_ready(Constant?, Constant?, Constant?, SignalReply,
                     PendingList?, PendingList).
find_ready(A, B, AddrA, ReplyA?, [], [needs(A?, B?, AddrA?, ReplyA)]).
%%                                          ↑   ↑    ↑       ↓
%%                                          R   R    R       W
```

Arg 6 is `PendingList` → ↑. Inside `needs` at ↑, `Constant` positions stay ↑ → readers. But `SignalReply?` flips ↑→↓ → `ReplyA` is a **writer** (stores the reply variable).

The same `?` in the type definition flips differently depending on which direction you start from. See `typed-glp-manual.md` Section 2A for the complete worked example.

## 3. SRSW (Single Reader Single Writer)

Each writer `X` can appear at most ONCE in a clause (one write).
Each reader `X?` can appear at most once in the head+body (one read).

**Guard occurrences don't count:** A reader `X?` may appear in a guard AND additionally once in the head+body.  (Its paired writer `X` must still occur in the head.)  For example:

```prolog
foo(X, Y?) :- known(X?) | bar(X?, Y).
```

`X?` appears twice — once in the guard (`known(X?)`) and once in the body (`bar(X?, Y)`) — but the guard occurrence doesn't count toward the single-reader limit, so this is valid.

**Groundness relaxation:** If `ground(X?)` or another groundness-implying guard (`integer`, `number`, `string`, `constant`, arithmetic comparisons, `=?=`) is present, `X?` can appear multiple times anywhere in the clause.

## 3b. Receiving a Variable and Passing It On

**This is the most fundamental GLP pattern.** When a process receives a variable
(e.g., from a message) and needs to pass it to another process:

```prolog
p(...X...) :- q(...X?...).
```

- `X` (writer, no `?`) in the HEAD — receives/captures the variable
- `X?` (reader) in the BODY — passes it on to q
- Total: 1 writer + 1 reader = SRSW satisfied

**WRONG** — two readers, zero writers:
```prolog
p(...X?...) :- q(...X?...).   %% SRSW VIOLATION: 2 readers, 0 writers
```

This is wrong because `X?` in the head is already a read, and `X?` in the body
is a second read, with no writer anywhere.

**Real example** — agent receives non-ground BenResult from friend channel and
passes it to an inject process:

```prolog
%% WRONG:
agent(Id, UserIn, [msg(From, Id1, escrow_offer(Time, BenResult?))|NetIn], ...) :-
    ... | inject(BenResult?, ...).   %% 2 readers, 0 writers — SRSW violation!

%% RIGHT:
agent(Id, UserIn, [msg(From, Id1, escrow_offer(Time, BenResult))|NetIn], ...) :-
    ... | inject(BenResult?, ...).   %% 1 writer + 1 reader — OK
```

This pattern appears throughout the codebase. Study `intro(From, Resp?)` in the
cold-call handler: head has `Resp?` (reader from network), body forwards `Resp`
(writer) in the outgoing message to the mediator.

## 3c. `?` in Type Definitions vs `?` on Clause Variables

These are DIFFERENT things. Do not confuse them.

- `?` in a **type definition** like `escrow_offer(Constant, EscrowBenResult?)` describes
  the mode of data as it flows through the data structure. It means "this position
  carries a reader reference in the structure."

- `?` on a **clause variable** like `BenResult?` marks the reader half of the variable
  in that particular clause.

The `?` in the type does NOT force the clause variable to be a reader. When you
write a clause head that matches `escrow_offer(Time, BenResult)`, `BenResult`
(writer, no `?`) is perfectly valid — it receives/captures the value from the
structure. The type's `?` is about the data, not about your clause variable.

## 3d. Forwarding a Writer Through a Structure

Dual of §3b.  To forward a WRITER through an output structure (so the
downstream can bind it), the type definition must carry `?` at the position:

```prolog
OutputEntry ::= output(OutputKey, Stream?).   %% ? enables writer-forwarding

procedure add_output(OutputKey?, Stream, OutputsList?, OutputsList).
add_output(Key, Out?, Outs, [output(Key?, Out) | Outs?]).
%%              ^^^^                       ^^^
%%              reader in input head       writer forwarded in output head
```

- `Out?` in arg 2 head = reader (captures from caller).
- `Out` in arg 4 head, inside the `?`-typed `Stream?` field = writer (forwarded).
- 1 reader + 1 writer = SRSW pair across head args, OK.

The downstream receives the OutputsList and WRITES to the stream by binding the
variable in its own head pattern, exactly as `lookup_send_step` does:

```prolog
lookup_send_step(Key, Msg, [output(K, [Msg?|Out1?])|Rest], ...) :- ...
```

**Without `?` in the type, the output position carries a reader, not a writer.**
If the downstream needs to write, add `?` to the type.  Do NOT redesign around it.

| Forwarding direction | Type carries `?` | Input head form | Output head form |
|---|---|---|---|
| Reader (§3b) | no | writer `X` | reader `X?` |
| Writer (§3d, this section) | yes | reader `X?` | writer `X` |

See typed-glp-manual §15 (reader-forwarding) and §15B (writer-forwarding).

## 3e. Threading a Stream Across Multiple Producers

When a procedure writes N messages to a stream and leaves a continuation
for the caller to use, the two stream args must have **asymmetric modes**:

```prolog
%% arg 5 (NetOut) at ↑ : caller-provides-writer; callee fills with messages.
%% arg 6 (Cont)   at ↓ : caller-provides-reader of the next-writer it will use.
exported procedure write_msgs(..., Stream(X), Stream(X)?).

write_msgs(..., [m1?, m2? | NewTail?], Cont) :-
    ... | write_msgs(..., NewTail, Cont?).

write_msgs(..., [], Cont?, Cont).  %% empty: alias arg 5 to arg 6.
```

The ↑/↓ asymmetry is what satisfies SRSW for the empty/alias clause:
writer (`Cont` at ↓) pairs with reader (`Cont?` at ↑).  Symmetric ↑/↑ fails
the type checker with "reader requires ↓ (consume), got ↑ (produce)".

Caller threads multiple stream-extending calls together:

```prolog
write_msgs(...,  NetOut,  NetOut1?),
write_more(..., NetOut1, NetOut2?),
agent(...,       NetOut2, ...).
```

Each call's arg 5 is the previous call's arg 6's paired writer.  When the
chain ends with `agent(..., NetOut2, ...)`, agent's clause head writes more.

## 4. The Bind Pattern

When a procedure needs to construct a typed value at a writer-mode position:

```prolog
%% TradeResponse (arg 1) is writer mode
procedure bind_trade_accept(TradeResponse, BondList?).
bind_trade_accept(trade_accept(Bonds?), Bonds).
```

The head CONSTRUCTS `trade_accept(Bonds?)` at the writer position.
`Bonds` (writer, from arg 2 position) and `Bonds?` (reader, inside the constructed term) form the standard pair.

More examples from the codebase:
```prolog
procedure bind_credit_accept(CreditResponse, Constant?, Constant?, Constant?, Constant?).
bind_credit_accept(credit_accept(MyBonds?), Id, Maturity, K, Serial) :-
    ground(Id?), ground(Maturity?), ground(K?), ground(Serial?) |
    create_bonds(Id?, Maturity?, K?, Serial?, MyBonds).

procedure bind_trade_decline(TradeResponse, BondList?).
bind_trade_decline(trade_decline(Bonds?), Bonds).

procedure bind_redeem(RedeemResponse, BondList?).
bind_redeem(redeem_ok(Bonds?), Bonds).

procedure bind_loan_reject(LoanResponse).
bind_loan_reject(loan_reject).
```

## 5. The Inject Pattern

An inject procedure monitors an unbound reader, passing through stream elements until the reader becomes known, then injects a message.

```prolog
procedure inject_credit_result(CreditResponse?, Constant?, Constant?, UserInStream?, UserInStream).

%% Case 1: response is credit_accept(Bonds) — inject credit_result
inject_credit_result(credit_accept(Bonds), From, K, Ys,
    [credit_result(From?, K?, Bonds?)|Ys?]) :-
    ground(From?), ground(K?), ground(Bonds?) | true.

%% Case 2: response is credit_reject — inject credit_was_rejected  
inject_credit_result(credit_reject, From, _, Ys,
    [credit_was_rejected(From?)|Ys?]) :-
    ground(From?) | true.

%% Pass-through: response not yet known, pass stream element through
inject_credit_result(Resp, From, K, [Y|Ys], [Y?|Ys1?]) :-
    inject_credit_result(Resp?, From?, K?, Ys?, Ys1).
```

Key points:
- First arg is the monitored reader (typed union, e.g., `CreditResponse?`)
- Each union constructor gets its own clause with HEAD PATTERN MATCH
- Last clause is the pass-through (copies stream elements while waiting)
- The output stream (last arg) prepends the injected message via list construction in the HEAD

## 6. The Handle Pattern (writer-mode dispatch)

When a procedure receives a writer-mode parameter and must construct different values based on a status:

```prolog
procedure handle_trade_fill(Constant?, Constant?, Constant?, TradeResponse, BondList?, BondList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).

%% OK: construct trade_accept(Selected?) at the TradeResponse writer position
handle_trade_fill(ok, Id, From,
    trade_accept(Selected?),    %% ← writer output constructed in HEAD
    OfferedBonds, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    append(Remaining?, OfferedBonds?, NewHoldings),
    agent(Id?, UserIn?, NetIn?, Outs?, NewHoldings?, NextSerial?).

%% FAIL: construct trade_decline(ReturnBonds?) at the TradeResponse writer position  
handle_trade_fill(fail, Id, From,
    trade_decline(ReturnBonds?),    %% ← writer output constructed in HEAD
    OfferedBonds, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    ReturnBonds = OfferedBonds?,    %% ← THIS use of = IS correct: 
                                    %%   ReturnBonds is a fresh writer inside
                                    %%   the head-constructed term, OfferedBonds?
                                    %%   is a reader from another parameter
    append(Selected?, Remaining?, OrigHoldings),
    lookup_send('_user', msg(agent, '_user', trade_failed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

Also study `handle_redeem_fill` — same pattern with `redeem_ok(Selected?)` and `redeem_ok(AllBonds?)` in the head.

## 7. The Do Pattern (select + dispatch)

```prolog
do_trade(Id, Target, GiveSpec, WantSpec, Holdings, UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(GiveSpec?, Holdings?, Status, Selected, Remaining),
    do_trade_result(Status?, Id?, Target?, WantSpec?, Selected?, Remaining?, UserIn?, NetIn?, Outs?, NextSerial?).
```

`Status` is writer from select, `Status?` is reader passed to dispatch.
`Selected` is writer from select, `Selected?` is reader passed to dispatch.

## 8. Guards

Guards appear between `:-` and `|`. They are three-valued: succeed, suspend, or fail.

```prolog
foo(X, Y?) :- ground(X?) | Y = X?.
```

Common guards: `ground(X?)`, `known(X?)`, `X? =?= Y?`, `X? > Y?`, `wait_until(T?)`.

`otherwise` succeeds when all previous clauses FAILED (not suspended).

**Only compile-time-unfoldable calls may appear in a guard.**  Built-ins above (and arithmetic comparisons, type tests, `otherwise`) are the always-safe set.  User-defined procedures may also appear iff the partial evaluator can fully unfold them — in practice this means **single-unit-clause** procedures (one clause, no body).  **Recursive procedures cannot be unfolded and are rejected in guards** (`[WARN] Unknown guard predicate` at load time).  List-search preconditions like "Inbox contains `msg(P, friend_request(_))`" are inherently recursive — put them in the clause body via dispatch (helper with `otherwise` fallthrough), not in the guard.  Full statement of the rule: typed-glp-manual §8.1.

## 8b. Channels: receive, close, and the two-clause closure idiom

`Channel(In, Out) ::= ch(In, Out?)` — read stream `In`, write stream `Out?`.  Two channel-consumption guards read a channel; the PE unfolds them into the head **before** type checking, so coverage is checked on the unfolded head:

- `receive(M, Ch?, Ch1)` — next message `M` off a **non-empty** read stream (`receive` is typed over `OpenStream(X) ::= [X | Stream(X)]`); yields continuation `Ch1`.  Unfolds the head to `ch([M|In], Out?)`.
- `close(Ch?)` — the **closed** read stream (`close` is typed over `Closed ::= []`); closes the write stream.  Unfolds the head to `ch([], [])`.

A read stream is a full `Stream(X) = OpenStream(X) ∪ Closed`, so a channel consumer needs **two clauses** to be well-typed:

```prolog
consume(Ch) :- receive(M, Ch?, Ch1) | handle(M?), consume(Ch1?).   %% non-empty -> ch([M|In], Out?)
consume(Ch) :- close(Ch?) | done.                                  %% closed    -> ch([], [])
```

🔴 A consumer with **only** the `receive` clause is rejected — `ch([], _)` (the closed channel) is an uncovered input path.  Same rule when destructuring in the head: pair one `ch([Msg|In], ...)` clause with one `ch([], ...)` clause; when the message clause matches a *specific* message, add an `otherwise`-guarded skip clause `consume(ch([_|In], Out?)) :- otherwise | consume(ch(In?, Out))` for the rest.  Full statement: typed-glp-manual §4.4, §8.4.

## 9. Spawning Concurrent Processes

Body goals run concurrently. To spawn a process, just call it in the body:

```prolog
agent(...) :-
    ... |
    escrow(T?, Bonds?, Cancel?, BenResult, DepResult),  %% spawns escrow
    inject_result(DepResult?, ...),                       %% spawns inject
    agent(...).                                           %% tail-recurse
```

Three concurrent processes: escrow, inject, and the next agent iteration.

## 10. What NOT To Do

- **Never use `=` to bind writer-mode output parameters** (construct in head)
- **Never use `assert`/`retract`** (GLP has no mutable database)  
- **Never use `cut` (`!`)** (GLP uses committed-choice, not cut)
- **Never use `if-then-else` (`->`)** (use multi-clause with guards)
- **Never use `findall`/`bagof`** (GLP has no meta-predicates)
- **Never treat `_` type as "anything goes"** — use proper typed unions
- **Never write `X = value` for output binding** — decompose in the head
- **Never use `otherwise` expecting it to fire after a suspending clause** — `otherwise` fires only after FAILURE, not suspension
- **Never use `true | true` for guardless clauses** — `true` is not a guard. Write a unit clause instead: `foo(X, bar(X?)).` (head + period, no `:-`)
- **Never operate on a non-constant-type list more than once in a clause** — SRSW means each list element can only be read once. To lookup and send on the same list, fuse the operations into a single traversal procedure

## 11. Before Writing Code

1. Read `docs/typed-glp-manual.md` completely
2. Study the existing `bond_agent.glp` — every pattern you need is already there
3. For each new procedure, find the closest existing analogue and follow its pattern exactly
4. Type-check after every change: `dart run bin/glp_repl.dart` with `load bond_agent.glp`

## 12. Modules

### Module Name

Every `.glp` file is a module; its name is the filename without `.glp`. To rename a module, rename the file.

### Procedure Visibility

```prolog
procedure helper(Integer?, Integer).              %% Local — only this module
exported procedure double(Integer?, Integer).      %% Public — callable via M # goal(...)
imported procedure math_service#double(Integer?, Integer).  %% Dependency — enables type checking
```

### Cross-Module Calls

Use the `#` operator to call an exported procedure in another module:
```prolog
test(X, Y?) :- math_service # double(X?, Y).
```

Under static linking the call resolves at compile time to a local call, entering the target directory through its `self.glp`. The qualifier is a single child directory or module file; multi-segment paths are future work.

### Imports Must Match Exports

The `imported procedure` declaration's types and modes must match the target module's `exported procedure`. This enables fully local type checking — no need to parse the other module.

### The self.glp Scope Chain

Each directory may have a `self.glp` defining types visible to all modules in that subtree, and it is the directory's interface — a call into the directory resolves only to what its `self.glp` exports. The root `programs/self.glp` defines all predefined types and procedures — visible everywhere.

### Loading a Program

For a multi-module program, load its directory:
```
GLP> social_graph/
✓ Loaded program: social_graph/
```

The program linker resolves all `M # goal(...)` calls at compile time.

**Program entry points.** A program's entry points are the root `self.glp`'s exported procedures. To make plays callable by name, the root `self.glp` exports each — either define it there, or forward it: `exported procedure play1.` with `play1 :- boot#play1.` (See manual §19, paper §Static Linking.) REPL goals are themselves type-checked before they run (manual §21); dynamic linking is retired (manual §19.7).

### Module Checklist

1. Every cross-module call needs a matching `imported procedure` declaration
2. Only `exported procedure` procedures are callable from outside
3. Types flow through declarations — no separate type export needed
4. Import/export modes must match exactly

## 13. Type Union

An alternative in a type definition may be a type name. This inherits all its alternatives (type union). All top-level functors must be distinct.

```prolog
AgentContent ::= connected(Constant) ; rejected.
FriendContent ::= friend_connected(Constant).

OutputContent ::= AgentContent ; FriendContent.
%% Inherits connected/1, rejected/0, friend_connected/1 — all distinct, valid
```

WRONG:
```prolog
A ::= msg(String).
B ::= msg(Integer).
C ::= A ; B.          %% INVALID: msg/1 in both A and B
```

Type identity is structural — two types with the same alternatives are compatible regardless of name or module.

## 14. Type Aliases of Primitive Types

You can alias primitives for self-documenting positions:

```prolog
Agent ::= Constant.
Epoch ::= Integer.

GsgCargo ::= friend_request(Epoch)
           ; stream_update(Agent, Epoch).
```

`stream_update(Agent, Epoch)` reads much better than `stream_update(Constant, Integer)`.  Structural identity (§13) makes the alias interoperable with the underlying primitive.

**Caveat — SRSW relaxation does NOT transfer through the alias.**  The §3.3-style relaxation that permits multiple readers of `Constant`/`Integer`/etc. does not auto-apply to `Agent` or `Epoch`.  If you forward an aliased variable to two places, the type checker reports:

> Reader variable X? occurs 2 times without ground guard or constant type

Workaround: explicit `ground/1` guard.

```prolog
broadcast(Agent, ...) :-
    ground(Agent?) | use(Agent?), use_again(Agent?).
```

## 15. Troubleshooting Common Type-Checker Errors

| Error | Likely cause | Fix |
|---|---|---|
| `Variable mode mismatch: reader requires ↓, got ↑` | Symmetric ↑/↑ args trying to pair writer + reader | Declare one arg with `?` so it's at ↓; pair across asymmetric modes (see §3e) |
| `Reader variable X? occurs 2 times without ground guard or constant type` | Multi-reader without relaxation; type-alias caveat (§14) | Add `ground(X?)` guard |
| `Cannot resolve type expression: foo(...)` | Compound list element used inline | Extract as named typedef: `FooEntry ::= foo(...).` then `List ::= [] ; [FooEntry \| List].` |
| `SRSW violation: Reader variable X? occurs N times` | Variable used in multiple non-guard occurrences without relaxation | Add `ground(X?)` if appropriate, or restructure to single-walk |
| `[WARN] Unknown guard predicate: foo` (load time) | `foo` is recursive or multi-clause, so the PE cannot unfold it as a guard (§8) | Move the test to the clause body via a dispatch helper (multi-clause with `otherwise` fallthrough), or restate the precondition as a single-unit-clause check the PE *can* unfold |
| `Procedure declaration for "X" has no clauses` | `procedure` in a `self.glp` without clauses | Co-locate clauses with the declaration; `self.glp` may hold the directory's shared procedures and its exported entries (defined or forwarded), not types alone |
| `UnknownTypeError: Foo` when loading a sibling-directory file | Types from `cva/self.glp` not visible in `gsg/` | Redefine the needed types in `gsg/self.glp` (structural identity, §13), OR move shared types to a parent `programs/SPM/self.glp` |
