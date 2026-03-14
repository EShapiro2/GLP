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

## 3. SRSW (Single Reader Single Writer)

Each reader `X?` can appear at most ONCE in a clause (one read).
Each writer `X` can appear at most ONCE in a clause (one write).

Exception: if `ground(X?)` is in the guard, `X?` can appear multiple times.

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

## 11. Before Writing Code

1. Read `docs/typed-glp-manual.md` completely
2. Study the existing `bond_agent.glp` — every pattern you need is already there
3. For each new procedure, find the closest existing analogue and follow its pattern exactly
4. Type-check after every change: `dart run bin/glp_repl.dart` with `load bond_agent.glp`

## 12. Modules

### Module Declaration

Every `.glp` file is a module. Declare the module name:
```prolog
-module(math_service).
```

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

The call is routed through a GLP channel to the target module's service loop at runtime.

### Imports Must Match Exports

The `imported procedure` declaration's types and modes must match the target module's `exported procedure`. This enables fully local type checking — no need to parse the other module.

### The self.glp Scope Chain

Each directory may have a `self.glp` defining types visible to all modules in that subtree. The root `programs/self.glp` defines all predefined types and procedures — visible everywhere.

### REPL Workflow

```
GLP> math_service.glp          %% Load and auto-activate (has exports)
GLP> dispatch_client.glp       %% Load client (has imported procedures)
GLP> test_double(5, X).        %% Run goal → X = 10
```

Modules with `exported procedure` declarations are auto-activated on load. Use `:activate <n>` to activate manually if needed.

### Static Linking (Projects)

For multi-module projects in a directory tree, load the entire directory:
```
GLP> social_graph/
✓ Loaded project: social_graph/
```

The project linker resolves all `M # goal(...)` calls at compile time.

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
