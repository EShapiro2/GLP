# Phase 8 Instructions: Fix select_bonds_min_maturity + Escrow

## Prerequisites

1. Read CLAUDE.md completely
2. Read docs/DISCIPLINE.md completely
3. Read docs/typed-glp-manual.md completely
4. **IMPORTANT**: This phase assumes `wait_until` has been fixed to SUSPEND (not fail) when time hasn't passed. If it still fails, these tests will not pass. Run baseline tests first — fplay1–fplay9, fplay4b.

## Part A: Fix select_bonds_min_maturity

### Problem

`select_bonds_min_maturity` currently uses `wait_until(MinMat?)`/`otherwise` to dispatch between mature and immature cases. With the `wait_until` fix, the first clause suspends instead of failing, so `otherwise` never fires. The goal hangs until time passes.

### Fix

Replace the time guard with arithmetic comparison. The caller passes the current time, and the procedure uses `MinMat? =< Now?` (a proper succeed/fail guard).

### Changes to bond_agent.glp

#### 1. Change `select_bonds_min_maturity` signature — add `Now` parameter

Old (6 args):
```prolog
procedure select_bonds_min_maturity(Constant?, Constant?, BondList?, BondList, BondList, Constant).

select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    wait_until(MinMat?) |
    select_any_bonds(K?, Hs?, Sel, Rem, Got).

select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    otherwise |
    select_bonds_min_maturity_strict(MinMat?, K?, Hs?, Sel, Rem, Got).
```

New (7 args — `Now` added as first parameter):
```prolog
procedure select_bonds_min_maturity(Constant?, Constant?, Constant?, BondList?, BondList, BondList, Constant).

%% When MinMaturity has passed (bonds are mature), any bond qualifies
select_bonds_min_maturity(Now, MinMat, K, Hs, Sel?, Rem?, Got?) :-
    MinMat? =< Now? |
    select_any_bonds(K?, Hs?, Sel, Rem, Got).

%% When MinMaturity has NOT passed, require M >= MinMaturity
select_bonds_min_maturity(Now, MinMat, K, Hs, Sel?, Rem?, Got?) :-
    otherwise |
    select_bonds_min_maturity_strict(MinMat?, K?, Hs?, Sel, Rem, Got).
```

`MinMat? =< Now?` succeeds when both are ground numbers and MinMat <= Now. Fails when MinMat > Now. Suspends if either is unbound. The `=<` guard implies groundness of both operands, so multiple reads are safe.

#### 2. Update the caller — redeem_request agent clause

Old:
```prolog
agent(Id, UserIn, [msg(From, Id1, redeem_request(K, MaxMaturity, ReturnedBonds, RedeemResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(K?), ground(MaxMaturity?), ground(ReturnedBonds?) |
    select_bonds_min_maturity(MaxMaturity?, K?, Holdings?, Selected, Remaining, Got),
    Shortfall := K? - Got?,
    handle_redeem_fill(Shortfall?, K?, MaxMaturity?, Id?, From?, ReturnedBonds?,
        RedeemResp, Selected?, Remaining?, NextSerial?, UserIn?, NetIn?, Outs?).
```

New:
```prolog
agent(Id, UserIn, [msg(From, Id1, redeem_request(K, MaxMaturity, ReturnedBonds, RedeemResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(K?), ground(MaxMaturity?), ground(ReturnedBonds?) |
    now(Now),
    select_bonds_min_maturity(Now?, MaxMaturity?, K?, Holdings?, Selected, Remaining, Got),
    Shortfall := K? - Got?,
    handle_redeem_fill(Shortfall?, K?, MaxMaturity?, Id?, From?, ReturnedBonds?,
        RedeemResp, Selected?, Remaining?, NextSerial?, UserIn?, NetIn?, Outs?).
```

`now(Now)` is a system predicate that binds Now to the current epoch milliseconds. It runs concurrently in the body. `select_bonds_min_maturity` suspends on `Now?` until it's bound (virtually instant), then proceeds.

Note: `now/1` is already used in the boot file (`bond_boot.glp`). The agent file is `-mode(system)`, so it has access to system predicates.

### No changes to select_bonds_min_maturity_strict or select_any_bonds

These don't use time guards.

### No changes to plays 1–4, 5–9

They don't call `select_bonds_min_maturity` directly — only the agent's redeem_request handler does. The caller change is transparent.

### Play 4b considerations

Play 4b demonstrates time-dependent redemption. Before T: `MinMat? =< Now?` fails (MinMat is future), `otherwise` fires, strict selection. After T: `MinMat? =< Now?` succeeds (MinMat is past), any bond qualifies. Same behavior as before, just using arithmetic instead of `wait_until`.

---

## Part B: Escrow Implementation

### Design

The escrow is a local concurrent process spawned by the depositor. It holds bonds and resolves based on a race between time passing and a cancel signal.

```prolog
procedure escrow(Constant?, BondList?, Constant?, Constant, Constant).

%% Time passes — beneficiary wins
escrow(T, Bonds, _, BenResult, DepResult) :-
    wait_until(T?) |
    BenResult = Bonds?, DepResult = expired.

%% Depositor cancels before time — depositor wins
escrow(T, Bonds, cancel, _, DepResult) :-
    DepResult = Bonds?.
```

Wait — `BenResult` needs to be set to `cancelled` in the cancel case. Let me fix:

```prolog
%% Time passes — beneficiary wins
escrow(T, Bonds, _, BenResult, DepResult) :-
    wait_until(T?) |
    BenResult = Bonds?, DepResult = expired.

%% Depositor cancels before time — depositor wins  
escrow(_, Bonds, cancel, BenResult, DepResult) :-
    DepResult = Bonds?, BenResult = cancelled.
```

**Race semantics**: Clause 1 suspends on `wait_until(T?)` (timer). Clause 2 suspends on head match of `cancel` (CancelSignal unbound). Whichever condition is satisfied first, that clause commits. The other is abandoned.

**Types for escrow results**:
```prolog
EscrowBenResult ::= BondList ; cancelled.
EscrowDepResult ::= BondList ; expired.
```

Actually, these are just constants or bond lists. Since we already have BondList, and `expired`/`cancelled` are atoms (constants), we can use a union:
```prolog
EscrowResult ::= BondList ; expired ; cancelled.
```

Or even simpler — don't declare a new type, just use `_` since the escrow procedure itself is in the agent file which is `-mode(system)`. The escrow process writes either a BondList or an atom to the shared variables.

### Escrow protocol

1. Alice sends `deposit_escrow(bob, GiveSpec, ReleaseTime)`.
2. Alice's agent selects bonds matching GiveSpec from holdings (same as trade).
   - Can't fill → notify `escrow_failed(bob)`, holdings unchanged. Done.
   - Ok → remove bonds from holdings, spawn escrow process, send `escrow_offer(BenResult?)` on friend channel to Bob. Start `inject_escrow_dep_result(DepResult?, ...)` to monitor. Store CancelSignal writer in pending.
3. Bob's agent receives `escrow_offer(BenResult?)`, starts `inject_escrow_ben_result(BenResult?, ...)`. Notifies Bob `escrow_received(alice, req(N))`.
4. Resolution (automatic — no user action needed):
   - Time passes → escrow binds BenResult=Bonds, DepResult=expired.
     - Bob's inject fires: `escrow_released(alice, Bonds)` injected. Agent adds bonds, notifies Bob.
     - Alice's inject fires: `escrow_expired(bob)` injected. Agent notifies Alice.
   - Alice cancels (`cancel_escrow(req(N))`) → CancelSignal bound to `cancel` → escrow binds DepResult=Bonds, BenResult=cancelled.
     - Alice's inject fires: `escrow_returned(bob, Bonds)` injected. Agent adds bonds back.
     - Bob's inject fires: `escrow_cancelled(alice)` injected. Agent notifies Bob.

### Notifications (5 total)

- `escrow_received(From, ReleaseTime)` — to beneficiary: someone deposited for you
- `escrow_released(From)` — to beneficiary: time passed, bonds added to holdings
- `escrow_cancelled(From)` — to beneficiary: depositor cancelled
- `escrow_expired(Target)` — to depositor: time passed, bonds went to beneficiary
- `escrow_returned(Target)` — to depositor: cancel succeeded, bonds returned

Plus one failure:
- `escrow_failed(Target)` — to depositor: couldn't fill spec from holdings

### New types (bond_agent.glp)

```prolog
%% Escrow cancel signal
EscrowCancel ::= cancel.
```

FriendContent addition:
```prolog
; escrow_offer(Constant, _)    %% escrow_offer(ReleaseTime, BenResult?)
```

Actually, let me think about what goes on the friend channel. The beneficiary needs to know the ReleaseTime (for display) and have the BenResult reader (to monitor). So:

```prolog
; escrow_offer(Constant, _)    %% escrow_offer(ReleaseTime, BenResult?)
```

The `_` is untyped because BenResult will be either a BondList or the atom `cancelled` — a heterogeneous type.

UserContent additions:
```prolog
; deposit_escrow(Constant, LotList, Constant)    %% deposit_escrow(Target, GiveSpec, ReleaseTime)
; cancel_escrow(Constant, PendingValue)           %% cancel_escrow(Target, PendingValue)
```

PendingValue addition:
```prolog
; escrow_pending(EscrowCancel?)    %% CancelSignal writer
```

Wait — PendingValue stores the CancelSignal writer so the depositor can cancel later. But the mediator handles pending lookup. Let me think about the flow:

When Alice deposits, the agent stores the CancelSignal writer. The mediator assigns a req(N) to it. When Alice sends `cancel_escrow(bob, req(N))`, the mediator looks up the pending and passes the CancelSignal writer to the agent, which binds it to `cancel`.

So PendingValue gets a new variant:
```prolog
; escrow_pending(EscrowCancel?)
```

Actually, `EscrowCancel?` is a writer. But PendingValue stores it as a reader-mode reference? Hmm. Looking at existing pending patterns: `response(Response?)` stores a writer-mode Response. `credit_pending(CreditResponse?, ...)` stores a writer-mode CreditResponse. So yes, PendingValue stores writers that the agent later binds.

But the existing pattern is: the agent sends the notification with the writer to the mediator, mediator stores it in pending. When user acts, mediator passes it back, agent binds it.

For escrow: the agent creates CancelSignal as an unbound writer/reader pair. The escrow process reads CancelSignal?. The agent stores the writer in a notification to the mediator:

```prolog
lookup_send('_user',
    msg(agent, '_user', escrow_deposited(Target?, ReleaseTime?, CancelSignal)),
    Outs?, Outs1),
```

Wait — `escrow_deposited` carries the CancelSignal writer to the mediator. The mediator stores it in pending and shows the user `escrow_deposited(Target, ReleaseTime, req(N))`. When user sends `cancel_escrow(Target, req(N))`, mediator looks up the CancelSignal writer and passes it to the agent.

But actually — does the depositor need a user notification for the deposit? The deposit was their own command. They just need the ability to cancel. So maybe:
- No notification to depositor on deposit (it was their command)
- Mediator stores CancelSignal in pending, returns req(N) to the actor
- Actor uses req(N) in `cancel_escrow` command later

Hmm, but the pattern in credit/loan is: proposer sends command, gets notification when the other side responds. The pending stores the other side's response variable.

For escrow: the depositor's pending stores the CancelSignal writer. The notification is just `escrow_deposited(Target, req(N))` confirming the deposit and providing the cancel handle.

Let me just simplify: use the same pattern as trade. The depositor sends the command, the deposit happens, and we need the cancel handle. The simplest approach: the agent notifies the user `escrow_deposited(Target, ReleaseTime, CancelWriter)` where CancelWriter is non-ground. The mediator stores it in pending and shows `escrow_deposited(Target, ReleaseTime, req(N))`.

AgentContent additions:
```prolog
; escrow_deposited(Constant, Constant, EscrowCancel?)    %% non-ground: CancelSignal in pending
; escrow_received(Constant, Constant)                     %% ground pass-through
; escrow_released(Constant)                               %% ground pass-through
; escrow_cancelled(Constant)                              %% ground pass-through
; escrow_expired(Constant)                                %% ground pass-through
; escrow_returned(Constant)                               %% ground pass-through
; escrow_failed(Constant)                                 %% ground pass-through
```

UserInMsg additions:
```prolog
; escrow_ben_released(Constant, BondList)     %% injected: beneficiary got bonds
; escrow_ben_cancelled(Constant)              %% injected: depositor cancelled
; escrow_dep_expired(Constant)                %% injected: time passed
; escrow_dep_returned(Constant, BondList)     %% injected: cancel succeeded
```

### New helper procedures

#### inject_escrow_ben_result

Monitors BenResult for the beneficiary:

```prolog
procedure inject_escrow_ben_result(_, Constant?, UserInStream?, UserInStream).
inject_escrow_ben_result(cancelled, From, Ys,
    [escrow_ben_cancelled(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_ben_result(BenResult, From, Ys,
    [escrow_ben_released(From?, BenResult?)|Ys?]) :-
    ground(From?), ground(BenResult?) | true.
inject_escrow_ben_result(BenResult, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_ben_result(BenResult?, From?, Ys?, Ys1).
```

Problem: the first two clauses both need to handle the case when BenResult becomes known. How do we distinguish BondList from `cancelled`? The `cancelled` atom is a string constant. BondList is a list. So:

```prolog
inject_escrow_ben_result(cancelled, From, Ys,
    [escrow_ben_cancelled(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_ben_result(BenResult, From, Ys,
    [escrow_ben_released(From?, BenResult?)|Ys?]) :-
    list(BenResult?), ground(From?), ground(BenResult?) | true.
inject_escrow_ben_result(BenResult, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_ben_result(BenResult?, From?, Ys?, Ys1).
```

Hmm, but `cancelled` would also match the second clause if checked only with `ground`. We need clause ordering. First clause checks for `cancelled` atom. Second clause checks for list (bonds). The head match on `cancelled` in clause 1 handles it.

Actually, in GLP with three-valued semantics: clause 1's head pattern `cancelled` — if BenResult is unbound, head match suspends. If BenResult is `cancelled`, head matches. If BenResult is a list, head fails. Clause 2: any head, but guard `list(BenResult?)` — if BenResult is a list, succeeds. If `cancelled`, fails. So the two clauses are mutually exclusive when BenResult is ground. Good.

But `list` doesn't imply `ground`. We need both `list(BenResult?)` and `ground(BenResult?)`:

```prolog
inject_escrow_ben_result(cancelled, From, Ys,
    [escrow_ben_cancelled(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_ben_result(BenResult, From, Ys,
    [escrow_ben_released(From?, BenResult?)|Ys?]) :-
    ground(From?), ground(BenResult?) | true.
inject_escrow_ben_result(BenResult, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_ben_result(BenResult?, From?, Ys?, Ys1).
```

Actually this works: clause 1 matches `cancelled` in head. Clause 2 catches everything else with `ground(BenResult?)`. If BenResult is `cancelled`, clause 1 commits first (head match is more specific). If BenResult is a BondList, clause 1's head fails, clause 2 matches. The pass-through clause handles the waiting case.

#### inject_escrow_dep_result

Monitors DepResult for the depositor:

```prolog
procedure inject_escrow_dep_result(_, Constant?, UserInStream?, UserInStream).
inject_escrow_dep_result(expired, From, Ys,
    [escrow_dep_expired(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_dep_result(DepResult, From, Ys,
    [escrow_dep_returned(From?, DepResult?)|Ys?]) :-
    ground(From?), ground(DepResult?) | true.
inject_escrow_dep_result(DepResult, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_dep_result(DepResult?, From?, Ys?, Ys1).
```

Same pattern: clause 1 matches `expired` atom, clause 2 catches BondList.

#### do_deposit_escrow

```prolog
procedure do_deposit_escrow(Constant?, Constant?, LotList?, Constant?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
do_deposit_escrow(Id, Target, GiveSpec, ReleaseTime, Holdings, UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(GiveSpec?, Holdings?, Status, Selected, Remaining),
    do_deposit_escrow_result(Status?, Id?, Target?, ReleaseTime?, Selected?, Remaining?, UserIn?, NetIn?, Outs?, NextSerial?).

procedure do_deposit_escrow_result(Constant?, Constant?, Constant?, Constant?, BondList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
do_deposit_escrow_result(ok, Id, Target, ReleaseTime, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    ground(Selected?) |
    escrow(ReleaseTime?, Selected?, CancelSignal?, BenResult, DepResult),
    inject_escrow_dep_result(DepResult?, Target?, UserIn?, UserIn1),
    lookup_send(friend(Target?),
        msg(Id?, Target?, escrow_offer(ReleaseTime?, BenResult?)),
        Outs?, Outs1),
    lookup_send('_user',
        msg(agent, '_user', escrow_deposited(Target?, ReleaseTime?, CancelSignal)),
        Outs1?, Outs2),
    agent(Id?, UserIn1?, NetIn?, Outs2?, Remaining?, NextSerial?).
do_deposit_escrow_result(fail, Id, Target, _, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    append(Selected?, Remaining?, OrigHoldings),
    lookup_send('_user', msg(agent, '_user', escrow_failed(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

Note: `CancelSignal?` is an unbound reader passed to escrow. `CancelSignal` (the writer) is sent to the mediator in `escrow_deposited`, stored in pending.

Wait — `CancelSignal` needs to be created as a writer/reader pair. In GLP, unbound variables in the body are fresh. But `CancelSignal` appears as both writer and reader. The escrow process reads `CancelSignal?` (third arg). The agent sends `CancelSignal` (writer) to the mediator.

Actually, in GLP clause bodies, when you write a new variable name it creates a writer/reader pair automatically. `CancelSignal` in the escrow call is the reader (third arg has `?`), and `CancelSignal` in the lookup_send is the writer. Wait, no — the escrow's third arg is `CancelSignal?` (reader). The `escrow_deposited` notification carries `CancelSignal` (writer, no `?`). So:
- `CancelSignal` = writer (goes to mediator, later bound to `cancel`)
- `CancelSignal?` = reader (goes to escrow, which matches against `cancel` in clause 2)

This follows the standard SRSW pattern. Good.

But `ground(Selected?)` in the ok clause — is it needed? `Selected` appears twice: once in `escrow(...)` and once... no, actually `Selected?` only appears once — in the escrow call. Wait, `Selected` appears in `select_bonds_by_spec` output (writer), then `Selected?` (reader) in the escrow call. That's one write, one read. Fine, no ground guard needed.

Actually wait — `Selected?` passes bonds to the escrow. That's one use. Do we need `ground(Selected?)` for any reason? Let me check: the escrow's second arg is `BondList?` — it reads the bonds. The bonds should be ground (they're concrete bond terms from holdings). But the type checker might want a ground guard to allow the `=` assignment in the escrow body. Actually, the escrow uses `BenResult = Bonds?` which reads Bonds once. So SRSW is fine without ground guard. Remove `ground(Selected?)` from the ok clause.

Revised:
```prolog
do_deposit_escrow_result(ok, Id, Target, ReleaseTime, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    escrow(ReleaseTime?, Selected?, CancelSignal?, BenResult, DepResult),
    inject_escrow_dep_result(DepResult?, Target?, UserIn?, UserIn1),
    lookup_send(friend(Target?),
        msg(Id?, Target?, escrow_offer(ReleaseTime?, BenResult?)),
        Outs?, Outs1),
    lookup_send('_user',
        msg(agent, '_user', escrow_deposited(Target?, ReleaseTime?, CancelSignal)),
        Outs1?, Outs2),
    agent(Id?, UserIn1?, NetIn?, Outs2?, Remaining?, NextSerial?).
```

Hmm wait — `BenResult` and `DepResult` are outputs of the escrow process (writers). `BenResult?` is the reader sent to the beneficiary. `DepResult?` is the reader passed to inject_escrow_dep_result. The `escrow` procedure's signature is:

```prolog
procedure escrow(Constant?, BondList?, _?, _, _).
```

Args 4 and 5 (BenResult, DepResult) are writers — the escrow writes to them. Args 1-3 are readers — the escrow reads from them.

So in the caller:
- `escrow(ReleaseTime?, Selected?, CancelSignal?, BenResult, DepResult)` — all good
- `BenResult?` reader sent to beneficiary
- `DepResult?` reader sent to inject
- `CancelSignal` writer stored in pending

### Agent clauses

#### Depositor: deposit_escrow command

```prolog
agent(Id, [msg('_user', Id1, deposit_escrow(Target, GiveSpec, ReleaseTime))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?), ground(GiveSpec?), ground(ReleaseTime?) |
    do_deposit_escrow(Id?, Target?, GiveSpec?, ReleaseTime?, Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).
```

#### Depositor: escrow_dep_expired (injected — time passed)

```prolog
agent(Id, [escrow_dep_expired(Target)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?) |
    lookup_send('_user', msg(agent, '_user', escrow_expired(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

#### Depositor: escrow_dep_returned (injected — cancel succeeded)

```prolog
agent(Id, [escrow_dep_returned(Target, OurBonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(Target?), ground(OurBonds?) |
    append(Holdings?, OurBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', escrow_returned(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

#### Depositor: cancel_escrow

```prolog
agent(Id, [msg('_user', Id1, cancel_escrow(Target,
        escrow_pending(CancelSignal?)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?) |
    CancelSignal = cancel,
    agent(Id?, UserIn?, NetIn?, Outs?, Holdings?, NextSerial?).
```

Wait — `CancelSignal?` is a reader in the pending. But we need to WRITE to it (bind it to `cancel`). The pattern from credit/loan: the agent receives the writer via pending lookup.

Looking at `reject_trade`:
```prolog
agent(Id, [msg('_user', Id1, reject_trade(From,
        trade_pending(TradeResp?, _, OfferedBonds)))|UserIn], ...
    bind_trade_decline(TradeResp, OfferedBonds?),
```

Here `TradeResp?` is decomposed from pending — it's the reader side. `bind_trade_decline(TradeResp, ...)` takes writer mode in the first arg.

Wait, looking more carefully: `trade_pending(TradeResp?, ...)` — the `?` means the pending stores the reader of TradeResp. But the agent needs the writer to bind it. Hmm.

Actually, I think the way it works: the mediator stores the raw term (which contains the unbound writer). When the agent receives it back from pending, it can access the writer through the decomposed pattern. Let me look at the befriend pattern:

```prolog
agent(Id, [msg('_user', Id1, decision(Dec, From, response(Resp?)))|UserIn], ...
    bind_response(Dec?, From?, Resp, Outs?, ...)
```

Here `response(Resp?)` decomposes the pending value. `Resp?` is the reader. `Resp` in `bind_response` is the writer. The head decomposition creates both the writer and reader from the pending term.

So for escrow:
```prolog
agent(Id, [msg('_user', Id1, cancel_escrow(Target,
        escrow_pending(CancelSignal?)))|UserIn], ...
```

`CancelSignal?` is the reader. `CancelSignal` (without `?`) would be the writer. But we only have `CancelSignal?` in the head. We need a bind procedure:

```prolog
procedure bind_escrow_cancel(EscrowCancel).
bind_escrow_cancel(cancel).
```

Then:
```prolog
agent(Id, [msg('_user', Id1, cancel_escrow(Target,
        escrow_pending(CancelSignal?)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?) |
    bind_escrow_cancel(CancelSignal),
    agent(Id?, UserIn?, NetIn?, Outs?, Holdings?, NextSerial?).
```

Wait, but `CancelSignal?` in the head is reader mode, and `bind_escrow_cancel(CancelSignal)` takes writer mode. The head provides the reader, but we need the writer. This is the same pattern as `bind_response`, `bind_credit_accept`, etc. The head decomposition of a pending value provides both the reader and writer sides.

Hmm, actually I need to look at this more carefully. In `decision(Dec, From, response(Resp?))`:
- `Resp?` is the reader
- `Resp` is the writer
- The head pattern `response(Resp?)` matches the pending value `response(SomeWriter?)` — the reader of SomeWriter
- When the match succeeds, `Resp` is bound to the writer half, `Resp?` to the reader half

Actually wait. The pending stores `response(Response?)` where `Response?` is a reader pointing to an unbound writer. When the agent's head matches against this pending value with `response(Resp?)`, the pattern matching maps:
- The `Response?` reader in the stored pending matches `Resp?` in the head
- But head variables are writer/reader pairs: `Resp` is writer, `Resp?` is reader
- The head match binds `Resp?` (reader) to the stored `Response?` (also a reader)

So `Resp` (writer) is NOT the same as the original writer. It's a fresh writer whose reader `Resp?` is matched against the stored reader.

But then `bind_response(yes, From, accept(RetCh?), ...)` writes to `Resp` (the head's writer). How does this propagate to the original writer?

I think I'm overcomplicating this. The GLP pattern is: decomposing a non-ground term in the head extracts the non-ground subparts. When the head matches `response(Resp?)`, `Resp` points to the same variable as the original `Response`. The writer in the pending IS `Resp`.

This is confirmed by the type declarations: `PendingValue ::= response(Response?) ; ...` — the `?` means the stored Response is in reader mode. But the agent's clause head decomposes it and gets access to the writer side.

OK, so for escrow: `escrow_pending(CancelSignal?)` stores the reader. The head decomposition gives the agent access to `CancelSignal` (writer). `bind_escrow_cancel(CancelSignal)` writes `cancel` to it. This follows the exact same pattern as all other pending bindings.

#### Beneficiary: incoming escrow_offer

```prolog
agent(Id, UserIn, [msg(From, Id1, escrow_offer(ReleaseTime, BenResult?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(ReleaseTime?) |
    inject_escrow_ben_result(BenResult?, From?, UserIn?, UserIn1),
    lookup_send('_user', msg(agent, '_user', escrow_received(From?, ReleaseTime?)), Outs?, Outs1),
    agent(Id?, UserIn1?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

Note: `BenResult?` is a reader — the escrow process hasn't resolved yet. The inject monitors it.

#### Beneficiary: escrow_ben_released (injected — time passed, bonds received)

```prolog
agent(Id, [escrow_ben_released(From, Bonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(From?), ground(Bonds?) |
    append(Holdings?, Bonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', escrow_released(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

#### Beneficiary: escrow_ben_cancelled (injected — depositor cancelled)

```prolog
agent(Id, [escrow_ben_cancelled(From)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?) |
    lookup_send('_user', msg(agent, '_user', escrow_cancelled(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

### Mediator clauses

#### Agent-to-user: escrow_deposited (non-ground — pending)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', escrow_deposited(Target, ReleaseTime, CancelSignal?)),
            AgentCh?, AgentCh1),
    ground(Target?), ground(ReleaseTime?) |
    send_user(escrow_deposited(Target?, ReleaseTime?, req(N?)), UserCh?, UserCh1),
    N1 := N? + 1,
    ui_mediator(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), escrow_pending(CancelSignal)) | Ps?], N1?).
```

Wait — `CancelSignal?` is reader in the receive. The mediator stores `escrow_pending(CancelSignal)` — the writer side — in pending. So when the agent later receives it back via pending lookup, it can bind the writer.

Hmm, but `CancelSignal?` in the receive is reader mode. `CancelSignal` (writer) in pending... The receive pattern decomposes the message. Like `befriend(From, Resp?)` in the mediator — `Resp` is the writer that gets stored.

I think the pattern is: the agent sends the writer in the message. The mediator's receive head decomposes it. The writer side of the decomposition goes into pending.

For escrow: the agent sends `escrow_deposited(Target?, ReleaseTime?, CancelSignal)` where `CancelSignal` is the writer. The mediator's receive matches `escrow_deposited(Target, ReleaseTime, CancelSignal?)` — `CancelSignal?` is the reader. The mediator stores `escrow_pending(CancelSignal)` — wait, is this the writer or reader?

OK let me look at the befriend case carefully:

Agent sends: `msg(agent, '_user', befriend(From?, Resp))` — `Resp` is writer (no `?`).
Mediator receives: `receive(msg(agent, '_user', befriend(From, Resp?)), ...)` — `Resp?` is reader.
Mediator stores: `[pending(req(N?), response(Resp)) | Ps?]` — `Resp` is writer.

Wait, that doesn't make sense. In the receive, `Resp?` is reader. Then `Resp` (writer) in the pending... In the head, the pattern `befriend(From, Resp?)` decomposes the message. `From?` and `Resp?` are readers matching the message content. But `Resp` (without `?`) is the writer half of the head variable. The message contained an unbound writer — the head match binds the head's reader `Resp?` to the message content (which includes the unbound writer). So `Resp` (the head's writer) IS the original writer.

No wait. The message contains `Resp` (writer) from the agent. The mediator's head has `befriend(From, Resp?)`. The head match: `From` matches the ground From value. `Resp?` matches the writer in the message. Since `Resp?` is a reader in the head, and the message contains a writer... this is the standard head-match: reader in head matches writer/value in message. The head's `Resp?` reader points to the message's `Resp` writer.

Then `response(Resp)` in pending: `Resp` here is the head's WRITER. Not the same as the message's writer. The head creates a new writer/reader pair where `Resp?` (reader) is matched against the message content.

I think the key insight is that the pending stores the non-ground structure as-is. When the agent's head later matches it, the writer half gives write access to the original unbound variable.

This is getting complicated. Let me just follow the existing patterns exactly and trust that they work (they've been tested through plays 1-9).

#### Agent-to-user: ground pass-through

Add mediator clauses for `escrow_received`, `escrow_released`, `escrow_cancelled`, `escrow_expired`, `escrow_returned`, `escrow_failed`. Standard pattern.

#### User-to-agent: deposit_escrow (pass through)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(deposit_escrow(Target, GiveSpec, ReleaseTime), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(GiveSpec?), ground(ReleaseTime?) |
    send_agent(msg('_user', Id?, deposit_escrow(Target?, GiveSpec?, ReleaseTime?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

#### User-to-agent: cancel_escrow (with pending lookup)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(cancel_escrow(Target, ReqId), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(ReqId?) |
    lookup_pending(ReqId?, Pv, Ps?, Ps1),
    send_agent(msg('_user', Id?, cancel_escrow(Target?, Pv?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps1?, N?).
```

### Actor types (bond_actors.glp)

UserCmd additions:
```prolog
; deposit_escrow(Constant, LotList, Constant)
; cancel_escrow(Constant, ReqId)
```

UserNotify additions:
```prolog
; escrow_deposited(Constant, Constant, ReqId)
; escrow_received(Constant, Constant)
; escrow_released(Constant)
; escrow_cancelled(Constant)
; escrow_expired(Constant)
; escrow_returned(Constant)
; escrow_failed(Constant)
```

### Play 10: Escrow — Time Release

Scenario: Alice deposits 3 coins in escrow for Bob, release time T = now + 500ms. Bob monitors. Time passes. Bob gets bonds. Alice gets `expired`.

alice_p10 (takes T as parameter, like play4b):
1. connect(bob)
2. Wait connected(bob)
3. credit(bob, 5, 0) — give both sides coins
4. Wait credit_opened(bob, _)
5. `deposit_escrow(bob, [lot(bob, 0, 3)], T)` — deposit 3 bob-coins, release at T
6. Wait escrow_deposited(bob, _, req(N)) — get cancel handle
7. Wait escrow_expired(bob) — time passed, bonds went to Bob
8. balance → Wait balance_report(_)
9. done

bob_p10:
1. Wait befriend → accept
2. Wait connected
3. Wait credit_proposed → accept
4. Wait credit_opened
5. Wait escrow_received(alice, _) — incoming escrow
6. Wait escrow_released(alice) — time passed, got bonds
7. balance → Wait balance_report(_)
8. done

Expected: After credit, Alice has 5 bob-coins, Bob has 5 alice-coins. Alice deposits 3 bob-coins in escrow. After 500ms, Bob gets them.
- Alice final: 2 bob-coins
- Bob final: 5 alice-coins + 3 bob-coins = 8 bonds

### Play 11: Escrow — Cancel

Scenario: Alice deposits 3 coins in escrow for Bob, release time T = now + 5000ms (far future). Alice cancels. Alice gets bonds back. Bob gets `cancelled`.

alice_p11 (takes T as parameter):
1. connect(bob)
2. Wait connected(bob)
3. credit(bob, 5, 0)
4. Wait credit_opened(bob, _)
5. `deposit_escrow(bob, [lot(bob, 0, 3)], T)` — deposit 3 bob-coins, release far future
6. Wait escrow_deposited(bob, _, req(N)) — get cancel handle
7. `cancel_escrow(bob, req(N))` — cancel!
8. Wait escrow_returned(bob) — bonds back
9. balance → Wait balance_report(_)
10. done

bob_p11:
1. Wait befriend → accept
2. Wait connected
3. Wait credit_proposed → accept
4. Wait credit_opened
5. Wait escrow_received(alice, _)
6. Wait escrow_cancelled(alice) — depositor cancelled
7. balance → Wait balance_report(_)
8. done

Expected:
- Alice final: 5 bob-coins (all back)
- Bob final: 5 alice-coins (no bob-coins received)

### Boot (bond_boot.glp)

Add play10/fplay10 and play11/fplay11. Both compute T and pass to actors.

Play 10: T = now + 500 (short wait, similar to play4b)
Play 11: T = now + 5000 (far future, cancel happens before)

## Testing

1. Run baseline: all existing plays must pass
2. Run fplay10: verify escrow_released on Bob's side, escrow_expired on Alice's side, correct balances
3. Run fplay11: verify escrow_cancelled on Bob's side, escrow_returned on Alice's side, correct balances (Alice has all coins back)

**IMPORTANT**: fplay10 requires ~500ms to complete (waiting for escrow timer). fplay11 should be fast (cancel happens immediately, no timer wait).

## OutputContent additions

```prolog
; escrow_offer(Constant, _)
; escrow_deposited(Constant, Constant, EscrowCancel?)
; escrow_received(Constant, Constant)
; escrow_released(Constant)
; escrow_cancelled(Constant)
; escrow_expired(Constant)
; escrow_returned(Constant)
; escrow_failed(Constant)
```

## Notes

- The escrow process is spawned in the depositor's body — it's a local concurrent process, not a named agent
- `wait_until` in the escrow SUSPENDS until time T (requires the runtime fix)
- The race between `wait_until(T?)` and head match on `cancel` is resolved by GLP's committed-choice semantics — first clause to succeed commits
- `now(Now)` in the redeem_request handler is a body goal that runs concurrently — `select_bonds_min_maturity` suspends on `Now?` until bound (virtually instant)
- Play 11 uses T = now + 5000 to ensure cancel happens well before time passes
