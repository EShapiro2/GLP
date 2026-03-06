# Phase 8B: Escrow Implementation — Corrected Instructions

## CRITICAL: Read Before Coding

Before writing ANY GLP code, read these files completely:
1. **`docs/glp-cheat-sheet.md`** — compact reference for GLP programming patterns. GLP is NOT Prolog. Study the wrong vs right examples.
2. `docs/typed-glp-manual.md` — the complete GLP manual
3. `docs/DISCIPLINE.md` — development standards
4. The existing `bond_agent.glp` — study the patterns, especially `handle_redeem_fill`, `bind_credit_accept`, `bind_trade_accept`, `inject_credit_result`, `inject_loan_result`, `inject_trade_result`

## THE KEY GLP PATTERN: Writer-Mode Output via Head Decomposition

**WRONG** (Prolog-style, will be rejected by type checker):
```prolog
escrow(T, Bonds, _, BenResult, DepResult) :-
    wait_until(T?) |
    BenResult = Bonds?,
    DepResult = expired.
```

**RIGHT** (GLP head decomposition):
```prolog
escrow(T, Bonds, _,
    escrow_bonds(Bonds?),
    escrow_expired) :-
    wait_until(T?) | true.
```

Writer-mode outputs are ALWAYS constructed in clause heads. Study `handle_redeem_fill`, `handle_trade_fill`, `bind_trade_accept`, `bind_trade_decline` for working examples.

## Prerequisites

- Part A (select_bonds_min_maturity fix) must be complete and tested
- All existing plays (fplay1–fplay9, fplay4b) must pass
- `wait_until` fix must be in place (suspends, not fails)

## Step 1: Add Escrow Result Types

The types section already has `EscrowCancel ::= cancel.` Add after it:

```prolog
%% Escrow results — typed unions for escrow output variables
EscrowBenResult ::= escrow_bonds(BondList) ; escrow_cancelled.
EscrowDepResult ::= escrow_bonds(BondList) ; escrow_expired.
```

## Step 2: Fix FriendContent and OutputContent

Change `escrow_offer(Constant, _)` to `escrow_offer(Constant, EscrowBenResult?)` in both FriendContent and OutputContent.

## Step 3: Escrow Procedure

Place after the trade helpers, before the AGENT/6 section.

```prolog
%% =============================================================================
%% ESCROW — Time-guarded bond custody
%% =============================================================================
%%
%% Spawned by depositor. Races wait_until(T) against cancel signal.
%% wait_until SUSPENDS until time T (does not fail).
%% Head match on 'cancel' suspends until CancelSignal is bound.
%% First clause to wake commits; the other is abandoned.

procedure escrow(Constant?, BondList?, EscrowCancel?, EscrowBenResult, EscrowDepResult).

%% Time passes — beneficiary gets bonds, depositor gets 'expired'
escrow(T, Bonds, _,
    escrow_bonds(Bonds?),
    escrow_expired) :-
    wait_until(T?) | true.

%% Depositor cancels — depositor gets bonds back, beneficiary gets 'cancelled'
%% Head match on 'cancel' suspends until CancelSignal is bound. No guard needed.
escrow(_, Bonds, cancel,
    escrow_cancelled,
    escrow_bonds(Bonds?)).
```

## Step 4: Inject Helpers

```prolog
%% =============================================================================
%% INJECT_ESCROW_BEN_RESULT — Monitor beneficiary's escrow outcome
%% =============================================================================

procedure inject_escrow_ben_result(EscrowBenResult?, Constant?, UserInStream?, UserInStream).
inject_escrow_ben_result(escrow_bonds(Bonds), From, Ys,
    [escrow_ben_released(From?, Bonds?)|Ys?]) :-
    ground(From?), ground(Bonds?) | true.
inject_escrow_ben_result(escrow_cancelled, From, Ys,
    [escrow_ben_cancelled(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_ben_result(Resp, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_ben_result(Resp?, From?, Ys?, Ys1).

%% =============================================================================
%% INJECT_ESCROW_DEP_RESULT — Monitor depositor's escrow outcome
%% =============================================================================

procedure inject_escrow_dep_result(EscrowDepResult?, Constant?, UserInStream?, UserInStream).
inject_escrow_dep_result(escrow_bonds(Bonds), From, Ys,
    [escrow_dep_returned(From?, Bonds?)|Ys?]) :-
    ground(From?), ground(Bonds?) | true.
inject_escrow_dep_result(escrow_expired, From, Ys,
    [escrow_dep_expired(From?)|Ys?]) :-
    ground(From?) | true.
inject_escrow_dep_result(Resp, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_escrow_dep_result(Resp?, From?, Ys?, Ys1).
```

## Step 5: bind_escrow_cancel

```prolog
%% =============================================================================
%% BIND_ESCROW_CANCEL — Bind cancel signal (writer mode)
%% =============================================================================

procedure bind_escrow_cancel(EscrowCancel).
bind_escrow_cancel(cancel).
```

## Step 6: do_deposit_escrow

```prolog
%% =============================================================================
%% DO_DEPOSIT_ESCROW — Select bonds and create escrow or fail
%% =============================================================================

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

## Step 7: Agent Clauses

Add before the Termination section, after the Trade section.

```prolog
%% --- Phase 8: Escrow ---

%% Depositor: deposit_escrow command
agent(Id, [msg('_user', Id1, deposit_escrow(Target, GiveSpec, ReleaseTime))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?), ground(GiveSpec?), ground(ReleaseTime?) |
    do_deposit_escrow(Id?, Target?, GiveSpec?, ReleaseTime?, Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).

%% Depositor: escrow expired (injected — time passed, bonds went to beneficiary)
agent(Id, [escrow_dep_expired(Target)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?) |
    lookup_send('_user', msg(agent, '_user', escrow_expired(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).

%% Depositor: escrow returned (injected — cancel succeeded, bonds back)
agent(Id, [escrow_dep_returned(Target, OurBonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(Target?), ground(OurBonds?) |
    append(Holdings?, OurBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', escrow_returned(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).

%% Depositor: cancel_escrow — bind CancelSignal to cancel
agent(Id, [msg('_user', Id1, cancel_escrow(Target,
        escrow_pending(CancelSignal?)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?) |
    bind_escrow_cancel(CancelSignal),
    agent(Id?, UserIn?, NetIn?, Outs?, Holdings?, NextSerial?).

%% Beneficiary: incoming escrow_offer
agent(Id, UserIn, [msg(From, Id1, escrow_offer(ReleaseTime, BenResult?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(ReleaseTime?) |
    inject_escrow_ben_result(BenResult?, From?, UserIn?, UserIn1),
    lookup_send('_user', msg(agent, '_user', escrow_received(From?, ReleaseTime?)), Outs?, Outs1),
    agent(Id?, UserIn1?, NetIn?, Outs1?, Holdings?, NextSerial?).

%% Beneficiary: escrow released (injected — time passed, bonds received)
agent(Id, [escrow_ben_released(From, Bonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(From?), ground(Bonds?) |
    append(Holdings?, Bonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', escrow_released(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).

%% Beneficiary: escrow cancelled (injected — depositor cancelled)
agent(Id, [escrow_ben_cancelled(From)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?) |
    lookup_send('_user', msg(agent, '_user', escrow_cancelled(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

## Step 8: Mediator Changes (bond_mediator.glp)

Add types `EscrowCancel`, `EscrowBenResult`, `EscrowDepResult` (same definitions as agent).
Change `escrow_offer(Constant, _)` to `escrow_offer(Constant, EscrowBenResult?)` in FriendContent.

### Agent-to-user: escrow_deposited (non-ground — pending)

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

### Ground pass-through clauses

One each for: `escrow_received(From, ReleaseTime)`, `escrow_released(Who)`, `escrow_cancelled(Who)`, `escrow_expired(Who)`, `escrow_returned(Who)`, `escrow_failed(Who)`. Follow exact pattern of trade_completed etc.

### User-to-agent: deposit_escrow (pass through)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(deposit_escrow(Target, GiveSpec, ReleaseTime), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(GiveSpec?), ground(ReleaseTime?) |
    send_agent(msg('_user', Id?, deposit_escrow(Target?, GiveSpec?, ReleaseTime?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

### User-to-agent: cancel_escrow (with pending lookup)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(cancel_escrow(Target, ReqId), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(ReqId?) |
    lookup_pending(ReqId?, Pv, Ps?, Ps1),
    send_agent(msg('_user', Id?, cancel_escrow(Target?, Pv?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps1?, N?).
```

## Step 9: Actors + Boot

### Actor types (bond_actors.glp)

Add types `EscrowBenResult`, `EscrowDepResult`, `EscrowCancel`, `Lot`, `LotList` if not already present.

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

Boot computes T = now + 500. Alice deposits 3 bob-coins for Bob, release at T. Time passes. Bob gets bonds.

alice_p10 (takes T as parameter):
1. connect(bob)
2. Wait connected(bob)
3. credit(bob, 5, 0) — give both sides coins
4. Wait credit_opened(bob, _)
5. deposit_escrow(bob, [lot(bob, 0, 3)], T)
6. Wait escrow_deposited(bob, _, req(N))
7. Wait escrow_expired(bob) — time passed
8. balance → Wait balance_report(_)
9. done

bob_p10:
1. Wait befriend → accept
2. Wait connected
3. Wait credit_proposed → accept
4. Wait credit_opened
5. Wait escrow_received(alice, _)
6. Wait escrow_released(alice) — got bonds
7. balance → Wait balance_report(_)
8. done

### Play 11: Escrow — Cancel

Boot computes T = now + 5000. Alice deposits, then cancels immediately.

alice_p11 (takes T as parameter):
1. connect(bob)
2. Wait connected(bob)
3. credit(bob, 5, 0)
4. Wait credit_opened(bob, _)
5. deposit_escrow(bob, [lot(bob, 0, 3)], T)
6. Wait escrow_deposited(bob, _, req(N))
7. cancel_escrow(bob, req(N))
8. Wait escrow_returned(bob)
9. balance → Wait balance_report(_)
10. done

bob_p11:
1. Wait befriend → accept
2. Wait connected
3. Wait credit_proposed → accept
4. Wait credit_opened
5. Wait escrow_received(alice, _)
6. Wait escrow_cancelled(alice)
7. balance → Wait balance_report(_)
8. done

### Boot

Add play10/fplay10 (T = now + 500) and play11/fplay11 (T = now + 5000). Same wiring as play4b — compute T in boot, pass to alice actor.

## Testing

1. Type-check after EACH step (agent, mediator, actors)
2. Run baseline: fplay1–fplay9, fplay4b
3. fplay10: ~500ms, verify escrow_released/escrow_expired
4. fplay11: fast, verify escrow_returned/escrow_cancelled
