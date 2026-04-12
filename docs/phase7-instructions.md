# Phase 7 Instructions: Trade (Sale of Debt + Forward Contract)

## Prerequisites

1. Read CLAUDE.md completely
2. Read docs/DISCIPLINE.md completely
3. Read docs/typed-glp-manual.md completely
4. Run baseline tests (fplay1–fplay7, fplay4b) before any changes

## Overview

Add a **trade** command: a pre-agreed bilateral swap from existing holdings. Proposer commits bonds from holdings into the message (locked). Responder either accepts (returns agreed bonds) or declines (returns proposer's bonds).

## Design

### Command

`trade(Target, GiveSpec, WantSpec)` where GiveSpec and WantSpec are lists of `lot(Issuer, Maturity, Count)`.

### TradeResponse

```prolog
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).
```

Both variants carry bonds: accept carries responder's bonds to proposer; decline carries proposer's bonds back.

### Protocol

1. Alice sends `trade(bob, GiveSpec, WantSpec)`.
2. Agent selects bonds matching GiveSpec from holdings.
   - Can't fill → notify `trade_failed(bob)`, holdings unchanged. Done.
   - Ok → remove selected from holdings, send `trade_propose(WantSpec, SelectedBonds, TradeResp?)` on friend channel. Start `inject_trade_result(TradeResp?, bob, ...)`.
3. Bob's agent receives, forwards to user via mediator as `trade_proposed(alice, WantSpec, req(N))`. The offered bonds and TradeResp are stored in pending.
4. Bob accepts or rejects:
   - **Accept**: Bob's agent selects bonds matching WantSpec from own holdings.
     - Ok: bind `trade_accept(BobSelected)`. Bob removes his selected bonds, adds Alice's offered bonds. No notification (acceptance is synchronous).
     - Can't fill: bind `trade_decline(OfferedBonds)`. Bob returns Alice's bonds. Holdings unchanged. Notify `trade_failed(alice)`.
   - **Reject**: bind `trade_decline(OfferedBonds)`. Bob returns Alice's bonds. Holdings unchanged. No notification (rejection is the responder's own action).
5. Alice's inject detects response:
   - `trade_accept(TheirBonds)` → inject `trade_complete(bob, TheirBonds)`. Agent adds TheirBonds, notifies `trade_completed(bob)`.
   - `trade_decline(OurBonds)` → inject `trade_returned_bonds(bob, OurBonds)`. Agent adds OurBonds back, notifies `trade_returned(bob)`.

### Freeze behavior

If responder never responds, proposer's bonds are locked (TradeResp stays unbound). By design.

### Notifications (4 total)

- `trade_proposed(From, WantSpec, req(N))` — to responder: incoming proposal
- `trade_completed(Target)` — to proposer: trade succeeded, bonds received
- `trade_failed(Other)` — to proposer: couldn't fill own offer; to responder: accepted but couldn't fill
- `trade_returned(Target)` — to proposer: responder declined, bonds back

## Part 1: New Types (all three typed files)

### New types

```prolog
Lot     ::= lot(Constant, Constant, Constant).
LotList ::= [] ; [Lot | LotList].
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).
```

### FriendContent addition

```prolog
; trade_propose(LotList, BondList, TradeResponse?)
```

### UserContent additions

```prolog
; trade(Constant, LotList, LotList)
; accept_trade(Constant, PendingValue)
; reject_trade(Constant, PendingValue)
```

### PendingValue addition

```prolog
; trade_pending(TradeResponse?, LotList, BondList)
```

### AgentContent additions

```prolog
; trade_proposed(Constant, LotList, TradeResponse?, BondList)
; trade_completed(Constant)
; trade_failed(Constant)
; trade_returned(Constant)
```

### OutputContent additions

Same as FriendContent and AgentContent additions.

### UserInMsg additions

```prolog
; trade_complete(Constant, BondList)
; trade_returned_bonds(Constant, BondList)
```

### Actor types (bond_actors.glp)

UserCmd:
```prolog
; trade(Constant, LotList, LotList)
; accept_trade(Constant, ReqId)
; reject_trade(Constant, ReqId)
```

UserNotify:
```prolog
; trade_proposed(Constant, LotList, ReqId)
; trade_completed(Constant)
; trade_failed(Constant)
; trade_returned(Constant)
```

## Part 2: New Helper Procedures (bond_agent.glp)

### select_bonds_exact — Select K bonds with exact Issuer+Maturity

```prolog
procedure select_bonds_exact(Constant?, Constant?, Constant?, BondList?, Constant, BondList, BondList).
select_bonds_exact(_, _, 0, Hs, ok, [], Hs?).
select_bonds_exact(Issuer, Maturity, K, [bond(I, M, S)|Rest], Status?, [bond(I?, M?, S?)|Sel?], Rem?) :-
    K? > 0, Issuer? =?= I?, Maturity? =?= M? |
    K1 := K? - 1,
    select_bonds_exact(Issuer?, Maturity?, K1?, Rest?, Status, Sel, Rem).
select_bonds_exact(Issuer, Maturity, K, [B|Rest], Status?, Sel?, [B?|Rem?]) :-
    otherwise |
    select_bonds_exact(Issuer?, Maturity?, K?, Rest?, Status, Sel, Rem).
select_bonds_exact(_, _, K, [], fail, [], []) :- K? > 0 | true.
```

### select_bonds_by_spec — Fold over LotList

```prolog
procedure select_bonds_by_spec(LotList?, BondList?, Constant, BondList, BondList).
select_bonds_by_spec([lot(Issuer, Maturity, Count)|Rest], Holdings, Status?, AllSel?, FinalRem?) :-
    ground(Issuer?), ground(Maturity?), ground(Count?) |
    select_bonds_exact(Issuer?, Maturity?, Count?, Holdings?, LotStatus, LotSel, LotRem),
    select_by_spec_continue(LotStatus?, Rest?, LotSel?, LotRem?, Status, AllSel, FinalRem).
select_bonds_by_spec([], Holdings, ok, [], Holdings?).

procedure select_by_spec_continue(Constant?, LotList?, BondList?, BondList?, Constant, BondList, BondList).
select_by_spec_continue(ok, Rest, LotSel, LotRem, Status?, AllSel?, FinalRem?) :-
    select_bonds_by_spec(Rest?, LotRem?, Status, RestSel, FinalRem),
    append(LotSel?, RestSel?, AllSel).
select_by_spec_continue(fail, _, LotSel, LotRem, fail, LotSel?, LotRem?).
```

## Part 3: Bind and Inject (bond_agent.glp)

### bind_trade_accept / bind_trade_decline

```prolog
procedure bind_trade_accept(TradeResponse, BondList?).
bind_trade_accept(trade_accept(Bonds?), Bonds).

procedure bind_trade_decline(TradeResponse, BondList?).
bind_trade_decline(trade_decline(Bonds?), Bonds).
```

### inject_trade_result

No OurBonds parameter — bonds come back through the response itself.

```prolog
procedure inject_trade_result(TradeResponse?, Constant?, UserInStream?, UserInStream).
inject_trade_result(trade_accept(TheirBonds), From, Ys,
    [trade_complete(From?, TheirBonds?)|Ys?]) :-
    ground(From?), ground(TheirBonds?) | true.
inject_trade_result(trade_decline(OurBonds), From, Ys,
    [trade_returned_bonds(From?, OurBonds?)|Ys?]) :-
    ground(From?), ground(OurBonds?) | true.
inject_trade_result(Resp, From, [Y|Ys], [Y?|Ys1?]) :-
    inject_trade_result(Resp?, From?, Ys?, Ys1).
```

## Part 4: do_trade (bond_agent.glp)

```prolog
procedure do_trade(Constant?, Constant?, LotList?, LotList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
do_trade(Id, Target, GiveSpec, WantSpec, Holdings, UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(GiveSpec?, Holdings?, Status, Selected, Remaining),
    do_trade_result(Status?, Id?, Target?, WantSpec?, Selected?, Remaining?, UserIn?, NetIn?, Outs?, NextSerial?).

procedure do_trade_result(Constant?, Constant?, Constant?, LotList?, BondList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
do_trade_result(ok, Id, Target, WantSpec, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    lookup_send(friend(Target?),
        msg(Id?, Target?, trade_propose(WantSpec?, Selected?, TradeResp)),
        Outs?, Outs1),
    inject_trade_result(TradeResp?, Target?, UserIn?, UserIn1),
    agent(Id?, UserIn1?, NetIn?, Outs1?, Remaining?, NextSerial?).
do_trade_result(fail, Id, Target, _, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    append(Selected?, Remaining?, OrigHoldings),
    lookup_send('_user', msg(agent, '_user', trade_failed(Target?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

## Part 5: handle_trade_accept (bond_agent.glp)

```prolog
procedure handle_trade_accept(Constant?, Constant?, LotList?, TradeResponse, BondList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
handle_trade_accept(Id, From, WantSpec, TradeResp, OfferedBonds, Holdings, UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(WantSpec?, Holdings?, Status, Selected, Remaining),
    handle_trade_fill(Status?, Id?, From?, TradeResp, OfferedBonds?, Selected?, Remaining?, UserIn?, NetIn?, Outs?, NextSerial?).

procedure handle_trade_fill(Constant?, Constant?, Constant?, TradeResponse, BondList?, BondList?, BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).
%% Can fill: bind accept(our bonds), take offered bonds
handle_trade_fill(ok, Id, From, trade_accept(Selected?), OfferedBonds, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    append(Remaining?, OfferedBonds?, NewHoldings),
    agent(Id?, UserIn?, NetIn?, Outs?, NewHoldings?, NextSerial?).
%% Can't fill: bind decline(offered bonds back), reconstruct own holdings
handle_trade_fill(fail, Id, From, trade_decline(ReturnBonds?), OfferedBonds, Selected, Remaining, UserIn, NetIn, Outs, NextSerial) :-
    ReturnBonds = OfferedBonds?,
    append(Selected?, Remaining?, OrigHoldings),
    lookup_send('_user', msg(agent, '_user', trade_failed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

TradeResponse is writer-mode, decomposed in clause heads. In the fail case, `ReturnBonds = OfferedBonds?` bridges reader to writer.

## Part 6: Agent Clauses (bond_agent.glp)

### Proposer: trade command

```prolog
agent(Id, [msg('_user', Id1, trade(Target, GiveSpec, WantSpec))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?), ground(GiveSpec?), ground(WantSpec?) |
    do_trade(Id?, Target?, GiveSpec?, WantSpec?, Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).
```

### Proposer: trade_complete (injected — accept)

```prolog
agent(Id, [trade_complete(From, TheirBonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(From?), ground(TheirBonds?) |
    append(Holdings?, TheirBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_completed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

### Proposer: trade_returned_bonds (injected — decline)

```prolog
agent(Id, [trade_returned_bonds(From, OurBonds)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(From?), ground(OurBonds?) |
    append(Holdings?, OurBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_returned(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

### Responder: incoming trade_propose

```prolog
agent(Id, UserIn, [msg(From, Id1, trade_propose(WantSpec, OfferedBonds, TradeResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    lookup_send('_user',
        msg(agent, '_user', trade_proposed(From?, WantSpec?, TradeResp, OfferedBonds?)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

### Responder: accept_trade

```prolog
agent(Id, [msg('_user', Id1, accept_trade(From,
        trade_pending(TradeResp?, WantSpec, OfferedBonds)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    handle_trade_accept(Id?, From?, WantSpec?, TradeResp, OfferedBonds?, Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).
```

### Responder: reject_trade

```prolog
agent(Id, [msg('_user', Id1, reject_trade(From,
        trade_pending(TradeResp?, _, OfferedBonds)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(OfferedBonds?) |
    bind_trade_decline(TradeResp, OfferedBonds?),
    agent(Id?, UserIn?, NetIn?, Outs?, Holdings?, NextSerial?).
```

## Part 7: Mediator (bond_mediator.glp)

### Agent-to-user: trade_proposed (non-ground — pending)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', trade_proposed(From, WantSpec, TradeResp?, OfferedBonds)),
            AgentCh?, AgentCh1),
    ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    send_user(trade_proposed(From?, WantSpec?, req(N?)), UserCh?, UserCh1),
    N1 := N? + 1,
    ui_mediator(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), trade_pending(TradeResp, WantSpec?, OfferedBonds?)) | Ps?], N1?).
```

### Agent-to-user: ground pass-through

Add mediator clauses for `trade_completed`, `trade_failed`, `trade_returned`. Each follows the standard pattern:

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', trade_completed(Who)), AgentCh?, AgentCh1),
    ground(Who?) |
    send_user(trade_completed(Who?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

Same pattern for trade_failed and trade_returned.

### User-to-agent: trade (pass through)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(trade(Target, GiveSpec, WantSpec), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(GiveSpec?), ground(WantSpec?) |
    send_agent(msg('_user', Id?, trade(Target?, GiveSpec?, WantSpec?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

### User-to-agent: accept_trade / reject_trade (with pending lookup)

```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(accept_trade(From, ReqId), UserCh?, UserCh1),
    ground(Id?), ground(From?), ground(ReqId?) |
    lookup_pending(ReqId?, Pv, Ps?, Ps1),
    send_agent(msg('_user', Id?, accept_trade(From?, Pv?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps1?, N?).

ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(reject_trade(From, ReqId), UserCh?, UserCh1),
    ground(Id?), ground(From?), ground(ReqId?) |
    lookup_pending(ReqId?, Pv, Ps?, Ps1),
    send_agent(msg('_user', Id?, reject_trade(From?, Pv?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps1?, N?).
```

## Part 8: Actors (bond_actors.glp)

### Play 8: Sale of Debt

After credit, Alice has bob-coins, Bob has alice-coins. Alice mints alice-bonds(maturity 10). Alice sells 3 alice-bonds(10) to Bob for 2 alice-coins (Bob returning Alice's own coins at discount).

alice_p8:
1. connect(bob)
2. Wait connected(bob)
3. credit(bob, 5, 0)
4. Wait credit_opened(bob, _)
5. mint(3, 10)
6. Wait minted(3, 10)
7. `trade(bob, [lot(alice, 10, 3)], [lot(alice, 0, 2)])`
8. Wait trade_completed(bob)
9. balance → Wait balance_report(_)
10. done

bob_p8:
1. Wait befriend → decision(yes)
2. Wait connected
3. Wait credit_proposed → accept_credit
4. Wait credit_opened
5. Wait `trade_proposed(alice, _, ReqId)` → `accept_trade(alice, ReqId)`
6. balance → Wait balance_report(_)
7. done

Note: Bob has no trade_opened/trade_completed notification — acceptance is synchronous. Bob just proceeds to balance.

Expected final holdings:
- Alice: 5 bob-coins + 2 alice-coins = 7 bonds (gave 3 alice-bonds(10))
- Bob: 3 alice-coins + 3 alice-bonds(10) = 6 bonds (gave 2 alice-coins)

### Play 9: Forward Contract

Both agents mint their own bonds, then exchange them.

alice_p9:
1. connect(bob)
2. Wait connected(bob)
3. mint(3, 50)
4. Wait minted(3, 50)
5. `trade(bob, [lot(alice, 50, 3)], [lot(bob, 50, 3)])`
6. Wait trade_completed(bob)
7. balance → Wait balance_report(_)
8. done

bob_p9:
1. Wait befriend → decision(yes)
2. Wait connected
3. mint(3, 50)
4. Wait minted(3, 50)
5. Wait `trade_proposed(alice, _, ReqId)` → `accept_trade(alice, ReqId)`
6. balance → Wait balance_report(_)
7. done

Expected final holdings:
- Alice: 3 bob-bonds(50)
- Bob: 3 alice-bonds(50)

## Part 9: Boot (bond_boot.glp)

Add play8/fplay8 and play9/fplay9. Same wiring as play5, using p8/p9 actors.

## Testing

1. Run baseline: fplay1–fplay7, fplay4b
2. Run fplay8: verify trade_completed on Alice's side, balance shows correct distribution
3. Run fplay9: verify trade_completed on Alice's side, balance shows correct distribution
4. Update status report at `Grassroots-Bonds/docs/bonds-glp-status-report.md`

## Important Notes

- `trade_decline(BondList)` returns the offered bonds through the response variable — the inject reads them from the response, no separate copy needed
- In `handle_trade_fill(fail, ...)`, use `ReturnBonds = OfferedBonds?` to bridge from reader to writer for the trade_decline response
- In `reject_trade` agent clause, `bind_trade_decline(TradeResp, OfferedBonds?)` passes offered bonds directly
- `select_bonds_exact` uses `=?=` for both Issuer and Maturity (exact equality)
- On partial failure in `select_bonds_by_spec`, reconstruct holdings via `append(Selected, Remaining, Orig)`
- Responder gets no notification on success or rejection (both are synchronous actions)
- Responder gets `trade_failed` only when accept_trade fails due to insufficient holdings
