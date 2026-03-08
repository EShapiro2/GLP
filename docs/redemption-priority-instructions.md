# Claude Code Instructions: Implement Redemption Priority Rule

## Mandatory Reading

CLAUDE.md → DISCIPLINE.md → manual → cheat sheet.
Then read:
1. `programs/typed_book/bonds/agent.glp` — full bond agent
2. `programs/typed_book/bonds/mediator.glp` — UI mediator

## Overview

The redemption rule specifies what happens when q receives a trade that
returns one of q's coins (a bond issued by q). Currently the agent
auto-accepts with whatever the proposer asked for. The new rule:

**Redemption priority** (q receives trade offering a q-coin from p):
1. q holds a p-coin → accept, return the p-coin (OVERRIDE what p asked for)
2. q holds no p-coin, but holds what p asked for → accept, return it
3. q holds neither → reject, return p's offered bonds PLUS a menu of
   available bonds so p can make an informed next request

**Rationale for the menu**: The paper says "machine states are visible" but
the implementation has no visibility mechanism. The menu IS the
implementation of visibility — it tells the rejected proposer what's
available so they can choose intelligently next time.

**Menu format**: One bond per issuer, earliest maturity from each. Since
redemption is 1-for-1, no quantities needed — just representative bonds.

## Part 1: agent.glp — Extend TradeResponse type

Replace:
```glp
TradeResponse ::= trade_accept(BondList) ; trade_decline(BondList).
```

With:
```glp
TradeResponse ::= trade_accept(BondList)
                ; trade_decline(BondList)
                ; trade_decline_menu(BondList, BondList).
```

`trade_decline_menu(ReturnedBonds, Menu)` — returned bonds are the proposer's
original offered bonds; Menu is a list of available bonds (one per issuer,
earliest maturity).

## Part 2: agent.glp — New helper: find_coin_by_issuer

Find and remove ONE coin (maturity 0) issued by a given issuer from holdings.
Returns `found` with the coin and remaining holdings, or `not_found` with
holdings unchanged.

```glp
%% =============================================================================
%% FIND_COIN_BY_ISSUER — Find one coin issued by Issuer in holdings
%% =============================================================================
%%
%% Searches for a bond(Issuer, 0, _) — a coin (maturity 0) issued by Issuer.
%% Returns found/not_found status, the coin if found, and remaining holdings.

procedure find_coin_by_issuer(Constant?, BondList?, Constant, BondList, BondList).

%% Found a coin by Issuer — return it
find_coin_by_issuer(Issuer, [bond(I, 0, S)|Rest], found,
    [bond(I?, 0, S?)], Rest?) :-
    Issuer? =?= I? | true.

%% Not a match — keep looking
find_coin_by_issuer(Issuer, [B|Rest], Status?, Coin?, [B?|Rem?]) :-
    otherwise |
    find_coin_by_issuer(Issuer?, Rest?, Status, Coin, Rem).

%% Not found
find_coin_by_issuer(_, [], not_found, [], []).
```

NOTE: This finds maturity-0 coins. For full maturity checking (t ≤ t*),
the agent would need access to current time, which is a future enhancement.

## Part 3: agent.glp — New helper: build_menu

Build a menu of available bonds: one per issuer, earliest maturity from each.
Skip bonds issued by the agent itself (own bonds have no value).

```glp
%% =============================================================================
%% BUILD_MENU — One bond per issuer, earliest maturity
%% =============================================================================
%%
%% Walks holdings, collecting the earliest bond per issuer (excluding own).
%% Menu items show what's available for a rejected redeemer to choose from.

procedure build_menu(Constant?, BondList?, BondList).
build_menu(Id, Holdings, Menu?) :-
    build_menu_acc(Id?, Holdings?, [], Menu).

procedure build_menu_acc(Constant?, BondList?, BondList?, BondList).

%% Skip own bonds
build_menu_acc(Id, [bond(I, _, _)|Rest], Acc, Menu?) :-
    Id? =?= I? |
    build_menu_acc(Id?, Rest?, Acc?, Menu).

%% Foreign bond — check if issuer already in accumulator
build_menu_acc(Id, [bond(I, M, S)|Rest], Acc, Menu?) :-
    otherwise |
    menu_update(I?, M?, S?, Acc?, Acc1),
    build_menu_acc(Id?, Rest?, Acc1?, Menu).

%% Done
build_menu_acc(_, [], Acc, Acc?).

%% --- menu_update: insert or replace if earlier maturity ---

procedure menu_update(Constant?, Constant?, Constant?, BondList?, BondList).

%% Found same issuer — keep earlier maturity
menu_update(I, M, S, [bond(I2, M2, S2)|Rest],
    [bond(I2?, M?, S?)|Rest?]) :-
    I? =?= I2?, M? < M2? | true.

%% Found same issuer — existing is earlier or equal, keep it
menu_update(I, M, _, [bond(I2, M2, S2)|Rest],
    [bond(I2?, M2?, S2?)|Rest?]) :-
    I? =?= I2? | true.

%% Different issuer — keep looking
menu_update(I, M, S, [B|Rest], [B?|Rest1?]) :-
    otherwise |
    menu_update(I?, M?, S?, Rest?, Rest1).

%% Not found — add new entry
menu_update(I, M, S, [], [bond(I?, M?, S?)]).
```

## Part 4: agent.glp — Replace trade_dispatch(yes) and handle_trade_auto

Replace the current `trade_dispatch(yes, ...)` clause and the entire
`handle_trade_auto` procedure with the new redemption logic.

The new flow:
1. `trade_dispatch(yes, ...)` calls `find_coin_by_issuer(From?, Holdings?, ...)`
2. `redemption_dispatch` branches on found/not_found:
   - found → accept with the p-coin (override WantSpec)
   - not_found → try WantSpec via select_bonds_by_spec
3. `redemption_fill` branches on ok/fail:
   - ok → accept with selected bonds
   - fail → reject with menu

```glp
%% Redemption — find proposer's coin in our holdings
trade_dispatch(yes, Id, From, WantSpec, OfferedBonds, TradeResp?,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    find_coin_by_issuer(From?, Holdings?, CoinStatus, Coin, HoldingsAfterCoin),
    redemption_dispatch(CoinStatus?, Id?, From?, WantSpec?, OfferedBonds?, TradeResp,
        Coin?, HoldingsAfterCoin?,
        UserIn?, NetIn?, Outs?, NextSerial?).
```

```glp
procedure redemption_dispatch(Constant?, Constant?, Constant?, LotList?, BondList?,
    TradeResponse, BondList?, BondList?,
    UserInStream?, NetInStream?, OutputsList?, Constant?).

%% Priority 1: We hold a p-coin — return it, ignore WantSpec
redemption_dispatch(found, Id, From, _, OfferedBonds, trade_accept(Coin?),
    Coin, Remaining,
    UserIn, NetIn, Outs, NextSerial) :-
    append(Remaining?, OfferedBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_completed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).

%% Priority 2/3: No p-coin — try what proposer asked for
redemption_dispatch(not_found, Id, From, WantSpec, OfferedBonds, TradeResp?,
    _, Holdings,
    UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(WantSpec?, Holdings?, Status, Selected, Remaining),
    redemption_fill(Status?, Id?, From?, TradeResp, OfferedBonds?,
        Selected?, Remaining?,
        UserIn?, NetIn?, Outs?, NextSerial?).
```

```glp
procedure redemption_fill(Constant?, Constant?, Constant?, TradeResponse, BondList?,
    BondList?, BondList?,
    UserInStream?, NetInStream?, OutputsList?, Constant?).

%% Priority 2: Can fill — accept with requested bonds
redemption_fill(ok, Id, From, trade_accept(Selected?), OfferedBonds,
    Selected, Remaining,
    UserIn, NetIn, Outs, NextSerial) :-
    append(Remaining?, OfferedBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_completed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).

%% Priority 3: Can't fill — reject with menu
redemption_fill(fail, Id, From, trade_decline_menu(OfferedBonds?, Menu?), OfferedBonds,
    Selected, Remaining,
    UserIn, NetIn, Outs, NextSerial) :-
    append(Selected?, Remaining?, OrigHoldings),
    build_menu(Id?, OrigHoldings?, Menu),
    lookup_send('_user', msg(agent, '_user', trade_failed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

Delete the old `handle_trade_auto` procedure entirely (both ok and fail clauses).

## Part 5: agent.glp — Extend inject_trade_result

Add a clause for trade_decline_menu. The proposer gets back their bonds
plus the menu of what's available.

Add BEFORE the passthrough clause:
```glp
inject_trade_result(trade_decline_menu(OurBonds, Menu), From, Ys,
    [trade_returned_bonds_menu(From?, OurBonds?, Menu?)|Ys?]) :-
    ground(From?), ground(OurBonds?), ground(Menu?) | true.
```

Add `trade_returned_bonds_menu` to UserInMsg:
```glp
; trade_returned_bonds_menu(Constant, BondList, BondList)
```

## Part 6: agent.glp — Handle trade_returned_bonds_menu in agent/6

Add a new agent clause to handle the menu rejection. The proposer gets
their bonds back and a notification containing the menu.

```glp
%% Proposer: trade returned with menu (redemption rejected — menu of available bonds)
agent(Id, [trade_returned_bonds_menu(From, OurBonds, Menu)|UserIn], NetIn, Outs, Holdings, NextSerial) :-
    ground(Id?), ground(From?), ground(OurBonds?), ground(Menu?) |
    append(Holdings?, OurBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_returned_menu(From?, Menu?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

Add `trade_returned_menu(Constant, BondList)` to AgentContent and OutputContent.

## Part 7: mediator.glp — Add trade_returned_menu pass-through

Add agent-to-user pass-through for trade_returned_menu:
```glp
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', trade_returned_menu(From, Menu)),
            AgentCh?, AgentCh1),
    ground(From?), ground(Menu?) |
    send_user(trade_returned_menu(From?, Menu?), UserCh?, UserCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

Add `trade_returned_menu(Constant, BondList)` to mediator's AgentContent and
UserNotify types.

## Part 8: actors.glp and play12/self.glp — Add new types

Add `trade_returned_menu(Constant, BondList)` to UserNotify in actors.glp
and play12/self.glp. Also add `trade_decline_menu(BondList, BondList)` to
TradeResponse in actors.glp and play12/self.glp if those types are present.

No actor behavior changes needed — existing plays don't exercise the
rejected-redemption path.

## Part 9: Test

Run all 12 plays:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
for P in fplay1 fplay2 fplay3 fplay4 fplay4b fplay5 fplay6 fplay8 fplay9 fplay10 fplay11 fplay12; do
  echo -e "../programs/typed_book/bonds/agent.glp\n../programs/typed_book/bonds/mediator.glp\n../programs/typed_book/bonds/actors.glp\n../programs/typed_book/bonds/boot.glp\n../programs/typed_book/bonds/play12/self.glp\n../programs/typed_book/bonds/play12/alice.glp\n../programs/typed_book/bonds/play12/bob.glp\n../programs/typed_book/bonds/play12/charlie.glp\n../programs/typed_book/bonds/play12/diana.glp\n../programs/typed_book/bonds/play12/eve.glp\n../programs/typed_book/bonds/play12/frank.glp\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/redeem-${P}.txt 2>&1
  echo "${P}: done"
done
```

All plays should still pass — the new code only changes auto-accept behavior,
and existing plays use the priority-1 path (proposer's coin returned) or
the priority-2 path (WantSpec fulfilled). No existing play triggers priority-3
(the menu path).

## Bug Protocol

Same as always. STOP on errors, show full output, do NOT fix without discussion.
