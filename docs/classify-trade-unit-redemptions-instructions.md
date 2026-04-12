# Claude Code Instructions: Classify Trade + Unit Redemptions

## Mandatory Reading

CLAUDE.md → DISCIPLINE.md → manual → cheat sheet.
Then read:
1. `programs/typed_book/bonds/agent.glp`
2. `programs/typed_book/bonds/actors.glp`
3. `programs/typed_book/bonds/play12/frank.glp`

## Overview

Three changes:
1. Replace `is_redemption` with `classify_trade` that distinguishes payment,
   redemption, and normal trade
2. Update `trade_dispatch` to handle all three
3. Decompose multi-coin redemptions in actors into unit (1-for-1) trades

## Part 1: agent.glp — Replace is_redemption with classify_trade

Replace the `is_redemption` procedure:

```glp
procedure is_redemption(Constant?, BondList?, Constant).
is_redemption(Id, [bond(I, 0, _)], yes) :- Id? =?= I? | true.
is_redemption(_, _, no) :- otherwise | true.
```

With:

```glp
%% =============================================================================
%% CLASSIFY_TRADE — Classify incoming trade for auto-accept
%% =============================================================================
%%
%% Three classifications:
%%   payment    — WantSpec is empty (proposer gives coins, wants nothing back).
%%                Auto-accept: nobody refuses free money.
%%   redemption — OfferedBonds is exactly one coin issued by me.
%%                Auto-accept with redemption priority rule.
%%   normal     — everything else. Present to user for decision.

procedure classify_trade(Constant?, LotList?, BondList?, Constant).

%% Payment: empty WantSpec
classify_trade(_, [], _, payment).

%% Redemption: exactly one of my coins
classify_trade(Id, _, [bond(I, 0, _)], redemption) :- Id? =?= I? | true.

%% Normal: everything else
classify_trade(_, _, _, normal) :- otherwise | true.
```

## Part 2: agent.glp — Update trade_dispatch

Replace the incoming trade_propose agent clause. Change:

```glp
    is_redemption(Id?, OfferedBonds?, IsRedemption),
    trade_dispatch(IsRedemption?, Id?, From?, WantSpec?, OfferedBonds?, TradeResp,
```

To:

```glp
    classify_trade(Id?, WantSpec?, OfferedBonds?, TradeClass),
    trade_dispatch(TradeClass?, Id?, From?, WantSpec?, OfferedBonds?, TradeResp,
```

Add a new `trade_dispatch` clause for payment, BEFORE the existing clauses:

```glp
%% Payment — auto-accept (proposer gives coins, wants nothing)
trade_dispatch(payment, Id, From, _, OfferedBonds, trade_accept([]),
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    append(Holdings?, OfferedBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_completed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).
```

Rename existing `trade_dispatch(yes, ...)` to `trade_dispatch(redemption, ...)`.
Rename existing `trade_dispatch(no, ...)` to `trade_dispatch(normal, ...)`.

## Part 3: actors.glp — Decompose multi-coin redemptions

### Play 3: Payment — NO CHANGE needed
`trade(bob, [lot(bob, 0, 2)], [])` is a payment (empty WantSpec). Auto-accepted
by the new payment clause. Keep as-is.

### Play 4: Alice redeems 2 bob-coins

Currently:
```
trade(bob, [lot(bob, 0, 2)], [lot(alice, 0, 2)]) → wait trade_completed(bob)
```

Replace with two unit redemptions:
```
trade(bob, [lot(bob, 0, 1)], [lot(alice, 0, 1)]) → wait trade_completed(bob)
trade(bob, [lot(bob, 0, 1)], [lot(alice, 0, 1)]) → wait trade_completed(bob)
```

This means alice_p4_wait_trade1 sends the first unit trade, waits for
trade_completed, then alice_p4_wait_trade2 sends the second unit trade,
waits for trade_completed, then proceeds to balance.

Bob needs to wait for TWO auto-accepted trade_completeds instead of one.

### Play 4b: Alice redeems 2 bob-coins twice

Step 6: `trade(bob, [lot(bob, 0, 2)], [lot(alice, T, 2)])` → decompose into
two unit trades: `trade(bob, [lot(bob, 0, 1)], [lot(alice, T, 1)])` x2.

Step 8: `trade(bob, [lot(bob, 0, 2)], [lot(alice, 0, 2)])` → decompose into
two unit trades: `trade(bob, [lot(bob, 0, 1)], [lot(alice, 0, 1)])` x2.

Bob currently waits for 3 auto trade_completeds. With decomposition, Bob waits
for 5 (1 from accept + 2 from pre-maturity + 2 from post-maturity).

UPDATE the play4b comments to reflect the new step counts.

### Play 12, Frank: Redeems 5 diana-coins

Currently:
```
trade(diana, [lot(diana, 0, 5)], [lot(frank, 28, 5)])
```

Replace with 5 sequential unit trades:
```
trade(diana, [lot(diana, 0, 1)], [lot(frank, 28, 1)]) → wait trade_completed(diana)
trade(diana, [lot(diana, 0, 1)], [lot(frank, 28, 1)]) → wait trade_completed(diana)
trade(diana, [lot(diana, 0, 1)], [lot(frank, 28, 1)]) → wait trade_completed(diana)
trade(diana, [lot(diana, 0, 1)], [lot(frank, 28, 1)]) → wait trade_completed(diana)
trade(diana, [lot(diana, 0, 1)], [lot(frank, 28, 1)]) → wait trade_completed(diana)
```

This replaces frank_p12_wait_escrow_released through frank_p12_wait_trade_redeem
with 5 sequential trade/wait_completed state transitions.

Diana waits for trade_completed(frank) for payments and redemptions.
Currently Diana sees one trade_completed(frank) for the 3-coin payment and
one trade_completed(frank) for the 5-coin redemption.
Under the new rules:
- 3-coin payment: still one trade_completed (payment auto-accept handles any count)
- 5-coin redemption: now 5 trade_completeds

Diana's actor in play12/diana.glp currently waits for one trade_completed(frank)
for the redemption. She needs to wait for 5 instead.

Also update play12/self.glp if needed.

## Part 4: play12/diana.glp — Wait for 5 redemption trade_completeds

Diana currently has (after the payment trade_completed from frank):
```
frank_p12_wait_trade_redeem → trade_completed(frank) → balance
```

Replace with 5 sequential waits:
```
diana_p12_wait_redeem1 → trade_completed(frank) →
diana_p12_wait_redeem2 → trade_completed(frank) →
diana_p12_wait_redeem3 → trade_completed(frank) →
diana_p12_wait_redeem4 → trade_completed(frank) →
diana_p12_wait_redeem5 → trade_completed(frank) → balance
```

Read diana.glp first to find the exact procedure names and flow.

## Part 5: Test

Run all 12 fplay tests. All should pass.

## Bug Protocol

STOP on errors, show full output, do NOT fix without discussion.
