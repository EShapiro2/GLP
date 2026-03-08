# Claude Code Instructions: Unify Pay/Redeem into Trade

## Mandatory Reading

CLAUDE.md → DISCIPLINE.md → manual → cheat sheet.
Then read:
1. `programs/typed_book/bonds/agent.glp` — full bond agent
2. `programs/typed_book/bonds/mediator.glp` — UI mediator
3. `programs/typed_book/bonds/actors.glp` — plays 1–11 actors
4. `programs/typed_book/bonds/play12/frank.glp` — uses redeem(diana, 5, 0)

## Overview

The paper spec says: Credit, Payment, and Redemption are all sub-cases of Swap.
The implementation currently has separate `pay` and `redeem` commands with their
own machinery. These must be eliminated. Trade handles everything.

**Key insight**: When a trade offer arrives where the offered bonds include a
bond issued by me, I cannot refuse — this is a valid redemption. When a trade
arrives with empty WantSpec (nothing requested back), it's a payment/gift —
also auto-accept.

## Part 1: agent.glp — Remove redeem machinery

Delete these procedures entirely (including all their clauses):

- `select_coins` (~10 lines)
- `do_pay` + `do_pay_result` (~15 lines)
- `select_bonds_by_issuer` (~10 lines)
- `select_bonds_min_maturity` + `select_bonds_min_maturity_strict` (~20 lines)
- `select_any_bonds` (~5 lines)
- `handle_redeem_fill` (~15 lines)
- `bind_redeem` (~3 lines)
- `inject_redeem_result` (~10 lines)
- `do_redeem` + `do_redeem_result` (~15 lines)

Delete these agent/6 clauses (identified by the comment or the message pattern):

- `agent(Id, [msg('_user', Id1, pay(Target, K))|UserIn], ...)` — "User requests payment"
- `agent(Id, UserIn, [msg(From, Id1, payment(Bonds))|NetIn], ...)` — "Incoming payment from friend"
- `agent(Id, [msg('_user', Id1, redeem(Target, K, MaxMaturity))|UserIn], ...)` — "User requests redemption"
- `agent(Id, [redeem_complete(From, K, ReceivedBonds)|UserIn], ...)` — "Injected redeem_complete"
- `agent(Id, UserIn, [msg(From, Id1, redeem_request(K, MaxMaturity, ReturnedBonds, RedeemResp?))|NetIn], ...)` — "Incoming redeem_request from friend"

Remove from types in agent.glp:
- `RedeemResponse` type definition
- `redeem_request(...)` from `FriendContent`
- `payment(BondList)` from `FriendContent`
- `pay(Constant, Constant)` from `UserContent`
- `redeem(Constant, Constant, Constant)` from `UserContent`
- `redeemed(Constant, Constant)` from `AgentContent`
- `redeem_failed(Constant, Constant)` from `AgentContent`
- `redeem_received(Constant, Constant)` from `AgentContent`
- `paid(Constant, Constant)` from `AgentContent`
- `payment_received(Constant, Constant)` from `AgentContent`
- `payment_failed(Constant, Constant)` from `AgentContent`
- `payment(BondList)` from `OutputContent`
- `paid(...)`, `payment_received(...)`, `payment_failed(...)` from `OutputContent`
- `redeemed(...)`, `redeem_failed(...)`, `redeem_received(...)` from `OutputContent`
- `redeem_request(...)` from `OutputContent`
- `redeem_complete(...)` from `UserInMsg`
- `credit_result` and `credit_was_rejected` can stay (credit removal is a separate task)

## Part 2: agent.glp — Add auto-accept to trade handler

Currently the incoming trade_propose handler unconditionally sends to user:

```glp
agent(Id, UserIn, [msg(From, Id1, trade_propose(WantSpec, OfferedBonds, TradeResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    lookup_send('_user',
        msg(agent, '_user', trade_proposed(From?, WantSpec?, TradeResp, OfferedBonds?)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

Replace with a call to a new helper that checks for auto-accept:

```glp
agent(Id, UserIn, [msg(From, Id1, trade_propose(WantSpec, OfferedBonds, TradeResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    trade_auto_accept_check(Id?, From?, WantSpec?, OfferedBonds?, TradeResp,
        Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).
```

New helper — walks the offered bonds looking for one issued by me:

```glp
procedure trade_auto_accept_check(Constant?, Constant?, LotList?, BondList?, TradeResponse,
    BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).

%% Found a bond I issued → auto-accept: select bonds matching WantSpec
trade_auto_accept_check(Id, From, WantSpec, [bond(I, _, _)|_], TradeResp,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    Id? =?= I? |
    select_bonds_by_spec(WantSpec?, Holdings?, Status, Selected, Remaining),
    handle_trade_auto(Status?, Id?, From?, TradeResp, ???, Selected?, Remaining?,
        UserIn?, NetIn?, Outs?, NextSerial?).

%% Not my bond — keep looking
trade_auto_accept_check(Id, From, WantSpec, [_|Rest], TradeResp,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    otherwise |
    trade_auto_accept_check(Id?, From?, WantSpec?, Rest?, TradeResp,
        Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).

%% No own bonds found → present to user for decision (existing behavior)
trade_auto_accept_check(Id, From, WantSpec, [], TradeResp,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    lookup_send('_user',
        msg(agent, '_user', trade_proposed(From?, WantSpec?, TradeResp, ???)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

**IMPORTANT**: The ??? above marks places where the OfferedBonds are needed
but have been consumed by the list walk. You need to keep the original
OfferedBonds available. Two approaches:
(a) Pass the original OfferedBonds as an extra parameter throughout
(b) Restructure: do the check as a separate predicate that returns yes/no,
    then branch in the agent clause

Approach (b) is cleaner. Define a guard-like check:

```glp
procedure contains_own_bond(Constant?, BondList?).
contains_own_bond(Id, [bond(I, _, _)|_]) :- Id? =?= I? | true.
contains_own_bond(Id, [_|Rest]) :- contains_own_bond(Id?, Rest?).
```

BUT this is a multi-clause procedure, so it can't be used as a guard.
Instead, use it as a body call that produces a result:

```glp
procedure check_own_bond(Constant?, BondList?, Constant).
check_own_bond(Id, [bond(I, _, _)|_], yes) :- Id? =?= I? | true.
check_own_bond(Id, [_|Rest], Result?) :- otherwise | check_own_bond(Id?, Rest?, Result).
check_own_bond(_, [], no).
```

Then the agent clause calls it:

```glp
agent(Id, UserIn, [msg(From, Id1, trade_propose(WantSpec, OfferedBonds, TradeResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(WantSpec?), ground(OfferedBonds?) |
    check_own_bond(Id?, OfferedBonds?, IsRedemption),
    trade_dispatch(IsRedemption?, Id?, From?, WantSpec?, OfferedBonds?, TradeResp,
        Holdings?, UserIn?, NetIn?, Outs?, NextSerial?).
```

```glp
procedure trade_dispatch(Constant?, Constant?, Constant?, LotList?, BondList?, TradeResponse,
    BondList?, UserInStream?, NetInStream?, OutputsList?, Constant?).

%% Redemption/payment — auto-accept
trade_dispatch(yes, Id, From, WantSpec, OfferedBonds, TradeResp,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    select_bonds_by_spec(WantSpec?, Holdings?, Status, Selected, Remaining),
    handle_trade_fill(Status?, Id?, From?, TradeResp, OfferedBonds?, Selected?, Remaining?,
        UserIn?, NetIn?, Outs?, NextSerial?).

%% Normal trade — present to user
trade_dispatch(no, Id, From, WantSpec, OfferedBonds, TradeResp,
    Holdings, UserIn, NetIn, Outs, NextSerial) :-
    lookup_send('_user',
        msg(agent, '_user', trade_proposed(From?, WantSpec?, TradeResp, OfferedBonds?)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

Note: `handle_trade_fill` already exists and handles both ok/fail cases.
For auto-accept ok: binds trade_accept(Selected), takes offered bonds.
For auto-accept fail: binds trade_decline(OfferedBonds), own holdings reconstructed.

The only issue: when auto-accept succeeds, the responder gets no notification.
Add a notification to the ok branch of handle_trade_fill, or add a new
handle_trade_auto that sends a notification. For now, reuse handle_trade_fill
as-is — the proposer will get trade_completed, which is the important one.

## Part 3: mediator.glp — Remove pay/redeem clauses

Delete these mediator clauses:
- `receive(pay(Target, K), ...)` — user-to-agent pay pass-through
- `receive(msg(agent, '_user', paid(Target, K)), ...)` — agent-to-user paid
- `receive(msg(agent, '_user', payment_received(From, K)), ...)` — agent-to-user payment_received
- `receive(msg(agent, '_user', payment_failed(Target, K)), ...)` — agent-to-user payment_failed
- `receive(redeem(Target, K, MaxMaturity), ...)` — user-to-agent redeem
- `receive(msg(agent, '_user', redeemed(Target, K)), ...)` — agent-to-user redeemed
- `receive(msg(agent, '_user', redeem_failed(Target, K)), ...)` — agent-to-user redeem_failed
- `receive(msg(agent, '_user', redeem_received(From, K)), ...)` — agent-to-user redeem_received

Remove from mediator types:
- `RedeemResponse` type
- `redeem_request(...)` from `FriendContent`
- `payment(BondList)` from `FriendContent`
- `pay(...)` from `UserContent` and `UserCmd`
- `redeem(...)` from `UserContent` and `UserCmd`
- `paid(...)`, `payment_received(...)`, `payment_failed(...)` from `AgentContent` and `UserNotify`
- `redeemed(...)`, `redeem_failed(...)`, `redeem_received(...)` from `AgentContent` and `UserNotify`

## Part 4: actors.glp — Convert existing plays

Search actors.glp for uses of `pay`, `redeem`, `credit`. For each:

**pay(Target, K)** → Replace with `trade(Target, [lot(Target, 0, K)], [])`.
The notification changes: instead of waiting for `paid(Target, K)`, wait
for `trade_completed(Target)`.

**redeem(Target, K, MaxMaturity)** → Replace with
`trade(Target, [lot(Target, MaxMaturity, K)], [lot(???, ???, K)])`.
The holder must specify what they want. Since the current plays use simple
redemptions, use reasonable defaults. Check each play's context.
Instead of waiting for `redeemed(Target, K)`, wait for `trade_completed(Target)`.

NOTE: The recipient notifications also change:
- Instead of `payment_received(From, K)`, the recipient now sees no notification
  (auto-accept happens silently). If the play actors wait for payment_received,
  they need to be adjusted. Check each play carefully.

IMPORTANT: This is the trickiest part. Read each play's actor sequences
carefully and trace through the new flow before making changes.

## Part 5: play12 actors — Convert frank's redeem

In `play12/frank.glp`, replace:
```glp
[redeem(diana, 5, 0)|Out?]
```
with a trade. Frank wants to give Diana back 5 diana-coins and get 5 of
whatever Diana holds. Since Frank wants frank-coins back (most useful to him):
```glp
[trade(diana, [lot(diana, 0, 5)], [lot(frank, 0, 5)])|Out?]
```

Also replace `wait_redeemed` state: instead of waiting for `redeemed(diana, 5)`,
wait for `trade_completed(diana)`.

Similarly, update all play12 actors that use `pay`:
- Bob: `pay(alice, 5)` → `trade(alice, [lot(alice, 0, 5)], [])`
  Wait for `trade_completed(alice)` instead of `paid(alice, 5)`
- Eve: `pay(charlie, 6)` → `trade(charlie, [lot(charlie, 0, 6)], [])`
  And `pay(alice, 3)` → `trade(alice, [lot(alice, 0, 3)], [])`
- Frank: `pay(diana, 3)` → `trade(diana, [lot(diana, 0, 3)], [])`
- Alice: waits for `payment_received(bob, 5)` and `payment_received(eve, 3)`.
  These notifications no longer exist. Alice instead sees `trade_proposed`
  for an auto-accepted trade. Since auto-accept handles it silently,
  Alice sees `trade_completed` if she's the proposer, but she's the RECEIVER.
  
**PROBLEM**: In auto-accept, the responder (recipient of payment) gets no
notification. But actors like Alice wait for `payment_received`. We need
auto-accept to also notify the responder.

**SOLUTION**: Add notification to the auto-accept path. When trade_dispatch(yes)
succeeds (handle_trade_fill returns ok), also send a `trade_completed(From)`
notification to the user. This requires modifying handle_trade_fill or adding
a handle_trade_auto wrapper.

Concretely, add a new procedure that wraps handle_trade_fill for auto-accept:

```glp
procedure handle_trade_auto(Constant?, Constant?, Constant?, TradeResponse, BondList?, BondList?, BondList?,
    UserInStream?, NetInStream?, OutputsList?, Constant?).

%% Can fill: bind accept, take offered bonds, notify user
handle_trade_auto(ok, Id, From, trade_accept(Selected?), OfferedBonds, Selected, Remaining,
    UserIn, NetIn, Outs, NextSerial) :-
    append(Remaining?, OfferedBonds?, NewHoldings),
    lookup_send('_user', msg(agent, '_user', trade_completed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NextSerial?).

%% Can't fill: bind decline, return offered bonds, notify user
handle_trade_auto(fail, Id, From, trade_decline(OfferedBonds?), OfferedBonds, Selected, Remaining,
    UserIn, NetIn, Outs, NextSerial) :-
    append(Selected?, Remaining?, OrigHoldings),
    lookup_send('_user', msg(agent, '_user', trade_failed(From?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, OrigHoldings?, NextSerial?).
```

Then trade_dispatch(yes) calls handle_trade_auto instead of handle_trade_fill.

With this, the responder gets `trade_completed(From)` on auto-accept success.
Alice's actor waits for `trade_completed(bob)` (was `payment_received(bob, 5)`).

## Part 6: play12/self.glp — Update types

Remove `pay` and `redeem` from `UserCmd`.
Remove `paid`, `payment_received`, `payment_failed` from `UserNotify`.
Remove `redeemed`, `redeem_failed`, `redeem_received` from `UserNotify`.

## Part 7: Test

Test all plays 1–12:
```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
for P in fplay1 fplay2 fplay3 fplay4 fplay4b fplay5 fplay6 fplay8 fplay9 fplay10 fplay11 fplay12; do
  echo -e "../programs/typed_book/bonds/agent.glp\n../programs/typed_book/bonds/mediator.glp\n../programs/typed_book/bonds/actors.glp\n../programs/typed_book/bonds/boot.glp\n../programs/typed_book/bonds/play12/self.glp\n../programs/typed_book/bonds/play12/alice.glp\n../programs/typed_book/bonds/play12/bob.glp\n../programs/typed_book/bonds/play12/charlie.glp\n../programs/typed_book/bonds/play12/diana.glp\n../programs/typed_book/bonds/play12/eve.glp\n../programs/typed_book/bonds/play12/frank.glp\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/unify-${P}.txt 2>&1
  echo "${P}: done"
done
```

Note: plays 3 (payment), 4/4b (redemption) are the most affected.
Plays 1, 2 (mint, credit) should be unaffected.
Play 12 needs the actor changes from Part 5.

## Part 8: Narrative text updates

After the functional changes work, update narrative text in play12 actors
to reflect the new command names. E.g., Frank's narrative should still say
"Redeemed 5 diana-coins from Diana" but the command is now a trade.

## Bug Protocol

Same as always. STOP on errors, show full output, do NOT fix without discussion.
