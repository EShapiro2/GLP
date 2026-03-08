# Claude Code Instructions: Unify Credit/Loan into Trade

## Mandatory Reading

CLAUDE.md → DISCIPLINE.md → manual → cheat sheet.
Then read:
1. `programs/typed_book/bonds/agent.glp` — full bond agent
2. `programs/typed_book/bonds/mediator.glp` — UI mediator
3. `programs/typed_book/bonds/actors.glp` — plays 1–11 actors

## Overview

Credit and loan are both patterns of swap (trade). The separate credit/loan
commands and their machinery are being eliminated. Actors mint first, then trade.

This is the same unification done earlier for pay/redeem, now extended to
credit and loan.

## Part 1: agent.glp — Remove credit machinery

Delete these procedures:
- `inject_credit_result` (~8 lines)
- `bind_credit_accept` (~5 lines)
- `bind_credit_reject` (~2 lines)

Delete these agent/6 clauses (6 clauses total):
- `agent(Id, [msg('_user', Id1, credit(Target, K, Maturity))|UserIn], ...)` — "User requests credit line"
- `agent(Id, [credit_result(From, K, TheirBonds)|UserIn], ...)` — "Credit result: other accepted"
- `agent(Id, [credit_was_rejected(From)|UserIn], ...)` — "Credit result: other rejected"
- `agent(Id, UserIn, [msg(From, Id1, credit_propose(K, Maturity, TheirBonds, CreditResp?))|NetIn], ...)` — "Incoming credit_propose"
- `agent(Id, [msg('_user', Id1, accept_credit(...))|UserIn], ...)` — "User accepts credit"
- `agent(Id, [msg('_user', Id1, reject_credit(...))|UserIn], ...)` — "User rejects credit"

Remove from types:
- `CreditResponse` type definition entirely
- `credit_propose(...)` from `FriendContent`
- `credit(...)`, `accept_credit(...)`, `reject_credit(...)` from `UserContent`
- `credit_proposed(...)`, `credit_opened(...)`, `credit_rejected(...)` from `AgentContent`
- `credit_pending(...)` from `PendingValue`
- `credit_result(...)`, `credit_was_rejected(...)` from `UserInMsg`
- `credit_propose(...)`, `credit_proposed(...)`, `credit_opened(...)`, `credit_rejected(...)` from `OutputContent`

## Part 2: agent.glp — Remove loan machinery

Delete these procedures:
- `inject_loan_result` (~10 lines)
- `bind_loan_accept` (~5 lines)
- `bind_loan_reject` (~2 lines)
- `schedule_total` (~3 lines)
- `create_bonds_from_schedule` (~8 lines)

Delete these agent/6 clauses (6 clauses total):
- `agent(Id, [msg('_user', Id1, loan(Target, Principal, Schedule))|UserIn], ...)` — "User requests loan"
- `agent(Id, [loan_result(From, Principal, FaceValue, BorrowerBonds)|UserIn], ...)` — "Injected loan_result"
- `agent(Id, [loan_was_rejected(From)|UserIn], ...)` — "Injected loan_was_rejected"
- `agent(Id, UserIn, [msg(From, Id1, loan_propose(Principal, Schedule, LenderCoins, LoanResp?))|NetIn], ...)` — "Incoming loan_propose"
- `agent(Id, [msg('_user', Id1, accept_loan(...))|UserIn], ...)` — "User accepts loan"
- `agent(Id, [msg('_user', Id1, reject_loan(...))|UserIn], ...)` — "User rejects loan"

Remove from types:
- `LoanResponse` type definition entirely
- `Installment`, `Schedule` type definitions entirely
- `loan_propose(...)` from `FriendContent`
- `loan(...)`, `accept_loan(...)`, `reject_loan(...)` from `UserContent`
- `loan_proposed(...)`, `loan_opened(...)`, `loan_rejected(...)` from `AgentContent`
- `loan_pending(...)` from `PendingValue`
- `loan_result(...)`, `loan_was_rejected(...)` from `UserInMsg`
- `loan_propose(...)`, `loan_proposed(...)`, `loan_opened(...)`, `loan_rejected(...)` from `OutputContent`

## Part 3: mediator.glp — Remove credit/loan clauses and types

Delete ALL mediator clauses that handle credit or loan. These are identified
by matching on `credit`, `accept_credit`, `reject_credit`, `credit_proposed`,
`credit_opened`, `credit_rejected`, `loan`, `accept_loan`, `reject_loan`,
`loan_proposed`, `loan_opened`, `loan_rejected`.

That's approximately 12 mediator clauses.

Remove corresponding entries from mediator's type definitions:
- `CreditResponse`, `LoanResponse`, `Installment`, `Schedule` types
- `credit_propose(...)` from `FriendContent`
- `loan_propose(...)` from `FriendContent`
- All credit/loan entries from `AgentContent`, `UserContent`, `UserCmd`, `UserNotify`, `PendingValue`

## Part 4: actors.glp — Convert plays 2–6, 8, 10, 11

Remove `credit(...)`, `accept_credit(...)`, `reject_credit(...)`, `loan(...)`,
`accept_loan(...)`, `reject_loan(...)` from actors' UserCmd type.
Remove `credit_proposed(...)`, `credit_opened(...)`, `credit_rejected(...)`,
`loan_proposed(...)`, `loan_opened(...)`, `loan_rejected(...)` from UserNotify.
Remove `Installment`, `Schedule` types.

### Play 2 (befriend + credit → befriend + mint + trade)

**alice_p2**: `connect(bob)` → wait `connected(bob)` → `mint(3, 0)` → wait
`minted(3, 0)` → `trade(bob, [lot(alice, 0, 3)], [lot(bob, 0, 3)])` → wait
`trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p2**: wait `befriend(alice, ReqId)` → `decision(yes, alice, ReqId)` →
wait `connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` → `mint(3, 0)` →
wait `minted(3, 0)` → `accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
`balance` → wait `balance_report(_)` → `done`

### Play 3 (credit + payment → mint + trade + trade)

Same as play 2 for the credit part, then the payment trade follows.

**alice_p3**: `connect(bob)` → wait `connected(bob)` → `mint(3, 0)` → wait
`minted(3, 0)` → `trade(bob, [lot(alice, 0, 3)], [lot(bob, 0, 3)])` → wait
`trade_completed(bob)` → `trade(bob, [lot(bob, 0, 2)], [])` → wait
`trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p3**: wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait
`connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` → `mint(3, 0)` →
wait `minted(3, 0)` → `accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
wait `trade_completed(alice)` (payment auto-accepted) → `balance` → wait
`balance_report(_)` → `done`

### Play 4 (credit + redemption → mint + trade + trade)

**alice_p4**: `connect(bob)` → wait `connected(bob)` → `mint(3, 0)` → wait
`minted(3, 0)` → `trade(bob, [lot(alice, 0, 3)], [lot(bob, 0, 3)])` → wait
`trade_completed(bob)` → `trade(bob, [lot(bob, 0, 2)], [lot(alice, 0, 2)])` →
wait `trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p4**: Same as bob_p3 but second trade auto-accepted is the redemption trade.

### Play 4b (time-dependent: coin credit + bond credit → two mint+trade pairs)

**alice_p4b(T)**:
`connect(bob)` → wait `connected(bob)` →
`mint(5, 0)` → wait `minted(5, 0)` →
`trade(bob, [lot(alice, 0, 5)], [lot(bob, 0, 5)])` → wait `trade_completed(bob)` →
`mint(5, T)` → wait `minted(5, T)` →
`trade(bob, [lot(alice, T, 5)], [lot(bob, T, 5)])` → wait `trade_completed(bob)` →
`trade(bob, [lot(bob, 0, 2)], [lot(alice, T, 2)])` → wait `trade_completed(bob)` →
`balance` → wait `balance_report(_)` → wait(500) →
`trade(bob, [lot(bob, 0, 2)], [lot(alice, 0, 2)])` → wait `trade_completed(bob)` →
`balance` → wait `balance_report(_)` → `done`

**bob_p4b**: Needs to handle TWO trade_proposed sequences (each requiring mint then accept), then two auto-accepted trades.
Wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait `connected(alice)` →
wait `trade_proposed(alice, _, ReqId2)` → `mint(5, 0)` → wait `minted(5, 0)` →
`accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
wait `trade_proposed(alice, _, ReqId3)` → `mint(5, T)` → wait `minted(5, T)` →
`accept_trade(alice, ReqId3)` →
wait `trade_completed(alice)` (first auto) →
wait `trade_completed(alice)` (second auto) →
`balance` → wait `balance_report(_)` → `done`

NOTE: bob_p4b needs T as parameter since he mints `mint(5, T)`. Change
signature to `bob_p4b(ActorChannel?, Constant?)` and pass T from boot.

### Play 5 (zero-coupon loan → mint + trade)

**alice_p5**: `connect(bob)` → wait `connected(bob)` → `mint(9, 0)` → wait
`minted(9, 0)` → `trade(bob, [lot(alice, 0, 9)], [lot(bob, 5, 10)])` → wait
`trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p5**: wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait
`connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` → `mint(10, 5)` →
wait `minted(10, 5)` → `accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
`balance` → wait `balance_report(_)` → `done`

### Play 6 (balloon loan → mint + trade)

**alice_p6**: `connect(bob)` → wait `connected(bob)` → `mint(10, 0)` → wait
`minted(10, 0)` → `trade(bob, [lot(alice, 0, 10)], [lot(bob, 100, 1), lot(bob, 200, 1), lot(bob, 300, 1), lot(bob, 300, 10)])` →
wait `trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p6**: wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait
`connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` →
`mint(1, 100)` → wait `minted(1, 100)` →
`mint(1, 200)` → wait `minted(1, 200)` →
`mint(1, 300)` → wait `minted(1, 300)` →
`mint(10, 300)` → wait `minted(10, 300)` →
`accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
`balance` → wait `balance_report(_)` → `done`

### Play 8 (credit + sale of debt → mint + trade + trade)

**alice_p8**: `connect(bob)` → wait `connected(bob)` → `mint(5, 0)` → wait
`minted(5, 0)` → `trade(bob, [lot(alice, 0, 5)], [lot(bob, 0, 5)])` → wait
`trade_completed(bob)` → `mint(3, 10)` → wait `minted(3, 10)` →
`trade(bob, [lot(alice, 10, 3)], [lot(alice, 0, 2)])` → wait
`trade_completed(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p8**: wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait
`connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` → `mint(5, 0)` →
wait `minted(5, 0)` → `accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
wait `trade_proposed(alice, _, ReqId3)` → `accept_trade(alice, ReqId3)` →
`balance` → wait `balance_report(_)` → `done`

### Play 10 (credit + escrow time-release → mint + trade + escrow)

**alice_p10(T)**: `connect(bob)` → wait `connected(bob)` → `mint(5, 0)` →
wait `minted(5, 0)` → `trade(bob, [lot(alice, 0, 5)], [lot(bob, 0, 5)])` →
wait `trade_completed(bob)` → `deposit_escrow(bob, [lot(bob, 0, 3)], T)` →
wait `escrow_deposited(bob, _, _)` → wait `escrow_expired(bob)` → `balance` →
wait `balance_report(_)` → `done`

**bob_p10**: wait `befriend(alice, ReqId)` → `decision(yes, ...)` → wait
`connected(alice)` → wait `trade_proposed(alice, _, ReqId2)` → `mint(5, 0)` →
wait `minted(5, 0)` → `accept_trade(alice, ReqId2)` → wait `trade_completed(alice)` →
wait `escrow_received(alice, _)` → wait `escrow_released(alice)` → `balance` →
wait `balance_report(_)` → `done`

### Play 11 (credit + escrow cancel → mint + trade + escrow)

Same credit conversion as play 10, rest unchanged.

**alice_p11(T)**: `connect(bob)` → wait `connected(bob)` → `mint(5, 0)` →
wait `minted(5, 0)` → `trade(bob, [lot(alice, 0, 5)], [lot(bob, 0, 5)])` →
wait `trade_completed(bob)` → `deposit_escrow(bob, [lot(bob, 0, 3)], T)` →
wait `escrow_deposited(bob, _, ReqId)` → `cancel_escrow(bob, ReqId)` →
wait `escrow_returned(bob)` → `balance` → wait `balance_report(_)` → `done`

**bob_p11**: Same credit conversion as bob_p10, then wait escrow_cancelled.

## Part 5: boot.glp — Update bob_p4b signature

If bob_p4b now needs T, update the boot wiring:
- `fplay4b`: pass T to bob_p4b as well as alice_p4b
- Update the imported declarations if applicable

## Part 6: play12 actors — No changes needed

Play 12 already uses trade for everything. Verify no credit/loan references remain.

## Part 7: play12/self.glp — Remove credit/loan types

Remove `Installment`, `Schedule` type definitions if present.
Remove `credit(...)`, `accept_credit(...)`, `reject_credit(...)` from UserCmd if present.
Remove `loan(...)`, `accept_loan(...)`, `reject_loan(...)` from UserCmd if present.
Remove all credit/loan entries from UserNotify if present.

## Part 8: Test

```bash
cd /Users/udi/Grassroots/GLP/glp_runtime
for P in fplay1 fplay2 fplay3 fplay4 fplay4b fplay5 fplay6 fplay8 fplay9 fplay10 fplay11 fplay12; do
  echo -e "../programs/typed_book/bonds/agent.glp\n../programs/typed_book/bonds/mediator.glp\n../programs/typed_book/bonds/actors.glp\n../programs/typed_book/bonds/boot.glp\n../programs/typed_book/bonds/play12/self.glp\n../programs/typed_book/bonds/play12/alice.glp\n../programs/typed_book/bonds/play12/bob.glp\n../programs/typed_book/bonds/play12/charlie.glp\n../programs/typed_book/bonds/play12/diana.glp\n../programs/typed_book/bonds/play12/eve.glp\n../programs/typed_book/bonds/play12/frank.glp\n${P}.\n:quit" | dart run bin/glp_repl.dart > /private/tmp/unify-credit-${P}.txt 2>&1
  echo "${P}: done"
done
```

## Bug Protocol

Same as always. STOP on errors, show full output, do NOT fix without discussion.
