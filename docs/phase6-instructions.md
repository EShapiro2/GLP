# Phase 6 Instructions: Generalize Loan to Schedule + Balloon Loan

## Prerequisites

1. Read CLAUDE.md completely
2. Read docs/DISCIPLINE.md completely  
3. Read docs/typed-glp-manual.md completely
4. Run baseline tests (fplay1–fplay5, fplay4b) before any changes

## Overview

Generalize the zero-coupon loan to accept a **schedule** — a list of `installment(Amount, Maturity)` entries. The borrower mints bonds per the schedule. Zero-coupon becomes a single-entry schedule. Balloon and fixed-payment loans are multi-entry schedules. Then revise play5 to use the new interface, test, and add play6 (balloon loan).

## Part 1: Type Changes

### New types (add to all three typed files: bond_agent.glp, bond_mediator.glp, bond_actors.glp)

```prolog
Installment ::= installment(Constant, Constant).
Schedule    ::= [] ; [Installment | Schedule].
```

### Changed types (in all files where they appear)

Old → New:

| Where | Old | New |
|-------|-----|-----|
| UserContent (loan) | `loan(Constant, Constant, Constant, Constant)` | `loan(Constant, Constant, Schedule)` |
| FriendContent (loan_propose) | `loan_propose(Constant, Constant, Constant, BondList, LoanResponse?)` | `loan_propose(Constant, Schedule, BondList, LoanResponse?)` |
| AgentContent (loan_proposed) | `loan_proposed(Constant, Constant, Constant, Constant, LoanResponse?, BondList)` | `loan_proposed(Constant, Constant, Schedule, LoanResponse?, BondList)` |
| OutputContent (loan_propose) | same as FriendContent | same as FriendContent |
| OutputContent (loan_proposed) | same as AgentContent | same as AgentContent |
| PendingValue (loan_pending) | `loan_pending(LoanResponse?, Constant, Constant, BondList)` | `loan_pending(LoanResponse?, Schedule, BondList)` |

Types that do NOT change:
- `LoanResponse ::= loan_accept(BondList) ; loan_reject.` — unchanged
- `loan_result(Constant, Constant, Constant, BondList)` in UserInMsg — unchanged (carries TotalFV as Constant)
- `loan_opened(Constant, Constant, Constant)` in AgentContent — unchanged (carries TotalFV)
- `loan_was_rejected(Constant)` — unchanged

### Mediator user-facing notification change

Old: `loan_proposed(Constant, Constant, Constant, Constant, ReqId)` — 5 args (From, Principal, FaceValue, Maturity, ReqId)

New: `loan_proposed(Constant, Constant, ReqId)` — 3 args (From, Principal, ReqId)

Schedule details are stored in the pending; the user/actor only sees From and Principal.

### Actor types change

In bond_actors.glp:
- UserCmd: `loan(Constant, Constant, Schedule)` replaces `loan(Constant, Constant, Constant, Constant)`
- UserNotify: `loan_proposed(Constant, Constant, ReqId)` replaces `loan_proposed(Constant, Constant, Constant, Constant, ReqId)`

## Part 2: New Helper Procedures (bond_agent.glp)

### schedule_total — Sum of amounts in a schedule

```prolog
procedure schedule_total(Schedule?, Constant).
schedule_total([installment(Amount, _)|Rest], Total?) :-
    schedule_total(Rest?, SubTotal),
    Total := SubTotal? + Amount?.
schedule_total([], 0).
```

### create_bonds_from_schedule — Create bonds for each installment

```prolog
procedure create_bonds_from_schedule(Constant?, Schedule?, Constant?, BondList, Constant).
create_bonds_from_schedule(Id, [installment(Amount, Maturity)|Rest], Serial, AllBonds?, NewSerial?) :-
    ground(Amount?), ground(Maturity?) |
    create_bonds(Id?, Maturity?, Amount?, Serial?, Batch),
    BatchSerial := Serial? + Amount?,
    create_bonds_from_schedule(Id?, Rest?, BatchSerial?, RestBonds, NewSerial),
    append(Batch?, RestBonds?, AllBonds).
create_bonds_from_schedule(_, [], Serial, [], Serial?).
```

Place these after the existing `create_bonds` and `append` procedures.

## Part 3: Changed Procedures (bond_agent.glp)

### bind_loan_accept — Now takes Schedule instead of single Maturity+FaceValue

Old:
```prolog
procedure bind_loan_accept(LoanResponse, Constant?, Constant?, Constant?, Constant?).
bind_loan_accept(loan_accept(MyBonds?), Id, Maturity, FaceValue, Serial) :-
    ground(Id?), ground(Maturity?), ground(FaceValue?), ground(Serial?) |
    create_bonds(Id?, Maturity?, FaceValue?, Serial?, MyBonds).
```

New:
```prolog
procedure bind_loan_accept(LoanResponse, Constant?, Schedule?, Constant?, Constant).
bind_loan_accept(loan_accept(MyBonds?), Id, Schedule, Serial, NewSerial?) :-
    ground(Id?), ground(Schedule?), ground(Serial?) |
    create_bonds_from_schedule(Id?, Schedule?, Serial?, MyBonds, NewSerial).
```

Note: now returns NewSerial as an output (5th arg is writer, not reader).

## Part 4: Changed Agent Clauses (bond_agent.glp)

### Lender side — loan command

Old:
```prolog
agent(Id, [msg('_user', Id1, loan(Target, Principal, FaceValue, Maturity))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?), ground(Principal?), ground(FaceValue?), ground(Maturity?) |
    NewNextSerial := NextSerial? + Principal?,
    create_bonds(Id?, 0, Principal?, NextSerial?, MyCoins),
    lookup_send(friend(Target?),
        msg(Id?, Target?, loan_propose(Principal?, FaceValue?, Maturity?, MyCoins?, LoanResp)),
        Outs?, Outs1),
    inject_loan_result(LoanResp?, Target?, Principal?, FaceValue?, UserIn?, UserIn1),
    agent(Id?, UserIn1?, NetIn?, Outs1?, Holdings?, NewNextSerial?).
```

New:
```prolog
agent(Id, [msg('_user', Id1, loan(Target, Principal, Schedule))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(Target?), ground(Principal?), ground(Schedule?) |
    NewNextSerial := NextSerial? + Principal?,
    create_bonds(Id?, 0, Principal?, NextSerial?, MyCoins),
    schedule_total(Schedule?, TotalFV),
    lookup_send(friend(Target?),
        msg(Id?, Target?, loan_propose(Principal?, Schedule?, MyCoins?, LoanResp)),
        Outs?, Outs1),
    inject_loan_result(LoanResp?, Target?, Principal?, TotalFV?, UserIn?, UserIn1),
    agent(Id?, UserIn1?, NetIn?, Outs1?, Holdings?, NewNextSerial?).
```

Note: `ground(Schedule?)` in guard allows Schedule? to be read twice (schedule_total + msg construction).

### Lender side — loan_result (injected) — NO CHANGE needed

The existing clause uses `loan_result(From, Principal, FaceValue, BorrowerBonds)` which now carries TotalFV as the third Constant. Same structure, same types.

### Lender side — loan_was_rejected — NO CHANGE needed

### Borrower side — incoming loan_propose

Old:
```prolog
agent(Id, UserIn, [msg(From, Id1, loan_propose(Principal, FaceValue, Maturity, LenderCoins, LoanResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(Principal?), ground(FaceValue?),
    ground(Maturity?), ground(LenderCoins?) |
    lookup_send('_user',
        msg(agent, '_user', loan_proposed(From?, Principal?, FaceValue?, Maturity?, LoanResp, LenderCoins?)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

New:
```prolog
agent(Id, UserIn, [msg(From, Id1, loan_propose(Principal, Schedule, LenderCoins, LoanResp?))|NetIn],
      Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(Principal?), ground(Schedule?), ground(LenderCoins?) |
    lookup_send('_user',
        msg(agent, '_user', loan_proposed(From?, Principal?, Schedule?, LoanResp, LenderCoins?)),
        Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, Holdings?, NextSerial?).
```

### Borrower side — accept_loan

Old:
```prolog
agent(Id, [msg('_user', Id1, accept_loan(From,
        loan_pending(LoanResp?, FaceValue, Maturity, LenderCoins)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(FaceValue?), ground(Maturity?), ground(LenderCoins?) |
    NewNextSerial := NextSerial? + FaceValue?,
    bind_loan_accept(LoanResp, Id?, Maturity?, FaceValue?, NextSerial?),
    append(Holdings?, LenderCoins?, NewHoldings),
    list_length(LenderCoins?, Principal),
    lookup_send('_user', msg(agent, '_user', loan_opened(From?, Principal?, FaceValue?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NewNextSerial?).
```

New:
```prolog
agent(Id, [msg('_user', Id1, accept_loan(From,
        loan_pending(LoanResp?, Schedule, LenderCoins)))|UserIn],
      NetIn, Outs, Holdings, NextSerial) :-
    Id? =?= Id1?, ground(From?), ground(Schedule?), ground(LenderCoins?) |
    bind_loan_accept(LoanResp, Id?, Schedule?, NextSerial?, NewNextSerial),
    schedule_total(Schedule?, TotalFV),
    append(Holdings?, LenderCoins?, NewHoldings),
    list_length(LenderCoins?, Principal),
    lookup_send('_user', msg(agent, '_user', loan_opened(From?, Principal?, TotalFV?)), Outs?, Outs1),
    agent(Id?, UserIn?, NetIn?, Outs1?, NewHoldings?, NewNextSerial?).
```

Note: `ground(Schedule?)` allows two reads (bind_loan_accept + schedule_total). `ground(LenderCoins?)` allows two reads (append + list_length) — same as before.

### Borrower side — reject_loan

Old:
```prolog
agent(Id, [msg('_user', Id1, reject_loan(_,
        loan_pending(LoanResp?, _, _, _)))|UserIn],
```

New:
```prolog
agent(Id, [msg('_user', Id1, reject_loan(_,
        loan_pending(LoanResp?, _, _)))|UserIn],
```

3 wildcards instead of 4 (loan_pending now has 3 args).

## Part 5: Mediator Changes (bond_mediator.glp)

### Agent-to-user: loan_proposed

Old:
```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', loan_proposed(From, Principal, FaceValue, Maturity, LoanResp?, LenderCoins)),
            AgentCh?, AgentCh1),
    ground(From?), ground(Principal?), ground(FaceValue?), ground(Maturity?), ground(LenderCoins?) |
    send_user(loan_proposed(From?, Principal?, FaceValue?, Maturity?, req(N?)), UserCh?, UserCh1),
    N1 := N? + 1,
    ui_mediator(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), loan_pending(LoanResp, FaceValue?, Maturity?, LenderCoins?)) | Ps?], N1?).
```

New:
```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(msg(agent, '_user', loan_proposed(From, Principal, Schedule, LoanResp?, LenderCoins)),
            AgentCh?, AgentCh1),
    ground(From?), ground(Principal?), ground(Schedule?), ground(LenderCoins?) |
    send_user(loan_proposed(From?, Principal?, req(N?)), UserCh?, UserCh1),
    N1 := N? + 1,
    ui_mediator(Id?, AgentCh1?, UserCh1?,
        [pending(req(N?), loan_pending(LoanResp, Schedule?, LenderCoins?)) | Ps?], N1?).
```

### User-to-agent: loan

Old:
```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(loan(Target, Principal, FaceValue, Maturity), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(Principal?), ground(FaceValue?), ground(Maturity?) |
    send_agent(msg('_user', Id?, loan(Target?, Principal?, FaceValue?, Maturity?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

New:
```prolog
ui_mediator(Id, AgentCh, UserCh, Ps, N) :-
    receive(loan(Target, Principal, Schedule), UserCh?, UserCh1),
    ground(Id?), ground(Target?), ground(Principal?), ground(Schedule?) |
    send_agent(msg('_user', Id?, loan(Target?, Principal?, Schedule?)),
         AgentCh?, AgentCh1),
    ui_mediator(Id?, AgentCh1?, UserCh1?, Ps?, N?).
```

### accept_loan and reject_loan — NO CHANGE needed

They use lookup_pending which returns the PendingValue opaquely. The structure changed (3 args vs 4) but the mediator doesn't inspect loan_pending internals.

## Part 6: Actor Changes (bond_actors.glp)

### Revised alice_p5

Change the loan command from:
```prolog
[loan(bob, 9, 10, 5)|Out?]
```
to:
```prolog
[loan(bob, 9, [installment(10, 5)])|Out?]
```

Everything else in alice_p5 stays the same (loan_opened(bob, _, _) still matches).

### Revised bob_p5

Change the loan_proposed match from:
```prolog
bob_p5_wait_loan_proposed([loan_proposed(alice, 9, 10, 5, ReqId)|In], ...)
```
to:
```prolog
bob_p5_wait_loan_proposed([loan_proposed(alice, 9, ReqId)|In], ...)
```

Everything else in bob_p5 stays the same.

### New: alice_p6 and bob_p6 (balloon loan)

Scenario: Alice lends Bob 10 coins. Bob provides a balloon schedule:
- 3 monthly interest payments: 1 bond each at maturities 100, 200, 300
- Principal return: 10 bonds at maturity 300
- Schedule: `[installment(1, 100), installment(1, 200), installment(1, 300), installment(10, 300)]`
- Total face value: 13

Alice:
1. connect(bob)
2. Wait connected(bob)
3. `loan(bob, 10, [installment(1, 100), installment(1, 200), installment(1, 300), installment(10, 300)])`
4. Wait loan_opened(bob, _, _)
5. balance
6. Wait balance_report(_)
7. done

Bob:
1. Wait befriend(alice, ReqId) → decision(yes, alice, ReqId)
2. Wait connected(alice)
3. Wait loan_proposed(alice, 10, ReqId2) → accept_loan(alice, ReqId2)
4. Wait loan_opened(alice, _, _)
5. balance
6. Wait balance_report(_)
7. done

Follow the exact same actor pattern as play5. Only differences:
- Alice sends the balloon schedule instead of single-installment
- Bob matches `loan_proposed(alice, 10, ReqId)` (Principal is 10, not 9)

### Expected output (fplay5 revised)

Same as before except:
- Alice sends `loan(bob, 9, [installment(10, 5)])` instead of `loan(bob, 9, 10, 5)`
- Bob sees `loan_proposed(alice, 9, req(N))` instead of `loan_proposed(alice, 9, 10, 5, req(N))`
- loan_opened notifications unchanged: `loan_opened(bob, 9, 10)` and `loan_opened(alice, 9, 10)`

### Expected output (fplay6)

```
tagged(alice, cmd(connect(bob)))
tagged(bob, notify(befriend(alice, req(1))))
tagged(bob, cmd(decision(yes, alice, req(1))))
tagged(alice, notify(connected(bob)))
tagged(bob, notify(connected(alice)))
tagged(alice, cmd(loan(bob, 10, [installment(1, 100), installment(1, 200), installment(1, 300), installment(10, 300)])))
tagged(bob, notify(loan_proposed(alice, 10, req(2))))
tagged(bob, cmd(accept_loan(alice, req(2))))
tagged(alice, notify(loan_opened(bob, 10, 13)))
tagged(bob, notify(loan_opened(alice, 10, 13)))
tagged(alice, cmd(balance))
tagged(alice, notify(balance_report([bond(bob, 100, 1), bond(bob, 200, 2), bond(bob, 300, 3), bond(bob, 300, 4), ..., bond(bob, 300, 13)])))
tagged(bob, cmd(balance))
tagged(bob, notify(balance_report([bond(alice, 0, 1), ..., bond(alice, 0, 10)])))
tagged(alice, cmd(done))
tagged(bob, cmd(done))
```

Alice should have 13 bob-bonds (1@100, 1@200, 1@300, 10@300). Bob should have 10 alice-coins.

## Part 7: Boot Changes (bond_boot.glp)

### Add play6/fplay6

Same wiring structure as play5 but using alice_p6 and bob_p6 actors. Copy play5/fplay5 blocks, change alice_p5→alice_p6 and bob_p5→bob_p6.

## Testing

1. Run baseline first: all existing fplay1–fplay5 + fplay4b must pass before changes
2. After changes: run fplay1–fplay4b (should be unaffected — no loan types involved)
3. Run revised fplay5: should produce same loan_opened(bob, 9, 10) / loan_opened(alice, 9, 10)
4. Run new fplay6: should produce loan_opened with TotalFV=13
5. Both balance reports should show correct bond distributions

## Important Notes

- The `ground(Schedule?)` guard is essential — it allows Schedule? to be read multiple times in the body (SRSW relaxation for ground-guarded variables)
- Installment components (Amount, Maturity) are Constants, so they're SRSW-exempt and can be read multiple times within create_bonds_from_schedule
- All existing plays (1–4, 4b) don't touch loan types, so they're unaffected
- The UserCmd type in actors needs the Schedule type defined
- The UserNotify loan_proposed arity changes from 5 to 3 — update in all three files
