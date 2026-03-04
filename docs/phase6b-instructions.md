# Phase 6b Instructions: Fixed-Payment Loan Play

## Prerequisites

1. Read CLAUDE.md completely
2. Read docs/DISCIPLINE.md completely
3. Run baseline tests (fplay1–fplay6, fplay4b) before any changes

## Overview

No code changes to agent, mediator, or helpers. The generalized schedule already handles fixed-payment loans. This task adds play7 actors and boot wiring to demonstrate a fixed-payment loan.

## Scenario

Alice lends Bob 12 coins. Bob repays in 4 equal installments of 4 bonds each (principal + interest blended), at maturities 100, 200, 300, 400.

Schedule: `[installment(4, 100), installment(4, 200), installment(4, 300), installment(4, 400)]`

Total face value: 16 (4 extra = interest on 12 principal).

## File: `bond_actors.glp`

Add `alice_p7` and `bob_p7`. Identical structure to play6 actors, with these differences:

- Alice sends: `loan(bob, 12, [installment(4, 100), installment(4, 200), installment(4, 300), installment(4, 400)])`
- Bob matches: `loan_proposed(alice, 12, ReqId)`

Follow the exact pattern of alice_p6/bob_p6. Update the file header comment to mention Phase 7.

## File: `bond_boot.glp`

Add `play7` and `fplay7`. Copy play6/fplay6, change alice_p6→alice_p7 and bob_p6→bob_p7.

## Expected output (fplay7)

```
tagged(alice, cmd(connect(bob)))
tagged(bob, notify(befriend(alice, req(1))))
tagged(bob, cmd(decision(yes, alice, req(1))))
tagged(alice, notify(connected(bob)))
tagged(bob, notify(connected(alice)))
tagged(alice, cmd(loan(bob, 12, [installment(4, 100), installment(4, 200), installment(4, 300), installment(4, 400)])))
tagged(bob, notify(loan_proposed(alice, 12, req(2))))
tagged(bob, cmd(accept_loan(alice, req(2))))
tagged(alice, notify(loan_opened(bob, 12, 16)))
tagged(bob, notify(loan_opened(alice, 12, 16)))
tagged(alice, cmd(balance))
tagged(alice, notify(balance_report([bond(bob, 100, 1), ..., bond(bob, 100, 4), bond(bob, 200, 5), ..., bond(bob, 400, 16)])))
tagged(bob, cmd(balance))
tagged(bob, notify(balance_report([bond(alice, 0, 1), ..., bond(alice, 0, 12)])))
tagged(alice, cmd(done))
tagged(bob, cmd(done))
```

Alice should have 16 bob-bonds (4@100, 4@200, 4@300, 4@400). Bob should have 12 alice-coins. loan_opened on both sides shows TotalFV=16.

## Testing

1. Run all existing plays first (fplay1–fplay6, fplay4b)
2. Run fplay7
3. Verify loan_opened shows Principal=12, TotalFV=16
4. Verify balance reports show correct bond distributions
5. Update the status report at `Grassroots-Bonds/docs/bonds-glp-status-report.md`
