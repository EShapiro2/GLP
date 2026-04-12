# Phase 8B Fixes and Remaining Code

## Read First

1. `docs/glp-cheat-sheet.md` — complete GLP patterns reference
2. `docs/typed-glp-manual.md` — full language manual
3. `docs/DISCIPLINE.md`

## Bug Fix 1: Escrow cancel clause — remove `true | true`

The parser rejects `true` as a guard. The cancel clause needs NO guard — it's a unit clause. Head match on `cancel` is the synchronization.

In `bond_agent.glp`, replace:
```prolog
escrow(_, Bonds, cancel,
    escrow_cancelled,
    escrow_bonds(Bonds?)) :-
    true | true.
```

With this unit clause (no `:-` at all):
```prolog
escrow(_, Bonds, cancel,
    escrow_cancelled,
    escrow_bonds(Bonds?)).
```

Compare with `bind_credit_reject` and `bind_loan_reject` — both are unit clauses in the same file:
```prolog
bind_credit_reject(credit_reject).
bind_loan_reject(loan_reject).
```

## Bug Fix 2: BenResult mode in do_deposit_escrow_result

In `do_deposit_escrow_result(ok, ...)`, the escrow_offer message has `BenResult` (writer) but needs `BenResult?` (reader).

The variable flow:
- `escrow(..., BenResult, DepResult)` — `BenResult` is writer (escrow writes to position 4)
- `escrow_offer(ReleaseTime?, BenResult?)` — needs READER to send value to beneficiary
- `inject_escrow_dep_result(DepResult?, ...)` — `DepResult?` is reader (correct)

Change this line:
```prolog
        msg(Id?, Target?, escrow_offer(ReleaseTime?, BenResult)),
```

To:
```prolog
        msg(Id?, Target?, escrow_offer(ReleaseTime?, BenResult?)),
```

This gives: `BenResult` writer × 1 (escrow call), `BenResult?` reader × 1 (message). SRSW satisfied.

NOTE: `CancelSignal` in the deposited message is CORRECT as writer (no `?`):
```prolog
        msg(agent, '_user', escrow_deposited(Target?, ReleaseTime?, CancelSignal)),
```
The mediator stores the writer so the agent can later bind it to `cancel`.

## After fixes: type-check

After both fixes, type-check `bond_agent.glp`. It should pass. If not, stop and report the exact error.

## Remaining work: Agent clauses

The escrow agent clauses have NOT been added yet. Add them before the Termination section, after the Trade section. The exact code is in `docs/phase8b-instructions.md` Step 7. Copy it exactly.

## Remaining work: Mediator, Actors, Boot

Follow `docs/phase8b-instructions.md` Steps 8, 9, 10 exactly.

Type-check each file after changes. Run baseline tests (fplay1–fplay9, fplay4b). Then run fplay10 and fplay11.
