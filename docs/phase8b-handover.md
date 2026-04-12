# Phase 8B Escrow Implementation — Handover for Claude Code

**Date**: 2026-03-06
**Status**: Types complete, procedures and agent clauses not yet added

## Prerequisites — Mandatory Reading

You should have already completed the CLAUDE.md startup sequence:
1. CLAUDE.md (this repo's instructions)
2. docs/DISCIPLINE.md (development standards)
3. docs/typed-glp-manual.md (GLP language manual)
4. docs/glp-cheat-sheet.md (GLP programming patterns — GLP is NOT Prolog)

If you have NOT read all four, STOP and read them now before proceeding.

**Additional required reading for this task:**
5. Study `programs/typed_book/bonds/bond_agent.glp` — read the ENTIRE file. Every pattern you need for escrow (bind, inject, handle, do) already exists there. Pay special attention to `handle_redeem_fill`, `bind_trade_accept`, `bind_trade_decline`, `inject_credit_result`, `inject_trade_result`, `do_trade`, `handle_trade_fill`.

## Current State

- **Phases 1–7 complete and tested** (fplay1–fplay9, fplay4b all pass)
- **Phase 8A complete**: `select_bonds_min_maturity` fixed to use `now(Now)` + arithmetic comparison instead of `wait_until`/`otherwise`
- **Phase 8B types**: All escrow types are already added to `bond_agent.glp` (types section). NO escrow procedures or agent clauses exist yet.
- **Mediator, actors, boot**: No escrow changes yet

## What Needs to Be Done

Implement escrow per `docs/phase8b-instructions.md`. That file has the complete specification with all code. Below is a summary of the key points.

## The Escrow Design

The escrow is a concurrent process spawned by the depositor. Two clauses race:

```prolog
procedure escrow(Constant?, BondList?, EscrowCancel?, EscrowBenResult, EscrowDepResult).

%% Time passes — beneficiary gets bonds
escrow(T, Bonds, _,
    escrow_bonds(Bonds?),
    escrow_expired) :-
    wait_until(T?) | true.

%% Depositor cancels — depositor gets bonds back
escrow(_, Bonds, cancel,
    escrow_cancelled,
    escrow_bonds(Bonds?)).
```

Clause 1 suspends on `wait_until(T?)` until time T. Clause 2 suspends on head match until CancelSignal is bound to `cancel`. First to wake commits.

**NOTE**: `wait_until` currently fails instead of suspending. There is a pending fix. The escrow code is written assuming the fix. fplay10 (time release) won't work until the fix is in, but fplay11 (cancel) should work since it doesn't depend on `wait_until` suspending.

## Types Already in Place

These are already in `bond_agent.glp`:

```prolog
EscrowCancel ::= cancel.
EscrowBenResult ::= escrow_bonds(BondList) ; escrow_cancelled.
EscrowDepResult ::= escrow_bonds(BondList) ; escrow_expired.
```

FriendContent already has `escrow_offer(Constant, EscrowBenResult?)`.
UserContent already has `deposit_escrow(...)` and `cancel_escrow(...)`.
PendingValue already has `escrow_pending(EscrowCancel?)`.
AgentContent already has all escrow notifications.
UserInMsg already has `escrow_ben_released`, `escrow_ben_cancelled`, `escrow_dep_expired`, `escrow_dep_returned`.
OutputContent already has all escrow entries.

## What to Add (in order)

### 1. Procedures in bond_agent.glp

Add these BEFORE the `AGENT/6` section, after the trade helpers. The exact code is in `docs/phase8b-instructions.md` steps 3–7.

Procedures to add:
- `escrow/5` — the concurrent race process (2 clauses)
- `inject_escrow_ben_result/4` — monitor beneficiary outcome (3 clauses)
- `inject_escrow_dep_result/4` — monitor depositor outcome (3 clauses)
- `bind_escrow_cancel/1` — bind cancel signal (1 clause)
- `do_deposit_escrow/9` — select bonds + dispatch (1 clause)
- `do_deposit_escrow_result/10` — ok/fail dispatch (2 clauses)

### 2. Agent clauses in bond_agent.glp

Add Phase 8 section before Termination. 7 clauses:
- `deposit_escrow` command (depositor)
- `escrow_dep_expired` injected (depositor)
- `escrow_dep_returned` injected (depositor)
- `cancel_escrow` command (depositor)
- `escrow_offer` from friend (beneficiary)
- `escrow_ben_released` injected (beneficiary)
- `escrow_ben_cancelled` injected (beneficiary)

### 3. Mediator clauses in bond_mediator.glp

Add types (`EscrowCancel`, `EscrowBenResult`, `EscrowDepResult`), update `escrow_offer` in FriendContent.
Add mediator clauses:
- `escrow_deposited` agent→user (non-ground, pending)
- 6 ground pass-through clauses
- `deposit_escrow` user→agent (pass-through)
- `cancel_escrow` user→agent (pending lookup)

### 4. Actors in bond_actors.glp

Add types, UserCmd/UserNotify additions.
Add `alice_p10`, `bob_p10` (time release play).
Add `alice_p11`, `bob_p11` (cancel play).

### 5. Boot in bond_boot.glp

Add `play10`/`fplay10` (T = now + 500).
Add `play11`/`fplay11` (T = now + 5000).

## Critical GLP Patterns — DO NOT USE PROLOG

1. **Writer outputs are constructed in CLAUSE HEADS, never via `=`**
2. **Unit clauses have no `:-`** — just `head.` (e.g., `bind_escrow_cancel(cancel).`)
3. **`true` is not a valid guard** — don't write `true | true`
4. **Follow existing patterns exactly** — every pattern needed exists in the file already

## Files to Modify

| File | What to Add |
|------|-------------|
| `programs/typed_book/bonds/bond_agent.glp` | Procedures + agent clauses |
| `programs/typed_book/bonds/bond_mediator.glp` | Types + mediator clauses |
| `programs/typed_book/bonds/bond_actors.glp` | Types + play10/play11 actors |
| `programs/typed_book/bonds/bond_boot.glp` | play10/fplay10, play11/fplay11 |

## Testing

1. Type-check after each file change
2. Run all existing plays first (fplay1–fplay9, fplay4b)
3. Run fplay11 (cancel — should work now)
4. Run fplay10 (time release — needs wait_until fix to work)
