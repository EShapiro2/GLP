# Phase 4b Instructions for Claude Code

## Prerequisites

1. Read CLAUDE.md completely
2. Read docs/DISCIPLINE.md completely
3. Read docs/typed-glp-manual.md completely
4. Read the Phase 4b spec: `/Users/udi/Grassroots/Grassroots-Bonds/docs/phase4b-time-spec.md`
5. Read the guards reference (wait_until and wait sections): `/Users/udi/Grassroots/GLP/docs/guards-reference.md`
6. Run baseline tests before any changes

## Summary

When Bob handles a redemption request, he must check whether the MinMaturity threshold has already passed (bonds matured). If so, any bond qualifies. Currently `select_bonds_min_maturity` always requires M >= MinMaturity. We add a time-aware clause using `wait_until`.

GLP time primitives:
- `wait_until(Timestamp)` — guard that succeeds if current time >= Timestamp, fails otherwise
- `wait(Duration)` — guard that suspends for Duration milliseconds then succeeds
- `execute('current_time', [T])` — system predicate that binds T to epoch milliseconds
- `otherwise` — guard that succeeds if all previous clauses failed

## File: `programs/typed_book/bonds/bond_agent.glp`

### Change 1: Add `select_any_bonds` helper

Place this right after the existing `select_bonds_min_maturity` section. Selects up to K bonds regardless of maturity:

```prolog
%% =============================================================================
%% SELECT_ANY_BONDS — Select up to K bonds regardless of maturity
%% =============================================================================

procedure select_any_bonds(Constant?, BondList?, BondList, BondList, Constant).
select_any_bonds(0, Hs, [], Hs?, 0).
select_any_bonds(K, [bond(I, M, S)|Rest], [bond(I?, M?, S?)|Sel?], Rem?, Got?) :-
    K? > 0 |
    K1 := K? - 1,
    select_any_bonds(K1?, Rest?, Sel, Rem, SubGot),
    Got := SubGot? + 1.
select_any_bonds(K, [], [], [], 0) :- K? > 0 | true.
```

### Change 2: Modify `select_bonds_min_maturity` to dispatch on time

Replace the entire `select_bonds_min_maturity` section with:

```prolog
%% =============================================================================
%% SELECT_BONDS_MIN_MATURITY — Select up to K bonds with maturity >= MinMaturity
%% =============================================================================
%%
%% Time-aware: if MinMaturity has passed (wait_until succeeds), any bond qualifies.
%% Otherwise, require M >= MinMaturity strictly.

procedure select_bonds_min_maturity(Constant?, Constant?, BondList?, BondList, BondList, Constant).

%% When MinMaturity has passed (bonds are mature), any bond qualifies
select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    wait_until(MinMat?) |
    select_any_bonds(K?, Hs?, Sel, Rem, Got).

%% When MinMaturity has NOT passed, require M >= MinMaturity
select_bonds_min_maturity(MinMat, K, Hs, Sel?, Rem?, Got?) :-
    otherwise |
    select_bonds_min_maturity_strict(MinMat?, K?, Hs?, Sel, Rem, Got).

%% =============================================================================
%% SELECT_BONDS_MIN_MATURITY_STRICT — Strict maturity check (M >= MinMaturity)
%% =============================================================================

procedure select_bonds_min_maturity_strict(Constant?, Constant?, BondList?, BondList, BondList, Constant).
select_bonds_min_maturity_strict(_, 0, Hs, [], Hs?, 0).
select_bonds_min_maturity_strict(MinMat, K, [bond(I, M, S)|Rest], [bond(I?, M?, S?)|Sel?], Rem?, Got?) :-
    K? > 0, M? >= MinMat? |
    K1 := K? - 1,
    select_bonds_min_maturity_strict(MinMat?, K1?, Rest?, Sel, Rem, SubGot),
    Got := SubGot? + 1.
select_bonds_min_maturity_strict(MinMat, K, [B|Rest], Sel?, [B?|Rem?], Got?) :-
    otherwise |
    select_bonds_min_maturity_strict(MinMat?, K?, Rest?, Sel, Rem, Got).
select_bonds_min_maturity_strict(_, _, [], [], [], 0).
```

### Change 3: No changes to agent/6 clauses

They already call `select_bonds_min_maturity` which now dispatches correctly.

## File: `programs/typed_book/bonds/bond_actors.glp`

Add play4b actors. The scenario demonstrates time-dependent redemption:

1. Alice cold-calls Bob, Bob accepts → friends
2. Both open credit with coins (maturity 0), K=5
3. Alice computes T = now + 500ms. Both open credit with maturity T, K=5
4. Alice redeems 2 bob-bonds (maturity T) with MaxMaturity=T. T hasn't passed, so Bob provides alice-bonds with M>=T. Standard case — succeeds.
5. Alice checks balance
6. Wait 500ms (use `wait(500)` guard on next actor clause)
7. Alice redeems 2 more bob-bonds (maturity T) with MaxMaturity=T. Now T is mature: `wait_until(T)` succeeds, so Bob can provide ANY bond including alice-coins. This is the key demonstration.
8. Alice checks balance, both done.

Implementation notes:
- Alice computes T using `execute('current_time', [Now])` and `execute('evaluate', [Now? + 500, T])` in the actor body (before the guard commit)
- Actually, `execute` is a body predicate, not a guard. So the actor needs to compute T in the body and pass it along. Use a helper or compute inline.
- Wait: `execute` runs in the body (after commit). So the actor clause body computes T, then sends credit(bob, 5, T). This should work.
- Bob receives T via the credit_proposed notification and uses it to accept_credit.
- The `wait(500)` goes in a guard of a later actor clause to delay the next command.
- Use 500ms to keep tests fast.

Actor naming: `alice_p4b` and `bob_p4b`.

## File: `programs/typed_book/bonds/bond_boot.glp`

Add `play4b` and `fplay4b` goals. Same structure as play4 but using `alice_p4b` and `bob_p4b`.

## Testing

1. Run baseline tests first (all existing plays 1-5 must pass)
2. After changes, run all existing tests to verify no regression
3. Add play4b test to `test/run_all_tests.sh`
4. play4b should terminate successfully
5. Balance reports should show bonds moving as expected after time-dependent redemption

Note: plays 1-4 use maturity 0 only. `wait_until(0)` always succeeds, so the new dispatch clause fires and delegates to `select_any_bonds` — which selects any bond, same as before. No regression.
