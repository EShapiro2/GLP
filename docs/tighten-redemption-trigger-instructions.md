# Claude Code Instructions: Tighten Redemption Trigger

## Mandatory Reading

CLAUDE.md → DISCIPLINE.md → manual → cheat sheet.
Then read `programs/typed_book/bonds/agent.glp`.

## Overview

The redemption auto-accept should trigger ONLY when the trade offer is
exactly one coin issued by the recipient. Currently `check_own_bond` checks
if ANY offered bond was issued by me — this is too broad.

A trade offering multiple of my coins, or offering an immature bond I issued,
is a bilateral trade requiring my consent — not a unilateral redemption.

## Edit: Replace check_own_bond

Replace the current `check_own_bond` procedure:

```glp
%% =============================================================================
%% CHECK_OWN_BOND — Check if any offered bond was issued by me
%% =============================================================================

procedure check_own_bond(Constant?, BondList?, Constant).
check_own_bond(Id, [bond(I, _, _)|_], yes) :- Id? =?= I? | true.
check_own_bond(Id, [_|Rest], Result?) :- otherwise | check_own_bond(Id?, Rest?, Result).
check_own_bond(_, [], no).
```

With:

```glp
%% =============================================================================
%% IS_REDEMPTION — Check if offer is exactly one coin issued by me
%% =============================================================================
%%
%% Redemption triggers auto-accept ONLY when the trade offer is exactly
%% one coin (maturity 0) issued by the recipient. Any other offer —
%% multiple bonds, immature bonds, or mixed offers — is a bilateral
%% trade requiring the user's consent.

procedure is_redemption(Constant?, BondList?, Constant).
is_redemption(Id, [bond(I, 0, _)], yes) :- Id? =?= I? | true.
is_redemption(_, _, no) :- otherwise | true.
```

## Edit: Update caller

In the incoming trade_propose agent clause, replace `check_own_bond` with
`is_redemption`:

Replace:
```glp
    check_own_bond(Id?, OfferedBonds?, IsRedemption),
```

With:
```glp
    is_redemption(Id?, OfferedBonds?, IsRedemption),
```

## Test

Run all 12 fplay tests. All should still pass — existing plays that trigger
redemption (plays 3, 4, 12) all offer exactly one coin.

## Bug Protocol

STOP on errors, show full output, do NOT fix without discussion.
