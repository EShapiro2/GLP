# Handover: Fix bond_agent.glp after wait_until semantics change

## What Changed Today

`wait_until(T)` was fixed across paper, specs, and runtime. It now **suspends** until time T (like `wait`), instead of failing. This is already pushed in the GLP repo.

## What Breaks

`bond_agent.glp` has `select_bonds_min_maturity` which uses `wait_until(MinMat?)`/`otherwise` as a two-way branch: if time has passed, first clause fires; if not, `otherwise` fires. Now that `wait_until` suspends instead of failing, `otherwise` never fires — the goal just hangs.

## The Fix

Replace the `wait_until` guard with an arithmetic comparison. The caller (`agent/4`, the `redeem_request` message handler) should compute the current time and pass it as a parameter. Then `select_bonds_min_maturity` uses `MinMat? =< Now?` instead of `wait_until(MinMat?)`.

## The `now/1` Type Checker Problem

`now/1` is a system predicate defined in `stdlib/time.glp` but the type checker doesn't see stdlib declarations. You need to make `now/1` visible. Read:

1. `GLP/glp_runtime/lib/analysis/type_checker/prelude.dart` — lists all builtin procedures
2. `GLP/programs/self.glp` — prelude declarations loaded via scope chain
3. `GLP/programs/stdlib/time.glp` — the actual `now/1` definition

The correct fix depends on how other system predicates (like `:=`, `=..`) are made visible to the type checker. Follow the same mechanism. `:=` is listed in `builtinGoals` in prelude.dart. Check how `=..` is handled — it's in `builtinProcedures`. `now/1` probably needs to be added there too, or declared in `self.glp`.

## Authoritative References

- **Paper** (just updated): `~/Grassroots/GLP-ICLP-2026/sections/appendix-guards.tex` — defines `wait_until` as suspending, defines `now/1` as a system predicate
- **Guards spec** (just updated): `~/Grassroots/GLP/docs/guards-reference.md` — `wait_until` section rewritten
- **Predicate taxonomy** (just updated): `~/Grassroots/GLP/docs/glp-predicate-taxonomy.md` — `wait_until` section rewritten  
- **Stdlib**: `~/Grassroots/GLP/programs/stdlib/time.glp` — `now(T?) :- '_now'(T).`
- **Audit report**: `~/Grassroots/GLP/docs/wait-until-audit-report.md` — full context on the issue

## Steps

1. Read the authoritative references above to understand the current state
2. Figure out how to make `now/1` visible to the type checker (follow the mechanism used by `:=` and `=..`)
3. Fix `select_bonds_min_maturity` in `bond_agent.glp`: add `Now` parameter, use `MinMat? =< Now?` guard
4. Fix the caller in `agent/4` (the `redeem_request` handler): call `now(Now)` in the body, pass `Now` to `select_bonds_min_maturity`
5. Run type checker and tests
6. Commit and push
