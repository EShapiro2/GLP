# GLP engine bug: high-arity head-constructing dispatch clause fails to commit

**Date:** 2026-07-23
**Found by:** P99 Code session (transcript `1aac4f42-2a7a-40e9-9acf-645934e6090f`), while writing P92 (Von Koch).
**Owner to fix:** IGLP (the engine/runtime).
**Status:** FIXED (GLP `e57f3ca2` head path + `43f6dcfc` body path, 2026-07-23) — see Resolution below. Reported to IGLP via Udi; was not blocking P99 (the affected code had been rewritten in the idiomatic packed-tuple form, which stayed under the trigger).
**Minimal repro:** `GLP/programs/tests/test_arity_dispatch_bug.glp` (committed, GLP `af921126`).

## Resolution (2026-07-23, GLP `e57f3ca2` + `43f6dcfc`)

**Root cause.** The engine hard-coded ten argument registers: operand slots 0–9 were argument slots, and any slot ≥ 10 was read as a temp/clause register. codegen started temporaries at 10 (`resetTemps`); the runner tested `argSlot >= 10` in `execHeadStructure`/`execHeadNil`. A procedure of arity ≥ 11 has a genuine argument at slot 10 (the repro's output `[t(...)|Out?]`, emitted as `HeadStructure('.',2,argSlot=10)`), which head-matching misread as a temp register, so the head never matched, the clause returned `nextClause`, fell through to the `no` clause (mismatch on `yes`), and the goal failed with `R` unbound. The code-format appendix (`app:code-format`) makes `argSlot` an unbounded `clen`, so the 10-register cap contradicted the spec — an engine bug, fixed in the engine.

**Fix** (behaviour-identical for arity ≤ 10, so zero suite regressions).

*Head path* (`e57f3ca2`):
- `glp_runtime/lib/bytecode/runner.dart` — `execHeadStructure`/`execHeadNil` classify an operand as a clause/temp register by `!env.argBySlot.containsKey(argSlot)` (the goal's actual argument set) instead of `argSlot >= 10`.
- `glp_runtime/lib/compiler/codegen.dart` — `resetTemps` starts temp registers above `max(varCount, headArity)` (keeping the historical floor of 10) so a temp index can never alias a real argument slot.

*Body path* (`43f6dcfc`) — the same ten-register assumption also lived in body-goal construction, where `execPutStructure` and the six structure-completion sites used `< 10` to decide whether a built compound goes into an argument register or a temp register:
- `glp_runtime/lib/bytecode/runner.dart` — `execPutStructure` now classifies a top-level body argument by nesting (`currentStructure == null`) rather than `argSlot < 10`; the completion sites drop the `< 10` upper bound (`clauseVars[-2]` is set only for genuine top-level args). A compound argument at slot ≥ 10 of a body call to a procedure of arity ≥ 11 now reaches its argument register.

**Verification.** `hi(yes,1..9,R)` → `R = [t(1,2,3,4,5)]`; head arity sweep commits with no cap through arity 20; a body call passing a compound at slots 8/10/12/15 and a nested `d(c(_))` at slot 10 all commit; full REPL suite 564/564 green; permanent regressions Z1/Z2 (head), Z3 (body), Z4 (trace) in `test/run_all_tests.sh`, exercising `programs/tests/test_arity_dispatch_bug.glp` (`hi`, `bp`). A related display-only cap in `reformatHead` (`i < 10`) that truncated `:trace` of arity ≥ 11 goals was also removed (GLP `b9072ede`).

## Summary

A two-clause committed-choice procedure, dispatched on a constant first argument, whose selected clause **builds a compound term in its head output** using ground-relaxed shared readers, **fails to commit at high arity**. The goal reduces to `failed` even though the program is well-typed, the correct clause's head matches, and its guard is satisfiable. The identical logic packed into a tuple — reducing the argument count — commits correctly. This is a soundness gap: a well-typed program that should reduce does not.

## Symptom

Calling the procedure with the literal that selects its head-constructing clause returns the output variable unbound and reports failure:

```
hi(yes, 1, 2, 3, 4, 5, 6, 7, 8, 9, R).
R = <unbound>
→ failed
```

The same logic with the eight threaded integers packed into one tuple commits:

```
hip(yes, 1, tt(2, 3, 4, 5, 6, 7, 8, 9), R).
R = [t(1, 2, 3, 4, 5)]
→ succeeds
```

Both procedures are accepted by the type checker with no warnings. `:trace` on the failing call shows the goal reaching the procedure with all arguments ground, then `→ failed` with no clause committing — not a suspension.

## Minimal reproduction

```prolog
Tup ::= t(Integer, Integer, Integer, Integer, Integer).
TList ::= [] ; [Tup | TList].
Flag ::= yes ; no.
Pack ::= tt(Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer).

% Fails: 11-argument version.
exported procedure hi(Flag?, Integer?, Integer?, Integer?, Integer?, Integer?, Integer?,
                      Integer?, Integer?, Integer?, TList).
hi(yes, A, B, C, D, E, F, G, H, K, [t(A?, B?, C?, D?, E?)|Out?]) :-
    integer(A?), integer(B?), integer(C?), integer(D?), integer(E?),
    integer(F?), integer(G?), integer(H?), integer(K?) |
    lo(A?, B?, C?, D?, E?, F?, G?, H?, K?, Out).
hi(no, _, _, _, _, _, _, _, _, _, Out?) :-
    lo(0, 0, 0, 0, 0, 0, 0, 0, 0, Out).
procedure lo(Integer?, Integer?, Integer?, Integer?, Integer?, Integer?, Integer?, Integer?,
             Integer?, TList).
lo(_, _, _, _, _, _, _, _, _, []).

% Works: same logic, arguments packed into one tuple.
exported procedure hip(Flag?, Integer?, Pack?, TList).
hip(yes, A, tt(B, C, D, E, F, G, H, K), [t(A?, B?, C?, D?, E?)|Out?]) :-
    integer(A?), integer(B?), integer(C?), integer(D?), integer(E?),
    integer(F?), integer(G?), integer(H?), integer(K?) |
    lop(A?, tt(B?, C?, D?, E?, F?, G?, H?, K?), Out).
hip(no, _, _, Out?) :-
    lop(0, tt(0, 0, 0, 0, 0, 0, 0, 0), Out).
procedure lop(Integer?, Pack?, TList).
lop(_, _, []).
```

Run from `glp_runtime/`:

```
load ../programs/tests/test_arity_dispatch_bug.glp
hi(yes, 1, 2, 3, 4, 5, 6, 7, 8, 9, R).      % R = <unbound>, failed  (WRONG)
hip(yes, 1, tt(2, 3, 4, 5, 6, 7, 8, 9), R). % R = [t(1,2,3,4,5)], succeeds  (correct)
```

## What was ruled out during isolation

Each of the following was reproduced in isolation and works, so none is the trigger on its own:

1. The nine `integer/1` guards on ground integers — a guards-only mirror commits.
2. The head compound construction `[t(A?, B?, C?, D?, E?)|Out?]` — a single-clause version commits.
3. The body call and its base clause — they commit standalone.
4. A two-clause `yes`/`no` dispatch with differing output structures — commits.
5. Ground-relaxed readers shared between the head output and the body — commits.
6. Mutual recursion between the dispatch procedure and its body callee — commits.
7. A variable (not literal) first argument at some call site — commits.
8. The clause selected alone (delete the sibling clause) — commits.

The failure appears only when the dispatch procedure carries many arguments. At 6 arguments the identical structure commits; at 11 it does not. Packing the invariant fields into a tuple (dropping the arity to 4–6) restores correct commitment. The `no` clause of the failing procedure commits normally; only the head-constructing `yes` clause fails to commit, and only in the presence of the sibling clause.

## Trigger (as characterised)

The combination that fails:

- a committed-choice procedure with **more than one clause**, dispatched on a constant argument;
- the selected clause **constructs a compound term in its head output** (here `[t(...)|Out?]`) rather than passing an output variable straight through;
- **high argument count** (fails at 11, commits at 6).

A procedure that only threads arguments to a helper without head construction (e.g. a `chkedges`-style fold at the same arity) commits correctly, so head construction is part of the trigger, not arity alone.

## Fix applied in P99

P92's expander originally threaded the per-node context (current node, remaining nodes, assignment, used differences, edges, node count) as six separate arguments through the branch/commit helpers, pushing the committing helper to eleven arguments. It was rewritten to pass that context as a single `ctx/6` tuple, which keeps the helpers at three to six arguments — the same idiom queens (`q(Placed, N)`) and Sudoku (`su(Cells)`) already use. This both fixes P92 and is the correct GLP style; it is not a bug-specific hack.

## Recommendation

The engine (or the compiler's clause-selection code generation) should commit the well-typed head-constructing clause regardless of arity. IGLP owns the engine and runtime per `glp-paper-code-map.md`. Making the type checker reject the program is **not** an acceptable resolution: it is well-typed and its guard is satisfiable, so it must reduce; rejecting it would be a completeness regression, and the type checker is TGLP-owned, outside the engine's scope.

## Handoff to IGLP

Directive for the IGLP (engine-owning) Code session; paste the block below into that session as-is. It follows the GLP bug-protocol and explicitly rules out the "make the type checker reject it" fallback (rejecting well-typed code is a completeness regression, and the type checker is TGLP-owned, outside the engine's scope).

```
You own the GLP engine/runtime and clause-selection codegen (glp-paper-code-map.md). Fix a reported soundness gap.

READ FIRST: /Users/udi/Grassroots/docs/glp-engine-bug-arity-dispatch-2026-07-23.md
Repro is committed: programs/tests/test_arity_dispatch_bug.glp

BUG: A multi-clause committed-choice procedure dispatched on a constant, whose selected clause constructs a compound in its head output ([t(...)|Out?]), fails to commit at high arity. hi(yes,1..9,R) reduces to `failed` with R unbound; the same logic packed into a tuple (hip, lower arity) commits. The program is well-typed and the guard is satisfiable, so it MUST reduce.

DO:
1. Reproduce from glp_runtime/:
   printf 'load ../programs/tests/test_arity_dispatch_bug.glp\nhi(yes, 1, 2, 3, 4, 5, 6, 7, 8, 9, R).\n:quit\n' | bin/glpc
   Confirm hi(...) fails and hip(...) succeeds. Use :trace.
2. Root-cause in the engine / clause-selection codegen. The report reproduces 8 candidate causes working in isolation; the trigger is multi-clause dispatch + head compound construction + high arity (commits at 6, fails at 11). Suspect an arity-indexed limit in the head-match/commit path (tentative head bindings / clause try / argument handling).
3. Fix at the engine or compiler so the well-typed head-constructing clause commits regardless of arity.

CONSTRAINTS (GLP discipline):
- Spec-first + bug-protocol: identify and quote the spec for the behavior. If the engine contradicts the spec, fix the engine. If the spec is silent, STOP and report to Udi before coding.
- No workaround: do not special-case arity, restructure to dodge the trigger, or mark the test expected-to-fail.
- Do NOT "fix" this by making the type checker reject the program. It is well-typed and must reduce; that lever is a completeness regression and the type checker is TGLP-owned, out of scope.
- Baseline-before-commit: bash test/run_all_tests.sh > /private/tmp/glp-baseline.txt 2>&1 ; confirm "ALL TESTS PASSED!" ; make the fix ; re-run ; commit only if clean.
- Add the repro as a permanent regression test in test/run_all_tests.sh (positive case: hi(yes,1..9,R) commits with R = [t(1,2,3,4,5)]). Tests are never removed.

DONE WHEN: hi(yes,1,2,3,4,5,6,7,8,9,R) succeeds with R = [t(1,2,3,4,5)], the full suite is green, the regression test is added, and you report the root cause to Udi.
```
