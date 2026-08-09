# GLP engine bug: body variable pairs disconnect in guarded clauses

**Date:** 2026-07-22.  **Reporter:** P99 Code session.  **To:** IGLP (engine/compiler; possibly the PE guard transform, then TGLP).  **Blocking:** P99 problems P35–P38, P40–P41 (factorisation, Goldbach) and P67 `string_tree`.
**Repro file:** `GLP/programs/tests/test_p99_probe4.glp` (committed, `cc0b64a0` and later).

## Symptom

In certain guarded clauses, the writer/reader pairs local to the body arrive disconnected at runtime: a helper call binds its output writer, but the later body goal that consumes the paired reader never sees the value and suspends forever.  The goal deadlocks (REPL reports failure with unbound outputs).

## Minimal reproduction (instance 1: arithmetic-expression guard)

**Failing goal:**
```
walkd(35, 4, R).
```
Expected `R = 6`; actual: deadlock.

**Declarations and clauses** (from the repro file):
```prolog
procedure bump(Integer?, Integer).
bump(2, 3).
bump(D, D1?) :- D? =\= 2 | D1 := D? + 2.

procedure walkd(Integer?, Integer?, Integer).
walkd(N, D, D?) :- D? * D? > N? | true.
walkd(N, D, R?) :- D? * D? =< N? | bump(D?, D1), walkd(N?, D1?, R).
```

The trace shows the second clause's body as `bump(4, X1), walkd(35, X2?, R)` — the `D1`/`D1?` pair split into unrelated `X1`, `X2`.

**Variant matrix** (all in the repro file): drop any one ingredient and it works:

| Variant | Guard | Body | Result |
|---|---|---|---|
| `walkd` | expression (`D? * D? =< N?`) | helper + self-recursion consuming its output | **deadlock** |
| `walkb` | plain comparison (`D? =< N?`) | same body | works |
| `walka` | expression | `:=` instead of the helper | works |
| `walke` | two expression conjuncts | helper + non-recursive consumer | works |
| `use` | none | helper chained into helper | works |

## Instance 2: constant-comparison guard (P67 parser)

`p99/btrees/p67.glp`, procedure `pt1` — guard `C? =?= '('`, body a chain
`pt → expect → pt → expect → mkt → =` of five goals with four local pairs.  The trace shows **every** pair renamed apart, and the chain suspends:

```
pt1([(, b, )], a, T, R) :- pt([b, )], X1, X2), expect(X3?, ,, X4),
    pt(X5?, X6, X7), expect(X8?, ), X9), mkt(a, X10?, X11?, T), =(X12?, R)
expect(X3?, ,, X4) → suspended
```

Goal: `string_tree([a, '(', b, ',', c, ')'], T).` — expected `t(a, t(b,nil,nil), t(c,nil,nil))`, actual deadlock.  A *minimal* `=?=`-guard variant (`self` in the repro file) does **not** trip the bug, so the trigger here is wider than one guard + one pair; `pt1` is the smallest known failing instance of this second form.  Note the same shape *works* in `p99/mtrees/p70.glp`/`p73.glp` (`nf`, `lpf`), so clause composition details matter.

## Affected P99 code (all committed, marked by comments)

- `p99/arithmetic/p35.glp` (`pf` clause 4 via `nxt`), hence P36–P38.
- `p99/arithmetic/p40.glp` (`gb`), hence P41.
- `p99/btrees/p67.glp` (`pt1`, `string_tree`).

## Acceptance tests

```
walkd(35, 4, R).                            → R = 6
prime_factors(315, R).                      → R = [3, 3, 5, 7]     (p99/arithmetic/)
goldbach(28, R).                            → R = found(g(5, 23))  (p99/arithmetic/)
string_tree([a,'(',b,',',c,')'], T).        → T = t(a, t(b,nil,nil), t(c,nil,nil))  (p99/btrees/)
```

## Aside (minor, unblocked)

Named anonymous variables (`_Fs`) fail at codegen with `Undefined variable`, though manual §9 permits them; plain `_` works.  Manual/implementation discrepancy — for whoever owns the codegen path.

## Progress Log

### 2026-07-22 — Fixed (IGLP Code session)

**The "pair disconnect" was a misread trace, not the cause.**  The trace renders each clause-local variable with a fresh display name, so the writer/reader pair `D1`/`D1?` printed as unrelated `X1`/`X2`; the pair was in fact intact.  Two independent bugs produced the deadlocks, one per instance in the report.

**Bug 1 — comparison guard fails instead of suspending on an unbound reader nested in an expression operand** (instance 1: `walkd`, hence P35–P38, P40–P41).  In `walkd(N, D, R?) :- D? * D? =< N? | bump(D?, D1), walkd(N?, D1?, R)`, the recursive `walkd(N?, D1?, R)` may be tried before the sibling `bump` binds `D1`.  Its guard `D1? * D1? =< N?` then has an unbound reader `D1?` *inside* the `*` expression.  `execGuard`'s top-level dereference does not descend into the expression structure, so the blocked reader was invisible there; `_evaluateGuard`'s `evaluateNumeric` hit the unbound reader, returned null, and the comparison returned `failure`.  Per the guards reference (comparison guards suspend on unbound-reader operands), it must **suspend**.  Fix in `glp_runtime/lib/bytecode/runner.dart`: `_evaluateGuard` now collects the addresses of readers that block numeric/constant evaluation (`blockedReaders`); when evaluation fails with that set non-empty, it adds them to `cx.U` and returns `GuardResult.suspend`, which `execGuard` maps to `nextClause` (suspension is negation-invariant).  `evaluateNumeric`/`evalConst` also now consult the clause-try tentative bindings (`cx.sigmaHat`) before the heap, mirroring `_dereferenceWithTracking`, so a guard sees writer bindings made during this clause's head match.

**Bug 2 — `=/2` was being *called* backwards in the P99 code, and its declaration had the modes reversed** (instance 2: `pt1`/`string_tree`).  The root `=` procedure is, correctly, `X? = X.` — argument 1 delivers to the caller's writer, argument 2 receives the data; the sanctioned call form is `Writer = Data` (manual §8: "`X = T` assigns `T` on the right to the writer `X` on the left").  The `pt1` body wrote it inverted, `R4? = Rest` (reader in the data position, writer on the right).  Under the paper's term-matching table (IGLP `concurrent-glp.tex`, Term Matching) this puts goal reader `R4?` against head reader `X?` — reader×reader has no writer MGU, so the only clause fails and the goal deadlocks.  Written the sanctioned way, `Rest = R4?`, it aliases immediately (`{Rest := X?, X := R4?}`) and `Rest?` sees the value when `R4` is later bound.  Fixes: corrected the call to `Rest = R4?` in `programs/p99/btrees/p67.glp` (`pt1`) and in the probe file; and corrected the **procedure declaration** from `=(_?, _)` (input reader / output — the reverse of the clause, which is why the type-checker had been *accepting* the broken call form and *rejecting* the sanctioned one) to `=(X, X?)` (wildcard types are no longer permitted; typed per the clause).  Root `self.glp` clause unchanged.  This declaration correction is the one change to a system predicate — approved by Udi, 2026-07-22.

No runtime change was needed for instance 2; no `=` clause change was needed for either instance.  (An earlier attempt in this session flipped the clause to `X = X?.` on a misdiagnosis; reverted after Udi corrected it.)

**Acceptance goals — all pass:**
```
walkd(35, 4, R).                       → R = 6
prime_factors(315, R).                 → R = [3, 3, 5, 7]
goldbach(28, R).                       → R = found(g(5, 23))
string_tree([a,'(',b,',',c,')'], T).   → T = t(a, t(b, [], []), t(c, [], []))
```
(`nil` prints as `[]` — the atom and empty list share a representation; structurally identical to the report's `t(a, t(b,nil,nil), t(c,nil,nil))`.)

**Suites:** REPL `run_all_tests.sh` **552/552**; Dart `dart test` **487 passed, 11 skipped, 0 failed**.  Added regression section **A32** (walkd expression-guard suspend, oncec `=/2` aliasing, walk pf-clause-4 shape, gst p67-chain) and updated the 8 pre-existing A23–A26 checks that had asserted the sanctioned `Writer = Value` form is ill-typed — under the corrected declaration those goals now succeed.

**Out of scope, noted:** `programs/examples/concurrent_prolog/cp_meta.glp` fails to load on a guard-position `=` reducibility error — **pre-existing** (fails identically before these changes), not a regression, untested file.  The `_Fs` named-anonymous codegen aside above is unaddressed (separate codegen path).
