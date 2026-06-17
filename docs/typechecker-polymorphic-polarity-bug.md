# Typechecker bug: reader/writer polarity not re-checked at polymorphic instantiation

**Date:** 2026-06-16
**Component:** GLP type checker (per-instantiation clause-template checking)
**Severity:** Soundness gap — accepts programs that deadlock/strand at runtime.

## Summary

When a procedure parameter has a polymorphic element type (e.g. `Stream(X)?`,
`Channel(X,Y)?`), reader/writer (producer/consumer) polarity obligations inside
that procedure's body are discharged against the **abstract** type variable and
are **not re-discharged** when the variable is instantiated to a concrete type
at a call site. A concrete parameter type carries the obligation to the wiring
site and the clash is caught; a polymorphic one absorbs it into the variable and
it is never enforced.

## Minimal repro (differential)

Two files identical except the consumer's declared parameter type.

`producer` emits `befriend(Constant, Response)` (slot 2 = **writer**);
the consumer body matches `befriend(From, Resp?)` (slot 2 = **reader**);
`go` wires them through one stream `S`, so the polarities must be dual.

**bug1** — concrete consumer `procedure pconsumer(Stream(ConsMsg)?)`:

```
Type checking failed:
  Head of producer is not well-typed: … at line 28           (A)
  Variable pair (S, S?) not dual across clause:
    Stream<ProdMsg> is not a subtype of Stream<ConsMsg> … at line 38   (B)
```

**bug2** — only change, polymorphic consumer `procedure pconsumer(Stream(X)?)`:

```
Type checking failed:
  Head of producer is not well-typed: … at line 23           (A)
```

(A) is identical, unrelated noise in `producer`'s own head. The decisive
difference is **(B)**: the producer/consumer duality clash at the `go` wiring is
reported for the concrete consumer and **silently dropped** for the polymorphic
one. The checker reports *both* errors in bug1, so it does not bail on the first
error — bug2's missing (B) is a genuine acceptance, not early exit.

Repro files: `programs/tests/min_polarity_bug.glp` (bug1),
`programs/tests/min_polarity_bug2.glp` (bug2).
Run: `echo -e 'load ../programs/tests/min_polarity_bug2.glp\n:quit' | dart run bin/glp_repl.dart` from `glp_runtime/`.

## Expected vs actual

- **Expected:** at `go([]) :- producer(S), pconsumer(S?).` the checker
  instantiates `X := ProdMsg` (by unifying `Stream(X)` with `Stream(ProdMsg)`)
  and re-checks `pconsumer`'s body under that instantiation, discovering that
  `befriend(From, Resp?)` (reader) cannot consume `befriend(Constant, Response)`
  (writer) — same error (B) as bug1.
- **Actual:** no error (B). The body's polarity obligation was discharged once
  against abstract `X` and never re-discharged at `X := ProdMsg`.

## Root cause

The per-instantiation clause-template check unifies the parameter's **shape**
(`Stream(X)` vs `Stream(ProdMsg)` ⇒ `X := ProdMsg`) but does not propagate the
instantiation into the **mode/polarity** obligations carried by occurrences of
`X`'s element inside the body. Reader/writer duality is a property of the
concrete constructor argument (`Response` vs `Response?`); while the element type
is the abstract variable `X`, there is no concrete argument to clash with, so the
obligation is vacuously satisfied and then forgotten.

## Real-world manifestation

`programs/book/social_graph/typed_ui_mediator.glp`:

```prolog
procedure ui_mediator(Constant?, Channel(X,Y)?, Channel(X,Y)?, PendingList?, Constant?).
```

The agent↔mediator channel message type is the variable `X`. The mediator body
captures the cold-call response slot with one polarity while the agent produces
it with the opposite polarity (`befriend(Constant, Response?)` reader vs the
writer the consumer needs). Because the channel is `Channel(X,Y)`, the polarity
obligation is discharged against abstract `X` and never instantiated-and-rechecked
at the concrete `befriend(Constant, Response?)`. Result: the program type-checks
but the response writer never reaches the agent's decision point — the befriend
round-trip strands at runtime (bob accepts, no `connected` on either side).

## Suggested fix direction

When the per-instantiation check binds a parameter type variable to a concrete
type at a call site, re-run the clause-template's **mode/polarity** discharge
under that substitution — not only the structural/shape unification. Equivalently:
treat reader/writer polarity of an element type as part of the obligation that
must survive instantiation, so that `X := ProdMsg` forces the body's
`befriend(_, Resp?)` to be checked against `ProdMsg`'s writer slot.

A test for the fix: bug2 must report error (B), matching bug1.
