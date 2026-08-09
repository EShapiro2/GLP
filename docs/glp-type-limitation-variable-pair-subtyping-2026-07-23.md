# GLP type-system limitation: writer/reader variable pairs require identical base types (no subtyping)

**Date:** 2026-07-23
**Found by:** P99 Code session (transcript `1aac4f42-2a7a-40e9-9acf-645934e6090f`), in P23–P25 (lottery) and again when consolidating the isomorphism check shared by P85 and P94.
**Owner:** TGLP (the type checker, `lib/analysis`).
**Status:** Open. Reported to TGLP via Udi. Not blocking — the affected code parameterises over the element type instead.

## Summary

A writer/reader variable pair is required to have the **same base type**.  Declared subtyping — in particular `Integer <: Constant` (and `Real`, `Number`, `String` `<: Constant`) — is **not** applied to a variable pair, at any level: not through a list or compound constructor, and not even for a single element.  A writer producing `Integer` cannot fill a reader expecting `Constant`.  (Literal terms are treated differently — an integer literal *is* accepted at a `Constant` position.)

The practical effect: code written against `Constant` cannot be reused, by call or by a copy/convert, on `Integer` data.  Sharing across the two requires parameterising over the element type.

## Observed behaviour

Loading the module below reports two pair errors and accepts the literal:

```prolog
IntList ::= [] ; [Integer | IntList].
CList   ::= [] ; [Constant | CList].

% (1) ACCEPTED: an integer literal list at a Constant-list position.
exported procedure lit(CList).
lit([1, 2, 3]).

% (2) REJECTED: a single Integer writer paired with a Constant reader.
exported procedure elem(Integer?, Constant).
elem(X, X?).

% (3) REJECTED: an IntList writer paired with a CList reader.
exported procedure src(IntList).           snk(_).
exported procedure snk(CList?).
exported procedure thru(Constant).
thru(R?) :- src(L), snk(L?), R = ok.
```

Errors:

```
Head of elem is not well-typed:
  Variable pair (X, X?) not dual: writer=(Constant, ↑), reader=(Integer?, ↓)
  ... Types must have same base: Constant vs Integer

Variable pair (L, L?) not dual across clause:
  Body variable pair: writer type IntList is not a subtype of CList
```

`lit/1` loads with no error.  So the checker coerces an integer **literal** to `Constant`, but treats an `Integer` **variable** as incompatible with a `Constant` position — the pair check demands an identical base type rather than `writer_base <: reader_base`.

## Why this is a limitation, not merely a style choice

`Integer` is a subtype of `Constant`: every integer is a constant, and a `Constant`-typed context can hold an integer at run time (the run time has no separate representation).  A reader that will only *read* a value as a `Constant` is safe to receive an `Integer` writer — reading demands less than the writer guarantees.  The natural rule for a writer/reader pair is covariant: accept the pair when `writer_base <: reader_base`.  The checker instead requires `writer_base = reader_base`.

Two consequences follow, both hit in P99:

1. **No conversion escape hatch.**  One cannot even copy `Integer` data into a `Constant`-typed structure: a clause `conv([X | Xs], [X? | Ys]) :- ...` with an `Integer` input element and a `Constant` output element is the rejected single-element pair (2) above.  So a `CList`-typed procedure cannot be reused on integer data by converting the data first.

2. **Forced parameterisation.**  The only way to write one procedure serving both `Constant` and `Integer` data is to parameterise over the element type — `Stream(X)`, `Edge(X) ::= e(X, X)`, etc. — and instantiate at `Constant` and at `Integer` separately.  This works (per-instantiation checking, Moded-Types §Parameterised Procedure Declarations), but forces the abstraction even where a plain `Constant` signature would be the natural one.

## Where it bit, in P99

- **P24 (lottery).**  `range/3` produces an `IntList` pool; feeding it to a `CList`-typed `rnd_select` was rejected (case 3).  Fixed by parameterising the selection over `Stream(X)` and instantiating `rnd_select` at `Integer` for the pool and at `Constant` for P23/P25.  (See `glp-paper-code-map.md` owner P99; `programs/p99/lists/{p23,p24}.glp`.)

- **P85 / P94 isomorphism.**  P85 checks graph isomorphism on atom nodes (`Constant`); P94's K-regular dedup checks it on integer nodes.  The logic is identical and `Integer <: Constant`, so one shared `iso` over `Constant` would seem to serve both — but a `Constant`-typed `iso` cannot take the integer edges (case 2/3), and no copy can retype them.  The shared check must therefore be parameterised over the node/edge element type and instantiated per caller.

## Question for TGLP

Should a writer/reader variable pair admit `writer_base <: reader_base` (covariant), so that an `Integer` writer may fill a `Constant` reader (and, through constructors, `IntList` fill `CList?`)?  If the identical-base rule is deliberate (e.g. to keep pair-checking a pure equality and avoid a subtyping lattice at variable pairs), that is worth stating in the type-system spec, since the natural expectation — reinforced by the literal coercion — is that `Integer` is usable wherever a `Constant` is read.

A minimal repro is the module above.
