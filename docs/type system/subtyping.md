# Subtyping

**Paper Reference**: Definitions 5.16, 5.17, 5.18, 5.19, 5.20, Section 5.3

## Motivation

Well-typing requires type consistency for variable pairs. The strict requirement is that the type of X be the dual of the type of X?. But this is stronger than necessary. All that is needed is that the type of X be a **subtype** of the dual of the type of X?.

Informally: anything produced by writer X can be consumed by reader X?.

## Definition 5.16 (Simple Prefix)

> A **simple prefix** of an output type T is a path in T's type automaton starting from T that contains no mode inversions. A simple prefix ends when it reaches either:
> - The produced primitive `_` (which accepts any produced term), or
> - A mode inversion point: any position whose type is marked with `?`

Since the type automaton is deterministic, each position along a simple prefix corresponds to exactly one functor.

## Definition 5.17 (Prefix Acceptance)

> A simple prefix p of type A is **accepted** by type B if B has a simple prefix q such that:
> - p and q have identical functor/position structure, except possibly at their endpoints, where:
> - If p ends at `_`, then q must also end at `_`.
> - If p ends at a specific output type S, then q must end at S or at `_`.
> - If p ends at a mode inversion point S?, then q must also end at a mode inversion point at the same position.

## Definition 5.18 (Subtyping)

> Let A and B be GLP output types. We say A is a **subtype** of B if:
>
> 1. Every simple prefix of A is accepted by B.
>
> 2. For every mode inversion point in A reached by a simple prefix—say, type A'? at that position—there is a corresponding mode inversion point B'? in B at the same position, and **B' is a subtype of A'**.

Condition 2 is **coinductive**: subtyping at mode inversion points is checked recursively with the containment direction **reversed** (contravariance at input positions).

## Definition 5.20 (Well-Typed Variable Pair)

> Let X and X? be a variable pair in a GLP clause, with X assigned type S and X? assigned type T?. The pair is **well-typed** if S is a subtype of T.

Subtyping generalizes exact duality: if S = T, then trivially S is a subtype of T.

## Example (Paper Example 5.19)

File system monitor with operations:
```
FileOp ::= read(Path?, Content) ; write(Path?, Content?) ; delete(Path?).
```

Read-only client:
```
ReadOp ::= read(Path?, Content).
```

**Verify ReadOp <: FileOp:**

Condition 1: The simple prefix of ReadOp (functor `read/2`) is accepted by FileOp (which includes `read/2`).

Condition 2: At the mode inversion point (Path?), both types have identical Path, so the contravariant check holds trivially.

Thus `ReadOp <: FileOp`, and a writer of type ReadOp paired with a reader of type FileOp? is well-typed.

## Variance

The coinductive structure embodies standard variance:
- **Covariant** in output positions: subtype's functors must be contained in supertype's
- **Contravariant** in input positions: at mode inversion points, containment direction reverses

This matches session type subtyping where output types are covariant and input types are contravariant.
