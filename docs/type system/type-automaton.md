# Type Automaton

**Paper Reference**: Definitions 5.11, 5.12, 5.13

## Definition 5.11 (Type Automaton)

> Given a typed GLP program P = (Cs, D), the type automaton A_D = (Q, Σ, δ, q₀, F) is defined as follows:

### States Q

1. **User-defined type states**: For each type name T defined in D, two states: T and T? (its dual).

2. **Procedure states**: For each procedure p/n declared in D, one state: p/n.

3. **Primitive type states**: The pairs Integer/Integer?, Real/Real?, Number/Number?, String/String?, and _/_?.

4. **Final states**: A distinguished final state ✓ (acceptance).

### Alphabet Σ

Transition labels are tuples (f, n, i, m) where f is a functor, n ≥ 0 is the arity, i ∈ {1,...,n} is the argument position, and m ∈ {↑, ↓} is the mode. For constants, the label is (c, 0, m).

### Initial State

The initial state for type checking a term of type T is T; for a procedure call p(...), it is p/n.

### Final States F

The states `_`, `_?`, and `✓` are final. Primitive type states become final after matching a literal of the appropriate type.

## Definition 5.12 (Transition Function)

### Procedure Transitions

For a procedure declaration `p(T₁, ..., Tₙ)`:
```
δ(p/n, (p, n, i, mᵢ)) = Tᵢ? if Tᵢ is input (written Tᵢ? in declaration)
                       = Tᵢ  if Tᵢ is output
```
The mode mᵢ is ↓ for input arguments, ↑ for output arguments.

### Type Definition Transitions

For a type definition `T ::= A₁ ; ... ; Aₖ`, each alternative Aⱼ contributes transitions from state T:
- If Aⱼ is a constant c: `δ(T, (c, 0, ↑)) = ✓`
- If Aⱼ is a compound f(S₁, ..., Sₙ): for each i, `δ(T, (f, n, i, mᵢ)) = Sᵢᵐⁱ`

### Dual Transitions

For each transition `δ(T, (f, n, i, m)) = S`, the dual automaton has:
```
δ(T?, (f, n, i, m̄)) = S?
```
where `↑̄ = ↓` and `↓̄ = ↑`.

### Primitive Type Transitions

- `δ(Integer, (k, 0, ↑)) = ✓` for any integer literal k
- `δ(String, (s, 0, ↑)) = ✓` for any string literal s
- Dual states have the same transitions with mode ↓.

### Wildcard States

The states `_` and `_?` accept any term:
- `_` accepts any produced term (writer or ground term with mode ↑)
- `_?` accepts any consumed term (reader or ground term with mode ↓)

**These are final states; no outgoing transitions are required.**

## Definition 5.13 (Dual Type Automaton)

> For each type T, the dual type T? has a corresponding automaton. The automaton for T? is derived from the definition of T?, which may be implicit or explicit:
>
> **Implicit dual (default):** If no explicit dual is provided, the automaton for T? is obtained from the automaton for T by:
> 1. Replacing each state S with its dual state S?
> 2. Replacing each mode annotation: ↑ becomes ↓, and ↓ becomes ↑
>
> **Explicit dual:** If an explicit dual definition T? is provided (as for abstract data types like channels and difference lists), the automaton for T? is constructed directly from that definition, preserving the internal structure.

### Example: Explicit Dual for Channel

```
Channel ::= ch(Stream?, Stream).
Channel? ::= ch(Stream?, Stream)?.
```

With the explicit dual, both `Channel` and `Channel?` have the same internal structure:
- Position 1: mode ↓ (consuming Stream?)
- Position 2: mode ↑ (producing Stream)

This preserves the invariant that position 1 is always the input stream and position 2 is always the output stream, regardless of whether a Channel is produced or consumed.

## Determinism Requirement

The type automaton must be deterministic: from any state, each transition label leads to at most one target state. This requires that type definitions have **disjoint alternatives** — alternatives must be distinguishable by their top-level functor.

Illegal overlapping definitions:
```
Any ::= _ ; _?.           % overlapping: both accept all terms
Ambiguous ::= _ ; Integer. % overlapping: integers match both
```

## Example: Stream Type Automaton

For `Stream ::= [] ; [_|Stream]`:

State **Stream** (producer view):
- `δ(Stream, ([], 0, ↑)) = ✓`
- `δ(Stream, (".", 2, 1, ↑)) = _`
- `δ(Stream, (".", 2, 2, ↑)) = Stream`

State **Stream?** (consumer view, dual):
- `δ(Stream?, ([], 0, ↓)) = ✓`
- `δ(Stream?, (".", 2, 1, ↓)) = _?`
- `δ(Stream?, (".", 2, 2, ↓)) = Stream?`
