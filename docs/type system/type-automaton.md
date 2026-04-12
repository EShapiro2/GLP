# Type Automaton

**Paper Reference**: Definitions 5.11, 5.12, 5.13; Appendix B.6

## Parameterized Types

Parameterized types are expanded to monomorphic types before automaton construction (see typed-program.md, Section "Parameterized Types"). The automaton is built only from the expanded monomorphic definitions. No changes to automaton construction, well-typing, or subtyping are needed.

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

The mode mᵢ for position i is determined by the type Sᵢ:
- If Sᵢ is written without `?` (e.g., `Stream`), mode is ↑ (produce)
- If Sᵢ is written with `?` (e.g., `Stream?`), mode is ↓ (consume)

### Dual Transitions

For each transition `δ(T, (f, n, i, m)) = S`, the dual automaton has:
```
δ(T?, (f, n, i, m̄)) = S?
```
where `↑̄ = ↓` and `↓̄ = ↑`.

This means that when a type T is consumed (as T?), all modes are complemented and all target states are dualized.

### Primitive Type Transitions

- `δ(Integer, (k, 0, ↑)) = ✓` for any integer literal k
- `δ(String, (s, 0, ↑)) = ✓` for any string literal s
- Dual states have the same transitions with mode ↓.

### Wildcard States

The states `_` and `_?` accept any term:
- `_` accepts any produced term (writer or ground term with mode ↑)
- `_?` accepts any consumed term (reader or ground term with mode ↓)

**These are final states; no outgoing transitions are required.**

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

## Example: Channel Type Automaton

Per paper Appendix B.6, the Channel type is:

```
Channel ::= ch(Stream, Stream?).
```

A produced Channel has:
- Position 1: an output stream (Stream, mode ↑) — the stream you send to your peer
- Position 2: an input stream (Stream?, mode ↓) — the stream you receive from your peer

State **Channel** (producer view):
- `δ(Channel, (ch, 2, 1, ↑)) = Stream`
- `δ(Channel, (ch, 2, 2, ↓)) = Stream?`

State **Channel?** (consumer view, dual — modes complemented, targets dualized):
- `δ(Channel?, (ch, 2, 1, ↓)) = Stream?`
- `δ(Channel?, (ch, 2, 2, ↑)) = Stream`

Note: When consuming a Channel (as Channel?), the roles of the two streams are swapped from the consumer's perspective. Position 1 becomes an input (the stream your peer sends to you), and position 2 becomes an output (the stream you send to your peer).

### Channel Operations (from paper)

```
procedure new_channel(Channel, Channel).
new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).

procedure send(_?, Channel?, Channel).
send(X, ch(In, [X?|Out?]), ch(In?, Out)).

procedure receive(_, Channel?, Channel).
receive(X?, ch([X|In], Out?), ch(In?, Out)).
```

The `new_channel` clause produces two cross-linked channels. In the moded form per the paper:
```
↑new_channel(↑ch(↑Xs, ↓Ys?), ↑ch(↑Ys, ↓Xs?)).
```

Both arguments are outputs (mode ↑). Within each Channel:
- Position 1 has mode ↑ (producing Stream)
- Position 2 has mode ↓ (consuming Stream?)

The reader `Xs?` at position 1 (mode ↑) becomes writer `Xs` in the moded head.
The writer `Ys` at position 2 (mode ↓) becomes reader `Ys?` in the moded head.
