# Moded Term

**Paper Reference**: Definition 5.1

## Definition 5.1 (Moded Term, Dual)

> Given a GLP term T, a **moded term** T' corresponding to T is the result of adding one of two mode annotations, consume ↓ or produce ↑, to T and to every non-variable subterm of T.
>
> Given a moded term T, its **dual** T? is obtained from T by flipping every mode annotation and replacing every variable by its paired variable.

## Modes

```
Mode = ↓ (consume) | ↑ (produce)

flip(↓) = ↑
flip(↑) = ↓
```

## Moded Term Structure

A moded term is:
- A mode-annotated compound: `m:f(T₁, ..., Tₙ)` where m is a mode and Tᵢ are moded terms
- A mode-annotated constant: `m:c` where m is a mode and c is a constant
- A variable: `X` (writer) or `X?` (reader) — variables have no explicit mode annotation

## Dual Operation

```
dual(m:f(T₁, ..., Tₙ)) = flip(m):f(dual(T₁), ..., dual(Tₙ))
dual(m:c)              = flip(m):c
dual(X)                = X?
dual(X?)               = X
```

The dual is an involution: `dual(dual(T)) = T`.

## Implicit Variable Mode

Variables have implicit mode based on their form:
- Writer `X` → implicit produce ↑
- Reader `X?` → implicit consume ↓

## Example

Term: `merge([X|Xs], Ys, [X?|Zs?])`

A moded term corresponding to it:
```
↓merge(↓[↓X|Xs], Ys, ↑[↑X?|Zs?])
```

Its dual:
```
↑merge(↑[↑X?|Xs?], Ys?, ↓[↓X|Zs])
```
