# Moded Head

**Paper Reference**: Definition 5.5

## Definition 5.5 (Moded Head)

> Given a head H, a **moded head** H' is obtained by:
>
> 1. Constructing an I/O-moded term corresponding to H, then
>
> 2. For each variable, if its form does not match its position's structural mode, replacing it with its paired variable.

## I/O-Moded Term

An **I/O-moded term** for a procedure `p(T₁, ..., Tₙ)` has:
- Root mode ↓ (the head is consumed by matching against goals)
- Input arguments (declared as `Type?`) have structural mode ↓
- Output arguments (declared as `Type`) have structural mode ↑
- Nested modes propagate according to type structure, flipping at each `?`

## Variable Replacement (Step 2)

| Structural Mode | Variable Form | Action |
|-----------------|---------------|--------|
| ↓ | Reader X? | Keep |
| ↓ | Writer X | Replace with X? |
| ↑ | Writer X | Keep |
| ↑ | Reader X? | Replace with X |

## Remark: Mode Correspondence

For a moded head H' constructed from head H with type D: the structural mode at any position p in H' equals the mode at position p in D. This follows from the construction.

## Example

Head: `merge([X|Xs], Ys, [X?|Zs?])`

Type: `procedure merge(Stream?, Stream?, Stream).`

Step 1 — Build I/O-moded term (modes from type):
```
↓merge(↓[↓X|Xs], Ys, ↑[↑X?|Zs?])
```

Step 2 — Replace variables where form ≠ structural mode:
- X at ↓ position: writer → replace with X?
- Xs at ↓ position: writer → replace with Xs?
- Ys at ↓ position: writer → replace with Ys?
- X? at ↑ position: reader → replace with X
- Zs? at ↑ position: reader → replace with Zs

Result:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```
