# Moded Head

**Paper Reference**: Definition 5.5

## Definition 5.5 (Moded Head)

> Given a head H for procedure p with type declaration p(T₁, ..., Tₙ), the **moded head** H' is constructed as follows:
>
> 1. **Assign modes:** For each argument position i, assign mode ↓ if Tᵢ is an input type (written with trailing `?`) and mode ↑ if Tᵢ is an output type. Modes propagate through nested term structure according to the type definition, complementing at embedded `?` annotations.
>
> 2. **Complement variables:** Replace each variable with its paired variable.

## I/O-Moded Term (Step 1)

An **I/O-moded term** for a procedure `p(T₁, ..., Tₙ)` has:
- Root mode ↓ (the head is consumed by matching against goals)
- Input arguments (declared as `Type?`) have mode ↓
- Output arguments (declared as `Type`) have mode ↑
- Nested modes propagate according to type structure, complementing at each `?`

## Variable Complementation (Step 2)

**Unconditional:** Every variable is replaced by its paired variable.

| Original Variable | In Moded Head |
|-------------------|---------------|
| Writer X | Reader X? |
| Reader X? | Writer X |

This reflects the semantic inversion of head variables: head writers become readers in the moded head (serving as input—bound by the goal), while head readers become writers in the moded head (serving as output—will be bound by the body).

## Remark: Mode Correspondence

For a moded head H' constructed from head H with type D: the mode at any position p in H' equals the mode at position p in D. This follows from the construction.

## Example

Head: `merge([X|Xs], Ys, [X?|Zs?])`

Type: `procedure merge(Stream?, Stream?, Stream).`

Step 1 — Assign modes (from type):
```
↓merge(↓[↓X|Xs], Ys, ↑[↑X?|Zs?])
```

Step 2 — Complement each variable (unconditional):
- X → X?
- Xs → Xs?
- Ys → Ys?
- X? → X
- Zs? → Zs

Result:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```
