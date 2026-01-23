# Well-Typed Clause

**Paper Reference**: Definition 5.7

## Definition 5.7 (Well-typed, input accepting clause)

> Let C = (H :- B) be a GLP clause and D a GLP type for all its procedures. Then C is **well-typed** by D if:
>
> 1. There is a moded head H' corresponding to H that is well-typed by D.
>
> 2. For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.
>
> 3. For every pair of dual variables X and X? in C:
>    - (a) If both occur in H, or both occur in B, they are assigned dual types by D.
>    - (b) If one occurs in H and the other in B, they are assigned the same type by D.
>
> In addition, C **accepts an input path** x ∈ paths(D) if H' has a path consistent with x.

## Produced Moded Term

A **produced moded term** for a body atom has root mode ↑ (the clause produces these goals). Argument modes follow the declared types as usual.

## Variable Type Rules

The location of variables determines the type relationship required:

| Writer Location | Reader Location | Required Relationship |
|-----------------|-----------------|----------------------|
| Head | Head | Dual types |
| Body | Body | Dual types |
| Head | Body | Same type |
| Body | Head | Same type |

## Error Reporting

Errors are reported as simple strings with sufficient detail to locate the problem. No elaborate error class hierarchy is required. Example error messages:

- "Head not well-typed: path inconsistent at position 2"
- "Body atom 3 not well-typed: variable X has wrong mode"
- "Variable pair (Y, Y?) not dual: head has Stream, body has Integer"
