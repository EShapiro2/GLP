# Consistent Paths

**Paper Reference**: Definition 5.2

## Moded Paths

A **moded path** is a sequence extracted from a moded term tree:
```
(0, m₀) → f/n → (i₁, m₁) → ... → (iₖ, mₖ) → leaf
```

Where each step has an argument index and structural mode, and the leaf is a constant or variable.

For a moded term T, **paths(T)** denotes the set of all its moded paths.

## Definition 5.2 (Consistent Paths)

> Let x be a moded term path and y a GLP type path, with lengths |x| and |y| respectively. Then x and y are **consistent** if one of the following holds:
>
> 1. **Equal length**: |x| = |y| and x and y are identical except for their last symbols, which are consistent per the primitive correspondence table.
>
> 2. **Term path shorter (variable at leaf)**: |x| < |y| and x is a prefix of y except for its last symbol, which is:
>    - (a) a reader X? and the structural mode at that position is ↓, or
>    - (b) a writer X and the structural mode at that position is ↑.
>
> 3. **Type path shorter (wildcard in type)**: |y| < |x| and y ends at a wildcard state (_ or _?), and:
>    - (a) if _?, the structural mode at position |y| in the term path is ↓, or
>    - (b) if _, the structural mode at position |y| in the term path is ↑.
>
> The remainder of the term path beyond position |y| is not examined; the wildcard accepts the entire subterm.

## Primitive Correspondence Table

| Term | Type State | Dual State |
|------|------------|------------|
| Writer X | _ | _? |
| Reader X? | _? | _ |
| Integer literal | Integer | Integer? |
| Real literal | Real | Real? |
| Numeric literal | Number | Number? |
| String literal | String | String? |
| Constant c | exact match | exact match |

The wildcard states `_` and `_?` accept any term of the appropriate mode.

## Variable Type Assignment

When a path ends in a variable (Case 2), the variable is assigned the type at that position:
- Reader X? at position with type T? → X? has type T?
- Writer X at position with type T → X has type T
