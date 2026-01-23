# Well-Typed Moded Term

**Paper Reference**: Definition 5.4

## Definition 5.4 (Well-Typed Moded Term)

> A moded term T is **well-typed** by a GLP type D if:
>
> 1. For each term path x ∈ paths(T) there is a consistent path y ∈ paths(D), and
>
> 2. For every pair of variables in T, their types as determined by D are dual.

## Variable Type Determination

When checking path consistency (Definition 5.2), each variable is assigned a type based on the position where it appears in the type automaton. Two types are **dual** if:
- They have the same base name, and
- One is the input form (T?) and one is the output form (T)

Examples of dual type pairs: `Stream`/`Stream?`, `Integer`/`Integer?`, `_`/`_?`.

## Algorithm

```
isWellTyped(modedTerm, type):
  variableTypes = {}
  
  for path in paths(modedTerm):
    if not hasConsistentPath(path, type):
      return (false, "path inconsistent: <details>")
    if path ends in variable V:
      assignedType = typeAtPosition(path, type)
      variableTypes[V] = assignedType
  
  for each variable pair (X, X?) in modedTerm:
    if not areDual(variableTypes[X], variableTypes[X?]):
      return (false, "variable pair not dual: <details>")
  
  return (true, null)
```

## Error Reporting

Errors are reported as simple strings with sufficient detail to locate the problem. No elaborate error class hierarchy is required.
