# Module: well-typed-clause

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.6 (lines 311-321), Example (lines 323-349)

## Purpose

Determines when a GLP clause is well-typed by a type D, and defines when a clause "accepts" an input path.

## Dependencies

- `moded-head` — modedHead(H, decl)
- `moded-term` — producedTerm(A)
- `well-typed-term` — isWellTyped(T, D)
- `path-consistency` — consistent(x, y)

## Paper Definition

### Definition 4.6: Well-typed Clause (lines 311-321)

> Let C = (H :- B) be a GLP clause and D a GLP type for all its procedures.
> Then C is **well-typed** by D if:
>
> 1. There is a moded head H' corresponding to H that is well-typed by D.
> 2. For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.
> 3. Every pair of variables that occur in C are assigned complementary types by D.
>
> In addition, C **accepts** an input path x ∈ paths(D) if H' has a path consistent with x.

## Three Conditions

### Condition 1: Head Well-Typed

Construct moded head H' (Definition 4.5) and verify it is well-typed by D (Definition 4.4).

The moded head:
- Is I/O moded (root ↓, at most one inversion to ↑)
- Has all variables flipped to their pairs

### Condition 2: Body Atoms Well-Typed

For each body atom A, construct the **produced** moded term A' (all modes ↑) and verify it is well-typed by D.

Body atoms are produced because they represent goals being called—the clause produces these goals.

### Condition 3: Complementary Variable Types

Every variable pair (X, X?) must be assigned complementary types:
- If X has type T, then X? must have type T?
- If X has type _, then X? must have type _?

## Accepts Predicate

A clause C **accepts** an input path x ∈ paths(D) if the moded head H' has a path consistent with x.

This is used in Definition 4.7 (Contravariance): every input path must be accepted by some clause.

## Example (lines 323-349)

### Clause

```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

### Condition 1: Head well-typed

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Each path in H' is consistent with a path in paths(D).

### Condition 2: Body atoms well-typed

Body atom as produced moded term:
```
↑merge(Ys?, Xs?, Zs)
```

Paths are consistent with type `merge(Stream?, Stream?, Stream)`:
- `Ys?` and `Xs?` are readers at consumed positions
- `Zs` is a writer at a produced position

### Condition 3: Complementary variable types

| Variable Pair | Type of X | Type of X? | Complement? |
|---------------|-----------|------------|-------------|
| X / X? | `_` (produced) | `_?` (consumed) | ✓ |
| Xs / Xs? | `Stream` | `Stream?` | ✓ |
| Ys / Ys? | `Stream?` | `Stream` | ✓ (note: inverted) |
| Zs / Zs? | `Stream` | `Stream?` | ✓ |

All conditions satisfied → clause is well-typed.

## Interface

### `ClauseCheckResult checkClause(Clause c, TypeEnv d)`

Checks if clause C is well-typed by type environment D.

**Returns:**
- `isWellTyped`: true if all three conditions hold
- `variableTypes`: map from variable name to assigned type
- `errors`: list of specific violations found

### `bool accepts(Clause c, TypePath inputPath, TypeEnv d)`

Returns true if clause C accepts input path x.

**Implementation:**
1. Construct moded head H' from C
2. Check if any path in paths(H') is consistent with inputPath

### Algorithm

```
checkClause(c, d):
  errors = []
  varTypes = {}
  
  // Get procedure declaration
  decl = d.getProcedure(c.head.functor, c.head.arity)
  
  // Condition 1: Head well-typed
  h' = modedHead(c.head, decl)
  result1 = checkWellTyped(h', decl.type)
  if not result1.success:
    errors.add("Head not well-typed: " + result1.reason)
  varTypes.addAll(result1.varTypes)
  
  // Condition 2: Body atoms well-typed
  for atom in c.body:
    atomDecl = d.getProcedure(atom.functor, atom.arity)
    a' = producedTerm(atom)
    result2 = checkWellTyped(a', atomDecl.type)
    if not result2.success:
      errors.add("Body atom not well-typed: " + result2.reason)
    varTypes.addAll(result2.varTypes)
  
  // Condition 3: Complementary variable types
  for varName in allVariables(c):
    typeOfWriter = varTypes[varName]
    typeOfReader = varTypes[varName + "?"]
    if not areComplementary(typeOfWriter, typeOfReader):
      errors.add("Variable pair not complementary: " + varName)
  
  return ClauseCheckResult(errors.isEmpty, varTypes, errors)

accepts(c, inputPath, d):
  decl = d.getProcedure(c.head.functor, c.head.arity)
  h' = modedHead(c.head, decl)
  for termPath in paths(h'):
    if consistent(termPath, inputPath):
      return true
  return false
```

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft from paper |
