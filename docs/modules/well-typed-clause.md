# Module: well-typed-clause

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.8 (lines 311-321), Example (lines 323-349)

## Purpose

Determines when a GLP clause is well-typed by a type D, and defines when a clause "accepts" an input path.

## Dependencies

- `moded-head` — modedHead(H, decl)
- `moded-term` — producedTerm(A)
- `well-typed-term` — isWellTyped(T, D)
- `path-consistency` — consistent(x, y)

## Paper Definition

### Definition 4.8: Well-typed Clause (lines 311-321)

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

Construct moded head H' (Definition 4.6) and verify it is well-typed by D (Definition 4.5).

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

This is used in Definition 4.10 (Contravariance): every input path must be accepted by some clause.

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

### Example: INVALID — Condition 1 Violation (Head not well-typed)

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

% Head has integer where Stream? is expected
merge(42, Ys, Zs).
```

**Problem:** Argument 1 has type `Stream?`, but the head has integer `42`. No type path in `Stream?` reaches an integer constant.

**Error:** `HeadNotWellTypedError("No consistent type path for term path ending in 42")`

### Example: INVALID — Condition 2 Violation (Body atom not well-typed)

```
Stream ::= [] ; [_|Stream].
procedure merge(Stream?, Stream?, Stream).

% Body atom passes integer where Stream? expected
merge(Xs, Ys, Zs) :- merge(42, Ys?, Zs?).
```

**Problem:** The body atom `merge(42, Ys?, Zs?)` has integer `42` at argument 1, which expects `Stream?`.

**Error:** `BodyAtomNotWellTypedError("Body atom merge/3: no consistent type path for 42")`

### Example: INVALID — Condition 3 Violation (Non-complementary types)

```
Stream ::= [] ; [_|Stream].
List ::= [] ; [_|List].
procedure foo(Stream?, List).

% X? has type _? (from Stream?), X has type _ (from List)
% These are complementary, OK.

% But consider:
procedure bar(Stream?, List?).
bar([X|Xs], [X?|Ys?]).  % X and X? both readers - INVALID
```

**Problem:** In `bar`, at argument 1 position 1, `X` is a writer getting type `_`. At argument 2 position 1, `X?` is a reader getting type `_?`. But wait—in the moded head, the variables are flipped: writer X becomes reader X?, and reader X? becomes writer X. So both positions try to assign X as output—this is an SRSW violation caught earlier, not a type violation.

Better example:

```
Stream ::= [] ; [_|Stream].
NatStream ::= [] ; [Integer|NatStream].
procedure convert(Stream?, NatStream).

convert([X|Xs], [X?|Ys]) :- convert(Xs?, Ys?).
```

**Problem:** `X` at argument 1 gets type `_` (any consumed element). `X?` at argument 2 gets type `Integer` (produced). But `_` and `Integer` are not complements—`_` would need to complement to `_?`, not `Integer`.

**Error:** `NonComplementaryVariablesError("X has type _, X? has type Integer—not complements")`

## Interface

### `ClauseCheckResult checkClause(Clause c, TypeEnv d)`

Checks if clause C is well-typed by type environment D.

**Preconditions:**
- `c` is a valid GLP clause
- `d` contains procedure declarations for all predicates used in `c`

**Postconditions:** Returns `ClauseCheckResult` where:
- `isWellTyped` is true iff all three conditions of Definition 4.8 hold
- `variableTypes` maps each variable name to its assigned type
- `errors` lists all violations found (empty if well-typed)

**Errors:**
- Throws `UndeclaredProcedureError` if head or any body atom's procedure is not declared in `d`

### `bool accepts(Clause c, TypePath inputPath, TypeEnv d)`

Returns true if clause C accepts input path x (per Definition 4.8).

**Preconditions:**
- `c` is a valid GLP clause
- `inputPath` is a valid type path with root mode ↓
- `d` contains the procedure declaration for `c`'s head predicate

**Postconditions:** Returns true iff the moded head H' has a path consistent with `inputPath`.

**Errors:**
- Throws `UndeclaredProcedureError` if procedure is not declared in `d`

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

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| Procedure not declared in type environment | `UndeclaredProcedureError` |
| Head not well-typed by D | `HeadNotWellTypedError` |
| Body atom not well-typed by D | `BodyAtomNotWellTypedError` |
| Variable pair has non-complementary types | `NonComplementaryVariablesError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft from paper |
| 0.2 | 2025-01-07 | Fix definition numbers |
| 0.3 | 2025-01-07 | Add Error Conditions, remove spurious ref |
| 0.4 | 2025-01-07 | Add negative examples for all three conditions, complete function specs |
