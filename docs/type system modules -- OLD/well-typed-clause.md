# Module: well-typed-clause

**Version**: 0.6
**Date**: 2025-01-10
**Status**: DRAFT
**Paper References**: Definition 4.8 (lines 366-377), Example (lines 382-408)

## Purpose

Determines when a GLP clause is well-typed by a type environment D.

## Dependencies

- `mode` — Mode enum
- `moded-head` — modedHead(), producedTerm()
- `well-typed-term` — checkModedTerm()
- `type-dfa` — ProgramDFA, Automaton, DFAState, buildProgramDFA()
- `type-environment` — TypeEnvironment, ProcDecl

## Definitions

### Definition 4.8: Well-typed Clause (lines 366-377)

> Let C = (H :- B) be a GLP clause and D a GLP type for all its procedures.
> Then C is **well-typed** by D if:
>
> 1. There is a moded head H' corresponding to H that is well-typed by D.
> 2. For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.
> 3. Every pair of variables that occur in C are assigned complementary types by D.

## Three Conditions

### Condition 1: Head Well-Typed

Construct moded head H' using `modedHead(H, decl)` and verify it is well-typed.

The moded head:
- Is I/O moded (root ↓, input args ↓, output args ↑)
- Has all variables flipped to their pairs

### Condition 2: Body Atoms Well-Typed

For each body atom A, construct the **produced** moded term A' using `producedTerm(A, decl)` and verify it is well-typed.

Body atoms are produced because they represent goals being called—the clause produces these goals.

### Condition 3: Complementary Variable Types

Every variable pair (X, X?) in the clause must be assigned complementary types. This is checked by aggregating variable types from the head and all body atoms, then verifying complementarity.

## Public Interface

### Types

#### `class ClauseCheckResult`

```dart
class ClauseCheckResult {
  final bool isWellTyped;
  final Map<String, VariableTypeInfo> variableTypes;
  final List<ClauseError> errors;
}

abstract class ClauseError {}

class HeadNotWellTypedError extends ClauseError {
  final List<TypeError> termErrors;
}

class BodyAtomNotWellTypedError extends ClauseError {
  final int atomIndex;
  final Term atom;
  final List<TypeError> termErrors;
}

class ClauseVariableNotComplementaryError extends ClauseError {
  final String variableName;
  final VariableTypeInfo writerType;
  final VariableTypeInfo readerType;
}
```

### Functions

#### `ClauseCheckResult checkClause(Clause clause, ProgramDFA dfa, TypeEnvironment env)`

Checks if a clause is well-typed per Definition 4.8.

**Preconditions:**
- `clause` is a valid GLP clause
- `dfa` is the compiled ProgramDFA for the type environment
- `env` contains procedure declarations for head and all body atoms

**Postconditions:** Returns ClauseCheckResult where:
- `isWellTyped` is true iff all three conditions hold
- `variableTypes` contains type assignments for all variables
- `errors` lists all violations

**Errors:**
- Throws `UndeclaredProcedureError` if any procedure is not declared

## Algorithms

### Algorithm: Clause Well-Typing Check

```
checkClause(clause, dfa, env):
  errors = []
  allVariableTypes = {}

  // Get procedure declaration for head
  headDecl = env.getProcedure(clause.head.functor, clause.head.arity)
  if headDecl == null:
    throw UndeclaredProcedureError(clause.head.functor, clause.head.arity)

  // Condition 1: Head well-typed
  modedH = modedHead(clause.head, headDecl)
  headAutomaton = dfa.getAutomaton(headDecl.key)  // e.g., "merge/3"
  headResult = checkModedTermPerArg(modedH, headDecl, dfa)

  if not headResult.isWellTyped:
    errors.add(HeadNotWellTypedError(headResult.errors))

  allVariableTypes.addAll(headResult.variableTypes)

  // Condition 2: Body atoms well-typed
  for i, atom in enumerate(clause.body):
    atomDecl = env.getProcedure(atom.functor, atom.arity)
    if atomDecl == null:
      throw UndeclaredProcedureError(atom.functor, atom.arity)

    modedA = producedTerm(atom, atomDecl)
    atomResult = checkModedTermPerArg(modedA, atomDecl, dfa)

    if not atomResult.isWellTyped:
      errors.add(BodyAtomNotWellTypedError(i, atom, atomResult.errors))

    // Merge variable types, checking consistency
    for (varKey, info) in atomResult.variableTypes:
      if varKey in allVariableTypes:
        if allVariableTypes[varKey].typeState.name != info.typeState.name:
          errors.add(InconsistentVariableAcrossClauseError(varKey))
      else:
        allVariableTypes[varKey] = info

  // Condition 3: Complementary variable types across entire clause
  complementErrors = checkClauseComplementarity(allVariableTypes)
  errors.addAll(complementErrors)

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors
  )
```

### Algorithm: Check Moded Term Per Argument

Each argument is checked against its declared type's automaton directly.

```
checkModedTermPerArg(modedTerm, decl, dfa):
  errors = []
  variableTypes = {}

  // modedTerm is a ModedCompound with args
  for i in 0..<decl.arity:
    argTerm = modedTerm.args[i]
    argType = decl.argTypes[i]

    // Get the automaton for the declared type directly
    // Type? → use T? automaton; Type → use T automaton
    argTypeName = getFullTypeName(argType)  // e.g., "Stream?" or "Stream"
    argAutomaton = dfa.getAutomaton(argTypeName)

    // Extract paths from this argument and check against automaton
    argPaths = pathsFromArg(modedTerm, i)

    for path in argPaths:
      result = checkPathAgainstAutomaton(path, argAutomaton, dfa)

      if not result.isConsistent:
        errors.add(InconsistentPathError(path, result.reason))
      else if result.variableAssignment != null:
        varKey = result.variableAssignment.varName
        if varKey in variableTypes:
          if variableTypes[varKey].typeState.name != result.variableAssignment.typeState.name:
            errors.add(InconsistentVariableError(varKey))
        else:
          variableTypes[varKey] = result.variableAssignment

  // Check complementarity within this term
  complementErrors = checkComplementarity(variableTypes)
  errors.addAll(complementErrors)

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors
  )

getFullTypeName(typeExpr):
  match typeExpr:
    PrimitiveModeAlt(isInput):
      return isInput ? '_?' : '_'
    TypeRef(name, isInput):
      return isInput ? name + '?' : name
```

### Algorithm: Complementarity Check Across Clause

```
checkClauseComplementarity(variableTypes):
  errors = []

  baseNames = groupByBaseName(variableTypes)

  for (baseName, variants) in baseNames:
    writerKey = baseName
    readerKey = "${baseName}?"

    if writerKey in variants and readerKey in variants:
      writerInfo = variants[writerKey]
      readerInfo = variants[readerKey]

      if not areComplementaryTypes(writerInfo, readerInfo):
        errors.add(ClauseVariableNotComplementaryError(baseName, writerInfo, readerInfo))

  return errors

areComplementaryTypes(writerInfo, readerInfo):
  // Writer must be in produce mode, reader in consume mode
  if writerInfo.mode != Mode.produce or readerInfo.mode != Mode.consume:
    return false

  // States must be complements: same baseName, opposite isComplement
  if writerInfo.typeState.baseName != readerInfo.typeState.baseName:
    return false

  // One must be complement, the other not
  return writerInfo.typeState.isComplement != readerInfo.typeState.isComplement
```

## Examples

### Example: Well-Typed merge Clause (Paper lines 382-408)

```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

Type: `procedure merge(Stream?, Stream?, Stream).`

**Condition 1: Head well-typed**

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Check each argument against its declared type's automaton:
- Arg 1 (`Stream?`): Use `Stream?` automaton → paths consistent ✓
- Arg 2 (`Stream?`): Use `Stream?` automaton → paths consistent ✓
- Arg 3 (`Stream`): Use `Stream` automaton → paths consistent ✓

**Condition 2: Body atom well-typed**

Produced moded term:
```
A' = ↑merge(Ys?, Xs?, Zs)
```

Check each argument:
- Arg 1 (`Stream?`): Ys? is reader at `Stream?` state ✓
- Arg 2 (`Stream?`): Xs? is reader at `Stream?` state ✓
- Arg 3 (`Stream`): Zs is writer at `Stream` state ✓

**Condition 3: Complementary types (Paper lines 400-407)**

| Variable | Type State | Mode |
|----------|------------|------|
| X | `_` (isComplement: false) | produce |
| X? | `_?` (isComplement: true) | consume |
| Xs | `Stream` (isComplement: false) | produce |
| Xs? | `Stream?` (isComplement: true) | consume |
| Ys | `Stream?` (isComplement: true) | produce |
| Ys? | `Stream` (isComplement: false) | consume |
| Zs | `Stream` (isComplement: false) | produce |
| Zs? | `Stream?` (isComplement: true) | consume |

All pairs: same baseName, opposite isComplement ✓

**Result: Well-typed**

### Example: NEGATIVE — Head Not Well-Typed

```
merge(42, Ys, Zs).
```

**Problem:** Argument 1 uses `Stream?` automaton. Integer 42 has no matching transition from `Stream?` state.

**Error:** `HeadNotWellTypedError([InconsistentPathError(...)])`

### Example: NEGATIVE — Non-Complementary Variables

```
convert([X|Xs], [X?|Ys]) :- convert(Xs?, Ys?).
```

With type `convert(Stream?, NatStream)` where:
- `Stream ::= [] ; [_|Stream]`
- `NatStream ::= [] ; [Integer|NatStream]`

**Problem:**
- X from `Stream?` arg gets type `_?`
- X? from `NatStream` arg gets type `Integer`
- `_?` and `Integer` have different baseNames — not complements!

**Error:** `ClauseVariableNotComplementaryError("X", ...)`

## Error Conditions

| Condition | Exception/Error |
|-----------|-----------------|
| Procedure not declared | `UndeclaredProcedureError` (thrown) |
| Head not well-typed | `HeadNotWellTypedError` |
| Body atom not well-typed | `BodyAtomNotWellTypedError` |
| Variable inconsistent across clause | `InconsistentVariableAcrossClauseError` |
| Variable pair not complementary | `ClauseVariableNotComplementaryError` |

## Changes from v0.5

- Use `ProgramDFA` and `Automaton` instead of `TypeDFA`
- `buildProcedureTypeDFA()` removed — use `dfa.getAutomaton(typeName)` directly
- No complement flag logic — automaton for `T?` already has correct states/modes
- Complementarity check uses `DFAState.baseName` and `isComplement`

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add getAcceptedLabels; complete algorithms |
| 0.6 | 2025-01-10 | Update for ProgramDFA v0.8: direct automaton lookup, no complement flag |
