# Module: well-typed-clause

**Version**: 0.5  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Definition 4.8 (lines 311-321), Example (lines 323-349)

## Purpose

Determines when a GLP clause is well-typed by a type environment D.

## Dependencies

- `mode` — Mode enum
- `moded-head` — modedHead(), producedTerm()
- `well-typed-term` — checkModedTerm()
- `type-dfa` — compileType(), complementDFA()
- `type-environment` — TypeEnvironment, ProcDecl

## Definitions

### Definition 4.8: Well-typed Clause (lines 311-321)

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

#### `ClauseCheckResult checkClause(Clause clause, TypeEnvironment env)`

Checks if a clause is well-typed per Definition 4.8.

**Preconditions:**
- `clause` is a valid GLP clause
- `env` contains procedure declarations for head and all body atoms

**Postconditions:** Returns ClauseCheckResult where:
- `isWellTyped` is true iff all three conditions hold
- `variableTypes` contains type assignments for all variables
- `errors` lists all violations

**Errors:**
- Throws `UndeclaredProcedureError` if any procedure is not declared

#### `Set<DFALabel> getAcceptedLabels(Clause clause, int argIndex, TypeEnvironment env)`

Returns the set of DFA labels that the clause head accepts at the given argument position. Used for coverage checking.

**Preconditions:**
- `clause` is a valid GLP clause
- `argIndex` is 1-based, within head arity
- `env` contains the procedure declaration

**Postconditions:** Returns set of DFA labels the head argument can match:
- Variable → accepts all labels (wildcard)
- Constant → accepts that specific constant
- Compound → accepts that functor/arity with nested structure

## Algorithms

### Algorithm: Clause Well-Typing Check

```
checkClause(clause, env):
  errors = []
  allVariableTypes = {}
  
  // Get procedure declaration for head
  headDecl = env.getProcedure(clause.head.functor, clause.head.arity)
  if headDecl == null:
    throw UndeclaredProcedureError(clause.head.functor, clause.head.arity)
  
  // Condition 1: Head well-typed
  modedH = modedHead(clause.head, headDecl)
  headDFA = buildProcedureTypeDFA(headDecl, env)
  headResult = checkModedTerm(modedH, headDFA)
  
  if not headResult.isWellTyped:
    errors.add(HeadNotWellTypedError(headResult.errors))
  
  allVariableTypes.addAll(headResult.variableTypes)
  
  // Condition 2: Body atoms well-typed
  for i, atom in enumerate(clause.body):
    atomDecl = env.getProcedure(atom.functor, atom.arity)
    if atomDecl == null:
      throw UndeclaredProcedureError(atom.functor, atom.arity)
    
    modedA = producedTerm(atom, atomDecl)
    atomDFA = buildProcedureTypeDFA(atomDecl, env)
    atomResult = checkModedTerm(modedA, atomDFA)
    
    if not atomResult.isWellTyped:
      errors.add(BodyAtomNotWellTypedError(i, atom, atomResult.errors))
    
    // Merge variable types, checking consistency
    for (varKey, info) in atomResult.variableTypes:
      if varKey in allVariableTypes:
        if allVariableTypes[varKey].typeState != info.typeState:
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

buildProcedureTypeDFA(decl, env):
  // Build a composite DFA for the procedure
  // Each argument position has its own type DFA
  argDFAs = []
  for argType in decl.argTypes:
    baseDFA = compileType(argType.baseName, env)
    if argType.isInput:  // Type?
      argDFAs.add(complementDFA(baseDFA))
    else:
      argDFAs.add(baseDFA)
  
  return ProcedureTypeDFA(decl.name, decl.arity, argDFAs)

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
  
  // Type states must be "the same type" (complementary positions)
  // For primitive types: _ complements _?
  // For defined types: T complements T?
  return typesAreComplements(writerInfo.typeState, readerInfo.typeState)
```

### Algorithm: Get Accepted Labels (for Coverage)

```
getAcceptedLabels(clause, argIndex, env):
  argTerm = clause.head.args[argIndex - 1]
  headDecl = env.getProcedure(clause.head.functor, clause.head.arity)
  argType = headDecl.argTypes[argIndex - 1]
  
  return extractAcceptedLabels(argTerm, argType, env)

extractAcceptedLabels(term, typeExpr, env):
  match term:
    Variable(name, isReader):
      // Variable accepts ALL labels for this type (wildcard)
      return ALL_LABELS  // Special marker meaning "accepts anything"
    
    Constant(value):
      // Constant accepts only its specific label
      return {DFALabel(symbol: value.toString(), arity: 0, argIndex: 0, mode: null)}
    
    Compound(functor, args):
      // Returns labels for this specific functor/arity
      // Used to check if clause accepts a specific alternative
      return {DFALabel(symbol: functor, arity: args.length, argIndex: 0, mode: contextMode)}
```

## Examples

### Example: Well-Typed merge Clause

```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

**Condition 1: Head well-typed**

Moded head:
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

All paths consistent with type DFA ✓

**Condition 2: Body atom well-typed**

Produced moded term:
```
A' = ↑merge(Ys?, Xs?, Zs)
```

All paths consistent ✓

**Condition 3: Complementary types**

| Variable | Type | Mode |
|----------|------|------|
| X | _ | produce |
| X? | _? | consume |
| Xs | Stream | produce |
| Xs? | Stream? | consume |
| Ys | Stream? | produce |
| Ys? | Stream | consume |
| Zs | Stream | produce |
| Zs? | Stream? | consume |

All pairs complementary ✓

**Result: Well-typed**

### Example: INVALID — Head Not Well-Typed

```
merge(42, Ys, Zs).
```

**Problem:** Argument 1 has type `Stream?`, but head has integer `42`. No path in `Stream?` accepts integer.

**Error:** `HeadNotWellTypedError([InconsistentPathError(...)])`

### Example: INVALID — Body Atom Not Well-Typed

```
merge(Xs, Ys, Zs) :- merge(42, Ys?, Zs?).
```

**Problem:** Body atom passes integer `42` at argument 1 expecting `Stream?`.

**Error:** `BodyAtomNotWellTypedError(0, merge(42, Ys?, Zs?), [...])`

### Example: INVALID — Non-Complementary Variables

```
convert([X|Xs], [X?|Ys]) :- convert(Xs?, Ys?).
```

With type `convert(Stream?, NatStream)` where Stream has `_` elements and NatStream has `Integer` elements.

**Problem:** X from Stream? gets type _, X? from NatStream gets type Integer. These are not complements.

**Error:** `ClauseVariableNotComplementaryError("X", ...)`

## Error Conditions

| Condition | Exception/Error |
|-----------|-----------------|
| Procedure not declared | `UndeclaredProcedureError` (thrown) |
| Head not well-typed | `HeadNotWellTypedError` |
| Body atom not well-typed | `BodyAtomNotWellTypedError` |
| Variable inconsistent across clause | `InconsistentVariableAcrossClauseError` |
| Variable pair not complementary | `ClauseVariableNotComplementaryError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add getAcceptedLabels for coverage; complete algorithms; more examples |
