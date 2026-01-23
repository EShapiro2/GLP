# Module: well-typed-clause

**Version**: 1.0
**Date**: 2026-01-23
**Status**: DRAFT
**Paper References**: Definition 5.7 (Well-typed Clause), Example 5.8 (Well-typed Clause Verification)

## Purpose

Determines when a GLP clause is well-typed by a type environment D.

## Dependencies

- `mode` — Mode enum
- `moded-head` — modedHead(), producedTerm()
- `well-typed-term` — checkModedTerm(), WellTypedResult
- `type-dfa` — ProgramDFA, Automaton, DFAState
- `type-environment` — TypeEnvironment, ProcDecl

## Preconditions

### SRSW Checked by Parser

The **Single-Reader/Single-Writer (SRSW) syntactic restriction** is a compile-time check performed by the parser **before** type checking. The type checker assumes all input clauses satisfy SRSW:

- Every variable X that occurs in a clause has its paired variable X? also occurring in the clause
- Each variable (reader or writer) occurs exactly once

This means the type checker will never encounter unpaired variables (e.g., X without X?, or X? without X). The duality check in Condition 3 only verifies that paired variables have appropriate **types**, not that pairs exist.

See `docs/SPEC_GUIDE.md` for SRSW enforcement details.

## Definitions

### Definition 5.7: Well-typed Clause

> Let C = (H :- B) be a GLP clause and D a GLP type for all its procedures.
> Then C is **well-typed** by D if:
>
> 1. There is a moded head H' corresponding to H that is well-typed by D.
> 2. For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.
> 3. For every pair of dual variables X and X? in C:
>    - (a) If both occur in H, or both occur in B, they are assigned dual types by D.
>    - (b) If one occurs in H and the other in B, they are assigned the same type by D.

### Clause Acceptance

In addition, C **accepts an input path** x ∈ paths(D) if H' has a path consistent with x. This is used for contravariance checking at the program level.

## Three Conditions

### Condition 1: Head Well-Typed

Construct moded head H' using `modedHead(H, decl, env)` and verify it is well-typed.

The moded head:
- Is I/O moded (root ↓, input args ↓, output args ↑)
- Has variables conditionally replaced to match structural modes (per Definition 5.5)

### Condition 2: Body Atoms Well-Typed

For each body atom A, construct the **produced** moded term A' using `producedTerm(A, decl, env)` and verify it is well-typed.

Body atoms are produced because they represent goals being called—the clause produces these goals.

### Condition 3: Variable Type Consistency

For every variable pair (X, X?) in the clause, the type relationship depends on where each occurs:

- **Head-Head or Body-Body**: If both occur in the same clause part (both in head, or both in body), they must be assigned **dual types**.
- **Head-Body**: If one occurs in the head and the other in the body, they must be assigned the **same type**.

This is checked by tracking variable locations (head vs body) during type aggregation, then verifying the appropriate relationship based on location.

## Public Interface

### Types

#### `class ClauseCheckResult`

```dart
class ClauseCheckResult {
  final bool isWellTyped;
  final Map<String, VariableTypeInfo> variableTypes;
  final List<ClauseError> errors;
  
  /// The moded head constructed for this clause
  final ModedTerm? modedHead;
  
  /// The moded body atoms constructed for this clause
  final List<ModedTerm> modedBodyAtoms;
}

abstract class ClauseError {}

class HeadNotWellTypedError extends ClauseError {
  final List<TypeError> termErrors;
  final ModedTerm modedHead;
}

class BodyAtomNotWellTypedError extends ClauseError {
  final int atomIndex;
  final Term atom;
  final ModedTerm modedAtom;
  final List<TypeError> termErrors;
}

class ClauseVariableTypeError extends ClauseError {
  final String variableBaseName;
  final VariableTypeInfo writerType;
  final VariableTypeInfo readerType;
  final String reason;
}

/// Tracks where a variable was found: head or body
enum VariableLocation { head, body }

class InconsistentVariableAcrossClauseError extends ClauseError {
  final String varName;
  final VariableTypeInfo firstOccurrence;
  final VariableTypeInfo secondOccurrence;
}
```

### Functions

#### `ClauseCheckResult checkClause(Clause clause, ProgramDFA dfa, TypeEnvironment env)`

Checks if a clause is well-typed per Definition 5.7.

**Preconditions:**
- `clause` is a valid GLP clause
- `dfa` is the compiled ProgramDFA for the type environment
- `env` contains procedure declarations for head and all body atoms

**Postconditions:** Returns ClauseCheckResult where:
- `isWellTyped` is true iff all three conditions hold
- `variableTypes` contains type assignments for all variables
- `errors` lists all violations
- `modedHead` contains the constructed moded head
- `modedBodyAtoms` contains the constructed moded body atoms

**Errors:**
- Throws `UndeclaredProcedureError` if any procedure is not declared

#### `Set<TransitionLabel> getAcceptedLabels(Clause clause, int argIndex, TypeEnvironment env)`

Returns the set of transition labels that the clause head accepts at the given argument position.

**Preconditions:**
- `clause` is a valid GLP clause
- `argIndex` is a valid argument index (1-based)

**Postconditions:**
- Returns `ALL_LABELS` if a variable appears at the root of that argument
- Returns the set of functor/constant labels the clause explicitly matches

## Algorithms

### Algorithm: Clause Well-Typing Check

```
checkClause(clause, dfa, env):
  errors = []
  headVariableTypes = {}  // Variables from moded head
  bodyVariableTypes = {}  // Variables from body atoms
  modedBodyAtoms = []

  // Get procedure declaration for head
  headDecl = env.getProcedure(clause.head.functor, clause.head.arity)
  if headDecl == null:
    throw UndeclaredProcedureError(clause.head.functor, clause.head.arity)

  // Condition 1: Head well-typed
  modedH = modedHead(clause.head, headDecl, env)
  headResult = checkModedTermPerArg(modedH, headDecl, dfa, env)

  if not headResult.isWellTyped:
    errors.add(HeadNotWellTypedError(headResult.errors, modedH))

  headVariableTypes.addAll(headResult.variableTypes)

  // Condition 2: Body atoms well-typed
  for i, atom in enumerate(clause.body):
    atomDecl = env.getProcedure(atom.functor, atom.arity)
    if atomDecl == null:
      throw UndeclaredProcedureError(atom.functor, atom.arity)

    modedA = producedTerm(atom, atomDecl, env)
    modedBodyAtoms.add(modedA)
    atomResult = checkModedTermPerArg(modedA, atomDecl, dfa, env)

    if not atomResult.isWellTyped:
      errors.add(BodyAtomNotWellTypedError(i, atom, modedA, atomResult.errors))

    // Merge variable types within body, checking consistency
    for (varKey, info) in atomResult.variableTypes:
      if varKey in bodyVariableTypes:
        if bodyVariableTypes[varKey].typeState.name != info.typeState.name:
          errors.add(InconsistentVariableAcrossClauseError(
            varKey, bodyVariableTypes[varKey], info))
      else:
        bodyVariableTypes[varKey] = info

  // Condition 3: Variable type consistency based on location
  consistencyErrors = checkClauseVariableTypes(headVariableTypes, bodyVariableTypes)
  errors.addAll(consistencyErrors)

  // Merge for result (both head and body)
  allVariableTypes = {}
  allVariableTypes.addAll(headVariableTypes)
  allVariableTypes.addAll(bodyVariableTypes)

  return ClauseCheckResult(
    isWellTyped: errors.isEmpty,
    variableTypes: allVariableTypes,
    errors: errors,
    modedHead: modedH,
    modedBodyAtoms: modedBodyAtoms
  )
```

### Algorithm: Check Moded Term Per Argument

Each argument is checked against its declared type's automaton directly.

```
checkModedTermPerArg(modedTerm, decl, dfa, env):
  errors = []
  variableTypes = {}

  // modedTerm is a ModedCompound with args
  for i in 0..<decl.arity:
    argTerm = modedTerm.args[i]
    argType = decl.argTypes[i]

    // Get the automaton for the declared type directly
    // Type? → use T? automaton; Type → use T automaton
    argTypeName = getFullTypeName(argType)
    argAutomaton = dfa.getAutomaton(argTypeName)

    // Check the argument term against the automaton
    argResult = checkModedTerm(argTerm, argAutomaton, dfa)

    if not argResult.isWellTyped:
      errors.addAll(argResult.errors)

    // Merge variable types
    for (varKey, info) in argResult.variableTypes:
      if varKey in variableTypes:
        if variableTypes[varKey].typeState.name != info.typeState.name:
          errors.add(InconsistentVariableError(varKey,
            variableTypes[varKey].typeState, info.typeState))
      else:
        variableTypes[varKey] = info

  return WellTypedResult(
    isWellTyped: errors.isEmpty,
    variableTypes: variableTypes,
    errors: errors
  )

getFullTypeName(typeExpr):
  match typeExpr:
    PrimitiveType(name, isInput):
      return isInput ? name + '?' : name
    TypeRef(name, isInput):
      return isInput ? name + '?' : name
```

### Algorithm: Variable Type Consistency Check Across Clause

The algorithm tracks where each variable occurs (head vs body) and applies location-dependent rules:

```
checkClauseVariableTypes(headVariableTypes, bodyVariableTypes):
  errors = []
  
  // Merge all variables with their locations
  allVariables = {}  // baseName -> {writer: (info, location), reader: (info, location)}
  
  for (varKey, info) in headVariableTypes:
    addVariableWithLocation(allVariables, varKey, info, VariableLocation.head)
  
  for (varKey, info) in bodyVariableTypes:
    addVariableWithLocation(allVariables, varKey, info, VariableLocation.body)
  
  // Check each variable pair
  for (baseName, pair) in allVariables:
    if pair.writer != null and pair.reader != null:
      writerInfo, writerLoc = pair.writer
      readerInfo, readerLoc = pair.reader
      
      if writerLoc == readerLoc:  // Both head-head or both body-body
        // Require DUAL types
        if not areDualTypes(writerInfo, readerInfo):
          errors.add(ClauseVariableTypeError(
            baseName, writerInfo, readerInfo,
            "Variables in same clause part (${writerLoc}) must have dual types"))
      else:  // One in head, one in body
        // Require SAME type
        if not areSameType(writerInfo, readerInfo):
          errors.add(ClauseVariableTypeError(
            baseName, writerInfo, readerInfo,
            "Variables across head/body must have same type"))
  
  return errors

addVariableWithLocation(allVariables, varKey, info, location):
  baseName = stripReaderSuffix(varKey)
  isReader = varKey.endsWith('?')
  
  if baseName not in allVariables:
    allVariables[baseName] = {writer: null, reader: null}
  
  if isReader:
    allVariables[baseName].reader = (info, location)
  else:
    allVariables[baseName].writer = (info, location)

areDualTypes(writerInfo, readerInfo):
  // Writer at T, reader at T? (or vice versa by mode)
  // States must be duals: same baseName, opposite isDual
  if writerInfo.typeState.baseName != readerInfo.typeState.baseName:
    return false
  return writerInfo.typeState.isDual != readerInfo.typeState.isDual

areSameType(writerInfo, readerInfo):
  // Both must have identical type states
  return writerInfo.typeState.name == readerInfo.typeState.name

stripReaderSuffix(varKey):
  if varKey.endsWith('?'):
    return varKey.substring(0, varKey.length - 1)
  return varKey
```

### Algorithm: Get Accepted Labels

```
getAcceptedLabels(clause, argIndex, env):
  arg = clause.head.args[argIndex - 1]
  
  match arg:
    Variable(_):
      return ALL_LABELS  // Variable accepts anything
    
    Compound(functor, subArgs):
      return {TransitionLabel.functor(functor, subArgs.length, 0, mode: null)}
    
    Constant(value):
      return {TransitionLabel.constant(value)}
```

## Examples

### Example 1: Well-Typed merge Clause (Paper Example 5.8)

```
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

Type: `procedure merge(Stream?, Stream?, Stream).`

**Condition 1: Head well-typed**

Moded head (after conditional variable replacement):
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Check each argument against its declared type's automaton:
- Arg 1 (`Stream?`): paths to X?, Xs? consistent ✓
- Arg 2 (`Stream?`): path to Ys? consistent ✓
- Arg 3 (`Stream`): paths to X, Zs consistent ✓

**Condition 2: Body atom well-typed**

Produced moded term (no variable replacement):
```
A' = ↑merge(↓Ys?, ↓Xs?, ↑Zs)
```

Check each argument:
- Arg 1 (`Stream?`): Ys? is reader at `Stream?` state ✓
- Arg 2 (`Stream?`): Xs? is reader at `Stream?` state ✓
- Arg 3 (`Stream`): Zs is writer at `Stream` state ✓

**Condition 3: Variable type consistency**

**From moded head H':**
- X? at arg 1 element → type `_?` (location: head)
- Xs? at arg 1 tail → type `Stream?` (location: head)
- Ys? at arg 2 → type `Stream?` (location: head)
- X at arg 3 element → type `_` (location: head)
- Zs at arg 3 tail → type `Stream` (location: head)

**From moded body A':**
- Ys? at arg 1 → type `Stream?` (location: body)
- Xs? at arg 2 → type `Stream?` (location: body)
- Zs at arg 3 → type `Stream` (location: body)

**Variable pair analysis:**

| Pair | Writer Location | Reader Location | Relationship | Check |
|------|-----------------|-----------------|--------------|-------|
| X/X? | head | head | head-head | dual types: `_` / `_?` ✓ |
| Xs/Xs? | — | head+body | only Xs? appears | — |
| Ys/Ys? | — | head+body | only Ys? appears | — |
| Zs/Zs? | head+body | — | only Zs appears | — |

**Note:** After moded head construction (Definition 5.5), some original variables are flipped. Here, only the X/X? pair has both forms appearing in the moded clause. For that pair, both occur in the head, so they must have dual types (`_` and `_?`), which they do.

**Result: Well-typed** ✓

### Example 2: NEGATIVE — Head Not Well-Typed

```
merge(42, Ys, Zs).
```

**Problem:** Argument 1 uses `Stream?` automaton. Integer 42 has no matching transition from `Stream?` state (Stream? expects [] or [|]).

**Error:** `HeadNotWellTypedError([InconsistentPathError("Constant 42 has no transition from Stream?")])`

### Example 3: NEGATIVE — Non-Dual Variables

```
convert([X|Xs], [X?|Ys]) :- convert(Xs?, Ys?).
```

With type `convert(Stream?, NatStream)` where:
- `Stream ::= [] ; [_|Stream]`
- `NatStream ::= [] ; [Integer|NatStream]`

**Analysis:**
- From arg 1 (Stream?): X? gets type `_?`
- From arg 2 (NatStream): X gets type `Integer`

Duality check for (X, X?):
- X: (`Integer`, produce)
- X?: (`_?`, consume)
- baseNames: "Integer" vs "_" → different!

**Error:** `ClauseVariableTypeError("X", "Types have different bases: Integer vs _?")`

### Example 4: Interactive Type — Monitor Clause

```
monitor(N, [read(N?)|In]) :- monitor(N?, In?).
```

With type `monitor(Integer?, Stream(CounterCall)?)` where `CounterCall ::= add ; clear ; read(Integer?)`.

**Moded head:**
```
H' = ↓monitor(↓N?, ↓[↓read(↑N)|↓In?])
```

Variable types from head:
- N? at `Integer?` → (`Integer?`, consume)
- N at `Integer` → (`Integer`, produce)  [inside read, mode flips due to Integer?]
- In? at `Stream(CounterCall)?` → (`Stream(CounterCall)?`, consume)

**Moded body:**
```
A' = ↑monitor(↓N?, ↓In?)
```

Variable types from body:
- N? at `Integer?` → (`Integer?`, consume) [consistent]
- In? at `Stream(CounterCall)?` → (`Stream(CounterCall)?`, consume) [consistent]

Duality:
- N/N?: (`Integer`, produce) and (`Integer?`, consume) → dual ✓

**Result: Well-typed** ✓

## Error Conditions

| Condition | Exception/Error |
|-----------|-----------------|
| Procedure not declared | `UndeclaredProcedureError` (thrown) |
| Head not well-typed | `HeadNotWellTypedError` |
| Body atom not well-typed | `BodyAtomNotWellTypedError` |
| Variable inconsistent across clause | `InconsistentVariableAcrossClauseError` |
| Variable pair type mismatch | `ClauseVariableTypeError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add getAcceptedLabels; complete algorithms |
| 0.6 | 2025-01-10 | Update for ProgramDFA v0.8 |
| 0.7 | 2025-01-12 | Update for paper Definition 4.10; add interactive type examples |
| 0.8 | 2025-01-12 | Add SRSW precondition; clarify complementarity checks types not existence |
| 0.9 | 2025-01-17 | Updated Condition 3: location-aware variable type consistency (head-head/body-body = dual types, head-body = same type) |
| 1.0 | 2026-01-23 | **Paper alignment**: Updated to Definition 5.7, Example 5.8; "complement" → "dual" throughout; `isComplement` → `isDual`; "complementary" → "dual" |
