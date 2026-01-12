# Module: well-typed-clause

**Version**: 0.8
**Date**: 2025-01-12
**Status**: DRAFT
**Paper References**: Definition 4.10 (Well-typed Clause), Example 4.11 (Well-typed Clause Verification)

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

This means the type checker will never encounter unpaired variables (e.g., X without X?, or X? without X). The complementarity check in Condition 3 only verifies that paired variables have complementary **types**, not that pairs exist.

See `docs/SPEC_GUIDE.md` for SRSW enforcement details.

## Definitions

### Definition 4.10: Well-typed Clause

> Let C = (H :- B) be a GLP clause and D a GLP type for all its procedures.
> Then C is **well-typed** by D if:
>
> 1. There is a moded head H' corresponding to H that is well-typed by D.
> 2. For each atom A ∈ B, the produced moded term A' corresponding to A is well-typed by D.
> 3. Every pair of variables that occur in C are assigned complementary types by D.

### Clause Acceptance

In addition, C **accepts an input path** x ∈ paths(D) if H' has a path consistent with x. This is used for contravariance checking at the program level.

## Three Conditions

### Condition 1: Head Well-Typed

Construct moded head H' using `modedHead(H, decl, env)` and verify it is well-typed.

The moded head:
- Is I/O moded (root ↓, input args ↓, output args ↑)
- Has variables conditionally replaced to match structural modes (per Definition 4.8)

### Condition 2: Body Atoms Well-Typed

For each body atom A, construct the **produced** moded term A' using `producedTerm(A, decl, env)` and verify it is well-typed.

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

class ClauseVariableNotComplementaryError extends ClauseError {
  final String variableBaseName;
  final VariableTypeInfo writerType;
  final VariableTypeInfo readerType;
  final String reason;
}

class InconsistentVariableAcrossClauseError extends ClauseError {
  final String varName;
  final VariableTypeInfo firstOccurrence;
  final VariableTypeInfo secondOccurrence;
}
```

### Functions

#### `ClauseCheckResult checkClause(Clause clause, ProgramDFA dfa, TypeEnvironment env)`

Checks if a clause is well-typed per Definition 4.10.

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
  allVariableTypes = {}
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

  allVariableTypes.addAll(headResult.variableTypes)

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

    // Merge variable types, checking consistency
    for (varKey, info) in atomResult.variableTypes:
      if varKey in allVariableTypes:
        if allVariableTypes[varKey].typeState.name != info.typeState.name:
          errors.add(InconsistentVariableAcrossClauseError(
            varKey, allVariableTypes[varKey], info))
      else:
        allVariableTypes[varKey] = info

  // Condition 3: Complementary variable types across entire clause
  complementErrors = checkClauseComplementarity(allVariableTypes)
  errors.addAll(complementErrors)

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
        errors.add(ClauseVariableNotComplementaryError(
          baseName, writerInfo, readerInfo,
          describeComplementaryFailure(writerInfo, readerInfo)))

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

describeComplementaryFailure(writerInfo, readerInfo):
  if writerInfo.mode != Mode.produce:
    return "Writer ${writerInfo.varName} has mode ${writerInfo.mode}, expected produce"
  if readerInfo.mode != Mode.consume:
    return "Reader ${readerInfo.varName} has mode ${readerInfo.mode}, expected consume"
  if writerInfo.typeState.baseName != readerInfo.typeState.baseName:
    return "Types have different bases: ${writerInfo.typeState.name} vs ${readerInfo.typeState.name}"
  return "Both types have same complement status"

groupByBaseName(variableTypes):
  groups = {}
  for (varKey, info) in variableTypes:
    baseName = info.baseName
    if baseName not in groups:
      groups[baseName] = {}
    groups[baseName][varKey] = info
  return groups
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

### Example 1: Well-Typed merge Clause (Paper Example 4.11)

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

**Condition 3: Complementary types**

| Variable | Type State | Mode |
|----------|------------|------|
| X | `_` | produce |
| X? | `_?` | consume |
| Xs | `Stream` | produce |
| Xs? | `Stream?` | consume |
| Ys | `Stream?` | produce |
| Ys? | `Stream?` | consume |
| Zs | `Stream` | produce |
| Zs? | `Stream?` | consume |

Wait—Ys has type `Stream?` but Ys? also has type... let me recalculate.

Actually from the head:
- Ys? (at arg 2) → type `Stream?`

From the body:
- Ys? (at arg 1 of body merge) → type `Stream?` ✓

So Ys? appears twice with same type. Now for Ys:
- Ys doesn't appear directly—only Ys?

So the pairs are:
- (X, X?): `_` and `_?` ✓
- (Xs, Xs?): from head Xs? has type `Stream?`, from body Xs? has type `Stream?`. Xs appears in head as... wait, in the moded head Xs becomes Xs?.

Let me redo this more carefully:

**In moded head H':**
- X? at arg 1, element → type `_?`
- Xs? at arg 1, tail → type `Stream?`
- Ys? at arg 2 → type `Stream?`
- X at arg 3, element → type `_`
- Zs at arg 3, tail → type `Stream`

**In moded body A':**
- Ys? at arg 1 → type `Stream?`
- Xs? at arg 2 → type `Stream?`
- Zs at arg 3 → type `Stream`

**Variable pairs:**
- (X, X?): X has `_`, X? has `_?` → complements ✓
- (Xs, Xs?): Xs? has `Stream?`. Where is Xs? In original clause, arg 1 has `[X|Xs]`. In moded head, it becomes Xs?. So Xs (writer) doesn't appear in moded clause. Actually the original Xs in the head became Xs? in moded head. So there's no explicit Xs writer... 

Actually, SRSW requires both X and X? appear. In the original clause:
- Head: X (writer), Xs (writer), Ys (writer), X? (reader), Zs? (reader)
- Body: Ys? (reader), Xs? (reader), Zs (writer)

Pairs in original clause: (X, X?), (Xs, Xs?), (Ys, Ys?), (Zs, Zs?)

In the moded head, variables are conditionally flipped to match structural modes. In the moded body, variables are unchanged.

After moding:
- Head: X?, Xs?, Ys?, X, Zs
- Body: Ys?, Xs?, Zs

Aggregated variable types:
- X? (head arg 1 element, mode ↓) → type `_?`, mode consume
- Xs? (head arg 1 tail, mode ↓) → type `Stream?`, mode consume
- Ys? (head arg 2, mode ↓) → type `Stream?`, mode consume
- X (head arg 3 element, mode ↑) → type `_`, mode produce
- Zs (head arg 3 tail, mode ↑) → type `Stream`, mode produce
- Ys? (body arg 1, mode ↓) → type `Stream?`, mode consume [same as head]
- Xs? (body arg 2, mode ↓) → type `Stream?`, mode consume [same as head]
- Zs (body arg 3, mode ↑) → type `Stream`, mode produce [same as head]

Variable pairs and complementarity:
- X/X?: X (`_`, produce) and X? (`_?`, consume) → same base `_`, opposite complement ✓
- Xs/Xs?: We only have Xs? in the moded clause. Where's Xs?

This is the key insight: after moding, we don't have explicit Xs anymore—only Xs?. But the SRSW requirement on the original clause means both must exist. The type checker works on the **moded** clause, but complementarity is about **original** variable pairs.

Actually, the types are assigned based on where variables appear in the moded term. If Xs doesn't appear (because it was flipped to Xs?), then Xs has no type assignment, and complementarity checking only applies to pairs where both appear.

Let me re-read the paper... Definition 4.10 says "Every pair of variables that occur in C are assigned complementary types by D."

In the moded representation, after step 2 of Definition 4.8, some variables are flipped. The complementarity check should be on the types assigned to the flipped variables.

Actually, I think the intent is: the original clause has variable pairs (X, X?), (Xs, Xs?), etc. The moded clause assigns types to the variables as they appear (possibly flipped). We need to check that for each original pair, the types are complementary.

But that's tricky because after flipping, we might have Xs? appearing where Xs was. The assigned type goes to Xs? now.

I think the cleaner interpretation: collect all variable types from the moded clause. For each base name B, if both B and B? have type assignments, check they're complementary. If only one has an assignment, that's fine (the other might not appear in this clause).

With this interpretation:
- X: `_`, X?: `_?` → complementary ✓
- Xs: not assigned, Xs?: `Stream?` → no check needed
- Ys: not assigned, Ys?: `Stream?` → no check needed  
- Zs: `Stream`, Zs?: not assigned → no check needed

Wait, but Zs? appears in the original head: `[X?|Zs?]`. In the moded head, this becomes `[X|Zs]`. So Zs? was flipped to Zs.

Let me be very precise. Original head: `merge([X|Xs], Ys, [X?|Zs?])`

Args with their structural modes (from type):
- Arg 1 (Stream?): mode ↓
- Arg 2 (Stream?): mode ↓
- Arg 3 (Stream): mode ↑

Building moded head:
- Arg 1 `[X|Xs]` at mode ↓:
  - List at mode ↓
  - Element X at mode ↓ (from `_?` in Stream?): X is writer, mode is ↓ → mismatch → flip to X?
  - Tail Xs at mode ↓ (from `Stream?` in Stream?): Xs is writer, mode is ↓ → mismatch → flip to Xs?
- Arg 2 `Ys` at mode ↓:
  - Ys is writer, mode is ↓ → mismatch → flip to Ys?
- Arg 3 `[X?|Zs?]` at mode ↑:
  - List at mode ↑
  - Element X? at mode ↑ (from `_` in Stream): X? is reader, mode is ↑ → mismatch → flip to X
  - Tail Zs? at mode ↑ (from `Stream` in Stream): Zs? is reader, mode is ↑ → mismatch → flip to Zs

So moded head is: `↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])`

Now in the moded head, the variables are: X?, Xs?, Ys?, X, Zs

Their type assignments:
- X? at `_?` → (`_?`, consume)
- Xs? at `Stream?` → (`Stream?`, consume)
- Ys? at `Stream?` → (`Stream?`, consume)
- X at `_` → (`_`, produce)
- Zs at `Stream` → (`Stream`, produce)

Pairs where both appear:
- X and X? both appear → check: `_` and `_?` → complementary ✓

Other base names (Xs, Ys, Zs) only have one form appearing, so no complementarity check needed within the head.

Now add body: `merge(Ys?, Xs?, Zs)` as produced term (no flipping)

Moded body: `↑merge(↓Ys?, ↓Xs?, ↑Zs)`

Types:
- Ys? at `Stream?` → (`Stream?`, consume) [consistent with head]
- Xs? at `Stream?` → (`Stream?`, consume) [consistent with head]
- Zs at `Stream` → (`Stream`, produce) [consistent with head]

After merging head and body, pairs where both forms appear:
- X and X?: both in head → complementary ✓

The other base names still only have one form. So all complementarity checks pass.

**Result: Well-typed** ✓

### Example 2: NEGATIVE — Head Not Well-Typed

```
merge(42, Ys, Zs).
```

**Problem:** Argument 1 uses `Stream?` automaton. Integer 42 has no matching transition from `Stream?` state (Stream? expects [] or [|]).

**Error:** `HeadNotWellTypedError([InconsistentPathError("Constant 42 has no transition from Stream?")])`

### Example 3: NEGATIVE — Non-Complementary Variables

```
convert([X|Xs], [X?|Ys]) :- convert(Xs?, Ys?).
```

With type `convert(Stream?, NatStream)` where:
- `Stream ::= [] ; [_|Stream]`
- `NatStream ::= [] ; [Integer|NatStream]`

**Analysis:**
- From arg 1 (Stream?): X? gets type `_?`
- From arg 2 (NatStream): X gets type `Integer`

Complementarity check for (X, X?):
- X: (`Integer`, produce)
- X?: (`_?`, consume)
- baseNames: "Integer" vs "_" → different!

**Error:** `ClauseVariableNotComplementaryError("X", "Types have different bases: Integer vs _?")`

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

Complementarity:
- N/N?: (`Integer`, produce) and (`Integer?`, consume) → complementary ✓

**Result: Well-typed** ✓

## Error Conditions

| Condition | Exception/Error |
|-----------|-----------------|
| Procedure not declared | `UndeclaredProcedureError` (thrown) |
| Head not well-typed | `HeadNotWellTypedError` |
| Body atom not well-typed | `BodyAtomNotWellTypedError` |
| Variable inconsistent across clause | `InconsistentVariableAcrossClauseError` |
| Variable pair not complementary | `ClauseVariableNotComplementaryError` |

## Changes from v0.7

- Added Preconditions section documenting SRSW is checked by parser before type checking
- Clarified that complementarity check verifies types are complementary, not that pairs exist (SRSW guarantees pair existence)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.5 | 2025-01-08 | Add getAcceptedLabels; complete algorithms |
| 0.6 | 2025-01-10 | Update for ProgramDFA v0.8 |
| 0.7 | 2025-01-12 | Update for paper Definition 4.10; add interactive type examples |
| 0.8 | 2025-01-12 | Add SRSW precondition; clarify complementarity checks types not existence |
