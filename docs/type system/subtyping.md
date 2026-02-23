# Subtyping — Implementation Specification

**Paper Reference**: Section 4.6, Definitions 4.5–4.9
**Theoretical Spec**: This file, sections 1–3 (unchanged)
**Implementation Spec**: Section 4 onward (new)
**Status**: Not yet implemented
**Branch**: `subtyping`

---

## 1. Motivation

Well-typing requires type consistency for variable pairs. The strict requirement is that the type of X be the dual of the type of X?. But this is stronger than necessary. All that is needed is that the type of X be a **subtype** of the dual of the type of X?.

Informally: anything produced by writer X can be consumed by reader X?.

## 2. Paper Definitions

### Definition 4.5 (Simple Prefix)

A **simple prefix** of an output type T is a path in T's type automaton starting from T that contains no mode inversions. A simple prefix ends when it reaches either:
- The produced primitive `_` (which accepts any produced term), or
- A mode inversion point: any position whose type is marked with `?` (including `_?`, `S?` for user-defined S, or `Integer?`, etc.).

Since the type automaton is deterministic, each position along a simple prefix corresponds to exactly one functor.

### Definition 4.6 (Prefix Acceptance)

A simple prefix p of type A is **accepted by** type B if B has a simple prefix q with identical functor/position structure, where endpoints satisfy:
- `_` matches only `_`
- output type S matches S or `_`
- mode inversion S? matches any mode inversion at the same position.

### Definition 4.7 (Subtyping)

Let A and B be GLP output types. We say A is a **subtype of** B, written A <: B, if:

1. Every simple prefix of A is accepted by B.
2. For every mode inversion point in A reached by a simple prefix—say, type A'? at that position—there is a corresponding mode inversion point B'? in B at the same position, and **B' is a subtype of A'**.

Condition 2 is **coinductive**: subtyping at mode inversion points is checked recursively with the containment direction **reversed** (since A' and B' are the output types before dualization).

### Definition 4.8 (Well-Typed Clause with Subtyping)

Same as Definition 4.6 (Well-Typed Clause) but condition 3(a) for body-body variable pairs is relaxed: instead of requiring dual types (S = T), it suffices that S <: T.

### Definition 4.9 (Well-Typed Program with Subtyping)

Same as Definition 4.7 (Well-Typed Program) with "well-typed clause" replaced by "well-typed clause with subtyping".

## 3. Variance

The coinductive structure embodies standard variance:
- **Covariant** in output positions: subtype's functors must be contained in supertype's
- **Contravariant** in input positions: at mode inversion points, containment direction reverses

This matches session type subtyping where output types are covariant and input types are contravariant.

---

## 4. Algorithm on the DFA

Subtyping is checked between two **output** DFAStates (isDual == false). The algorithm walks the type automata of both types simultaneously, checking that every transition from A has a matching transition from B.

### 4.1 Core Algorithm: `isSubtype(stateA, stateB, dfa, visited)`

```
isSubtype(stateA, stateB, dfa, visited):
  // Coinductive: if we've already assumed this pair, succeed
  if (stateA, stateB) in visited: return true
  visited.add((stateA, stateB))

  // Reflexivity
  if stateA == stateB: return true

  // Both must be output types (not dual)
  assert !stateA.isDual && !stateB.isDual

  // Handle primitive type hierarchy (section 4.2)
  if stateA.isPrimitiveType or stateB.isPrimitiveType:
    return checkPrimitiveSubtype(stateA, stateB)

  // Get automata for both types
  automA = dfa.getAutomaton(stateA.name)
  automB = dfa.getAutomaton(stateB.name)

  // Check every transition from A has a matching transition from B
  for each (stateA, label) -> targetA in automA.transitions:
    targetB = automB.transition(stateB, label)
    if targetB is null: return false    // A has an alternative B lacks

    // Skip trivially equal targets
    if targetA == targetB: continue

    // Check target compatibility
    if not checkTargetSubtype(targetA, targetB, dfa, visited):
      return false

  return true
```

### 4.2 Target Compatibility: `checkTargetSubtype(targetA, targetB, dfa, visited)`

The targets of matching transitions can be:

1. **Both output types** (neither isDual): Covariant recursion.
   `return isSubtype(targetA, targetB, dfa, visited)`

2. **Both dual types** (both isDual): Mode inversion point. Contravariant recursion.
   Extract the base output types and recurse in reverse direction:
   ```
   innerA = dfa.getState(targetA.baseName)  // output type A'
   innerB = dfa.getState(targetB.baseName)  // output type B'
   return isSubtype(innerB, innerA, dfa, visited)  // REVERSED
   ```

3. **Mixed** (one isDual, other not): Incompatible mode structure.
   `return false`

### 4.3 Primitive Type Subtyping

Primitive types have a fixed subtype lattice. No transitions need checking.

| Relation | Holds? | Reason |
|----------|--------|--------|
| Integer <: Integer | yes | reflexive |
| Real <: Real | yes | reflexive |
| Number <: Number | yes | reflexive |
| String <: String | yes | reflexive |
| Integer <: Number | yes | integers are numbers |
| Real <: Number | yes | reals are numbers |
| _ <: _ | yes | reflexive |
| T <: _ | yes | _ is the top output type |
| _ <: T (T ≠ _) | no | _ is broader than any specific type |
| Integer <: Real | no | disjoint |
| Integer <: String | no | disjoint |
| Number <: Integer | no | numbers include reals |

Summary:
- `_` is top (any output type is a subtype of `_`)
- `Integer <: Number`, `Real <: Number`
- Otherwise, primitive types must be identical

### 4.4 Wildcard Handling

- If stateA is `_` (produced wildcard): only subtype of `_` itself. `_` accepts everything, so it is NOT a subtype of narrower types.
- If stateB is `_` (produced wildcard): any type is a subtype of `_`. Return true.
- Same logic applies to `_FINAL_` (anonymous final state).

### 4.5 Coinductive Termination

The `visited` set stores `(stateA, stateB)` pairs. Since the DFA has finitely many states, the recursion terminates. The coinductive assumption is sound: if we revisit a pair, the check would loop forever on the same cyclic structure, which means the types are structurally compatible along that cycle.

---

## 5. Integration Point

### 5.1 Where Subtyping is Checked

In `well_typed_clause.dart`, function `_checkClauseDuality`:

**Current behavior (exact duality):**
For body-body variable pairs (writer X at type S, reader X? at type T?):
- Requires `S.baseName == T.baseName` (exact match)

**New behavior (subtyping):**
For body-body variable pairs:
- Requires `isSubtype(S_output, T_output, dfa)` where:
  - `S_output` is the output DFAState for the writer's type
  - `T_output` is the output DFAState corresponding to the reader's dual

In concrete terms: writer X has type state S (output). Reader X? has type state T? (dual). The duality requirement is: S <: T. Both S and T are output types.

### 5.2 What Does NOT Change

- **Head-head pairs**: Still require exact duality (S = T).
- **Head-body pairs**: Still require same type.
- **Well-typed term checking**: No change.
- **Moded term construction**: No change.
- **Input coverage checking**: No change.
- **DFA construction**: No change.

### 5.3 Signature

```dart
/// Check if output type A is a subtype of output type B.
///
/// Both stateA and stateB must be output types (isDual == false).
/// Uses coinductive algorithm with visited set for cycle detection.
///
/// Paper Reference: Definition 4.7 (Subtyping)
bool isSubtype(DFAState stateA, DFAState stateB, ProgramDFA dfa);
```

---

## 6. Test Programs

Test programs are in `glp_runtime/test/programs/typechecker/`:

### 6.1 Positive (require subtyping, should pass after implementation)

Directory: `positive/subtyping/`

| File | Scenario | Subtyping Required |
|------|----------|-------------------|
| `basic_readop_fileop.glp` | Paper example: ReadOp <: FileOp | Fewer struct alternatives |
| `constants_fewer_alternatives.glp` | WarmColor <: Color | Fewer constant alternatives |
| `contravariant_response_slot.glp` | FlexibleReq <: FullReq | Contravariance at mode inversion |
| `direct_constant_subtype.glp` | Pet <: Animal | Direct, no streams |
| `struct_fewer_functors.glp` | GetOp <: DbOp | Fewer struct alternatives |

### 6.2 Negative (should fail even with subtyping)

Directory: `negative/subtyping/`

| File | Scenario | Why It Must Fail |
|------|----------|-----------------|
| `wrong_direction_fileop_readop.glp` | FileOp ≮: ReadOp | Extra alternatives in subtype |
| `contravariant_wrong_direction.glp` | Contravariance violation | WideResp ≮: NarrowResp |
| `disjoint_types.glp` | Fruit ≮: Color | Zero overlap |
| `arg_type_mismatch.glp` | Integer ≠ String at output position | Incompatible primitives |

---

## 7. Error Messages

When subtyping fails, the error message should indicate:
- The writer variable, its type, and location
- The reader variable, its dual type, and location
- That subtyping failed (not just duality)
- Ideally: the first transition in A that has no match in B

Example:
```
Variable pair (X, X?) in body: writer type ReadOpStream is not a subtype of 
FileOpStream (required for body-body pair). ReadOpStream has alternative 
'write' not accepted by FileOpStream.
```
