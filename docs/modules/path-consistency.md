# Module: path-consistency

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.3 (Consistent Paths), lines 229-245; Example 4.4, lines 247-273

## Purpose

Determines when a moded term path and a GLP type path are consistent. This is the core predicate for well-typing: a moded term is well-typed if every term path has a consistent type path.

## Dependencies

- `moded-term` — ModedPath, PathStep, Mode
- `type-dfa` — TypeDFA, type path representation

## Definitions

### Consistent Paths (Paper Definition 4.3, lines 229-245)

Let `x` be a moded term path and `y` be a GLP type path. Then `x` and `y` are **consistent** if:

1. **Equal length, matching structure:** They are of equal length and identical except for their last symbols, which are consistent (see Consistent Endings below).

2. **Term path is prefix (variable at leaf):** `x` is a prefix of `y` except for its last symbol, which is:
   - (a) a reader `X?` and the mode of the corresponding type symbol is consume `↓`, or
   - (b) a writer `X` and the mode of the corresponding type symbol is produce `↑`

3. **Type path is prefix (primitive type at leaf):** `y` is a prefix of `x` except for its last symbol, which is:
   - (a) `_?` and the mode of the corresponding term symbol is consume `↓`, or
   - (b) `_` and the mode of the corresponding term symbol is produce `↑`

### Consistent Endings (Paper lines 216-226)

When term path and type path have equal length, their last symbols must be consistent:

| Primitive Term | Consistent Primitive Type |
|----------------|---------------------------|
| integer `1` | `Integer`, or the same integer `1` |
| string `"c"` | `String`, or the same string `"c"` |
| reader `X?` | `_?` (consumed term) |
| writer `X` | `_` (produced term) |

### Variable Type Assignment

When a term path ending in a variable is consistent with a type path, the variable is assigned the type at the corresponding position:
- Reader `X?` at position with type `T` and mode `↓` → `X?` has type `T` (consumed)
- Writer `X` at position with type `T` and mode `↑` → `X` has type `T` (produced)

## Public Interface

### Functions

#### `bool areConsistent(ModedPath termPath, TypePath typePath)`
Checks if a term path and type path are consistent per Definition 4.3.

**Preconditions:**
- `termPath` is a valid moded term path
- `typePath` is a valid type path

**Postconditions:** Returns true iff the paths satisfy one of the three consistency conditions.

#### `ConsistencyResult checkConsistency(ModedPath termPath, TypePath typePath)`
Checks consistency and returns detailed result including variable type assignment.

**Postconditions:** Returns a result indicating:
- Whether paths are consistent
- Which case applies (1, 2a, 2b, 3a, 3b, or none)
- Variable type assignment if applicable

### Types

#### `class ConsistencyResult`
```dart
class ConsistencyResult {
  final bool isConsistent;
  final ConsistencyCase? matchCase;  // Which case matched
  final String? variableName;         // If case 2, the variable
  final String? assignedType;         // Type assigned to variable
  final Mode? assignedMode;           // Mode of assigned type
}

enum ConsistencyCase {
  equalLength,      // Case 1
  termPrefixReader, // Case 2(a)
  termPrefixWriter, // Case 2(b)
  typePrefixInput,  // Case 3(a)
  typePrefixOutput, // Case 3(b)
}
```

## Algorithms

### Algorithm: Path Consistency Check

```
areConsistent(termPath, typePath):
  // Get path lengths (excluding root annotation)
  termLen = termPath.steps.length
  typeLen = typePath.steps.length
  
  // Case 1: Equal length
  if termLen == typeLen:
    return checkEqualLengthConsistency(termPath, typePath)
  
  // Case 2: Term path is prefix of type path
  if termLen < typeLen:
    return checkTermPrefixConsistency(termPath, typePath)
  
  // Case 3: Type path is prefix of term path
  if typeLen < termLen:
    return checkTypePrefixConsistency(termPath, typePath)
  
  return false

checkEqualLengthConsistency(termPath, typePath):
  // All steps except last must match exactly (same argIndex, same mode)
  for i in 0..<termPath.steps.length - 1:
    if not stepsMatch(termPath.steps[i], typePath.steps[i]):
      return false
  
  // Last symbols must be consistent endings
  termLeaf = termPath.leaf
  typeLeaf = typePath.leaf
  return areConsistentEndings(termLeaf, typeLeaf)

checkTermPrefixConsistency(termPath, typePath):
  // Term path is shorter; its leaf must be a variable
  termLeaf = termPath.leaf
  
  if not isVariable(termLeaf):
    return false
  
  // All term steps except last must match corresponding type steps
  for i in 0..<termPath.steps.length - 1:
    if not stepsMatch(termPath.steps[i], typePath.steps[i]):
      return false
  
  // Get the type symbol at the position where term path ends
  correspondingTypeStep = typePath.steps[termPath.steps.length - 1]
  typeMode = correspondingTypeStep.mode
  
  // Case 2(a): reader X? with consume mode
  if isReader(termLeaf) and typeMode == Mode.consume:
    return true
  
  // Case 2(b): writer X with produce mode  
  if isWriter(termLeaf) and typeMode == Mode.produce:
    return true
  
  return false

checkTypePrefixConsistency(termPath, typePath):
  // Type path is shorter; its leaf must be a primitive type
  typeLeaf = typePath.leaf
  
  if not isPrimitiveType(typeLeaf):
    return false
  
  // All type steps except last must match corresponding term steps
  for i in 0..<typePath.steps.length - 1:
    if not stepsMatch(termPath.steps[i], typePath.steps[i]):
      return false
  
  // Get the term symbol at the position where type path ends
  correspondingTermStep = termPath.steps[typePath.steps.length - 1]
  termMode = correspondingTermStep.mode
  
  // Case 3(a): _? with consume mode
  if typeLeaf.symbol == "_?" and termMode == Mode.consume:
    return true
  
  // Case 3(b): _ with produce mode
  if typeLeaf.symbol == "_" and termMode == Mode.produce:
    return true
  
  return false

stepsMatch(termStep, typeStep):
  return termStep.argIndex == typeStep.argIndex and
         termStep.mode == typeStep.mode and
         structuresMatch(termStep.symbol, typeStep.symbol)

structuresMatch(termSymbol, typeSymbol):
  // Term has functor/arity, type has type name
  // They match if same structural symbol (functor/arity)
  // Type names are stripped for comparison
  return extractStructure(termSymbol) == extractStructure(typeSymbol)

areConsistentEndings(termLeaf, typeLeaf):
  termSymbol = termLeaf.symbol
  typeSymbol = typeLeaf.symbol
  
  // Integer constant matches Integer or same value
  if isInteger(termSymbol):
    return typeSymbol == "Integer" or typeSymbol == termSymbol
  
  // String constant matches String or same value
  if isString(termSymbol):
    return typeSymbol == "String" or typeSymbol == termSymbol
  
  // Reader matches _?
  if isReader(termSymbol):
    return typeSymbol == "_?"
  
  // Writer matches _
  if isWriter(termSymbol):
    return typeSymbol == "_"
  
  // Atom/constant matches same atom/constant
  return termSymbol == typeSymbol
```

## Examples

### Example: Case 2(a) — Reader at Consumed Position

From paper Example 4.4 (lines 256-260):

Type path for argument 1 of `merge(Stream?, Stream?, Stream)`:
```
(0,↓) --> merge --(1,↓)--> Stream? --(1,↓)--> "."/2 --(1,↓)--> _?
```

Term path in moded head `H'`:
```
(0,↓) --> merge --(1,↓)--> "."/2 --(1,↓)--> X?
```

**Analysis:**
- Term path length: 3 (merge, "."/2, X?)
- Type path length: 4 (merge, Stream?, "."/2, _?)
- Term path is prefix → Case 2
- Last term symbol is reader `X?`
- Corresponding type mode is `↓` (consume)
- **Case 2(a) applies: consistent**

Variable assignment: `X?` has type `_?` (consumed).

### Example: Case 2(b) — Writer at Produced Position

From paper Example 4.4 (lines 266-270):

Type path for argument 3:
```
(0,↓) --> merge --(3,↑)--> Stream --(1,↑)--> "."/2 --(1,↑)--> _
```

Term path in moded head `H'`:
```
(0,↓) --> merge --(3,↑)--> "."/2 --(1,↑)--> X
```

**Analysis:**
- Term path is prefix → Case 2
- Last term symbol is writer `X`
- Corresponding type mode is `↑` (produce)
- **Case 2(b) applies: consistent**

Variable assignment: `X` has type `_` (produced).

### Example: Case 1 — Equal Length with Constant

Term path:
```
(0,↓) --> merge --(1,↓)--> []
```

Type path:
```
(0,↓) --> merge --(1,↓)--> Stream? --(1,↓)--> []
```

Wait, these aren't equal length. Let me reconsider...

Actually for Case 1, both paths must reach a leaf:
```
Term: (0,↓) --> foo --(1,↑)--> 42
Type: (0,↓) --> foo --(1,↑)--> Integer
```

**Analysis:**
- Equal length: 2 steps each
- All steps match except last
- Last: `42` matches `Integer`
- **Case 1 applies: consistent**

### Example: Inconsistent — Mode Mismatch

Term path:
```
(0,↓) --> merge --(1,↓)--> "."/2 --(1,↓)--> X    // Writer at ↓ position!
```

Type path:
```
(0,↓) --> merge --(1,↓)--> Stream? --(1,↓)--> "."/2 --(1,↓)--> _?
```

**Analysis:**
- Term path is prefix → Check Case 2
- Last term symbol is writer `X`
- Corresponding type mode is `↓` (consume)
- Case 2(b) requires `↑` (produce), but we have `↓`
- **Inconsistent!**

## Error Conditions

This module does not throw exceptions—it returns boolean or result indicating consistency.

## Notes

### Relationship to Well-Typing

Path consistency is used in Definition 4.4 (Well-Typed Moded Term):
> A moded term T is well-typed by a GLP type D if for each term path x ∈ paths(T) there is a consistent path y ∈ paths(D).

The type checker iterates over all term paths and checks each against the type DFA.

### Complementary Variable Types

When checking well-typing, paired variables must have complementary types (paper line 276-277):
- If `X?` has type `T?` (consumed)
- Then `X` must have type `T` (produced)

This is checked at the clause level, not within path consistency.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
