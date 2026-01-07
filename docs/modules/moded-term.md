# Module: moded-term

**Version**: 0.3  
**Date**: 2025-01-08  
**Status**: DRAFT  
**Paper References**: Definition 4.2 (Moded Term, Complement), lines 179-211

## Purpose

Represents moded terms—GLP terms with mode annotations (↓ consume, ↑ produce) on non-variable subterms—and provides operations for extracting moded paths and computing complements.

## Dependencies

- `mode` — Mode enum
- `compiler/ast` — GLP term representation (Term, Variable, Compound, Constant)

## Definitions

### Definition 4.2: Moded Term

A **moded term** T' corresponding to a GLP term T is the result of adding a **mode annotation** (either ↓ *consume* or ↑ *produce*) to T and to every non-variable subterm of T.

Variables do not receive explicit mode annotations; their communication direction is determined by whether they are readers (`X?`) or writers (`X`):
- Reader `X?` → consumed (↓)
- Writer `X` → produced (↑)

### Definition 4.2: Complement

Given a moded term T, its **complement** T? is obtained by:
1. Flipping every mode annotation (↓ ↔ ↑)
2. Replacing every variable by its paired variable (`X` ↔ `X?`)

The complement operation is an involution: (T?)? = T.

### Moded Paths (Paper lines 202-210)

A **moded path** is a sequence extracted from a moded term:
```
(0, m₀) --> f/n --(i₁, m₁)--> ... --(iₖ, mₖ)--> leaf
```

The leaf is either:
- A constant (integer, string, atom)
- A variable (reader or writer)

For a moded term T, **paths(T)** denotes the set of all moded paths.

## Public Interface

### Types

#### `class ModedTerm`

A moded term: a GLP term with mode annotations.

```dart
abstract class ModedTerm {
  Mode get mode;  // The mode annotation of this term
}
```

#### `class ModedCompound extends ModedTerm`

A compound term with mode annotation.

```dart
class ModedCompound extends ModedTerm {
  final Mode mode;
  final String functor;
  final int arity;
  final List<ModedTerm> args;
}
```

#### `class ModedConstant extends ModedTerm`

A constant (integer, string, atom) with mode annotation.

```dart
class ModedConstant extends ModedTerm {
  final Mode mode;
  final Object value;  // int, String, or atom name
}
```

#### `class ModedVariable extends ModedTerm`

A variable (reader or writer). Variables have implicit mode based on reader/writer status.

```dart
class ModedVariable extends ModedTerm {
  final String name;
  final bool isReader;  // true for X?, false for X
  
  // Implicit mode: readers are consumed, writers are produced
  Mode get mode => isReader ? Mode.consume : Mode.produce;
}
```

#### `class ModedPath`

A path through a moded term.

```dart
class ModedPath {
  final List<PathStep> steps;
  
  PathStep get root => steps.first;
  PathStep get leaf => steps.last;
  
  bool get isInputPath => root.mode == Mode.consume;
  bool get isOutputPath => root.mode == Mode.produce;
}
```

#### `class PathStep`

A single step in a moded path.

```dart
class PathStep {
  final String symbol;    // functor/arity for compound, value for constant, name for variable
  final int argIndex;     // 0 for root, 1-based for arguments
  final Mode mode;        // Mode at this position
  final bool isVariable;  // True if this is a variable leaf
  final bool isReader;    // If isVariable, whether it's a reader
}
```

### Functions

#### `ModedTerm complement(ModedTerm t)`

Constructs the complement of a moded term per Definition 4.2.

**Postconditions:** Returns a ModedTerm where:
- Every mode annotation is flipped (↓ ↔ ↑)
- Every variable is replaced by its pair (X ↔ X?)
- complement(complement(t)) == t (involution)

#### `Set<ModedPath> paths(ModedTerm t)`

Extracts all moded paths from a moded term.

**Postconditions:** Returns the set of all paths from root to leaves.

## Algorithms

### Algorithm: Complement Construction

```
complement(t):
  match t:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(
        mode.flip,
        functor,
        arity,
        args.map(complement)
      )
    ModedConstant(mode, value):
      return ModedConstant(mode.flip, value)
    ModedVariable(name, isReader):
      return ModedVariable(name, !isReader)  // Flip reader/writer
```

### Algorithm: Path Extraction

```
paths(t):
  result = {}
  rootStep = PathStep(
    symbol: symbolOf(t),
    argIndex: 0,
    mode: t.mode,
    isVariable: t is ModedVariable,
    isReader: t is ModedVariable ? t.isReader : false
  )
  extractPaths(t, [rootStep], result)
  return result

extractPaths(t, prefix, result):
  match t:
    ModedCompound(mode, functor, arity, args):
      for i in 1..arity:
        child = args[i-1]
        childStep = PathStep(
          symbol: symbolOf(child),
          argIndex: i,
          mode: child.mode,
          isVariable: child is ModedVariable,
          isReader: child is ModedVariable ? child.isReader : false
        )
        extractPaths(child, prefix + [childStep], result)
    
    ModedConstant, ModedVariable:
      // Leaf reached - add path
      result.add(ModedPath(prefix))

symbolOf(t):
  match t:
    ModedCompound(_, functor, arity, _): return "$functor/$arity"
    ModedConstant(_, value): return value.toString()
    ModedVariable(name, isReader): return isReader ? "$name?" : name
```

## Examples

### Example: Moded Head Paths

Moded head (from paper):
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Extracted paths:
```
Path 1: (merge/3, 0, ↓) → ([|]/2, 1, ↓) → (X?, 1, ↓)     // reader at ↓
Path 2: (merge/3, 0, ↓) → ([|]/2, 1, ↓) → (Xs?, 2, ↓)    // reader at ↓
Path 3: (merge/3, 0, ↓) → (Ys?, 2, ↓)                     // reader at ↓
Path 4: (merge/3, 0, ↓) → ([|]/2, 3, ↑) → (X, 1, ↑)      // writer at ↑
Path 5: (merge/3, 0, ↓) → ([|]/2, 3, ↑) → (Zs, 2, ↑)     // writer at ↑
```

### Example: Complement

Original:
```
↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Complement:
```
↑merge(↑[↑X|Xs], Ys, ↓[↓X?|Zs?])
```

Note: modes flipped (↓↔↑), variables flipped (X?↔X, Xs?↔Xs, Ys?↔Ys, Zs↔Zs?).

## Error Conditions

None. All operations succeed on valid moded terms.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Add negative examples |
| 0.3 | 2025-01-08 | Simplify: remove consumedTerm, producedTerm, ioModedTerm (moved to moded-head); depend on mode module |
