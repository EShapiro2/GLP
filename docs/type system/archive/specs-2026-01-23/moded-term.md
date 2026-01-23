# Module: moded-term

**Version**: 0.9
**Date**: 2026-01-23
**Status**: DRAFT  
**Paper References**: Definition 5.1 (Moded Term, Dual), Remark [Structural Mode vs. Variable Mode]

## Purpose

Represents moded terms—GLP terms with mode annotations (↓ consume, ↑ produce) on non-variable subterms—and provides operations for extracting moded paths and computing duals.

## Dependencies

- `mode` — Mode enum
- `compiler/ast` — GLP term representation (Term, Variable, Compound, Constant)

## Definitions

### Definition 5.1: Moded Term

A **moded term** T' corresponding to a GLP term T is the result of adding one of two **mode annotations**, consume ↓ or produce ↑, to T and to every non-variable subterm of T.

Variables do not receive explicit mode annotations; their communication direction is determined by whether they are readers (`X?`) or writers (`X`):
- Reader `X?` → implicit mode ↓ (consume)
- Writer `X` → implicit mode ↑ (produce)

### Definition 5.1: Dual

Given a moded term T, its **dual** T? is obtained by:
1. Flipping every mode annotation (↓ ↔ ↑)
2. Replacing every variable by its paired variable (`X` ↔ `X?`)

The dual operation is an involution: (T?)? = T.

### Remark: Structural Mode vs. Variable Mode

The mode annotations in a moded term tree are **structural**—they describe the direction of data flow at each position, as determined by the type context. Variables do not carry mode annotations themselves. Instead, each variable has an **implicit mode**:
- Readers have implicit mode consume (↓)
- Writers have implicit mode produce (↑)

### Anonymous Variables in Programs

The anonymous variable `_` in GLP programs is distinct from `_` as a type symbol:

| Context | Syntax | Meaning |
|---------|--------|--------|
| Type definition | `_` | Primitive output type (any produced term) |
| Type definition | `_?` | Primitive input type (any consumed term) |
| Program | `_` | Anonymous variable (discards a value) |
| Program | `_?` | **Not allowed** (no use case) |

**SRSW Exception for Anonymous Variables:**

The SRSW restriction requires every variable to have a paired counterpart (`X` paired with `X?`). The anonymous variable `_` is an exception to this rule:

1. Each occurrence of `_` is treated as a **fresh writer with no paired reader**
2. Anonymous `_` may appear anywhere a writer variable may appear
3. Values assigned to `_` are discarded
4. `_?` is **never permitted** in programs—there is no use case for an anonymous reader

**Example:**
```
% Valid: _ discards values in head
second([_, X | _], X?).

% Valid: _ discards output in body
foo(X) :- bar(_, X?).

% Invalid: _? is never allowed in programs  
bad(_?, X).  % Error: anonymous reader not permitted
```

**Rationale:** The anonymous variable allows programmers to indicate that a value is intentionally discarded without cluttering the namespace with unused variable names.

For a moded term to be well-typed, each variable's implicit mode must be **consistent** with the structural mode of its position. Specifically:
- A reader X? (implicit mode ↓) may appear at a position with structural mode ↓
- A writer X (implicit mode ↑) may appear at a position with structural mode ↑

The structural mode is inherited from the enclosing type context and propagates through the term structure; the implicit mode is intrinsic to the variable's reader/writer form. These are distinct concepts that must agree at variable positions.

### Moded Term Classification (Paper lines 186-194)

A moded term is classified by its mode structure:

| Classification | Definition | Example |
|----------------|------------|---------|
| **Consumed** | All structural modes are ↓ | `↓[↓X?|Xs?]` |
| **Produced** | All structural modes are ↑ | `↑[↑X|Xs]` |
| **I/O** | Root mode is ↓, with at most one flip to ↑ (for a produced subtree) on any path | `↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])` |

**Invariant:** A well-formed moded head is always I/O—it starts consumed (↓) and may contain produced (↑) subtrees for output arguments.

### Path Direction (Paper lines 202-210)

A moded path has a **direction** determined by its root mode:

| Direction | Root Mode | Expected Variable at Leaf |
|-----------|-----------|---------------------------|
| **Input path** | ↓ (consume) | Reader (X?) |
| **Output path** | ↑ (produce) | Writer (X) |

### Moded Paths (Paper lines 202-210)

A **moded path** is a sequence extracted from a moded term:
```
(0, m₀) --> f/n --(i₁, m₁)--> ... --(iₖ, mₖ)--> leaf
```

Where:
- m₀ is the root structural mode
- Each (iⱼ, mⱼ) is an argument index and structural mode
- The leaf is either a constant or a variable

For a moded term T, **paths(T)** denotes the set of all moded paths.

## Public Interface

### Types

#### `class ModedTerm`

A moded term: a GLP term with mode annotations.

```dart
abstract class ModedTerm {
  /// The structural mode annotation of this term position
  Mode get mode;
}
```

#### `class ModedCompound extends ModedTerm`

A compound term with mode annotation.

```dart
class ModedCompound extends ModedTerm {
  final Mode mode;          // Structural mode at this position
  final String functor;
  final int arity;
  final List<ModedTerm> args;
}
```

#### `class ModedConstant extends ModedTerm`

A constant (integer, real, string, atom) with mode annotation.

```dart
class ModedConstant extends ModedTerm {
  final Mode mode;          // Structural mode at this position
  final Object value;       // int, double, String, or atom name
  
  bool get isInteger => value is int;
  bool get isReal => value is double;
  bool get isNumeric => value is num;
  bool get isString => value is String && !isAtom;
  bool get isAtom => /* atom detection logic */;
}
```

#### `class ModedVariable extends ModedTerm`

A variable (reader or writer). Variables have implicit mode based on reader/writer status.

```dart
class ModedVariable extends ModedTerm {
  final String name;
  final bool isReader;  // true for X?, false for X
  
  /// Implicit mode: readers are consumed, writers are produced
  /// Note: This is the variable's intrinsic mode, not the structural mode
  Mode get mode => isReader ? Mode.consume : Mode.produce;
  
  /// The paired variable (X ↔ X?)
  ModedVariable get paired => ModedVariable(name, !isReader);
}
```

#### `class ModedPath`

A path through a moded term.

```dart
class ModedPath {
  final List<PathStep> steps;
  
  PathStep get root => steps.first;
  PathStep get leaf => steps.last;
  
  /// Direction based on root structural mode
  bool get isInputPath => root.mode == Mode.consume;
  bool get isOutputPath => root.mode == Mode.produce;
  
  /// The structural mode at the leaf position
  Mode get leafStructuralMode => steps[steps.length - 1].mode;
}
```

#### `class PathStep`

A single step in a moded path.

```dart
class PathStep {
  final String symbol;      // functor/arity for compound, value for constant, name for variable
  final int argIndex;       // 0 for root, 1-based for arguments
  final Mode mode;          // Structural mode at this position
  final bool isVariable;    // True if this is a variable leaf
  final bool isReader;      // If isVariable, whether it's a reader
  final bool isConstant;    // True if this is a constant leaf
  final Object? value;      // If isConstant, the constant value
}
```

### Functions

#### `bool isConsumed(ModedTerm t)`

Returns true if all structural mode annotations in t are consume (↓).

**Postconditions:**
- Returns true iff every non-variable subterm has mode = Mode.consume

#### `bool isProduced(ModedTerm t)`

Returns true if all structural mode annotations in t are produce (↑).

**Postconditions:**
- Returns true iff every non-variable subterm has mode = Mode.produce

#### `bool isIO(ModedTerm t)`

Returns true if t is an I/O moded term: root is consume (↓) with at most one mode-inversion to produce (↑) on any path.

**Preconditions:**
- t is a valid moded term

**Postconditions:**
- Returns true iff:
  - Root mode is Mode.consume, AND
  - On every path from root to leaf, mode transitions only in the direction ↓ → ↑ (never ↑ → ↓)

**Algorithm:**
```
isIO(t):
  if t.mode != Mode.consume:
    return false
  return allPathsValidIO(t, Mode.consume)

allPathsValidIO(t, parentMode):
  currentMode = t.mode

  // Check valid transition: only ↓→↑ allowed, not ↑→↓
  if parentMode == Mode.produce and currentMode == Mode.consume:
    return false  // Invalid: flipped back from ↑ to ↓

  match t:
    ModedCompound(_, _, _, args):
      return args.all(arg => allPathsValidIO(arg, currentMode))
    ModedConstant, ModedVariable:
      return true  // Leaf reached
```

#### `ModedTerm dual(ModedTerm t)`

Constructs the dual of a moded term per Definition 5.1.

**Postconditions:** Returns a ModedTerm where:
- Every mode annotation is flipped (↓ ↔ ↑)
- Every variable is replaced by its pair (X ↔ X?)
- dual(dual(t)) == t (involution)

#### `Set<ModedPath> paths(ModedTerm t)`

Extracts all moded paths from a moded term.

**Postconditions:** Returns the set of all paths from root to leaves.

#### `bool variableMatchesStructuralMode(ModedVariable v, Mode structuralMode)`

Checks if a variable's implicit mode matches the structural mode at its position.

**Postconditions:**
- Returns true iff:
  - v.isReader AND structuralMode == Mode.consume, OR
  - !v.isReader AND structuralMode == Mode.produce

## Algorithms

### Algorithm: Dual Construction

```
dual(t):
  match t:
    ModedCompound(mode, functor, arity, args):
      return ModedCompound(
        mode.flip,
        functor,
        arity,
        args.map(dual)
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
    isReader: t is ModedVariable ? t.isReader : false,
    isConstant: t is ModedConstant,
    value: t is ModedConstant ? t.value : null
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
          isReader: child is ModedVariable ? child.isReader : false,
          isConstant: child is ModedConstant,
          value: child is ModedConstant ? child.value : null
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

### Example 1: Moded Head Paths

Moded head (from paper):
```
H' = ↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Extracted paths with structural modes:
```
Path 1: (merge/3, 0, ↓) → ([|]/2, 1, ↓) → (X?, 1, ↓)     // reader at ↓ position ✓
Path 2: (merge/3, 0, ↓) → ([|]/2, 1, ↓) → (Xs?, 2, ↓)    // reader at ↓ position ✓
Path 3: (merge/3, 0, ↓) → (Ys?, 2, ↓)                     // reader at ↓ position ✓
Path 4: (merge/3, 0, ↓) → ([|]/2, 3, ↑) → (X, 1, ↑)      // writer at ↑ position ✓
Path 5: (merge/3, 0, ↓) → ([|]/2, 3, ↑) → (Zs, 2, ↑)     // writer at ↑ position ✓
```

All variables match their structural modes.

### Example 2: Dual

Original:
```
↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

Dual (all modes flipped, all variables flipped):
```
↑merge(↑[↑X|Xs], Ys, ↓[↓X?|Zs?])
```

### Example 3: Interactive Type (HollowIntegers)

Moded head for `consumer([X1, X2, X3 | Xs?])` with type `consumer(HollowIntegers)`:
```
H' = ↓consumer(↑[↓X1?, ↓X2?, ↓X3? | ↑Xs])
```

Note the mixed modes within the list:
- The list structure at arg 1 has mode ↑ (output type)
- The head elements have mode ↓ (due to Integer? in type definition)
- The tail has mode ↑

Paths:
```
Path 1: (consumer/1, 0, ↓) → ([|]/2, 1, ↑) → (X1?, 1, ↓)  // reader at ↓ position ✓
Path 2: (consumer/1, 0, ↓) → ([|]/2, 1, ↑) → ([|]/2, 2, ↑) → (X2?, 1, ↓)  // reader at ↓ ✓
...
Path n: (consumer/1, 0, ↓) → ... → (Xs, 2, ↑)             // writer at ↑ position ✓
```

### Example 4: Mode Mismatch (NEGATIVE)

If we incorrectly constructed:
```
↓foo(↓X)   // Writer X at position with structural mode ↓
```

Path analysis:
```
Path: (foo/1, 0, ↓) → (X, 1, ↓)  // writer at ↓ position ✗
```

The writer X has implicit mode ↑, but structural mode is ↓ — mismatch!

### Example 5: I/O Classification

```
↓merge(↓[↓X?|Xs?], Ys?, ↑[↑X|Zs])
```

- Root mode: ↓ ✓
- Path to X?: ↓ → ↓ → ↓ (no ↑→↓ transition) ✓
- Path to X: ↓ → ↑ → ↑ (only ↓→↑ transition) ✓

Result: isIO = true

### Example 6: NOT I/O (NEGATIVE)

```
↑foo(↓[↓X?|Xs?])  // Root is ↑, not ↓
```

isIO = false (root must be ↓)

```
↓bar(↑[↓X?])  // Has ↑→↓ transition
```

isIO = false (↑→↓ transition not allowed)

## Error Conditions

None. All operations succeed on valid moded terms.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Add negative examples |
| 0.3 | 2025-01-08 | Simplify: remove consumedTerm, producedTerm, ioModedTerm (moved to moded-head); depend on mode module |
| 0.4 | 2025-01-09 | Format cleanup |
| 0.5 | 2025-01-09 | Add isConsumed(), isProduced(), isIO() classification methods |
| 0.6 | 2025-01-12 | Add Remark 4.4 (Structural vs Variable Mode); add interactive type examples |
| 0.7 | 2025-01-14 | Add Anonymous Variables in Programs section (SRSW exception, _ vs _? distinction) |
| 0.8 | 2025-01-16 | Allow anonymous `_` anywhere (paper update) |
| 0.9 | 2026-01-23 | **Paper alignment**: "Complement" → "Dual" throughout; updated paper references to Definition 5.1; renamed `complement()` → `dual()` |
