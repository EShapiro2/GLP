# Module: moded-term

**Version**: 0.1  
**Date**: 2025-01-07  
**Status**: DRAFT  
**Paper References**: Definition 4.2 (Moded Term, Complement), lines 179-211

## Purpose

Represents moded terms—GLP terms with mode annotations (↓ consume, ↑ produce) on non-variable subterms—and provides operations for constructing moded terms, computing complements, and extracting moded paths.

## Dependencies

- `compiler/ast.dart` — GLP term representation (Term, Variable, Compound, Constant)

## Definitions

### Definition 4.2: Moded Term

A **moded term** T' corresponding to a GLP term T is the result of adding a **mode annotation** (either ↓ *consume* or ↑ *produce*) to T and to every non-variable subterm of T.

Variables do not receive explicit mode annotations; their communication direction is determined by whether they are readers (`X?`) or writers (`X`), which implies an implicit mode:
- Reader `X?` → consumed (↓)
- Writer `X` → produced (↑)

### Definition 4.2: Complement

Given a moded term T, its **complement** T? is obtained by:
1. Flipping every mode annotation (↓ ↔ ↑)
2. Replacing every variable by its paired variable (`X` ↔ `X?`)

The complement operation is an involution: (T?)? = T.

### Moded Term-Tree (Paper lines 191-201)

A moded term can be represented as a **moded term-tree**: a term tree with pair-labelled edges. Each edge is labelled with `(i, m)` where:
- `i` is the argument index (1-based) of the subterm within its parent
- `m` is the mode annotation (↓ or ↑)

The root has an incoming edge labelled `(0, m)` where `m` is the term's root mode.

### Moded Paths (Paper lines 202-210)

A **moded path** is a sequence extracted from a moded term-tree:
```
(0, m₀) --> f/n --(i₁, m₁)--> ... --(iₖ, mₖ)--> leaf
```

The leaf is either:
- A constant (integer, string, atom)
- A variable (reader or writer)

For a moded term T, **paths(T)** denotes the set of all moded paths in its moded term-tree.

### Path Classification (Paper line 211)

- **Input path**: A path where the root mode is ↓ (consume)
- **Output path**: A path where the root mode is ↑ (produce)

### Moded Term Classification (Paper line 211)

- **Consumed moded term**: All mode annotations are ↓
- **Produced moded term**: All mode annotations are ↑
- **I/O moded term**: Root is ↓, with at most one mode-inversion from ↓ to ↑ on any path (and no inversion back to ↓)

## Public Interface

### Types

#### `enum Mode`
```dart
enum Mode { consume, produce }
```
- `consume` represents ↓
- `produce` represents ↑

#### `Mode.flip`
```dart
Mode get flip => this == consume ? produce : consume;
```

#### `class ModedTerm`
A moded term: a GLP term with mode annotations.

```dart
abstract class ModedTerm {
  Mode get mode;  // The mode annotation of this term
  ModedTerm get complement;  // Definition 4.2: complement operation
  Set<ModedPath> get paths;  // Extract all moded paths
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
  
  // Paired variable: flip reader/writer status
  ModedVariable get paired => ModedVariable(name, !isReader);
}
```

#### `class ModedPath`
A path through a moded term-tree.

```dart
class ModedPath {
  final List<PathStep> steps;
  
  bool get isInput => steps.first.mode == Mode.consume;
  bool get isOutput => steps.first.mode == Mode.produce;
  
  PathStep get leaf => steps.last;
}
```

#### `class PathStep`
A single step in a moded path.

```dart
class PathStep {
  final int argIndex;  // 0 for root, 1-based for arguments
  final Mode mode;
  final String symbol;  // functor/arity, constant value, or variable (with ? for readers)
}
```

### Functions

#### `ModedTerm consumedTerm(Term t)`
Constructs a consumed moded term (all annotations ↓) from a GLP term.

**Preconditions:** `t` is a valid GLP term.

**Postconditions:** Returns a ModedTerm where:
- The root mode is `consume`
- Every non-variable subterm has mode `consume`

**Errors:** None. Always succeeds for valid terms.

#### `ModedTerm producedTerm(Term t)`
Constructs a produced moded term (all annotations ↑) from a GLP term.

**Preconditions:** `t` is a valid GLP term.

**Postconditions:** Returns a ModedTerm where:
- The root mode is `produce`
- Every non-variable subterm has mode `produce`

**Errors:** None. Always succeeds for valid terms.

#### `ModedTerm ioModedTerm(Term t, TypeDFA type)`
Constructs an I/O moded term from a GLP term guided by a type.

**Preconditions:** 
- `t` is a valid GLP term
- `type` provides mode information for each position

**Postconditions:** Returns a ModedTerm where:
- The root mode is `consume`
- Mode inversions from ↓ to ↑ occur where the type indicates production
- At most one inversion occurs on any path
- No inversion from ↑ back to ↓ occurs

**Errors:** Throws `InvalidIOModeError` if the type would require more than one inversion on a path or an inversion back to ↓.

#### `bool isConsumed(ModedTerm t)`
Returns true if all mode annotations in `t` are `consume`.

**Preconditions:** `t` is a valid ModedTerm.

**Postconditions:** Returns true iff every mode annotation in `t` is `consume`.

**Errors:** None. Always succeeds.

#### `bool isProduced(ModedTerm t)`
Returns true if all mode annotations in `t` are `produce`.

**Preconditions:** `t` is a valid ModedTerm.

**Postconditions:** Returns true iff every mode annotation in `t` is `produce`.

**Errors:** None. Always succeeds.

#### `bool isIO(ModedTerm t)`
Returns true if `t` is I/O moded: root is ↓ with at most one ↓→↑ inversion on any path and no ↑→↓ inversions.

**Preconditions:** `t` is a valid ModedTerm.

**Postconditions:** Returns true iff `t` satisfies the I/O moded term definition.

**Errors:** None. Always succeeds.

## Algorithms

### Algorithm: Complement Construction

Given a moded term `t`, construct its complement `t?`:

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
      return ModedVariable(name, !isReader)  // flip reader/writer
```

### Algorithm: Path Extraction

Given a moded term `t`, extract all paths:

```
paths(t):
  result = {}
  extractPaths(t, currentArgIndex=0, prefix=[], result)
  return result

extractPaths(t, currentArgIndex, prefix, result):
  currentStep = PathStep(
    argIndex: currentArgIndex,
    mode: t.mode,
    symbol: symbolOf(t)
  )
  newPrefix = prefix + [currentStep]
  
  match t:
    ModedCompound(_, _, _, args):
      for i in 1..args.length:
        extractPaths(args[i-1], i, newPrefix, result)
    ModedConstant, ModedVariable:
      result.add(ModedPath(newPrefix))

symbolOf(t):
  match t:
    ModedCompound(_, functor, arity, _): return "$functor/$arity"
    ModedConstant(_, value): return value.toString()
    ModedVariable(name, isReader): return isReader ? "$name?" : name
```

### Algorithm: I/O Moded Term Construction

Given a term `t` and type `type`, construct an I/O moded term:

```
ioModedTerm(t, type):
  return buildIO(t, type, parentMode=Mode.consume, inversionOccurred=false)

buildIO(t, typeState, parentMode, inversionOccurred):
  // Determine this node's mode from the type
  thisMode = modeFromType(typeState)
  
  // Check I/O constraint
  if parentMode == Mode.consume and thisMode == Mode.produce:
    if inversionOccurred:
      throw InvalidIOModeError("Multiple inversions on path")
    inversionOccurred = true
  else if parentMode == Mode.produce and thisMode == Mode.consume:
    throw InvalidIOModeError("Inversion from produce back to consume")
  
  match t:
    Compound(functor, args):
      modedArgs = []
      for i, arg in enumerate(args):
        childTypeState = typeState.transition(i+1)
        modedArgs.add(buildIO(arg, childTypeState, thisMode, inversionOccurred))
      return ModedCompound(thisMode, functor, args.length, modedArgs)
    Constant(value):
      return ModedConstant(thisMode, value)
    Variable(name, isReader):
      return ModedVariable(name, isReader)
```

## Examples

### Example: Consumed Moded Term

GLP term:
```
append([1,2], [3], Zs)
```

Consumed moded term:
```
↓append(↓[↓1,↓2], ↓[↓3], Zs)
```

### Example: Produced Moded Term

GLP term:
```
merge(Xs?, Ys?, [3|Zs])
```

Produced moded term:
```
↑merge(Xs?, Ys?, ↑[↑3|Zs])
```

### Example: I/O Moded Term (from paper, corrected)

GLP term (clause head):
```
merge([3|Xs?], Ys?, [3|Zs])
```

With type `merge(Stream?, Stream?, Stream)`:
- Arguments 1, 2 are consumed (Stream?)
- Argument 3 is produced (Stream)

I/O moded term:
```
↓merge(↓[↓3|Xs?], Ys?, ↑[↑3|Zs])
```

This is I/O: root is ↓, with one inversion to ↑ on argument 3.

### Example: Moded Term-Tree

For `↓merge(↓[↓3|Xs?], Ys?, ↑[↑3|Zs])`:

```
(0,↓): merge/3
    (1,↓): "."/2
        (1,↓): 3
        (2,↓): Xs?
    (2,↓): Ys?
    (3,↑): "."/2
        (1,↑): 3
        (2,↑): Zs
```

### Example: Extracted Paths

```
(0,↓) --> merge/3 --(1,↓)--> "."/2 --(1,↓)--> 3
(0,↓) --> merge/3 --(1,↓)--> "."/2 --(2,↓)--> Xs?
(0,↓) --> merge/3 --(2,↓)--> Ys?
(0,↓) --> merge/3 --(3,↑)--> "."/2 --(1,↑)--> 3
(0,↓) --> merge/3 --(3,↑)--> "."/2 --(2,↑)--> Zs
```

### Example: Complement

Original:
```
↓merge(↓[↓3|Xs?], Ys?, ↑[↑3|Zs])
```

Complement:
```
↑merge(↑[↑3|Xs], Ys, ↓[↓3|Zs?])
```

Note: modes flipped (↓↔↑), variables flipped (Xs?↔Xs, Ys?↔Ys, Zs↔Zs?).

### Example: INVALID — Multiple Inversions on Path

Attempting to construct an I/O moded term where a path has more than one inversion:

```
% Hypothetical type where list element is consumed but element's subfield is produced
BadType ::= bad([inner]).
inner ::= wrap(_?).  % _? inside a produce position would cause ↓→↑→↓

% Attempting: ↓bad(↑[↓wrap(↑X)])  -- TWO inversions on the path to X
```

**Error:** `InvalidIOModeError("Multiple inversions on path")`

I/O moded terms allow at most ONE inversion (from ↓ to ↑) on any path.

### Example: INVALID — Inversion from Produce back to Consume

Attempting to construct an I/O moded term with ↑→↓ inversion:

```
% Hypothetical type structure
BadStructure ::= outer(Inner?).  % Inner? is consumed
Inner ::= inner(_).              % But _ is produced

% This would require: ↑outer(↓inner(↑X))  -- inversion from ↑ to ↓ then back to ↑
```

**Error:** `InvalidIOModeError("Inversion from produce back to consume")`

Once mode has inverted to ↑ (produce), it cannot revert to ↓ (consume).

## Error Conditions

| Condition | Exception |
|-----------|-----------|
| I/O moding requires >1 inversion on a path | `InvalidIOModeError` |
| I/O moding requires ↑→↓ inversion | `InvalidIOModeError` |

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.1 | 2025-01-07 | Initial draft |
| 0.2 | 2025-01-07 | Add negative examples (invalid I/O terms), complete function specs with Errors sections |
