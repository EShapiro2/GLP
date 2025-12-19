# MT/PMT Implementation Plan

**Status:** Active
**Date:** 2025-12-14
**Branch:** feature/mt-types

---

## Overview

Implement Moded Types (MT) and Polymorphic Moded Types (PMT) for GLP, providing:
- Static SRSW verification
- Type checking (terms match declared types)
- Annotation derivation from types
- Documentation of predicate interfaces

---

## Terminology

**MT (Moded Types):** User-defined types with embedded modes, no type parameters.
```glp
BinaryDigit := zero | one.
List := [] | [_ | List].
BBList := [] | [_? | BBList].
DiffList := dl(List?, List).
```

**PMT (Polymorphic Moded Types):** MT plus type parameters.
```glp
List(A) := [] | [A | List(A)].
Goals(X) := true | X | (X, Goals(X)).
DiffList(A) := dl(List(A)?, List(A)).
```

---

## File Structure

```
lib/
  compiler/
    pmt/
      mode_table.dart      # Stores pred/arity → modes
      type_table.dart      # Stores type definitions
      occurrence.dart      # Classifies variable occurrences
      checker.dart         # SRSW verification
      type_checker.dart    # Type verification
      errors.dart          # PMT-specific errors
      deriver.dart         # Annotation derivation
      validator.dart       # High-level validation API

test/
  pmt/
    mode_table_test.dart
    type_table_test.dart
    occurrence_test.dart
    checker_test.dart
    type_checker_test.dart
    type_definition_test.dart
    validator_test.dart
    programs/
      valid/
      invalid/

docs/
  mt-pmt-plan.md           # This document
  mt-guidelines.md         # Guidelines developed during Phase 4
  pmt-guidelines.md        # Guidelines developed during Phase 8
```

---

## Phase 1: MT Parser ✓ COMPLETE

Parse Moded Type definitions with union syntax and embedded modes.

### Syntax

```glp
BinaryDigit := zero | one.
BinaryDigit := zero.
BinaryDigit := one.
List := [] | [_ | List].
BBList := [] | [_? | BBList].
DiffList := dl(List?, List).
Cmd := inc | dec | get(Num).
```

### AST Nodes

```dart
class TypeDefinition extends AstNode {
  final String typeName;
  final List<String> typeParams;  // empty for MT
  final List<TypeConstructor> constructors;
}

abstract class TypeConstructor {}

class AtomConstructor extends TypeConstructor {
  final String name;
}

class StructConstructor extends TypeConstructor {
  final String functor;
  final List<TypeArg> args;
}

class ListConstructor extends TypeConstructor {
  final TypeArg? head;
  final TypeArg? tail;
}

class TupleConstructor extends TypeConstructor {
  final List<TypeArg> elements;
}

class TypeArg {
  final String typeName;
  final List<String> typeParams;
  final bool isReader;
}
```

### Disambiguation

| Form | Classification |
|------|----------------|
| `Name := atom.` | Type constructor (atom) |
| `Name := atom \| atom.` | Union type |
| `Name := [] \| [_ \| Name].` | Recursive list type |
| `Name := struct(Type?, Type).` | Type with embedded mode |
| `Name := pred(Type?, Type).` | Mode declaration |
| `Name := halt.` | Type constructor |
| `Name := halt().` | Mode declaration (nullary) |

### Files

- `lib/compiler/ast.dart` — TypeDefinition, TypeConstructor hierarchy
- `lib/compiler/parser.dart` — Type definition parsing
- `lib/compiler/pmt/type_table.dart` — TypeTable class

### Tests

- `test/pmt/type_definition_test.dart` — 18 tests

### Status: ✓ COMPLETE

---

## Phase 2: MT Mode Table & SRSW Checker ✓ COMPLETE

### Mode Table

```dart
enum Mode { reader, writer }

class ModeTable {
  final Map<String, List<Mode>> _modes = {};

  void addDeclaration(ModeDeclaration decl);
  List<Mode>? getModes(String predicate, int arity);
  bool hasDeclaration(String predicate, int arity);
  ModeDeclaration? getDeclaration(String predicate, int arity);
  static ModeTable fromDeclarations(List<ModeDeclaration> declarations);
}
```

### Occurrence Classifier

```dart
enum OccurrenceType { writer, reader }

class Occurrence {
  final String variable;
  final OccurrenceType type;
  final int line;
  final int column;
}

class OccurrenceClassifier {
  final ModeTable modeTable;

  List<Occurrence> classifyClause(Clause clause, List<Mode> headModes);
}
```

### Classification Rules

| Location | Argument Mode | → Occurrence Type |
|----------|---------------|-------------------|
| Head | reader (`T?`) | writer |
| Head | writer (`T`) | reader |
| Body | reader (`T?`) | reader |
| Body | writer (`T`) | writer |
| Guard | any | reader |

### SRSW Checker

```dart
class PmtChecker {
  final ModeTable modeTable;
  final OccurrenceClassifier classifier;

  List<PmtError> checkClause(Clause clause, List<Mode> headModes);
  List<PmtError> checkProcedure(Procedure proc);
}
```

### SRSW Rules

For each variable V in clause:
- Count writer occurrences (w)
- Count reader occurrences (r)
- Valid if:
  - `w = 1 AND r = 1`, OR
  - `w = 1 AND r > 1 AND ground(V) in guards`

### Ground-Implying Guards

```dart
final groundImplyingGuards = {
  'ground',
  'number', 'integer', 'float', 'atom', 'string',
  'list', 'tuple', 'compound', 'var', 'nonvar',
  'is_mutual_ref', 'unknown',
  '<', '>', '=<', '>=', '=:=', '=\\=', '=?='
};
```

### Error Messages

| Condition | Message |
|-----------|---------|
| w = 0 | "Variable X has no writer occurrence" |
| w > 1 | "Variable X has N writer occurrences (expected 1)" |
| r = 0 | "Variable X has no reader occurrence" |
| r > 1, no ground | "Variable X has N reader occurrences; add ground(X) guard" |

### Files

- `lib/compiler/pmt/mode_table.dart`
- `lib/compiler/pmt/occurrence.dart`
- `lib/compiler/pmt/checker.dart`
- `lib/compiler/pmt/errors.dart`
- `lib/compiler/pmt/validator.dart`

### Tests

- `test/pmt/mode_table_test.dart` — 10 tests
- `test/pmt/occurrence_test.dart` — 11 tests
- `test/pmt/checker_test.dart` — 14 tests
- `test/pmt/validator_test.dart` — 13 tests

### Status: ✓ COMPLETE

---

## Phase 3: MT Type Checker ← NEXT

Verify that terms in clauses match their declared types.

### What to Check

1. **Constants match declared types**
   - `one` is a valid `BinaryDigit`
   - `true` is a valid `Goals`

2. **Struct constructors match types**
   - `and([], [], [])` matches `Gates` (via `And`)

3. **List elements match types**
   - In `[one|Xs]`, `one` matches list element type

4. **Nested structures match recursively**

### Algorithm

```dart
class TypeChecker {
  final TypeTable typeTable;
  final ModeTable modeTable;

  List<TypeError> checkClause(Clause clause, ModeDeclaration modeDecl);
  List<TypeError> checkTerm(Term term, String expectedType);
  bool isValidConstructor(Term term, String typeName);
}
```

```
checkTerm(term, expectedType):
  if term is Variable:
    return []  // Type inferred from context

  if term is Constant:
    if isValidConstructor(term, expectedType):
      return []
    else:
      return [TypeError("'$term' is not a valid '$expectedType'")]

  if term is List [H|T]:
    if expectedType is List:
      errors += checkTerm(H, elementType)
      errors += checkTerm(T, expectedType)
    return errors

  if term is Struct:
    if isValidConstructor(term.functor, expectedType):
      // Check args against constructor's arg types
      ...
    else:
      return [TypeError("'$term' is not a valid '$expectedType'")]
```

### Error Messages

| Situation | Message |
|-----------|---------|
| Wrong constant | `'foo' at line 5 is not a valid 'BinaryDigit'. Valid constructors: zero, one` |
| Wrong struct | `'bar(X)' at line 7 is not a valid 'Gates'. Expected: and(...), or(...), not(...)` |
| List element | `'abc' at line 9 is not a valid element of 'List'. Expected element type from context.` |

### Files

- `lib/compiler/pmt/type_checker.dart` — new
- `lib/compiler/pmt/type_error.dart` — new
- `test/pmt/type_checker_test.dart` — new

### Test Cases

```dart
test('valid constant', () {
  // BinaryDigit := zero | one.
  // Check: one matches BinaryDigit
});

test('invalid constant', () {
  // BinaryDigit := zero | one.
  // Check: two does NOT match BinaryDigit
});

test('valid struct', () {
  // Gates := And | Or | Not.
  // And := and(List?, List?, List).
  // Check: and([], [], []) matches Gates
});

test('valid list elements', () {
  // List element one matches BinaryDigit in List(BinaryDigit)
});

test('invalid list element', () {
  // List element foo does NOT match BinaryDigit
});
```

### Status: NOT STARTED

---

## Phase 4: MT Declarations for All Book Programs

Add MT type and mode declarations to all book programs.

### Protocol

1. Process files in book order
2. For each file:
   a. Show file contents
   b. Identify predicates and determine modes
   c. Write type definitions
   d. Write mode declarations
   e. Test validation (SRSW + type checking)
   f. Iterate until pass
   g. Record guidelines developed
   h. Commit and proceed

### File Order

**Part I: Foundations**
1. constants/gates/gates.glp ← IN PROGRESS

**Part II: Concurrent Programming**
2. streams/producers_consumers/producer_consumer.glp
3. streams/producers_consumers/fair_merge.glp
4. streams/producers_consumers/mwm.glp
5. streams/producers_consumers/distribute.glp
6. streams/producers_consumers/distribute_ground.glp
7. streams/producers_consumers/distribute_indexed.glp
8. streams/producers_consumers/distribute_binary.glp
9. streams/producers_consumers/observer.glp
10. streams/producers_consumers/channels.glp
11. streams/producers_consumers/parallel_table.glp
12. streams/producers_consumers/merge_tree.glp
13. streams/producers_consumers/dynamic_merger.glp
14. streams/producers_consumers/cooperative_producers.glp
15. streams/buffered_communication/bounded_buffer.glp
16. streams/buffered_communication/switch2x2.glp
17. streams/objects_monitors/counter.glp
18. streams/objects_monitors/monitor.glp
19. streams/objects_monitors/observed_monitor.glp
20. streams/objects_monitors/queue_manager.glp
21. streams/objects_monitors/network_switch.glp
22. streams/objects_monitors/network_switch_3way.glp
23. streams/objects_monitors/many_counters.glp
24. streams/objects_monitors/plus_constraint.glp
25. recursive/arithmetic_trees/*.glp (12 files)
26. recursive/list_processing/*.glp (19 files)
27. recursive/structure_processing/*.glp (9 files)
28. meta/plain/*.glp (3 files)
29. meta/enhanced/*.glp (8 files)
30. meta/debugging/*.glp

**Part III: Multiagent**
31. multiagent/social_graph/*.glp (17 files)
32. multiagent/social_networks/*.glp (16 files)

**Library**
33. lib/*.glp (4 directories)

### Deliverables

- All book programs with validated MT declarations
- `docs/mt-guidelines.md` — Guidelines developed during process

### Status: IN PROGRESS (gates.glp)

---

## Phase 5: PMT Parser ✓ COMPLETE (done early)

Extend parser for parameterized types.

### Syntax

```glp
List(A) := [] | [A | List(A)].
Goals(X) := true | X | (X, Goals(X)).
DiffList(A) := dl(List(A)?, List(A)).
Merge(A) := merge(List(A)?, List(A)?, List(A)).
```

### AST Extensions

TypeDefinition and ModeDeclaration already support `typeParams`.

### Files

- `lib/compiler/parser.dart` — Already handles type parameters
- `lib/compiler/pmt/type_table.dart` — Already stores parameterized types

### Status: ✓ COMPLETE

---

## Phase 6: PMT Type Checker

Extend type checker for parameterized types.

### What to Check

1. **Type parameter instantiation**
   - `Goals(Gates)` instantiates X with Gates
   - Check terms against instantiated type

2. **Consistent instantiation**
   - If `List(A)` appears twice, A must be same type

### Algorithm Extension

```dart
checkTerm(term, expectedType, typeBindings):
  if expectedType is TypeParameter:
    if typeBindings.contains(expectedType):
      return checkTerm(term, typeBindings[expectedType], typeBindings)
    else:
      // Infer type from term
      typeBindings[expectedType] = inferType(term)
      return []

  if expectedType is ParameterizedType:
    // Instantiate and check
    ...
```

### Files

- `lib/compiler/pmt/type_checker.dart` — extend
- `test/pmt/type_checker_test.dart` — extend

### Status: NOT STARTED

---

## Phase 7: Annotation Deriver

Generate reader/writer annotations from types.

### Purpose

Given pure LP + PMT declarations, derive annotated GLP:

Input:
```glp
Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Ys, Xs, Zs).
```

Output:
```glp
merge([], Ys, Ys?).
merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
```

### Algorithm

```dart
class AnnotationDeriver {
  String deriveClause(Clause clause, ModeDeclaration modeDecl);
  String deriveAnnotations(Module module);
}
```

For each variable occurrence:
- If writer occurrence → emit `X`
- If reader occurrence → emit `X?`

### Files

- `lib/compiler/pmt/deriver.dart`
- `test/pmt/deriver_test.dart`

### Status: NOT STARTED

---

## Phase 8: PMT Declarations for All Book Programs

Upgrade MT declarations to PMT where beneficial.

### Protocol

Same as Phase 4.

### Benefits of PMT over MT

- `List(BinaryDigit)` more precise than `List`
- `Goals(Gates)` captures relationship
- Better documentation
- Enables more precise type checking

### Deliverables

- All book programs upgraded to PMT where appropriate
- `docs/pmt-guidelines.md` — Extended guidelines

### Status: NOT STARTED

---

## Current Status Summary

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | MT Parser | ✓ Complete |
| 2 | MT SRSW Checker | ✓ Complete |
| 3 | MT Type Checker | Not started |
| 4 | MT Book Programs | In progress (gates.glp) |
| 5 | PMT Parser | ✓ Complete |
| 6 | PMT Type Checker | Not started |
| 7 | Annotation Deriver | Not started |
| 8 | PMT Book Programs | Not started |

**Test counts:**
- Type definition tests: 18
- Total PMT tests: 75+
- All passing

**Branch:** `claude/mt-types-01MoatMPg26ZQQqxQAF2eDq9`

---

## Rules

1. Complete each phase before starting next
2. Complete each file before moving to next
3. No shortcuts or compromises
4. Document all guidelines as developed
5. All tests must pass before proceeding
6. Commit after each significant change
