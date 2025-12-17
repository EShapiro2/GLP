# MT/PMT Complete Reference

**Status:** Active Development
**Version:** 0.2
**Date:** 2025-12-17
**Branch:** `claude/mt-types-01MoatMPg26ZQQqxQAF2eDq9`

---

## Document Structure

This document consolidates all MT (Moded Types) and PMT (Polymorphic Moded Types) documentation:

1. **Overview** — What MT/PMT provides
2. **Terminology** — Key concepts and definitions
3. **Syntax Reference** — Declaration syntax with examples
4. **Occurrence Classification** — How variables are classified as reader/writer
5. **SRSW Checking** — Algorithm and rules
6. **Implementation** — File structure and APIs
7. **GLP Files with MT Declarations** — Book programs with declarations
8. **Implementation Plan** — Phases and status
9. **Open Issues** — Current problems being worked on

---

## 1. Overview

MT/PMT provides static SRSW verification for GLP through mode and type declarations, enabling:

- **Compile-time SRSW checking** — Verify single-reader/single-writer constraint
- **Type checking** — Verify terms match declared types
- **Annotation derivation** — Automatically derive `X?` reader annotations
- **Documentation** — Predicate interfaces documented in code
- **Compiler optimizations** — Mode information enables optimizations

---

## 2. Terminology

### MT (Moded Types)
User-defined types with embedded modes, no type parameters.

```glp
BinaryDigit := zero | one.
List := [] | [_ | List].
DiffList := dl(List?, List).
```

### PMT (Polymorphic Moded Types)
MT plus type parameters.

```glp
List(A) := [] | [A | List(A)].
Goals(X) := true | X | (X, Goals(X)).
DiffList(A) := dl(List(A)?, List(A)).
```

### Mode Declaration
Specifies data flow direction for predicate arguments.

```glp
Merge(A) := merge(List(A)?, List(A)?, List(A)).
%% merge/3 has modes: [reader, reader, writer]
```

### Union of Modes
Multimodal predicates with alternative mode patterns.

```glp
Observe := observe(Any?, Any, Any?) | observe(Any, Any?, Any?) | observe(Any?, Any, Any).
%% observe/3 can be called in three different modes
```

### Reader/Writer
- **Reader (`X?`)** — Reads from variable X
- **Writer (`X`)** — Writes to variable X (source of truth)

### SRSW (Single-Reader/Single-Writer)
Each variable must have exactly 1 writer occurrence and 1+ reader occurrences.

---

## 3. Syntax Reference

### Type Definitions

```glp
%% Atom union
BinaryDigit := zero | one.

%% Recursive list
List := [] | [_ | List].

%% Parameterized list
List(A) := [] | [A | List(A)].

%% Struct with embedded mode
DiffList := dl(List?, List).

%% Compound type
Gates := And | Or | Not.
Goals(X) := true | X | (X, Goals(X)).
```

### Mode Declarations

```glp
%% Simple mode declaration
Merge(A) := merge(List(A)?, List(A)?, List(A)).
%% Yields: merge/3 → [reader, reader, writer]

%% Mode with embedded structure
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).

%% Multimodal (union of modes)
Observe := observe(Any?, Any, Any?) | observe(Any, Any?, Any?).
```

### Components

| Component | Description |
|-----------|-------------|
| `TypeName` | Capitalized identifier (distinguishes from clauses) |
| `(Params)` | Optional type parameters (A, B, ...) |
| `predicate` | Lowercase predicate name |
| `ArgType?` | Reader argument (receives data from caller) |
| `ArgType` | Writer argument (produces data to caller) |
| `\|` | Union separator (for types or modes) |

### Disambiguation

| Form | Classification |
|------|----------------|
| `Name := atom.` | Type constructor (atom) |
| `Name := atom \| atom.` | Union type |
| `Name := []\| [_ \| Name].` | Recursive list type |
| `Name := struct(Type?, Type).` | Type with embedded mode |
| `Name := pred(Type?, Type).` | Mode declaration |
| `Name := halt.` | Type constructor |
| `Name := halt().` | Mode declaration (nullary predicate) |

---

## 4. Occurrence Classification

### Basic Rules

| Location | Argument Mode | → Occurrence Type |
|----------|---------------|-------------------|
| Head | reader (`T?`) | writer |
| Head | writer (`T`) | reader |
| Body | reader (`T?`) | reader |
| Body | writer (`T`) | writer |
| Guard | any | reader |

**Mnemonic:** Head inverts, body preserves.

### Double Inversion Rule

When a reader-form variable (`X?`) appears, its occurrence type is inverted:

| Arg Mode | Var Form | Occurrence |
|----------|----------|------------|
| reader | writer (`X`) | writer |
| reader | reader (`X?`) | reader |
| writer | writer (`X`) | reader |
| writer | reader (`X?`) | writer |

**Note:** Double inversion does NOT apply to guards — guards always produce reader occurrences.

### Example

```glp
%% Mode: merge(List?, List?, List)  →  [reader, reader, writer]

merge([X|Xs], Ys, [X|Zs]) :- merge(Ys, Xs, Zs).

%% Head occurrences:
%%   X in arg1 (reader mode): writer occurrence
%%   Xs in arg1 (reader mode): writer occurrence
%%   Ys in arg2 (reader mode): writer occurrence
%%   X in arg3 (writer mode): reader occurrence
%%   Zs in arg3 (writer mode): reader occurrence

%% Body occurrences:
%%   Ys in arg1 (reader mode): reader occurrence
%%   Xs in arg2 (reader mode): reader occurrence
%%   Zs in arg3 (writer mode): writer occurrence

%% Final counts:
%%   X: 1 writer, 1 reader ✓
%%   Xs: 1 writer, 1 reader ✓
%%   Ys: 1 writer, 1 reader ✓
%%   Zs: 1 reader, 1 writer ✓
```

---

## 5. SRSW Checking

### Algorithm

```
1. For each clause of predicate P:
   a. Look up mode declaration(s) for P
   b. For each variable V in clause:
      - Count writer occurrences (w)
      - Count reader occurrences (r)
   c. Verify:
      - w = 1 AND r = 1, OR
      - w = 1 AND r > 1 AND ground(V) in guards
   d. For multimodal predicates:
      - Clause is valid if ANY mode alternative passes
   e. Report errors with source locations
```

### Ground-Implying Guards

These guards allow multiple reader occurrences:

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
| r > 1, no guard | "Variable X has N reader occurrences; add ground(X) guard" |
| No declaration | "No mode declaration for predicate p/n" |
| No mode matches | "Clause does not match any declared mode. Available modes: ..." |

---

## 6. Implementation

### File Structure

```
lib/compiler/pmt/
├── mode_table.dart      # Stores pred/arity → modes (supports unions)
├── type_table.dart      # Stores type definitions
├── occurrence.dart      # Classifies variable occurrences
├── checker.dart         # SRSW verification
├── type_checker.dart    # Type verification
├── errors.dart          # PMT-specific errors
├── validator.dart       # High-level validation API

test/pmt/
├── mode_table_test.dart
├── occurrence_test.dart
├── checker_test.dart
├── pmt_parser_test.dart
├── type_checker_test.dart
├── type_definition_test.dart
├── validator_test.dart

bin/
├── validate_pmt.dart    # CLI validation tool
```

### Key APIs

```dart
// Mode Table
class ModeTable {
  void addDeclaration(ModeDeclaration decl);
  List<Mode>? getModes(String predicate, int arity);
  List<List<Mode>>? getAllModes(String predicate, int arity);  // For unions
  bool hasDeclaration(String predicate, int arity);
}

// Occurrence Classifier
class OccurrenceClassifier {
  List<Occurrence> classifyClause(Clause clause, List<Mode> headModes);
}

// PMT Checker
class PmtChecker {
  List<PmtError> checkClause(Clause clause, List<Mode> headModes);
  List<PmtError> checkClauseAgainstModes(Clause clause, List<List<Mode>> allModes);
  List<PmtError> checkProcedure(Procedure proc);
}
```

### CLI Tool

```bash
# Validate a GLP file
dart run bin/validate_pmt.dart path/to/file.glp

# Output: PASS or FAIL with error details
```

---

## 7. GLP Files with MT Declarations

### constants/gates/

| File | Status | Notes |
|------|--------|-------|
| gates.glp | ✓ | Full PMT with BinaryDigit, List(A), Gates, Goals(X) |
| gates_pmt.glp | ✓ | PMT version |
| gates_pmt_valid.glp | ✓ | Validated version |

### streams/producers_consumers/

| File | Status | Notes |
|------|--------|-------|
| observer.glp | ✓ | Multimodal (3 modes for bidirectional observe) |
| channels.glp | BUG | Uses `=` in guards — invalid GLP |
| mwm.glp | ✓ | Multiway merge |
| fair_merge.glp | ✓ | |
| producer_consumer.glp | ✓ | |
| cooperative_producers.glp | ✓ | |
| dynamic_merger.glp | ✓ | |
| merge_tree.glp | ✓ | |
| distribute.glp | ✓ | |
| distribute_binary.glp | ✓ | |
| distribute_indexed.glp | ✓ | |
| distribute_ground.glp | ✓ | |

### streams/buffered_communication/

| File | Status | Notes |
|------|--------|-------|
| switch2x2.glp | ✓ | PE output passes PMT |
| bounded_buffer.glp | ✓ | |

### streams/objects_monitors/

| File | Status | Notes |
|------|--------|-------|
| counter.glp | WIP | 1 PMT error — investigating double inversion |

---

## 8. Implementation Plan

### Phases

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | MT Parser | ✓ Complete |
| 2 | MT SRSW Checker | ✓ Complete |
| 3 | MT Type Checker | ✓ Complete |
| 4 | MT Book Programs | In Progress |
| 5 | PMT Parser | ✓ Complete |
| 6 | PMT Type Checker | Not Started |
| 7 | Annotation Deriver | Not Started |
| 8 | PMT Book Programs | Not Started |

### Test Counts

- Type definition tests: 18
- Mode table tests: 10
- Occurrence tests: 11
- Checker tests: 14+
- Validator tests: 13
- **Total PMT tests: 75+**

---

## 9. Open Issues

### Double Inversion in Body (ACTIVE)

**Question:** Should the double inversion rule apply to BODY positions, or only HEAD?

Current behavior applies double inversion to both HEAD and BODY (but not guards).

**Test case under investigation:**
```glp
observe(X?, Y, Z?) :- observe(Y?, X, Z).
```

With mode `observe(reader, writer, reader)`:
- Z? in head (reader mode + reader form) → reader occurrence
- Z in body (reader mode + writer form) → reader occurrence
- **Result:** Z has 0 writers, 2 readers — SRSW violation?

The test comment claims this should be valid. Need to clarify:
1. Is the double inversion rule correct as implemented?
2. Should body behave differently?
3. Is the test clause actually invalid GLP?

### counter.glp PMT Error

```glp
counter([show(State?)|S], State) :- number(State?) | counter(S?, State?).
```

With mode `counter(List?, Any?)`:
- State has 2 writer occurrences (reported error)
- Investigating if this is a double inversion issue

---

## Example: Complete GLP File with MT Declarations

```glp
%% gates.glp - Logic gate simulation in GLP

%% Type definitions
BinaryDigit := zero | one.
List(A) := [] | [A | List(A)].
Gates := And | Or | Not.
Goals(X) := true | X | (X, Goals(X)).

%% Mode declarations
And := and(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
Or := or(List(BinaryDigit)?, List(BinaryDigit)?, List(BinaryDigit)).
Not := not(List(BinaryDigit)?, List(BinaryDigit)).
Reduce := reduce(Gates?, Goals(Gates)).

%% Clauses
and([], [], []).
and([one|Xs], [one|Ys], [one|Zs?]) :- and(Xs?, Ys?, Zs).
and([one|Xs], [zero|Ys], [zero|Zs?]) :- and(Xs?, Ys?, Zs).
and([zero|Xs], [one|Ys], [zero|Zs?]) :- and(Xs?, Ys?, Zs).
and([zero|Xs], [zero|Ys], [zero|Zs?]) :- and(Xs?, Ys?, Zs).

or([], [], []).
or([one|Xs], [one|Ys], [one|Zs?]) :- or(Xs?, Ys?, Zs).
%% ... more clauses

not([], []).
not([one|Xs], [zero|Zs?]) :- not(Xs?, Zs).
not([zero|Xs], [one|Zs?]) :- not(Xs?, Zs).
```

---

## References

- GLP Book, Chapter: Polymorphic Moded Types
- Mercury mode system
- Yardeni-Shapiro type system for concurrent Prolog
- FCP AM implementation (`/tmp/FCP/Savannah`)
