# Polymorphic Moded Types (PMT) Specification

**Status:** Draft
**Version:** 0.1
**Date:** 2025-12-11

## Overview

PMT provides static SRSW verification for GLP through mode declarations. A mode declaration specifies the data flow direction of each predicate argument, enabling:
- Compile-time SRSW checking
- Automatic reader/writer annotation derivation
- Compiler optimizations

## Declaration Syntax

```
TypeName(Params) := predicate(ArgType1, ArgType2, ...).
```

Examples:
```
Merge(A) := merge(List(A)?, List(A)?, List(A)).
Append(A) := append(List(A)?, List(A)?, List(A)).
Not := not(Num?, Num).
HalfAdder := half_adder(Num?, Num?, Num, Num).
```

Components:
- **TypeName** — Capitalized identifier, distinguishes from clauses
- **Params** — Optional type parameters (A, B, ...)
- **predicate** — Lowercase predicate name
- **ArgType?** — Reader argument (receives data)
- **ArgType** — Writer argument (produces data)

## Mode Extraction

From declaration, extract:
1. Predicate name
2. Arity
3. Mode list: `reader` or `writer` per argument

Example:
```
Merge(A) := merge(List(A)?, List(A)?, List(A)).
```
Yields: `merge/3 → [reader, reader, writer]`

## Reader/Writer Occurrence Classification

Given a clause and its predicate's mode:

**Definition:** A variable occurrence is classified as:
- **Writer occurrence** if in:
  - Head reader argument, OR
  - Body writer argument
- **Reader occurrence** if in:
  - Head writer argument, OR
  - Body reader argument

Mnemonic: head inverts, body preserves.

## SRSW Checking Algorithm

```
1. For each clause of predicate P:
   a. Look up mode declaration for P
   b. For each variable V in clause:
      - Count writer occurrences (w)
      - Count reader occurrences (r)
   c. Verify:
      - w = 1 AND r = 1, OR
      - w = 1 AND r > 1 AND ground(V) in guards
   d. Report errors with source locations
```

## Error Messages

| Condition | Message |
|-----------|---------|
| w = 0 | "Variable X has no writer occurrence" |
| w > 1 | "Variable X has N writer occurrences (expected 1)" |
| r = 0 | "Variable X has no reader occurrence" |
| r > 1, no guard | "Variable X has N reader occurrences; add ground(X) guard" |
| No declaration | "No mode declaration for predicate p/n" |

## Annotation Derivation

From valid LP + PMT, derive annotated GLP:

```
For each variable occurrence:
  if writer occurrence → emit X
  if reader occurrence → emit X?
```

## Extended Features

### Ground Guards

`ground(X)` in guard allows multiple reader occurrences of X.

### Embedded Modes

Types may contain mode inversions:
```
DiffList(A) := dl(List(A)?, List(A)).
Cmd := inc | dec | get(Num).
```

In `get(Num)`, the Num position is a writer embedded in a reader structure.

## Module Integration

PMT declarations belong in module headers:
```
-module(streams).
-export([merge/3, split/3]).

Merge(A) := merge(List(A)?, List(A)?, List(A)).
Split(A) := split(List(A)?, List(A), List(A)).

merge([X|Xs], Ys, [X|Zs]) :- merge(Ys, Xs, Zs).
...
```

## Implementation Phases

1. **Parser extension** — Recognize PMT declarations
2. **Mode table** — Store pred/arity → modes
3. **Checker** — Classify occurrences, verify SRSW
4. **Reporter** — Error messages with locations
5. **Deriver** — Generate annotated GLP (optional)
6. **Optimizer** — Use modes for code generation

## References

- GLP Book, Chapter: Polymorphic Moded Types
- Mercury mode system
- Yardeni-Shapiro type system for concurrent Prolog
