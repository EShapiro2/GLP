# MT/PMT Implementation Plan

**Status:** Draft for approval
**Date:** 2025-12-14
**Branch:** feature/mt-types

---

## Overview

Implement Moded Types (MT) and Polymorphic Moded Types (PMT) for GLP, providing:
- Static SRSW verification
- Type checking (terms match declared types)
- Documentation of predicate interfaces

---

## Terminology

**MT (Moded Types):** User-defined types with embedded modes, no type parameters.
```glp
BinaryDigit := zero | one.
List := [] | [_ | List].
DiffList := dl(List?, List).
```

**PMT (Polymorphic Moded Types):** MT plus type parameters.
```glp
List(A) := [] | [A | List(A)].
Goals(X) := true | X | (X, Goals(X)).
DiffList(A) := dl(List(A)?, List(A)).
```

---

## Phases

### Phase 1: MT Parser ✓ COMPLETE
- Parse type definitions with union syntax
- Parse embedded modes in types
- Parse mode declarations
- Store in TypeTable and ModeTable

### Phase 2: MT SRSW Checker ✓ COMPLETE
- Classify variable occurrences (reader/writer)
- Verify SRSW constraints
- Report errors with locations

### Phase 3: MT Type Checker ← NEXT
- Verify constants match declared types
- Verify struct constructors match types
- Verify list elements match types
- Report type errors with suggestions

### Phase 4: MT Declarations for All Book Programs
- One file at a time, book order
- Protocol: agree → test → iterate → next
- Accumulate guidelines in docs/mt-guidelines.md
- All files must pass SRSW + type checking

### Phase 5: PMT Parser
- Parse type parameters: `List(A)`, `Goals(X)`
- Parse parameterized type instantiation: `List(BinaryDigit)`
- Extend TypeTable for parameterized types

### Phase 6: PMT Type Checker
- Verify type parameter instantiation consistency
- Check parameterized types against terms

### Phase 7: PMT Declarations for All Book Programs
- Upgrade MT declarations to PMT where beneficial
- Same protocol as Phase 4

---

## Current Status

| Phase | Status |
|-------|--------|
| 1. MT Parser | ✓ Complete |
| 2. MT SRSW Checker | ✓ Complete |
| 3. MT Type Checker | Not started |
| 4. MT Book Programs | gates.glp in progress |
| 5. PMT Parser | ✓ Complete (done early) |
| 6. PMT Type Checker | Not started |
| 7. PMT Book Programs | Not started |

**Note:** PMT parser was implemented early (Phase 5). Proceeding with Phase 3 (MT Type Checker).

---

## File Order for Phase 4 and 7

Book order from main_AofGLP.tex:

**Part I: Foundations**
1. constants/gates/gates.glp ← current

**Part II: Concurrent Programming**
2. streams/producers_consumers/*.glp
3. streams/buffered_communication/*.glp
4. streams/objects_monitors/*.glp
5. recursive/arithmetic_trees/*.glp
6. recursive/list_processing/*.glp
7. recursive/structure_processing/*.glp
8. meta/plain/*.glp
9. meta/enhanced/*.glp
10. meta/debugging/*.glp

**Part III: Multiagent**
11. multiagent/social_graph/*.glp
12. multiagent/social_networks/*.glp

**Library**
13. lib/*.glp

---

## Protocol for Each File

1. Show file contents
2. Identify predicates and determine modes
3. Write type definitions (MT or PMT depending on phase)
4. Write mode declarations
5. Claude Code tests validation
6. Iterate until pass
7. Record guidelines developed
8. Commit and proceed to next file

---

## Deliverables

- All book programs with validated type/mode declarations
- docs/mt-guidelines.md — MT guidelines
- docs/pmt-guidelines.md — PMT guidelines (Phase 7)
- Type checker implementation
- Comprehensive test suites

---

## Rules

1. Complete each phase before starting next
2. Complete each file before moving to next
3. No shortcuts or compromises
4. Document all guidelines as developed
5. All tests must pass before proceeding
