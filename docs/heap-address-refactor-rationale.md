# Heap Address Refactor Rationale

**Version**: 1.1  
**Date**: 2026-01-19  
**Status**: PROPOSED  
**Related**: glp-runtime-spec.txt v2.17, heap-address-refactor-plan.md

## 1. Executive Summary

This document explains why the GLP runtime heap is being refactored from a varId-based design to an address-based design. The current implementation introduces an unnecessary abstraction layer that adds complexity without providing any benefit. The refactor aligns the implementation with established abstract machine principles where heap addresses serve directly as variable identities.

## 2. Background

### 2.1 The Two-Cell Variable Design

In GLP, each logical variable consists of two heap cells allocated at consecutive addresses. The writer cell (at address N) contains a pointer to the reader cell (at address N+1). The reader cell initially contains null, later holds suspension lists when goals block on it, and eventually holds the bound value when the writer is assigned.

This two-cell design is well-established in concurrent logic programming implementations. The writer and reader are distinct entities with different roles: writers can be bound, readers can only be read from and suspended on.

### 2.2 How the Current Implementation Works

The current implementation in `heap_fcp.dart` allocates variables as follows:

```dart
int allocateVariable() {
  final varId = nextVarId++;
  final wAddr = HP++;
  final rAddr = HP++;
  cells.add(HeapCell(Pointer(rAddr), CellTag.WrtTag));
  cells.add(HeapCell(null, CellTag.RoTag));
  varTable[varId] = (wAddr, rAddr);
  return varId;
}
```

This creates a single `varId` that maps via `varTable` to a pair of heap addresses. To distinguish between writer and reader, the code uses a `VarRef` structure containing both the `varId` and an `isReader` boolean flag:

```dart
class VarRef extends Term {
  final int varId;
  final bool isReader;
}
```

When code needs to access the actual heap cell, it must look up `varTable[varId]` to get the address pair, then select the appropriate address based on `isReader`.

## 3. The Problem

### 3.1 Discovery

The issue was discovered while investigating the API method `allocateFreshPair()`:

```dart
(int, int) allocateFreshPair() {
  final varId = allocateVariable();
  return (varId, varId);  // Same ID for compatibility
}
```

This method returns the same value twice. The intent was presumably to return distinct identifiers for writer and reader, but since there is only one `varId` per variable, the method cannot fulfill this contract. The distinction between writer and reader is instead handled by the `isReader` flag in `VarRef`, not by having different identifiers.

This revealed a fundamental confusion in the design: the implementation conflates a single variable identifier with what are logically two distinct heap entities.

### 3.2 Unnecessary Indirection

The `varTable` mapping adds an indirection layer that serves no purpose:

1. Every variable reference requires a map lookup to translate varId to addresses
2. The `isReader` flag must be carried everywhere and checked to select the correct address
3. The varTable must be maintained in sync with heap allocations
4. The abstraction obscures the simple reality that variables live at heap addresses

### 3.3 Specification Inconsistency

The runtime specification (prior to v2.17) contained contradictory guidance. The prose mentioned that "Dart implementation can use integer IDs that map to (writerAddr, readerAddr) pairs via lookup table" while code examples showed returning two distinct addresses. This ambiguity allowed the unnecessary abstraction to persist.

## 4. The Correct Design

### 4.1 Addresses as Identity

In the correct design, the heap address is the variable's identity. There is no separate ID namespace and no mapping table. The allocation function returns two distinct addresses:

```dart
(int, int) allocateVariable() {
  final writerAddr = HP++;
  final readerAddr = HP++;
  cells.add(HeapCell(Pointer(readerAddr), CellTag.WrtTag));
  cells.add(HeapCell(null, CellTag.RoTag));
  return (writerAddr, readerAddr);
}
```

A variable reference is simply an address:

```dart
class VarRef extends Term {
  final int addr;
}
```

To determine whether an address refers to a writer or reader, examine the cell's tag:

```dart
bool isWriter(int addr) => cells[addr].tag == CellTag.WrtTag;
bool isReader(int addr) => cells[addr].tag == CellTag.RoTag;
```

### 4.2 Why This Is Better

The address-based design eliminates unnecessary complexity. There is no map lookup on every variable access. There is no `isReader` flag to carry and check. The code directly reflects what is happening at the heap level. Writer and reader have genuinely distinct identities (their addresses) rather than being distinguished by a flag.

For freshly allocated variable pairs, the relationship between writer and reader is encoded in the allocation structure: the writer cell at address N is paired with the reader cell at address N+1. Given either address of a freshly allocated pair, the paired address can be computed by simple arithmetic (writerAddr + 1 = readerAddr, or readerAddr - 1 = writerAddr) rather than a map lookup.

**Important clarification**: This arithmetic relationship is a property of the allocation structure, not a general navigation tool. When a writer is bound to another variable (creating a chain), the "paired reader" concept refers to the original allocation pair, not to whatever variable the chain eventually resolves to. Suspension and dereferencing logic must follow variable chains explicitly rather than relying on address arithmetic. The spec's wake-and-retry mechanism correctly handles this by following chains during suspension.

### 4.3 Established Practice

This design matches how abstract machines for logic programming languages have traditionally worked. The heap is a contiguous array of cells, and variables are referenced by their heap addresses. This is not a novel design choice but rather an alignment with proven practice that the current implementation had inadvertently departed from.

## 5. What Changes

### 5.1 Core Heap

The `varTable` map is removed. The `allocateVariable` function returns `(writerAddr, readerAddr)`. All heap methods that previously took a `varId` parameter now take an address directly.

### 5.2 Variable References

The `VarRef` class is simplified to contain only an `addr` field. The `isReader` flag is removed since the cell's tag provides this information.

### 5.3 All Callers

Code throughout the runtime that creates or inspects `VarRef` values must be updated to work with addresses. The substitution maps (`sigmaHat`) and suspension sets (`si`, `U`) that previously contained varIds now contain addresses.

### 5.4 Multiagent Layer

The multiagent variable table (V_p) stores entries keyed by address rather than by (varId, isReader) pairs. The serialization layer that converts between local and global variable identities continues to work conceptually the same way, but the local identifier is now an address rather than a varId.

## 6. What Stays the Same

### 6.1 GLP Semantics

The language semantics are unchanged. Writers can be bound, readers suspend and eventually receive values, the SRSW constraint applies, and suspension/reactivation works as before.

### 6.2 Bytecode Format

The bytecode instructions are unchanged. The compiler continues to generate the same instructions. Only the runtime's internal representation of variables changes.

### 6.3 Multiagent Protocol

The multiagent communication protocol is unchanged. Messages still carry global variable identifiers in the format `creator:localId`. The only difference is that `localId` is now an address rather than a varId, which is an internal representation detail not visible at the protocol level.

### 6.4 Test Behavior

All existing tests should continue to pass. The refactor changes internal representation, not observable behavior.

## 7. Risks and Mitigations

### 7.1 Scope of Change

This refactor touches many files across the codebase. The mitigation is an incremental migration strategy with testing after each change, plus a temporary compatibility layer that allows partial migration.

### 7.2 Subtle Bugs

Changing fundamental data representation risks introducing subtle bugs where some code path was not updated correctly. The mitigation is comprehensive existing test coverage plus careful code review.

### 7.3 Multiagent Complexity

The multiagent layer has additional complexity around variable identity across agent boundaries. The mitigation is that the conceptual model (creator + local identifier) remains the same; only the representation of the local identifier changes.

## 8. Decision

The refactor should proceed because the current design adds complexity without benefit, the correct design is well-established and simpler, and the change is primarily mechanical (updating representation) rather than semantic (changing behavior). The implementation plan provides a structured approach to making the change safely.

## 9. Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-19 | Claude | Initial rationale |
| 1.1 | 2026-01-19 | Claude | Clarified Section 4.2 re: arithmetic relationship applies to allocation structure, not variable chains |
