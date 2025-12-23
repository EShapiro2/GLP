# Moded Type System Implementation Plan

**Version:** 3.0
**Date:** 2025-12-22
**Status:** Architectural Fix Required

---

## 1. Overview

This document describes the implementation plan for the complete moded type system for GLP, extending the Yardeni-Shapiro type checking algorithm with mode tracking.

### 1.1 Goals

1. **Full YS Fixpoint Checking**: Verify T_M^{α,m}(S) = S
2. **Mode Coverage**: Verify clauses cover required mode alternatives under `::=` semantics
3. **Guard Integration**: Extract type constraints from guards; handle ground guards
4. **Predefined Types**: Every, Any, List, Stream, DiffList, Channel with operations

### 1.2 Critical Discovery (2025-12-22)

**The implementation has a fundamental architectural gap with the spec:**

| Spec v1.5 (Section 5) | Current Implementation |
|----------------------|----------------------|
| `primitiveStateModes: Map<DFAState, Set<Mode>>` | `anyValueStates: Set<DFAState>` |
| `_` → `{Mode.output}` | `_` → anyValueStates (mode lost) |
| `_?` → `{Mode.input}` | `_?` → anyValueStates (mode lost) |
| `Every` → `{Mode.output, Mode.input}` | `Every` → anyValueStates (mode lost) |
| Mode checked during type traversal | ModeChecker walks AST separately |

**Consequence:** Mode checking cannot work correctly because mode information is lost at compile time.

### 1.3 Current State (from handover)

| Component | Status | Notes |
|-----------|--------|-------|
| Type Parser | ✓ Complete | Parses `::=`, `::<`, `?` suffix |
| Type Compiler | ⚠ Partial | Uses `anyValueStates`, not `primitiveStateModes` |
| Type DFA | ⚠ Partial | Has intersection workarounds, missing mode tracking |
| Ground Path Checking | ✓ Complete | Checks constructors match type |
| Variable Type Inference | ✓ Complete | But doesn't track mode |
| Mode Checking (per-clause) | ⚠ Broken | Separate system, disconnected from DFA |
| Mode Coverage | ⚠ Partial | Can't work correctly without mode in DFA |
| Guard Type Constraints | ✓ Partial | Extraction works, intersection has workarounds |
| Clause Contribution | ✗ Missing | Does not compute T_{C}^{α,m}(S) |
| Fixpoint Check | ✗ Missing | Comment admits "simplified check" |
| Predefined Types | ✓ Complete | Every, Any, List, Stream, etc. |

**Test Results:**
- Guard types: 5/11 passing (6 failing due to mode issues)
- Total: 137/199 passing
- 62 failures from test redefinition errors (not implementation bugs)

---

## 2. Implementation Phases

### Phase 0: Architectural Fix - Primitive State Modes (BLOCKING)
**Effort:** 2-3 days
**Dependencies:** None
**Blocking:** All other phases

This phase implements the spec Section 5 design, replacing `anyValueStates` with `primitiveStateModes`.

#### 2.0.1 Changes to TypeDFA

**File:** `lib/analysis/type_checker/type_dfa.dart`

```dart
// REMOVE:
final Set<DFAState> anyValueStates;

// ADD:
/// Mode information at primitive type states.
/// 
/// A state appears in this map iff it corresponds to a primitive type
/// position (_ or _?) in a type definition:
/// - {Mode.output} for _ (program produces value)
/// - {Mode.input} for _? (program consumes value)
/// - {Mode.output, Mode.input} for Every ::= _ ; _?
///
/// States not in this map are structural (non-primitive) positions.
final Map<DFAState, Set<Mode>> primitiveStateModes;

/// Check if state is a primitive type position
bool isPrimitiveState(DFAState state) => 
    primitiveStateModes.containsKey(state);

/// Get accepted modes at a primitive state (empty for non-primitive)
Set<Mode> getModesAt(DFAState state) => 
    primitiveStateModes[state] ?? {};
```

**Update all DFA constructors** to use `primitiveStateModes` instead of `anyValueStates`.

**Update `intersect()` method:**
- Remove workarounds for `anyValueStates`
- Implement proper mode-aware intersection
- When intersecting primitive states, intersect their mode sets

**Update `NumberTypeDFA` and `StringTypeDFA`:**
- These are ground types (no mode alternatives)
- `primitiveStateModes` should be empty (they accept all values structurally)

#### 2.0.2 Changes to TypeCompiler

**File:** `lib/analysis/type_checker/type_compiler.dart`

```dart
void _addTransitionsForAlt(DFAState fromState, TypeExpr alt, ...) {
  if (alt is PrimitiveModeAlt) {
    // Primitive type: mark state with its mode
    final mode = alt.isInput ? Mode.input : Mode.output;
    primitiveStateModes[fromState] = 
        (primitiveStateModes[fromState] ?? <Mode>{})..add(mode);
    finalStates.add(fromState);  // Primitive positions are accepting
    return;
  }
  // ... handle constructors, type references
}
```

**Handle `TypeRef` for subtype declarations:**
- When compiling `Any ::< Every`, copy mode set from `Every`

#### 2.0.3 Changes to TypeChecker

**File:** `lib/analysis/type_checker/type_checker.dart`

Integrate mode checking into `_inferVariableTypes()`:

```dart
TypeDFA? _inferVariableTypes(ast.Clause clause, ...) {
  // ... existing code ...
  
  // When reaching a variable at a primitive position:
  for (final varInfo in variableInfos) {
    final dfaState = stateAfterPath(varInfo.path);
    if (dfaState != null && dfa.isPrimitiveState(dfaState)) {
      // Check mode
      final acceptedModes = dfa.getModesAt(dfaState);
      final varMode = varInfo.isReader ? Mode.input : Mode.output;
      
      if (!acceptedModes.contains(varMode)) {
        errors.add(ModeError(
          'Variable ${varInfo.name} has ${varMode} mode, '
          'but position accepts only $acceptedModes',
          varInfo.line, varInfo.column,
        ));
      }
    }
  }
}
```

#### 2.0.4 Simplify or Remove ModeChecker

**File:** `lib/analysis/type_checker/mode_checker.dart`

After mode is integrated into TypeChecker:
- Mode coverage checking may still be needed as a separate pass
- But per-variable mode checking should be in TypeChecker
- Evaluate what remains needed in ModeChecker

#### 2.0.5 Tests for Phase 0

**Create:** `test/analysis/type_checker/primitive_state_modes_test.dart`

```dart
// Positive controls:
test('_ state has {Mode.output}', ...);
test('_? state has {Mode.input}', ...);
test('Every state has {Mode.output, Mode.input}', ...);
test('Any inherits modes from Every', ...);
test('List element position has Any modes', ...);

// Negative controls:
test('writer at _? position rejected', ...);
test('reader at _ position rejected', ...);
test('non-primitive position has no mode constraint', ...);

// Integration:
test('mode checked during type inference', ...);
test('guard constraints preserve mode information', ...);
```

---

### Phase 1: DFA Operations
**Effort:** 2-3 days
**Dependencies:** Phase 0
**Blocking:** Phases 4, 5

After Phase 0, the DFA has proper mode tracking. Now implement full DFA operations.

#### 2.1.1 New Methods

```dart
TypeDFA union(TypeDFA other);
TypeDFA complement();
TypeDFA complete();  // Add sink state
bool isSubsetOf(TypeDFA other);
bool isEquivalent(TypeDFA other);
TypeDFA minimize();
```

#### 2.1.2 Mode-Aware Operations

All operations must preserve/combine `primitiveStateModes`:

- **Union:** `primitiveStateModes[s] = this.modes[s] ∪ other.modes[s]`
- **Intersection:** `primitiveStateModes[s] = this.modes[s] ∩ other.modes[s]`
- **Complement:** Structural complement only (modes unchanged)

---

### Phase 2: Predefined Types Prelude
**Effort:** 0.5 days
**Dependencies:** Phase 0
**Status:** ✓ Already implemented (needs verification after Phase 0)

Verify prelude works correctly with new `primitiveStateModes` design:
- Every has `{output, input}` at start state
- Any inherits from Every
- List element positions have Any modes

---

### Phase 3: Guard Type Checking
**Effort:** 1 day
**Dependencies:** Phase 0
**Status:** Partially implemented (5/11 tests passing)

After Phase 0, the 6 failing tests should be re-evaluated:

| Test | Current Issue | Expected After Phase 0 |
|------|---------------|----------------------|
| number(X?) constrains X | Mode error | Should pass (mode in DFA) |
| arithmetic guards | Variable inconsistent types | May need additional fix |
| ground(X?) multiple readers | Guard type inconsistent | Should pass |
| ground covers all modes | Missing implementation | Implement in mode coverage |
| number implies ground | Guard type inconsistent | Should pass |
| ground on nested | Missing implementation | Implement in mode coverage |

**Remaining work after Phase 0:**
1. Implement ground variable mode coverage in `_checkModeCoverage()`
2. Handle variable reuse across positions

---

### Phase 4: Clause Contribution
**Effort:** 2 days
**Dependencies:** Phase 0, Phase 1

Compute T_{C}^{α,m}(S) for each clause. With proper mode tracking, this becomes:

```dart
TypeDFA computeClauseContribution(Clause clause, varTypes) {
  // Build DFA from head pattern using inferred variable types
  // Include mode information at primitive positions
}
```

---

### Phase 5: Fixpoint Check
**Effort:** 1-2 days
**Dependencies:** Phase 1, Phase 4

```dart
// Tuple-distributive closure with modes
final inferred = union(contributions).tupleDistributiveClosure();

// Check fixpoint
if (!inferred.isEquivalent(declared)) {
  // Report incomplete or over-broad definition
}
```

---

### Phase 6: Fix Test Suite
**Effort:** 0.5 days
**Dependencies:** None (can be done anytime)

Fix 62 test redefinition errors:
- Option A: Remove redundant type definitions from tests
- Option B: Add `--no-prelude` flag for tests
- **Recommended:** Option A (simpler, cleaner tests)

---

### Phase 7: Comprehensive Tests
**Effort:** 2 days
**Dependencies:** All previous phases

Test all components with both positive and negative controls.

---

## 3. Dependency Graph

```
Spec Section 5 (v1.5) ◄─── APPROVED
         │
         ▼
Phase 0 (Primitive State Modes) ◄── BLOCKING, ARCHITECTURAL
         │
         ├───────────────────────────────┐
         ▼                               ▼
Phase 1 (DFA Ops)              Phase 2 (Prelude verify)
         │                               │
         ▼                               │
Phase 4 (Contribution)                   │
         │                               │
         ▼                               │
Phase 5 (Fixpoint) ──────────────────────┤
                                         │
Phase 3 (Guards) ────────────────────────┤
                                         │
Phase 6 (Test Suite) ────────────────────┤
                                         ▼
                                 Phase 7 (Tests)
```

**Critical Path:** Phase 0 → Phase 1 → Phase 4 → Phase 5 → Phase 7

---

## 4. Effort Summary

| Phase | Description | Days | Status |
|-------|-------------|------|--------|
| **0** | **Primitive State Modes** | **2-3** | **NEW - BLOCKING** |
| 1 | DFA Operations | 2-3 | Not started |
| 2 | Predefined Types Prelude | 0.5 | Verify after Phase 0 |
| 3 | Guard Type Checking | 1 | Re-evaluate after Phase 0 |
| 4 | Clause Contribution | 2 | Not started |
| 5 | Fixpoint Check | 1-2 | Not started |
| 6 | Fix Test Suite | 0.5 | Can do anytime |
| 7 | Comprehensive Tests | 2 | Not started |
| **Total** | | **11-14 days** | |

---

## 5. Success Criteria

### Phase 0 Success Criteria

1. `anyValueStates` removed from codebase
2. `primitiveStateModes: Map<DFAState, Set<Mode>>` implemented
3. All existing passing tests still pass
4. New primitive mode tests pass (both positive and negative)
5. At least 4 of 6 failing guard tests now pass
6. Mode checking integrated into type traversal

### Overall Success Criteria

1. All predefined types compile correctly with mode info
2. Guard constraints work with mode tracking
3. Fixpoint check catches incomplete/over-broad definitions
4. 116+ book programs pass (82% target maintained)
5. Spec ↔ Implementation gap closed

---

## 6. Questions Resolved

From handover:

1. **Test redefinition errors**: Update tests to avoid redefining prelude types (Phase 6)

2. **Mode complementation**: The test may be incorrect. After Phase 0, mode checking will be integrated and we can evaluate properly.

3. **Priority**: Phase 0 first (architectural fix), then guard tests will be re-evaluated.

---

## 7. Claude Code Instructions for Phase 0

**Verbatim instruction to copy:**

```
Phase 0: Replace anyValueStates with primitiveStateModes

SPEC REFERENCE: docs/moded-type-system-spec.md Section 5

TASK: Implement the spec Section 5 design in the codebase.

FILES TO MODIFY:
1. lib/analysis/type_checker/type_dfa.dart
2. lib/analysis/type_checker/type_compiler.dart  
3. lib/analysis/type_checker/type_checker.dart

STEP 1 - type_dfa.dart:
- Remove: `final Set<DFAState> anyValueStates;`
- Add: `final Map<DFAState, Set<Mode>> primitiveStateModes;`
- Add: `bool isPrimitiveState(DFAState state) => primitiveStateModes.containsKey(state);`
- Add: `Set<Mode> getModesAt(DFAState state) => primitiveStateModes[state] ?? {};`
- Update all constructors to use primitiveStateModes
- Update intersect() to handle mode sets:
  - Intersection of mode sets at same position
  - Remove anyValueStates workarounds
- Update NumberTypeDFA and StringTypeDFA (empty primitiveStateModes)

STEP 2 - type_compiler.dart:
- In _addTransitionsForAlt(), when alt is PrimitiveModeAlt:
  ```dart
  final mode = alt.isInput ? Mode.input : Mode.output;
  primitiveStateModes[fromState] = 
      (primitiveStateModes[fromState] ?? <Mode>{})..add(mode);
  finalStates.add(fromState);
  ```
- Handle TypeRef for ::< declarations: copy mode set from referenced type

STEP 3 - type_checker.dart:
- In _inferVariableTypes(), at variable positions:
  - Check if state is primitive: dfa.isPrimitiveState(state)
  - If primitive, verify variable mode ∈ dfa.getModesAt(state)
  - Report mode error if mismatch

STEP 4 - Create test:
- test/analysis/type_checker/primitive_state_modes_test.dart
- Positive: _ has {output}, _? has {input}, Every has {both}
- Negative: writer at _? rejected, reader at _ rejected

RUN TESTS:
- dart test test/analysis/type_checker/primitive_state_modes_test.dart
- dart test test/analysis/type_checker/guard_types_test.dart
- dart test test/analysis/type_checker/

EXPECTED: All existing passing tests still pass. At least 4 of 6 failing guard tests should now pass.

DO NOT MERGE until all tests pass and I review.
```

---

## 8. References

- Spec: `docs/moded-type-system-spec.md` (v1.5)
- Paper: "Moded Types for Grassroots Logic Programs"
- Handover: `docs/handover-2025-12-22-moded-types-phase3.md`
- Yardeni & Shapiro, "A Type System for Logic Programs", JLP 1991

---

## Appendix A: Changelog

### v3.0 (2025-12-22)
- Added Phase 0: Architectural fix for primitiveStateModes
- Identified spec ↔ implementation gap
- Incorporated handover report status
- Added Claude Code instructions for Phase 0
- Updated all phases to depend on Phase 0
