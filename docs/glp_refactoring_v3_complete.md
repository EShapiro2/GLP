# GLP Runtime Refactoring Plan v3 - Complete Single-ID Variable Migration
**Version:** 3.0  
**Date:** November 11, 2024  
**Purpose:** Complete transition from two-ID (Writer+1=Reader) to single-ID variable system

---

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [Completed Work](#completed-work)
3. [Current Situation](#current-situation)
4. [Phase 2: Complete Single-ID Migration](#phase-2-complete-single-id-migration)
5. [Phase 2.5: Integration Validation](#phase-25-integration-validation)
6. [Phase 3: Array-Based Registers](#phase-3-array-based-registers)
7. [Phase 4: Comprehensive Integration](#phase-4-comprehensive-integration)
8. [Phase 5: Migration Completion](#phase-5-migration-completion)
9. [Risk Mitigation](#risk-mitigation)

---

## Executive Summary

### Goal
Complete the transition from two-ID variable system (separate Writer/Reader IDs) to single-ID system (one ID per variable, accessed as writer or reader).

### Current State
- HeapV2 created with single-ID design ✅
- HeapV2Adapter deployed but causing problems (duplicate storage)
- 53 occurrences of WriterTerm/ReaderTerm still in runner.dart
- Nested structure bug present (`test_conj(Z)` returns `<unbound>`)

### Strategy
- Incremental migration with testing after each group
- Fix nested structure bug AFTER migration (may resolve itself)
- Use temporary helper functions during transition
- Maintain ability to rollback at each checkpoint

---

## Completed Work

### ✅ Phase 0: BASELINE CAPTURE
**Status:** Complete at commit 86538ca
- 191/206 tests passing (92.7%)
- Writer+1=Reader pattern confirmed
- O(n) lookup performance documented

### ✅ Phase 1: SINGLE ID VARIABLE SYSTEM
**Status:** Complete at commit 86538ca
- HeapV2 implemented (186 lines)
- 98.9% performance improvement
- Compatibility tests passing

### ✅ Phase 1.5: HEAPV2 INTEGRATION VALIDATION
**Status:** Complete

#### Step 1.5.1: Integration Test Harness Created
File: `test/refactoring/heap_v2_integration_test.dart`
```dart
/// Integration test that runs actual GLP programs using HeapV2
/// without modifying the production codebase
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap_v2.dart' as v2;
import 'package:glp_runtime/runtime/heap.dart' as v1;
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  group('HeapV2 Integration Tests', () {
    test('Simple unification: p(a). q(X?) :- p(X).', () {
      // Create modified runner using HeapV2
      final heapV2 = v2.Heap();
      final modifiedRunner = createTestRunner(heapV2);
      
      // Run program and verify results
      final result = modifiedRunner.execute(simpleProgram);
      expect(result.bindings['X'], equals('a'));
    });
    
    test('List merge with suspension', () {
      // Test merge([1,2],[a],Xs) with HeapV2
    });
    
    test('Complex suspension: r(X) :- s(X?). s(b).', () {
      // Test suspension/reactivation with HeapV2
    });
  });
}
```

#### Step 1.5.2: Side-by-Side Comparison Completed
- Heap V1 vs V2 comparison documented
- Performance gains verified

#### Step 1.5.3: Critical Paths Validated
✅ Variable allocation during bytecode execution  
✅ Binding through bytecode instructions  
✅ Suspension/ROQ interaction  
✅ Dereferencing chains  
✅ Structure building with variables

### ✅ Phase 1.5b: HEAPV2 ADAPTER IMPLEMENTATION
**Status:** Complete but problematic

#### Created HeapV2Adapter
File: `lib/runtime/heap_v2_adapter.dart`
```dart
/// Adapter that presents old Heap interface but uses HeapV2 internally
import 'heap.dart' as old;
import 'heap_v2.dart';
import 'terms.dart';

class HeapV2Adapter extends old.Heap {  // PROBLEM: Inherits from old Heap
  final HeapV2 _v2 = HeapV2();
  final Map<int, int> _writerToVar = {};
  final Map<int, int> _readerToVar = {};
  int _nextSyntheticId = 1000;

  @override
  (int, int) allocateFreshPair() {
    // Allocate single variable in V2
    int varId = _v2.allocateFreshVar();

    // Create synthetic IDs for compatibility
    int writerId = _nextSyntheticId++;
    int readerId = _nextSyntheticId++;

    // Map synthetic IDs to real variable
    _writerToVar[writerId] = varId;
    _readerToVar[readerId] = varId;

    return (writerId, readerId);
  }

  @override
  void bindWriterStruct(int writerId, String f, List<Term> args) {
    super.bindWriterStruct(writerId, f, args);  // PROBLEM: Duplicate storage!
    // Also stores in V2...
  }

  // ... rest of adapter
}
```

**Problems Discovered:**
1. Adapter uses BOTH heaps (duplicate storage)
2. Dereferencing inconsistency between heaps
3. Performance overhead from double operations
4. Confusion about source of truth

---

## Current Situation

### Architecture Issues
1. **53 occurrences** of WriterTerm/ReaderTerm in runner.dart
2. **VarRef exists** but only in heap_v2.dart, not used elsewhere
3. **HeapV2Adapter** causing duplicate storage and inconsistencies
4. **Two-ID system** still pervasive throughout codebase

### Active Bug
- Nested structures failing: `test_conj(Z)` returns `<unbound>` instead of `b`
- Parent context fix applied but incomplete
- Likely related to two-ID confusion in unification

### Decision Made
**Fix nested structure bug AFTER migration** - it may resolve itself with cleaner single-ID code

---

## Phase 2: Complete Single-ID Migration

### Step 2.0: Pre-Migration Setup

#### Step 2.0.1: Create Migration Branch
```bash
git checkout -b single-id-migration
git add -A
git commit -m "checkpoint: before single-ID migration"
```

#### Step 2.0.2: Move VarRef to terms.dart
**File:** `lib/runtime/terms.dart`
```dart
// Move from heap_v2.dart to terms.dart
class VarRef implements Term {
  final int varId;
  final bool isReader;  // Keep for now, review in Phase 3
  
  VarRef(this.varId, {required this.isReader});
  
  @override
  String toString() => isReader ? 'R$varId?' : 'W$varId';
  
  @override
  bool operator ==(Object other) =>
      other is VarRef &&
      other.varId == varId &&
      other.isReader == isReader;
  
  @override
  int get hashCode => Object.hash(varId, isReader);
}

// Keep WriterTerm/ReaderTerm temporarily for gradual migration
// Mark as @deprecated
```

#### Step 2.0.3: Create Migration Helper
**File:** `lib/bytecode/migration_helper.dart`
```dart
/// Temporary helpers during migration phase
/// Will be deleted after migration complete
class MigrationHelper {
  /// Convert old term types to VarRef
  static Term toVarRef(Term t) {
    if (t is WriterTerm) return VarRef(t.writerId, isReader: false);
    if (t is ReaderTerm) return VarRef(t.readerId, isReader: true);
    return t;
  }
  
  /// Convert VarRef back to old types (for compatibility during migration)
  static Term fromVarRef(Term t) {
    if (t is VarRef) {
      return t.isReader 
        ? ReaderTerm(t.varId)
        : WriterTerm(t.varId);
    }
    return t;
  }
  
  /// Check if term is any variable type
  static bool isVariable(Term t) {
    return t is VarRef || t is WriterTerm || t is ReaderTerm;
  }
}
```

**Test after 2.0.3:**
```bash
dart compile exe bin/glp_repl.dart
# Should compile without errors
```

### Step 2.1: Create Unified Instruction Set
**File:** `lib/bytecode/opcodes_v2.dart`
```dart
/// Unified instructions with mode flag replacing separate Writer/Reader variants
abstract class OpV2 {
  String get mnemonic;
}

/// Unified head variable instruction
class HeadVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  
  HeadVariable(this.varIndex, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'head_reader' : 'head_writer';
}

/// Unified unify variable instruction
class UnifyVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  
  UnifyVariable(this.varIndex, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'unify_reader' : 'unify_writer';
}

/// Unified put variable instruction
class PutVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  final int argSlot;
  
  PutVariable(this.varIndex, this.argSlot, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'put_reader' : 'put_writer';
}

/// Similar for SetVariable, GetVariable, etc.
```

### Step 2.2: Create Instruction Migration Adapter
**File:** `lib/bytecode/instruction_adapter.dart`
```dart
/// Adapter to run old bytecode with new unified instructions
class InstructionAdapter {
  /// Convert old instruction to new unified format
  static OpV2 migrate(Op oldOp) {
    switch(oldOp.runtimeType) {
      case HeadWriter:
        final hw = oldOp as HeadWriter;
        return HeadVariable(hw.varIndex, isReader: false);
      
      case HeadReader:
        final hr = oldOp as HeadReader;
        return HeadVariable(hr.varIndex, isReader: true);
      
      case UnifyWriter:
        final uw = oldOp as UnifyWriter;
        return UnifyVariable(uw.varIndex, isReader: false);
      
      case UnifyReader:
        final ur = oldOp as UnifyReader;
        return UnifyVariable(ur.varIndex, isReader: true);
      
      case PutWriter:
        final pw = oldOp as PutWriter;
        return PutVariable(pw.varIndex, pw.argSlot, isReader: false);
      
      case PutReader:
        final pr = oldOp as PutReader;
        return PutVariable(pr.varIndex, pr.argSlot, isReader: true);
      
      // ... continue for all variable-related instructions
      
      default:
        throw UnimplementedError('Migration for ${oldOp.runtimeType}');
    }
  }
  
  /// Check if instruction needs migration
  static bool needsMigration(Op op) {
    return op is HeadWriter || op is HeadReader ||
           op is UnifyWriter || op is UnifyReader ||
           op is PutWriter || op is PutReader ||
           op is SetWriter || op is SetReader ||
           op is GetVariable || op is GetValue;
  }
}
```

### Step 2.3: Systematic Runner Migration (53 occurrences)

#### Group 2.3.1: Variable Allocation (5 occurrences)
**Lines:** 946, 967, 1050, 1060, 1087 in runner.dart

**Pattern to find:**
```dart
final (freshWriterId, freshReaderId) = cx.rt.heap.allocateFreshPair();
cx.rt.heap.addWriter(WriterCell(freshWriterId, freshReaderId));
cx.rt.heap.addReader(ReaderCell(freshReaderId));
```

**Replace with:**
```dart
final varId = cx.rt.heap.allocateFreshVar();
cx.rt.heap.addVariable(varId);
// Use VarRef(varId, isReader: false) for writer
// Use VarRef(varId, isReader: true) for reader
```

**Test after 2.3.1:**
```bash
dart test test/bytecode/asm_smoke_test.dart
# If pass:
dart test test/custom/variable_allocation_test.dart
# If all pass:
git commit -m "refactor: migrate variable allocation to single-ID (Group 2.3.1)"
```

#### Group 2.3.2: Structure Arguments (8 occurrences)
**Locations:** Where StructTerm contains WriterTerm/ReaderTerm

**Pattern to find:**
```dart
struct.args[i] = WriterTerm(writerId);
struct.args[i] = ReaderTerm(readerId);
```

**Replace with:**
```dart
struct.args[i] = VarRef(varId, isReader: false);
struct.args[i] = VarRef(varId, isReader: true);
```

**Test after 2.3.2:**
```bash
dart test test/bytecode/asm_smoke_test.dart
dart test test/custom/structure_test.dart
git commit -m "refactor: migrate structure args to VarRef (Group 2.3.2)"
```

#### Group 2.3.3: SetWriter/SetReader Handlers (12 occurrences)
**Lines:** SetWriter handler (~1570-1640), SetReader handler (~1680-1780)

**BODY phase structure building operations**

**Pattern to find:**
```dart
if (op is SetWriter) {
  // ... creates WriterTerm
  struct.args[cx.S] = WriterTerm(writerId);
}

if (op is SetReader) {
  // ... creates ReaderTerm
  struct.args[cx.S] = ReaderTerm(readerId);
}
```

**Replace with:**
```dart
if (op is SetWriter) {
  // ... creates writer VarRef
  struct.args[cx.S] = VarRef(varId, isReader: false);
}

if (op is SetReader) {
  // ... creates reader VarRef
  struct.args[cx.S] = VarRef(varId, isReader: true);
}
```

**Test after 2.3.3:**
```bash
dart test test/bytecode/asm_smoke_test.dart
dart test test/custom/body_phase_test.dart
git commit -m "refactor: migrate SetWriter/SetReader to VarRef (Group 2.3.3)"
```

#### Group 2.3.4: UnifyWriter/UnifyReader Handlers (10 occurrences)
**Lines:** UnifyWriter handler (~924-1029), UnifyReader handler (~1030-1120)

**HEAD phase matching operations**

**Pattern to find:**
```dart
if (value is WriterTerm) {
  cx.clauseVars[op.varIndex] = value.writerId;
}
if (value is ReaderTerm) {
  cx.clauseVars[op.varIndex] = value;
}
```

**Replace with:**
```dart
if (value is VarRef && !value.isReader) {
  cx.clauseVars[op.varIndex] = value.varId;
}
if (value is VarRef && value.isReader) {
  cx.clauseVars[op.varIndex] = value;
}
```

**Test after 2.3.4:**
```bash
dart test test/bytecode/asm_smoke_test.dart
dart test test/custom/head_phase_test.dart
git commit -m "refactor: migrate UnifyWriter/UnifyReader to VarRef (Group 2.3.4)"
```

#### Group 2.3.5: HeadWriter/HeadReader Handlers (5 occurrences)
**If these exist separately from UnifyWriter/UnifyReader**

**Similar pattern to Group 2.3.4**

**Test after 2.3.5:**
```bash
dart test test/bytecode/asm_smoke_test.dart
git commit -m "refactor: migrate HeadWriter/HeadReader to VarRef (Group 2.3.5)"
```

#### Group 2.3.6: Dereferencing and Binding Operations (8 occurrences)
**Lines:** Various locations where heap dereferencing occurs

**Pattern to find:**
```dart
if (term is WriterTerm) {
  final value = heap.valueOfWriter(term.writerId);
}
if (term is ReaderTerm) {
  final wid = heap.writerIdForReader(term.readerId);
}
```

**Replace with:**
```dart
if (term is VarRef) {
  final value = heap.getValue(term.varId);
}
```

**Test after 2.3.6:**
```bash
dart test test/bytecode/asm_smoke_test.dart
dart test test/custom/unification_test.dart
git commit -m "refactor: migrate dereferencing to single-ID (Group 2.3.6)"
```

#### Group 2.3.7: ROQ/Suspension Handling (5 occurrences)
**Lines:** Where reader suspension and wake operations occur

**Pattern to find:**
```dart
cx.si.add(readerId);  // Suspension set
heap.addSuspension(readerId, goalId);
```

**Replace with:**
```dart
cx.si.add(varId);  // Same variable ID
heap.addSuspension(varId, goalId);
```

**Test after 2.3.7:**
```bash
dart test test/bytecode/asm_smoke_test.dart
dart test test/custom/suspension_test.dart
git commit -m "refactor: migrate ROQ/suspension to single-ID (Group 2.3.7)"
```

### Step 2.4: Replace HeapV2Adapter with Direct HeapV2

#### Step 2.4.1: Fix HeapV2Adapter Duplicate Storage
**File:** `lib/runtime/heap_v2_adapter.dart`

**Remove inheritance:**
```dart
// OLD: class HeapV2Adapter extends Heap
// NEW: class HeapV2Adapter implements HeapInterface
```

**Remove duplicate calls:**
```dart
@override
void bindWriterStruct(int writerId, String f, List<Term> args) {
  // REMOVE: super.bindWriterStruct(writerId, f, args);
  
  // Dereference ReaderTerms before converting to V2
  final dereferencedArgs = args.map((arg) {
    if (arg is ReaderTerm) {
      final wid = writerIdForReader(arg.readerId);
      if (wid != null && isWriterBound(wid)) {
        return valueOfWriter(wid);
      }
    }
    return arg;
  }).toList();
  
  // Convert and store in V2 only
  final v2Args = dereferencedArgs.map((arg) => _convertToV2(arg)).toList();
  final varId = _writerToVar[writerId]!;
  _v2.bindVariableStruct(varId, f, v2Args);
}
```

#### Step 2.4.2: Update Runtime to Use HeapV2 Directly
**File:** `lib/runtime/runtime.dart`

```dart
// OLD:
import 'heap_v2_adapter.dart';
final heap = HeapV2Adapter();

// NEW:
import 'heap_v2.dart';
final heap = HeapV2();
```

**Test after 2.4:**
```bash
dart test  # Run full test suite
# Expected: 206 tests passing
git commit -m "refactor: remove HeapV2Adapter, use HeapV2 directly"
```

### Step 2.5: Cleanup

#### Step 2.5.1: Remove Old Term Types
**File:** `lib/runtime/terms.dart`

```dart
// DELETE these classes:
class WriterTerm implements Term { ... }
class ReaderTerm implements Term { ... }
```

#### Step 2.5.2: Delete Old Files
```bash
git rm lib/runtime/heap.dart
git rm lib/runtime/heap_v2_adapter.dart
git rm lib/runtime/cells.dart  # If no longer needed
git rm lib/bytecode/migration_helper.dart  # No longer needed
```

#### Step 2.5.3: Rename HeapV2
```bash
git mv lib/runtime/heap_v2.dart lib/runtime/heap.dart
```

#### Step 2.5.4: Update Imports
```bash
find lib -name "*.dart" -exec sed -i 's/heap_v2/heap/g' {} \;
find test -name "*.dart" -exec sed -i 's/heap_v2/heap/g' {} \;
```

**Final test:**
```bash
dart test
# Expected: All 206 tests passing
git commit -m "refactor: complete single-ID migration cleanup"
```

---

## Phase 2.5: Integration Validation

### Test Matrix

| Test Case | Description | Expected Result | Actual | Pass? |
|-----------|-------------|-----------------|--------|-------|
| Basic unification | `p(a). q(X?) :- p(X).` | X = a | | |
| List operations | `merge([1,2],[a,b],Xs)` | Xs = [1,2,a,b] | | |
| Nested structures | `test_conj(Z)` | Z = b | | |
| Suspension/wake | Reader dependency | Resumes correctly | | |
| Performance | Operations/second | >10,000 | | |
| Memory usage | Heap size | <Previous | | |

### Validation Script
**File:** `test/refactoring/phase2_validation.dart`

```dart
void main() {
  group('Phase 2 Validation', () {
    test('No WriterTerm/ReaderTerm remain', () {
      // Scan codebase
      final result = Process.runSync('grep', [
        '-r', 'WriterTerm\\|ReaderTerm', 'lib/'
      ]);
      expect(result.stdout.toString().trim(), isEmpty);
    });
    
    test('All variables use single ID', () {
      final heap = HeapV2();
      final varId = heap.allocateFreshVar();
      
      // Same ID for writer and reader access
      final writer = VarRef(varId, isReader: false);
      final reader = VarRef(varId, isReader: true);
      
      expect(writer.varId, equals(reader.varId));
    });
    
    test('No duplicate storage', () {
      // Verify HeapV2Adapter is gone
      expect(() => HeapV2Adapter(), throwsA(isA<Error>()));
    });
    
    test('Nested structure bug fixed', () {
      // Run test_conj(Z)
      final result = runGlpProgram('test_conj_var.glp', 'test_conj(Z)');
      expect(result['Z'], equals('b'));
    });
  });
}
```

### Decision Tree for Issues

```
All tests pass?
├─ YES → Proceed to Phase 3
└─ NO → Which failed?
    ├─ Basic unification → Check VarRef conversion in UnifyWriter/UnifyReader
    ├─ Lists → Check list building with VarRef
    ├─ Nested structures → Check structure traversal in HEAD
    ├─ Suspension → Check ROQ handling with single IDs
    └─ Performance → Profile to find bottleneck
        ├─ Heap lookups slow? → Check V2 implementation
        ├─ Extra conversions? → Remove migration helpers
        └─ Memory issue? → Check for leaks
```

---

## Phase 3: Array-Based Registers

### Step 3.1: Implement Array-Based Context
**File:** `lib/bytecode/runner_context_v2.dart`

```dart
class RunnerContextV2 {
  // Replace Map<int, dynamic> with arrays
  final List<Term?> X = List.filled(256, null);  // Clause registers
  final List<Term?> Y = List.filled(64, null);   // Environment permanent vars
  final List<Term?> A = List.filled(32, null);   // Argument registers
  
  // O(1) access instead of map lookup
  Term? getX(int index) => X[index];
  void setX(int index, Term? value) => X[index] = value;
  
  Term? getY(int index) => Y[index];
  void setY(int index, Term? value) => Y[index] = value;
  
  Term? getA(int index) => A[index];
  void setA(int index, Term? value) => A[index] = value;
  
  // Clear registers for reuse
  void clearX() => X.fillRange(0, X.length, null);
  void clearA() => A.fillRange(0, A.length, null);
}
```

### Step 3.2: Migrate Runner to Use Arrays
**Update runner.dart to use array-based context**

**Pattern to find:**
```dart
cx.clauseVars[index] = value;  // Map access
final value = cx.clauseVars[index];
```

**Replace with:**
```dart
cx.setX(index, value);  // Array access
final value = cx.getX(index);
```

### Step 3.3: Benchmark Array Performance
```dart
void main() {
  benchmark('Map vs Array', () {
    // Compare map-based vs array-based performance
    final mapContext = RunnerContext();
    final arrayContext = RunnerContextV2();
    
    // Measure access times
  });
}
```

### CHECKPOINT 3: Array Implementation Validation

---

## Phase 3.5: Register Integration Test

### Step 3.5.1: Test Array Registers
**File:** `test/refactoring/register_integration_test.dart`

```dart
void main() {
  group('Array Register Tests', () {
    test('Nested structures with array registers', () {
      // Test: run((merge([1,2,3], Xs), helper(Xs?)))
      final result = runWithArrayRegisters(complexProgram);
      expect(result.success, isTrue);
    });
    
    test('Deep recursion with Y registers', () {
      // Test environment frame handling
      final result = runDeepRecursion(1000);
      expect(result.stackOverflow, isFalse);
    });
    
    test('Register allocation boundaries', () {
      // Test programs that use many variables
      final result = runManyVariables(250);
      expect(result.success, isTrue);
    });
    
    test('Register clearing between clauses', () {
      // Verify registers are properly cleared
      final context = RunnerContextV2();
      context.setX(0, VarRef(1000, isReader: false));
      context.clearX();
      expect(context.getX(0), isNull);
    });
  });
}
```

### CHECKPOINT 3.5: Register Integration Results

| Test | Expected | Actual | Pass? |
|------|----------|--------|-------|
| Nested structures | Works | | |
| Deep recursion | No overflow | | |
| Many variables | Handles 250+ | | |
| Register clearing | Proper cleanup | | |

---

## Phase 4: Comprehensive Integration Testing

### Step 4.1: Create Full Integration Harness
**File:** `lib/runtime/runtime_v2.dart`

```dart
class GlpRuntimeV2 {
  final heap = HeapV2();  // Single-ID heap
  final Map<int, RunnerContextV2> contexts = {};  // Array-based contexts
  
  // Run with all new components
  Result execute(Program program, Goal goal) {
    // Create context with array registers
    final context = RunnerContextV2();
    
    // Use unified instructions if available
    final bytecode = program.bytecode;
    if (bytecode.usesV2Instructions) {
      return executeV2(bytecode, context);
    } else {
      // Migrate old instructions on the fly
      final migrated = migrateInstructions(bytecode);
      return executeV2(migrated, context);
    }
  }
  
  Result executeV2(BytecodeV2 bytecode, RunnerContextV2 context) {
    // Main execution loop with all optimizations
  }
}
```

### Step 4.2: Run Full Test Suite
```bash
# Run all tests with V2 runtime
dart test --name "v2"

# Run benchmark suite
dart run benchmark/full_suite.dart

# Compare V1 vs V2 performance
dart run benchmark/comparison.dart
```

### Step 4.3: Side-by-Side Validation

| Test Category | V1 Result | V2 Result | Match? | Performance |
|---------------|-----------|-----------|--------|-------------|
| Basic unification | PASS | | | |
| List operations | PASS | | | |
| Nested structures | FAIL | | | |
| Suspension | PASS | | | |
| Meta-interpreter | PASS | | | |
| Overall | 191/206 | | | |

### Step 4.4: Performance Analysis
```dart
void main() {
  final v1Runtime = GlpRuntime();   // Old
  final v2Runtime = GlpRuntimeV2(); // New
  
  benchmark('Unification', () {
    // Measure unification performance
    v1Runtime.unify(term1, term2);
    v2Runtime.unify(term1, term2);
  });
  
  benchmark('Structure building', () {
    // Measure structure building
  });
  
  benchmark('ROQ operations', () {
    // Measure suspension/wake
  });
}
```

### CHECKPOINT 4: Full Integration Results

---

## Phase 5: Migration Completion

### Step 5.1: Switch to V2 Implementation

#### Step 5.1.1: Update All Imports
```bash
# Update runtime imports
find lib -name "*.dart" -exec sed -i 's/runtime.dart/runtime_v2.dart/g' {} \;

# Update heap imports (should already be done)
find lib -name "*.dart" -exec sed -i 's/heap_v2/heap/g' {} \;

# Update context imports
find lib -name "*.dart" -exec sed -i 's/runner_context.dart/runner_context_v2.dart/g' {} \;
```

#### Step 5.1.2: Verify No Old Code Remains
```bash
# Check for WriterTerm/ReaderTerm
grep -r "WriterTerm\|ReaderTerm" lib/ | wc -l
# Expected: 0

# Check for old heap
grep -r "heap_v2_adapter\|HeapV2Adapter" lib/ | wc -l
# Expected: 0

# Check for old context
grep -r "Map<int.*clauseVars" lib/ | wc -l
# Expected: 0
```

### Step 5.2: Remove Old Implementation

```bash
# Remove old files (if not already done)
git rm -f lib/runtime/heap_old.dart
git rm -f lib/runtime/heap_v2_adapter.dart
git rm -f lib/bytecode/runner_context_old.dart
git rm -f lib/bytecode/opcodes_old.dart

# Remove migration helpers
git rm -f lib/bytecode/migration_helper.dart
git rm -f lib/bytecode/instruction_adapter.dart
```

### Step 5.3: Update Documentation

#### Step 5.3.1: Update README.md
```markdown
# GLP Runtime

## Performance Improvements (v3.0)
- 98% faster heap operations through single-ID variable system
- O(1) register access with array-based contexts
- Eliminated duplicate storage from adapter layer
- Unified instruction set reduces dispatch overhead

## Architecture
- Single-ID variables (one ID per variable, accessed as writer or reader)
- Array-based register allocation
- Direct HeapV2 implementation
```

#### Step 5.3.2: Create ARCHITECTURE.md
```markdown
# GLP Runtime Architecture

## Variable System
GLP uses a single-ID variable system where each variable has one ID
that can be accessed in two modes:
- Writer mode: Can bind the variable
- Reader mode: Can read or suspend on the variable

## Register Allocation
- X registers: Clause-local variables (256 slots)
- Y registers: Environment permanent variables (64 slots)
- A registers: Argument passing (32 slots)
```

#### Step 5.3.3: Update Inline Documentation
Add comments explaining single-ID system throughout codebase.

### CHECKPOINT 5: Migration Complete

Final validation:
```bash
# Run complete test suite
dart test
# Expected: 206/206 tests passing

# Run benchmarks
dart run benchmark/full_suite.dart
# Expected: >95% performance improvement

# Check code quality
dart analyze
# Expected: No issues found
```

---

## Risk Mitigation

### Rollback Procedures

Each phase is independently revertible:

#### If Phase 2 fails:
```bash
git log --oneline | head -20  # Find last good commit
git reset --hard [last-good-commit]
git checkout main
git branch -D single-id-migration
```

#### If Phase 3 fails:
```bash
# Keep single-ID but revert array registers
git revert [array-register-commit]
```

#### If Phase 4 fails:
```bash
# Keep components but fix integration
git checkout -b fix-integration
# Debug and fix
```

### Backup Strategy

Before each major phase:
```bash
# Create backup branch
git checkout -b backup-before-phase-X
git checkout single-id-migration

# Create tar backup
tar -czf glp-runtime-backup-$(date +%Y%m%d).tar.gz .
```

### Testing Strategy

**Incremental Testing:**
- After each group: `dart test test/bytecode/asm_smoke_test.dart`
- After each phase: `dart test`
- Before merge: Full regression suite

**Performance Monitoring:**
- Benchmark after each phase
- Alert if >5% performance regression
- Profile if memory usage increases

### Success Criteria

**Proceed to next phase only if:**
- ✅ All integration tests pass
- ✅ No performance regression (>95% of previous)
- ✅ No semantic changes (same results)
- ✅ Clean git status (all committed)
- ✅ No analyzer warnings

### Communication Plan

**After each phase:**
1. Update team with progress
2. Document any issues encountered
3. Get approval before proceeding

**If blocked:**
1. Document the blocker
2. Propose solutions
3. Get input before major changes

---

## Appendix A: File Mapping

| Old File | New File | Status |
|----------|----------|--------|
| heap.dart | heap.dart (from heap_v2) | To migrate |
| heap_v2_adapter.dart | (deleted) | To remove |
| terms.dart (WriterTerm/ReaderTerm) | terms.dart (VarRef) | To migrate |
| runner_context.dart | runner_context_v2.dart | To create |
| opcodes.dart | opcodes_v2.dart | Partially done |

## Appendix B: Critical Code Sections

### Variable Allocation (runner.dart)
- Lines 946, 967, 1050, 1060, 1087

### Structure Building (runner.dart)
- SetWriter: Lines 1570-1640
- SetReader: Lines 1680-1780

### Unification (runner.dart)
- UnifyWriter: Lines 924-1029
- UnifyReader: Lines 1030-1120

### Suspension (runner.dart)
- ROQ operations: Various locations

## Appendix C: Test Programs

### test_conj_var.glp
```prolog
test_conj(Y?) :- foo((bar(X), baz(X?,Y))).
foo((bar(a), baz(a,b))).
```

### merge.glp
```prolog
merge([],Xs,Xs?).
merge(Xs,[],Xs?).
merge([X|Xs],[Y|Ys],[X|Zs]) :- X =< Y | merge(Xs?,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X > Y | merge([X|Xs],Ys?,Zs).
```

---

## Summary

This plan completes the transition from two-ID to single-ID variables that was started in Phase 1 but never finished. The key is incremental migration with testing at each step to ensure nothing breaks. The nested structure bug will likely be fixed as a side effect of the cleaner single-ID implementation.

Total estimated time: 8-12 hours of careful work
Risk level: Medium (mitigated by incremental approach)
Benefit: Cleaner architecture, better performance, foundation for future optimizations
