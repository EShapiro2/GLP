# GLP Runtime Refactoring Plan v2 - With Early Integration Validation
**Revision Date:** 2025-11-09  
**Key Change:** Added integration checkpoints after each component phase

## Executive Summary
Refactor GLP runtime to align with FCP design with **incremental integration testing** to validate each component before proceeding.

### Phase Structure (Revised)
- **Phase 0**: Baseline capture ✅ COMPLETE
- **Phase 1**: Single ID variables ✅ COMPLETE
- **Phase 1.5**: HeapV2 Integration Test ✅ COMPLETE
- **Phase 1.5b**: HeapV2 Adapter Implementation 🆕
- **Phase 2**: Instruction unification
- **Phase 2.5**: Instruction Integration Test 🆕
- **Phase 3**: Array-based registers
- **Phase 3.5**: Register Integration Test 🆕
- **Phase 4**: Full integration testing
- **Phase 5**: Migration completion

### Integration Checkpoint Benefits
- **Early problem detection** - Find issues immediately, not after 3 phases
- **Lower risk** - Validate each component works before building on it
- **Easier debugging** - Isolate which change caused problems
- **Confidence building** - Know each layer is solid

---

# PHASE 0: BASELINE CAPTURE ✅ COMPLETE

**Status:** Complete at commit 86538ca
- 191/206 tests passing (92.7%)
- Writer+1=Reader pattern confirmed
- O(n) lookup performance documented

---

# PHASE 1: SINGLE ID VARIABLE SYSTEM ✅ COMPLETE

**Status:** Complete at commit 86538ca
- HeapV2 implemented (186 lines)
- 98.9% performance improvement
- Compatibility tests passing

---

# PHASE 1.5: HEAPV2 INTEGRATION VALIDATION 🆕

## Overview
Before proceeding to Phase 2, validate HeapV2 works with the actual bytecode runner on real GLP programs.

## Step 1.5.1: Create Integration Test Harness
Create file: `test/refactoring/heap_v2_integration_test.dart`

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

## Step 1.5.2: Run Side-by-Side Comparison
```bash
# Run same programs with both heaps
dart test test/refactoring/heap_v2_integration_test.dart

# Compare results
echo "=== HEAP V1 vs V2 COMPARISON ===" > refactoring/reports/heap_integration.md
```

## Step 1.5.3: Validate Critical Paths
Test these specific scenarios that exercise heap integration:
1. **Variable allocation during bytecode execution**
2. **Binding through bytecode instructions**
3. **Suspension/ROQ interaction**
4. **Dereferencing chains**
5. **Structure building with variables**

## CHECKPOINT 1.5: Integration Validation

### Required Results
```
HeapV2 Integration Test Results:
- Simple unification: PASS/FAIL
- List operations: PASS/FAIL
- Suspension/reactivation: PASS/FAIL
- Structure building: PASS/FAIL
- Performance maintained: YES/NO

Ready for Phase 2: YES/NO
```

### Decision Tree
- ✅ All pass → Proceed to Phase 2
- ⚠️ Minor issues → Fix and retest
- ❌ Major issues → Investigate heap assumptions
- 🔴 Fundamental problems → Reconsider approach

---

# PHASE 1.5b: HEAPV2 ADAPTER IMPLEMENTATION 🆕

## Overview
Phase 1.5 discovered that HeapV2 cannot be directly integrated due to type system incompatibility (WriterTerm/ReaderTerm vs VarRef). This phase creates an adapter layer to bridge the gap.

## Step 1.5b.1: Create HeapV2 Adapter
Create file: `lib/runtime/heap_v2_adapter.dart`

```dart
/// Adapter that presents old Heap interface but uses HeapV2 internally
import 'heap.dart' as old;
import 'heap_v2.dart';
import 'terms.dart';

class HeapV2Adapter implements old.Heap {
  final HeapV2 _v2 = HeapV2();
  final Map<int, int> _writerToVar = {};
  final Map<int, int> _readerToVar = {};
  int _nextSyntheticId = 1000;  // Start high to avoid conflicts

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
  void bindWriter(int writerId, Term value) {
    int varId = _writerToVar[writerId]!;
    // Convert Term to V2 format and bind
    _v2.bind(VarRef(varId, isReader: false), _convertTerm(value));
  }

  @override
  Term? deref(Term term) {
    if (term is WriterTerm) {
      int? varId = _writerToVar[term.id];
      if (varId != null) {
        var v2Term = _v2.deref(VarRef(varId, isReader: false));
        return _convertBackTerm(v2Term);
      }
    } else if (term is ReaderTerm) {
      int? varId = _readerToVar[term.id];
      if (varId != null) {
        var v2Term = _v2.deref(VarRef(varId, isReader: true));
        return _convertBackTerm(v2Term);
      }
    }
    return term;
  }

  // Bridge ROQ operations
  @override
  void suspendGoalOnReader(int readerId, int goalId) {
    int varId = _readerToVar[readerId]!;
    _v2.suspendOn(varId, goalId);
  }

  @override
  Set<int>? takeSuspendedGoals(int writerId) {
    int varId = _writerToVar[writerId]!;
    return _v2.takeSuspended(varId);
  }

  // Internal conversion helpers
  Term _convertTerm(Term old) { /* Convert old format to V2 */ }
  Term _convertBackTerm(dynamic v2Term) { /* Convert V2 to old format */ }
}
```

## Step 1.5b.2: Test Adapter Integration
Update file: `test/refactoring/heap_v2_integration_test.dart`

```dart
void main() {
  group('HeapV2Adapter Integration', () {
    test('Adapter works with existing bytecode runner', () {
      // Create runner with adapter instead of direct HeapV2
      final heap = HeapV2Adapter();
      final runner = BytecodeRunner(heap: heap);

      // Run test programs
      final result = runner.execute(testProgram);
      expect(result.success, isTrue);
    });

    test('Performance improvement maintained', () {
      // Verify adapter doesn't negate performance gains
      final adapter = HeapV2Adapter();
      // Benchmark operations through adapter
    });
  });
}
```

## Step 1.5b.3: Validate Production Compatibility
```bash
# Temporarily replace heap in one test file
cp lib/runtime/heap.dart lib/runtime/heap_original.dart
cp lib/runtime/heap_v2_adapter.dart lib/runtime/heap.dart

# Run specific test to validate
dart test test/custom/merge_test.dart

# Restore original
cp lib/runtime/heap_original.dart lib/runtime/heap.dart
```

## CHECKPOINT 1.5b: Adapter Validation

### Required Results
```
Adapter Test Results:
- Existing programs work: PASS/FAIL
- Performance maintained: YES/NO (>90% of direct V2 speed)
- No semantic changes: YES/NO
- Memory overhead acceptable: YES/NO

Ready for Phase 2: YES/NO
```

### Decision Tree
- ✅ All pass → Proceed to Phase 2 with adapter approach
- ⚠️ Performance degraded → Optimize adapter layer
- ❌ Semantic differences → Fix term conversion logic
- 🔴 Doesn't work → Reconsider approach (might need bigger refactor)

### Migration Strategy
With working adapter, we can:
1. **Immediate** - Deploy adapter for instant performance gain
2. **Phase 2-3** - Continue building other optimizations
3. **Phase 4** - Gradually migrate from WriterTerm/ReaderTerm to VarRef
4. **Phase 5** - Remove adapter, use HeapV2 directly

---

# PHASE 2: INSTRUCTION UNIFICATION

## Step 2.1: Create Unified Instruction Set
Create file: `lib/bytecode/opcodes_v2.dart`

```dart
/// Unified instructions with mode flag
class HeadVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  
  HeadVariable(this.varIndex, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'head_reader' : 'head_writer';
}
// Similar for UnifyVariable, PutVariable
```

## Step 2.2: Create Migration Adapter
Create file: `lib/bytecode/instruction_adapter.dart`

```dart
/// Adapter to run old bytecode with new instructions
class InstructionAdapter {
  static OpV2 migrate(Op oldOp) {
    switch(oldOp.runtimeType) {
      case HeadWriter: 
        return HeadVariable(oldOp.varIndex, isReader: false);
      case HeadReader:
        return HeadVariable(oldOp.varIndex, isReader: true);
      // ... other mappings
    }
  }
}
```

## CHECKPOINT 2: Report unified instruction creation

---

# PHASE 2.5: INSTRUCTION INTEGRATION TEST 🆕

## Step 2.5.1: Test Unified Instructions
Create file: `test/refactoring/instruction_integration_test.dart`

```dart
/// Verify unified instructions work with existing runner
void main() {
  test('Migrated bytecode produces same results', () {
    // Take existing bytecode
    final oldBytecode = loadTestProgram();
    
    // Migrate to unified instructions
    final newBytecode = migrateInstructions(oldBytecode);
    
    // Run both versions
    final oldResult = runWithOldInstructions(oldBytecode);
    final newResult = runWithNewInstructions(newBytecode);
    
    // Verify identical behavior
    expect(newResult, equals(oldResult));
  });
}
```

## Step 2.5.2: Performance Validation
```bash
# Measure instruction dispatch overhead
dart run test/refactoring/instruction_benchmark.dart
```

## CHECKPOINT 2.5: Instruction Integration Results

---

# PHASE 3: ARRAY-BASED REGISTERS

## Step 3.1: Implement Array-Based Context
Create file: `lib/bytecode/runner_context_v2.dart`

```dart
class RunnerContextV2 {
  // Replace Map<int, dynamic> with arrays
  final List<Term?> X = List.filled(256, null);  // Clause registers
  final List<Term?> Y = List.filled(64, null);   // Environment
  
  // O(1) access instead of map lookup
  Term? getRegister(int index) => X[index];
  void setRegister(int index, Term value) => X[index] = value;
}
```

## CHECKPOINT 3: Report array implementation

---

# PHASE 3.5: REGISTER INTEGRATION TEST 🆕

## Step 3.5.1: Test Array Registers
Create file: `test/refactoring/register_integration_test.dart`

```dart
/// Verify array registers work with complex programs
void main() {
  test('Nested structures with array registers', () {
    // Test complex structure building
    // run((merge([1,2,3], Xs), helper(Xs?)))
  });
  
  test('Deep recursion with Y registers', () {
    // Test environment frame handling
  });
  
  test('Register allocation boundaries', () {
    // Test programs that use many variables
  });
}
```

## CHECKPOINT 3.5: Register Integration Results

---

# PHASE 4: COMPREHENSIVE INTEGRATION TESTING

Now that each component has been individually validated, run comprehensive tests with ALL new components together.

## Step 4.1: Create Full Integration Harness
```dart
class GLPRuntimeV2 {
  final heapV2 = HeapV2();
  final contextV2 = RunnerContextV2();
  final instructionsV2 = UnifiedInstructions();
  
  // Run with all new components
  Result execute(Program p) { ... }
}
```

## Step 4.2: Run Full Test Suite
```bash
# Run all 206 tests with v2 runtime
dart test --name "v2"

# Run benchmark suite
dart run benchmark/full_suite.dart
```

## Step 4.3: Side-by-Side Validation
Run every test program with both V1 and V2, verify:
1. **Identical results** - Same bindings, same success/failure
2. **Same suspension behavior** - Programs suspend on same conditions
3. **Performance improvements** - V2 faster across the board

## CHECKPOINT 4: Full Integration Results

### Validation Matrix
```
Test Category     | V1 Result | V2 Result | Match?
------------------|-----------|-----------|--------
Basic unification | PASS      | PASS      | YES
List operations   | PASS      | PASS      | YES
Suspension        | PASS      | PASS      | YES
Meta-interpreter  | PASS      | PASS      | YES
Performance       | Baseline  | +98%      | BETTER
```

---

# PHASE 5: MIGRATION COMPLETION

## Step 5.1: Switch to V2 Implementation
```bash
# Update all imports
find lib -name "*.dart" -exec sed -i '' 's/heap.dart/heap_v2.dart/g' {} \;
find lib -name "*.dart" -exec sed -i '' 's/opcodes.dart/opcodes_v2.dart/g' {} \;
find lib -name "*.dart" -exec sed -i '' 's/runner_context.dart/runner_context_v2.dart/g' {} \;

# Run final validation
dart test
```

## Step 5.2: Remove Old Implementation
```bash
git rm lib/runtime/heap.dart
git rm lib/bytecode/opcodes_old.dart
git mv lib/runtime/heap_v2.dart lib/runtime/heap.dart
```

## Step 5.3: Update Documentation
- Update README.md with performance improvements
- Document new architecture in ARCHITECTURE.md
- Update inline documentation

## CHECKPOINT 5: Migration Complete

---

# Decision Points & Risk Mitigation

## After Each Integration Checkpoint
**Questions to answer:**
1. Does the component work in isolation? ✅
2. Does it work integrated with the system? ✅  
3. Are there hidden dependencies we missed? 
4. Is performance still improved?
5. Any semantic differences?

## Rollback Procedures
Each phase is independently revertible:
```bash
# Phase 1.5 fails
git revert HEAD  # Just remove integration test

# Phase 2 fails  
git reset --hard [phase1-commit]  # Back to HeapV2 only

# Phase 3 fails
git reset --hard [phase2-commit]  # Keep heap + instructions
```

## Success Criteria
**Proceed to next phase only if:**
- ✅ All integration tests pass
- ✅ No performance regression
- ✅ No semantic changes
- ✅ Clean git status (all committed)

---

# Summary of Changes from v1 Plan

1. **Added Phase 1.5** - HeapV2 integration validation
2. **Added Phase 2.5** - Instruction integration validation  
3. **Added Phase 3.5** - Register integration validation
4. **Earlier problem detection** - Test each component immediately
5. **Explicit rollback points** - Can revert any phase independently
6. **Lower risk** - No "big bang" integration

This incremental approach adds ~2 hours total but could save days of debugging.
