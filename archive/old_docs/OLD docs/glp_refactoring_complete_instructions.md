# GLP Runtime Refactoring Plan - Complete Instructions for Claude Code

## Executive Summary
Refactor GLP runtime to align with FCP design: GLP = FCP - general unification + SRSW compile-time check.

### Major Simplifications
1. **Single ID variables** - eliminate Writer ID ≠ Reader ID complexity
2. **Suspend on first reader** - eliminate Set<int> suspension tracking
3. **Array-based registers** - replace maps with arrays for O(1) access
4. **Unified instructions** - merge HeadWriter/HeadReader variants
5. **Direct variable access** - eliminate O(n) writerIdForReader lookups
6. **1-to-1 ROQ mapping** - one reader suspends at most one goal

### Expected Benefits
- **Performance**: O(1) variable access, faster dereferencing
- **Memory**: Eliminate Set overhead, smaller ROQ
- **Simplicity**: ~30% less code, clearer semantics
- **Maintainability**: Aligned with proven FCP design

## Simplification Summary

| Component | Before (Complex) | After (Simple) | Benefit |
|-----------|-----------------|----------------|---------|
| Variable IDs | Writer=1000, Reader=1001 | Single ID=1000 | No O(n) lookup |
| Suspension | Set<int> Si, U | Single int? | Less memory, simpler |
| ROQ | Map<int, Set<int>> | Map<int, int> | 1-to-1, no sets |
| Registers | Map<int, dynamic> | Array[256] | O(1) access |
| Instructions | HeadWriter, HeadReader | HeadVariable(isReader) | Half the opcodes |
| Dereferencing | No optimization | Path compression | Faster chains |
| Clause try | Accumulate suspensions | Stop on first | Simpler control |

## Critical Changes Required
1. **Single ID variable system** - eliminate Writer ID ≠ Reader ID
2. **Array-based registers** - replace maps with arrays for performance  
3. **Unified instructions** - merge HeadWriter/HeadReader into single ops
4. **Simplified tentative execution** - keep atomicity, remove unnecessary complexity
5. **Direct variable access** - eliminate O(n) writerIdForReader lookups

---

# PHASE 0: COMPREHENSIVE BASELINE ANALYSIS (Execute First)

## Overview
Before any refactoring, establish a complete understanding of current system behavior, performance characteristics, and test coverage.

## Step 0.1: Test Coverage Analysis
```bash
cd /Users/udi/GLP/glp_runtime

# Create baseline directory structure
mkdir -p refactoring/baseline/{tests,performance,behavior,coverage}
mkdir -p refactoring/reports
mkdir -p refactoring/tools

# Install coverage tools if needed
dart pub global activate coverage

# Run tests with coverage
dart run coverage:test_with_coverage --branch-coverage --function-coverage

# Generate coverage report
genhtml coverage/lcov.info -o refactoring/baseline/coverage/html
open refactoring/baseline/coverage/html/index.html

# Save coverage summary
echo "=== BASELINE COVERAGE REPORT ===" > refactoring/baseline/coverage/summary.txt
echo "Date: $(date)" >> refactoring/baseline/coverage/summary.txt
grep -A 3 "Lines" coverage/lcov.info >> refactoring/baseline/coverage/summary.txt
```

## Step 0.2: Create Comprehensive Test Suite
Create file: `refactoring/baseline/tests/comprehensive_baseline.dart`

```dart
import 'package:test/test.dart';
import 'package:glp_runtime/glp_runtime.dart';
import 'dart:io';
import 'dart:convert';

// Test categories to establish baseline behavior
void main() {
  final results = <String, dynamic>{};
  
  group('1. Variable Representation Tests', () {
    test('ID allocation patterns', () {
      final heap = Heap();
      final patterns = <Map<String, int>>[];
      
      for (int i = 0; i < 100; i++) {
        final (wid, rid) = heap.allocateFreshPair();
        patterns.add({
          'writer': wid,
          'reader': rid,
          'difference': rid - wid,
        });
      }
      
      results['idPatterns'] = {
        'samples': patterns.take(10).toList(),
        'consistent': patterns.every((p) => p['difference'] == 1),
        'writerAlwaysEven': patterns.every((p) => p['writer']! % 2 == 0),
      };
    });
    
    test('Writer-Reader lookup performance', () {
      final timings = <int, double>{};
      
      for (final size in [10, 100, 1000, 10000]) {
        final heap = Heap();
        for (int i = 0; i < size; i++) {
          final (w, r) = heap.allocateFreshPair();
          heap.addWriter(WriterCell(w, r));
          heap.addReader(ReaderCell(r));
        }
        
        final sw = Stopwatch()..start();
        for (int i = 0; i < 1000; i++) {
          heap.writerIdForReader(size * 2 - 1);
        }
        sw.stop();
        timings[size] = sw.elapsedMicroseconds / 1000;
      }
      
      results['lookupPerformance'] = timings;
    });
  });
  
  group('2. Suspension Behavior Tests', () {
    test('Suspension set accumulation', () {
      // Test if multiple readers are accumulated or just first
      final rt = GlpRuntime();
      final goalId = 100;
      
      // Create goal with multiple unbound readers
      // Track how many suspensions are recorded
      
      results['suspensionBehavior'] = {
        'accumulatesMultiple': false, // or true based on test
        'suspendsOnFirst': true, // or false
      };
    });
    
    test('ROQ structure analysis', () {
      // Analyze if ROQ is 1-to-1 or 1-to-many
      // Check Set<int> vs int storage
      
      results['roqStructure'] = {
        'usesSet': true, // Current implementation
        'multipleGoalsPerReader': false,
      };
    });
  });
  
  group('3. Instruction Usage Analysis', () {
    test('Count instruction variants', () {
      // Scan bytecode for usage of each instruction type
      final instructionCounts = <String, int>{};
      
      // Count HeadWriter vs HeadReader
      // Count UnifyWriter vs UnifyReader
      // Count PutWriter vs PutReader
      
      results['instructionUsage'] = instructionCounts;
    });
  });
  
  group('4. Performance Benchmarks', () {
    test('Merge benchmark', () {
      final sizes = [10, 100, 1000];
      final timings = <int, double>{};
      
      for (final size in sizes) {
        final list1 = List.generate(size, (i) => i);
        final list2 = List.generate(size, (i) => 'a$i');
        
        final sw = Stopwatch()..start();
        // Run merge(list1, list2, Result)
        sw.stop();
        
        timings[size] = sw.elapsedMilliseconds.toDouble();
      }
      
      results['mergeBenchmark'] = timings;
    });
    
    test('Metainterpreter benchmark', () {
      // Test with increasing clause counts
      final timings = <int, double>{};
      
      for (final clauseCount in [10, 50, 100]) {
        // Generate N clause/2 facts
        // Run metainterpreter
        // Measure time and goal count
      }
      
      results['metainterpreterBenchmark'] = timings;
    });
  });
  
  group('5. Memory Usage Analysis', () {
    test('Memory per goal', () {
      // Measure memory used by different components
      
      results['memoryUsage'] = {
        'perGoal': {
          'suspensionSet': 'XX bytes (Set<int>)',
          'clauseVars': 'XX bytes (Map)',
          'argWriters': 'XX bytes (Map)',
          'argReaders': 'XX bytes (Map)',
        },
        'perVariable': {
          'writerCell': 'XX bytes',
          'readerCell': 'XX bytes',
        },
      };
    });
  });
  
  group('6. Dereferencing Analysis', () {
    test('Dereferencing chains', () {
      // Test chains of different lengths
      // Measure dereferencing time
      // Check if path compression exists
      
      results['dereferencing'] = {
        'maxChainLength': 0,
        'hasPathCompression': false,
        'averageChainLength': 0.0,
      };
    });
  });
  
  // Save all results
  tearDownAll(() {
    final json = JsonEncoder.withIndent('  ').convert(results);
    File('refactoring/baseline/test_results.json').writeAsStringSync(json);
    print('Baseline results saved to refactoring/baseline/test_results.json');
  });
}
```

## Step 0.3: Behavior Documentation
Create file: `refactoring/baseline/behavior/document_current_behavior.dart`

```dart
import 'dart:io';

void main() {
  final doc = StringBuffer();
  
  doc.writeln('# Current GLP Implementation Behavior');
  doc.writeln('Generated: ${DateTime.now()}\n');
  
  // Document actual behavior through tests
  
  doc.writeln('## 1. Variable System');
  doc.writeln('- Dual ID system: Writer and Reader have different IDs');
  doc.writeln('- ID Pattern: Reader = Writer + 1');
  doc.writeln('- Lookup: O(n) search through all writers\n');
  
  doc.writeln('## 2. Suspension Mechanism');
  doc.writeln('- Uses Set<int> for suspension accumulation');
  doc.writeln('- Behavior: [Document through testing]');
  doc.writeln('- ROQ: Map<int, Set<int>> structure\n');
  
  doc.writeln('## 3. Register Architecture');
  doc.writeln('- clauseVars: Map<int, dynamic>');
  doc.writeln('- argWriters: Map<int, int>');
  doc.writeln('- argReaders: Map<int, int>');
  doc.writeln('- Access time: O(log n) for maps\n');
  
  doc.writeln('## 4. Instruction Set');
  doc.writeln('- Separate Writer/Reader variants');
  doc.writeln('- Total unique instructions: [Count]');
  doc.writeln('- Redundancy factor: 2x for most operations\n');
  
  doc.writeln('## 5. Performance Characteristics');
  doc.writeln('- Variable lookup: O(n)');
  doc.writeln('- Register access: O(log n)');
  doc.writeln('- Suspension check: O(n) for set operations');
  doc.writeln('- Dereferencing: No optimization\n');
  
  File('refactoring/baseline/behavior/current_behavior.md')
      .writeAsStringSync(doc.toString());
}
```

## Step 0.4: Create Test Stability Matrix
Create file: `refactoring/baseline/tests/stability_check.dart`

```dart
/// Run each test multiple times to check for non-determinism
import 'package:test/test.dart';

void main() {
  final stabilityResults = <String, List<bool>>[];
  
  test('Test stability check', () {
    final criticalTests = [
      'merge_test.dart',
      'metainterp_test.dart',
      'suspension_test.dart',
    ];
    
    for (final testFile in criticalTests) {
      final results = <bool>[];
      
      // Run each test 10 times
      for (int i = 0; i < 10; i++) {
        final process = Process.runSync('dart', ['test', testFile]);
        results.add(process.exitCode == 0);
      }
      
      stabilityResults.add({
        'test': testFile,
        'runs': results,
        'stable': results.every((r) => r == results.first),
      });
    }
    
    // Report any non-deterministic tests
    for (final result in stabilityResults) {
      if (!result['stable']) {
        print('WARNING: ${result['test']} is non-deterministic!');
      }
    }
  });
}
```

## Step 0.5: Performance Profiling
```bash
# Profile current implementation
cd /Users/udi/GLP/glp_runtime

# Create profiling script
cat > refactoring/tools/profile_baseline.dart << 'EOF'
import 'package:glp_runtime/glp_runtime.dart';
import 'dart:developer';

void main() {
  // Profile merge operation
  Timeline.startSync('merge_profile');
  
  // Run merge with large lists
  final list1 = List.generate(1000, (i) => i);
  final list2 = List.generate(1000, (i) => 'a$i');
  
  // Execute merge
  // ... 
  
  Timeline.finishSync();
  
  // Profile metainterpreter
  Timeline.startSync('meta_profile');
  
  // Run metainterpreter with many clauses
  // ...
  
  Timeline.finishSync();
}
EOF

# Run with profiling enabled
dart --observe --timeline-streams=all refactoring/tools/profile_baseline.dart

# Save timeline to file
# Access observatory at http://127.0.0.1:8181 and save timeline
```

## Step 0.6: Generate Comprehensive Baseline Report
Create file: `refactoring/tools/generate_baseline_report.dart`

```dart
import 'dart:io';
import 'dart:convert';

void main() {
  final report = StringBuffer();
  
  report.writeln('# COMPREHENSIVE BASELINE REPORT');
  report.writeln('Generated: ${DateTime.now()}\n');
  
  // 1. Test Coverage
  report.writeln('## 1. TEST COVERAGE');
  final coverage = File('refactoring/baseline/coverage/summary.txt')
      .readAsStringSync();
  report.writeln(coverage);
  report.writeln('');
  
  // 2. Test Results
  report.writeln('## 2. TEST RESULTS');
  final testResults = File('refactoring/baseline/test_results.json')
      .readAsStringSync();
  final results = jsonDecode(testResults);
  
  report.writeln('### Passing Tests: ${results['passing']}/170');
  report.writeln('### Failing Tests: ${results['failing']}');
  report.writeln('');
  
  // 3. Performance Baseline
  report.writeln('## 3. PERFORMANCE BASELINE');
  report.writeln('| Operation | Time | Complexity |');
  report.writeln('|-----------|------|------------|');
  report.writeln('| Variable lookup | ${results['lookupPerformance']['1000']}µs | O(n) |');
  report.writeln('| Register access | XXµs | O(log n) |');
  report.writeln('| Merge 1000 items | XXms | O(n) |');
  report.writeln('');
  
  // 4. Memory Usage
  report.writeln('## 4. MEMORY USAGE');
  report.writeln('| Component | Memory | Notes |');
  report.writeln('|-----------|--------|-------|');
  report.writeln('| Per Goal | ~XXX bytes | Sets + Maps |');
  report.writeln('| Per Variable | ~XX bytes | Dual cells |');
  report.writeln('| ROQ Entry | ~XX bytes | Set<int> |');
  report.writeln('');
  
  // 5. Behavioral Analysis
  report.writeln('## 5. BEHAVIORAL ANALYSIS');
  report.writeln('- Suspension: ${results['suspensionBehavior']}');
  report.writeln('- ID Pattern: ${results['idPatterns']['consistent']}');
  report.writeln('- Instruction redundancy: 2x (separate Writer/Reader)');
  report.writeln('');
  
  // 6. Identified Issues
  report.writeln('## 6. IDENTIFIED ISSUES');
  report.writeln('- [ ] O(n) variable lookup');
  report.writeln('- [ ] Unnecessary Set<int> for suspensions');
  report.writeln('- [ ] Map-based registers (should be arrays)');
  report.writeln('- [ ] Duplicate instruction variants');
  report.writeln('- [ ] No dereferencing optimization');
  report.writeln('');
  
  // 7. Risk Assessment
  report.writeln('## 7. REFACTORING RISKS');
  report.writeln('- Non-deterministic tests: [List any]');
  report.writeln('- Uncovered code: XX%');
  report.writeln('- Complex dependencies: [List]');
  report.writeln('');
  
  // Save report
  File('refactoring/reports/comprehensive_baseline.md')
      .writeAsStringSync(report.toString());
  
  print('Report saved to refactoring/reports/comprehensive_baseline.md');
}
```

Run: `dart refactoring/tools/generate_baseline_report.dart`

## CHECKPOINT 0: COMPREHENSIVE BASELINE COMPLETE

### What to Report to Claude Chat:
```
=== COMPREHENSIVE BASELINE COMPLETE ===

TEST COVERAGE:
- Line coverage: XX%
- Branch coverage: XX%  
- Uncovered areas: [List critical gaps]

TEST RESULTS:
- Total tests: 170
- Passing: XXX
- Failing: XX
- Non-deterministic: XX

PERFORMANCE BASELINE:
- Variable lookup: XXXµs at 1000 vars (O(n))
- Dereferencing: XXµs per chain
- Merge 1000 items: XXms
- Metainterpreter: XX goals created

MEMORY BASELINE:
- Per goal: XXX bytes
- Per variable pair: XX bytes
- Total for 1000 goals: XX MB

BEHAVIORAL FINDINGS:
- Suspension: [Accumulates sets / Stops on first]
- ID Pattern: Reader = Writer + 1 confirmed
- ROQ: 1-to-many with Set<int>
- Instructions: 2x redundancy confirmed

STABILITY:
- All critical tests deterministic: YES/NO
- Race conditions found: YES/NO

REFACTORING FEASIBILITY:
- Ready to proceed: YES/NO
- Blocking issues: [List any]

[Attach comprehensive_baseline.md]
```

## 🛑 STOP AND WAIT 🛑

### Generate Phase 0 Report
Create file: `refactoring/reports/phase0_report.md`

```bash
cat > refactoring/reports/phase0_report.md << 'EOF'
# Phase 0 Baseline Report
Date: $(date)

## Test Results
$(cat refactoring/baseline/summary.txt)

## Performance Baseline
$(cat refactoring/baseline/performance_baseline.json | head -20)

## Key Findings
- Current ID pattern: Writer N, Reader N+1
- Lookup complexity: O(n) 
- Tests passing: [INSERT COUNT]
- Tests failing: [INSERT COUNT]

## Issues Found
[List any unexpected findings]

## Ready for Phase 1: YES/NO
EOF
```

### What to Report to Claude Chat:
1. **Test summary** - exact pass/fail counts
2. **Performance metrics** - lookup times at different heap sizes
3. **ID allocation pattern** - confirm Writer+1=Reader
4. **Any unexpected issues** or deviations from expected behavior
5. **Confirmation ready** to proceed to Phase 1

### How to Report:
```
=== PHASE 0 COMPLETE ===
Test Results: XXX/170 passing
Performance: O(n) lookup confirmed, XXXµs at 1000 entries
ID Pattern: Writer+1=Reader confirmed
Issues: [none/list issues]
Ready for Phase 1: YES

[Attach refactoring/reports/phase0_report.md content]
```

## 🛑 STOP AND WAIT 🛑
**DO NOT PROCEED TO PHASE 1**

**REQUIRED ACTIONS:**
1. Post the Phase 0 report above to Claude Chat
2. Wait for explicit approval from Claude Chat
3. Claude Chat will respond with one of:
   - "Proceed to Phase 1" → Continue
   - "Fix [issue] first" → Address issue and re-report
   - "Abort refactoring" → Restore from baseline tag

**DO NOT:**
- Skip this checkpoint
- Assume approval
- Continue if any tests are failing
- Proceed if performance baseline shows unexpected patterns

**WAIT FOR:** Explicit message from Claude Chat saying "Proceed to Phase 1"

---

# CRITICAL: SUSPENSION SIMPLIFICATION FROM FCP

## Key Insight: Suspend on First Unbound Reader
FCP (and thus GLP) suspends on the **first** unbound reader encountered, not multiple:
- No need to accumulate suspension sets (Si)
- No need for union operation (U = ∪Si)  
- Each goal suspends on at most ONE reader
- "Delayed failure detection" not part of abstract semantics

## Design Simplifications Required

### 1. **Eliminate Set-Based Suspension**
```dart
// REMOVE: Overcomplicated set tracking
class RunnerContext {
  Set<int> si = {};  // DELETE THIS
  Set<int> U = {};   // DELETE THIS
}

// REPLACE WITH: Single suspension point
class RunnerContext {
  int? suspendOnReader;  // null = no suspension
}
```

### 2. **Simplify ROQ to 1-to-1 Mapping**
```dart
// OLD: One reader -> many goals
class ROQ {
  Map<int, Set<int>> _suspensions;  // reader -> Set<goalId>
}

// NEW: One reader -> one goal (first to suspend wins)
class ROQ {
  Map<int, int> _suspensions;  // reader -> goalId
  
  bool suspend(int readerId, int goalId) {
    if (_suspensions.containsKey(readerId)) {
      return false;  // Another goal already suspended here
    }
    _suspensions[readerId] = goalId;
    return true;
  }
  
  int? wake(int readerId) {
    return _suspensions.remove(readerId);  // Wake THE goal
  }
}
```

### 3. **Simplify HEAD Phase Execution**
```dart
// OLD: Continue after suspension to accumulate
if (isUnboundReader(term)) {
  si.add(readerId);
  continue;  // Keep going to find more
}

// NEW: Stop immediately on first unbound reader
if (isUnboundReader(term)) {
  suspendOnReader = readerId;
  return ExecutionResult.SUSPEND;  // STOP HERE
}
```

### 4. **Simplify Suspension Instructions**
```dart
// OLD: Complex suspend instruction
class Suspend {
  execute(RunnerContext cx) {
    if (cx.U.isNotEmpty) {  // Multiple readers
      for (int reader in cx.U) {
        cx.rt.roq.addSuspension(reader, cx.goalId);
      }
    }
  }
}

// NEW: Simple suspend
class Suspend {
  execute(RunnerContext cx) {
    if (cx.suspendOnReader != null) {
      cx.rt.roq.suspend(cx.suspendOnReader!, cx.goalId);
    }
  }
}
```

## Impact on Clause Try/Next

### OLD: Complex Multi-Clause Suspension
```
try clause1 -> suspend on {r1, r2, r3} -> add to Si
try clause2 -> suspend on {r4} -> add to Si  
try clause3 -> fail
suspend on U = {r1, r2, r3, r4}
```

### NEW: Simple First-Reader Suspension
```
try clause1 -> suspend on r1 -> STOP
(never try clause2 or clause3)
suspend on r1 only
```

This means:
- `clause_next` is only reached on FAIL, never on SUSPEND
- No Si accumulation across clauses
- Simpler control flow

## Impact on Guards

Guards also stop at first unbound reader:
```dart
// Guard: ground(X), integer(Y) |
// OLD: Check X, accumulate if unbound, check Y, accumulate...
// NEW: Check X, if unbound SUSPEND immediately (never check Y)
```

This aligns with FCP's operational semantics where suspension stops evaluation immediately.

## Impact on Clause Selection

With suspend-on-first:
```dart
// Goal tries clauses in order:
clause_try 1 -> HEAD fails -> clause_next 2
clause_try 2 -> HEAD has unbound reader -> SUSPEND immediately
// Never tries clause 3,4,5...
// When resumed, starts from clause 2 again
```

This is simpler than accumulating suspensions across all clauses.

## Testing This Simplification

Create file: `refactoring/tests/single_suspension_test.dart`

```dart
import 'package:test/test.dart';

void main() {
  test('Goal suspends on first reader only', () {
    final rt = Runtime();
    
    // Create goal with multiple unbound readers
    // p(X?, Y?, Z?) :- ...
    
    final result = execute(goal);
    
    // Should suspend on X? only, not Y? or Z?
    expect(rt.roq.getSuspensions().length, equals(1));
    expect(rt.roq.getSuspendedGoal(xReaderId), equals(goalId));
    expect(rt.roq.getSuspendedGoal(yReaderId), isNull);
    expect(rt.roq.getSuspendedGoal(zReaderId), isNull);
  });
  
  test('Second goal cannot suspend on same reader', () {
    final rt = Runtime();
    
    // Goal 1 suspends on reader R
    rt.roq.suspend(readerId, goal1);
    
    // Goal 2 tries to suspend on same reader
    final success = rt.roq.suspend(readerId, goal2);
    
    expect(success, isFalse);
    expect(rt.roq.getSuspendedGoal(readerId), equals(goal1));
  });
}
```

---

# CRITICAL: DEREFERENCING IN GLP

## Understanding Dereferencing Chains
GLP allows dereferencing chains through reader bindings:
- X = Y? (X bound to reader of Y)
- Y = Z? (Y bound to reader of Z)  
- Z = a (Z bound to constant)
- Dereferencing X requires: X → Y? → Z? → a

## FCP Dereferencing Optimizations to Implement

### 1. **Inline Dereferencing Cache**
```dart
// FCP optimization: Cache dereferenced values to avoid repeated lookups
class VariableCell {
  final int varId;
  Term? value;
  Term? dereferencedCache;  // Cache final dereferenced value
  int cacheVersion = 0;      // Invalidate on heap changes
}
```

### 2. **Dereferencing with Early Termination**
```dart
// Optimized dereferencing - stop at first unbound or ground term
Term dereference(Term term, HeapV2 heap) {
  Set<int> visited = {};  // Cycle detection (shouldn't happen with SRSW)
  
  while (term is VarRef) {
    if (!heap.isBound(term.varId)) {
      return term;  // Unbound - stop here
    }
    
    if (visited.contains(term.varId)) {
      throw StateError('Cycle detected - SRSW violation!');
    }
    visited.add(term.varId);
    
    term = heap.getValue(term.varId)!;
    
    // Early termination for ground terms
    if (term is ConstTerm || term is StructTerm) {
      break;
    }
  }
  return term;
}
```

### 3. **Path Compression (from FCP)**
```dart
// After dereferencing, update intermediate variables to point directly to result
Term dereferenceWithCompression(Term term, HeapV2 heap) {
  if (term is! VarRef) return term;
  
  List<int> path = [];
  Term current = term;
  
  // Follow chain, recording path
  while (current is VarRef && heap.isBound(current.varId)) {
    path.add(current.varId);
    current = heap.getValue(current.varId)!;
  }
  
  // Compress path - make all intermediate vars point to final value
  if (path.length > 1 && current is! VarRef) {
    for (int varId in path.sublist(0, path.length - 1)) {
      heap._vars[varId]!.dereferencedCache = current;
    }
  }
  
  return current;
}
```

### 4. **Suspension-Aware Dereferencing**
```dart
// Track which variables to suspend on during dereferencing
class DereferenceResult {
  final Term term;
  final Set<int> suspendOn;  // Unbound readers encountered
  
  DereferenceResult(this.term, this.suspendOn);
}

DereferenceResult dereferenceForSuspension(Term term, HeapV2 heap) {
  Set<int> suspendOn = {};
  
  while (term is VarRef) {
    if (!heap.isBound(term.varId)) {
      if (term.isReader) {
        suspendOn.add(term.varId);  // Add to suspension set
      }
      break;  // Can't dereference further
    }
    term = heap.getValue(term.varId)!;
  }
  
  return DereferenceResult(term, suspendOn);
}
```

## Testing Dereferencing

Create file: `refactoring/tests/dereferencing_test.dart`

```dart
import 'package:test/test.dart';

void main() {
  group('Dereferencing Optimizations', () {
    test('Simple chain', () {
      final heap = HeapV2();
      
      // Create chain: X = Y?, Y = Z?, Z = 'a'
      final x = heap.allocateFreshVar();
      final y = heap.allocateFreshVar();
      final z = heap.allocateFreshVar();
      
      heap.bindVariable(x, VarRef(y, true));  // X = Y?
      heap.bindVariable(y, VarRef(z, true));  // Y = Z?
      heap.bindVariable(z, ConstTerm('a'));    // Z = 'a'
      
      // Dereference X should give 'a'
      final result = heap.dereference(VarRef(x, false));
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('a'));
    });
    
    test('Path compression', () {
      final heap = HeapV2();
      
      // Create long chain
      final vars = List.generate(10, (_) => heap.allocateFreshVar());
      
      // Link them
      for (int i = 0; i < 9; i++) {
        heap.bindVariable(vars[i], VarRef(vars[i + 1], true));
      }
      heap.bindVariable(vars[9], ConstTerm('end'));
      
      // First dereference should follow full chain
      final sw1 = Stopwatch()..start();
      heap.dereferenceWithCompression(VarRef(vars[0], false));
      sw1.stop();
      
      // Second dereference should be faster (cached)
      final sw2 = Stopwatch()..start();
      heap.dereferenceWithCompression(VarRef(vars[0], false));
      sw2.stop();
      
      expect(sw2.elapsedMicroseconds, lessThan(sw1.elapsedMicroseconds));
    });
    
    test('Suspension detection', () {
      final heap = HeapV2();
      
      // Chain with unbound reader: X = Y?, Y unbound
      final x = heap.allocateFreshVar();
      final y = heap.allocateFreshVar();
      
      heap.bindVariable(x, VarRef(y, true));  // X = Y?
      // Y remains unbound
      
      final result = heap.dereferenceForSuspension(VarRef(x, false));
      expect(result.suspendOn, contains(y));
      expect(result.term, isA<VarRef>());
    });
  });
}
```

---

# PHASE 1: SINGLE ID VARIABLE SYSTEM

## Step 1.1: Create Parallel Data Structures
Create file: `lib/runtime/heap_v2.dart`

```dart
/// New heap with single ID variable system
/// Run in parallel with existing heap for testing
class HeapV2 {
  // Single ID for each variable
  final Map<int, VariableCell> _vars = {};
  int _nextId = 1000;
  
  // ROQ remains similar but indexed by (varId, isReader)
  final Map<int, Set<int>> _roq = {};
  
  int allocateFreshVar() {
    return _nextId++;
  }
  
  void addVariable(int varId) {
    _vars[varId] = VariableCell(varId);
  }
  
  Term? getValue(int varId) {
    return _vars[varId]?.value;
  }
  
  void bindVariable(int varId, Term value) {
    final cell = _vars[varId];
    if (cell != null && cell.value == null) {
      cell.value = value;
      _processROQ(varId);
    }
  }
  
  void _processROQ(int varId) {
    final suspended = _roq[varId];
    if (suspended != null) {
      // Wake suspended goals
      for (final goalId in suspended) {
        // Reactivate goal
      }
      _roq.remove(varId);
    }
  }
  
  bool isBound(int varId) {
    return _vars[varId]?.value != null;
  }
}

/// Single variable cell (replaces WriterCell/ReaderCell)
class VariableCell {
  final int varId;
  Term? value;
  
  VariableCell(this.varId);
}

/// New term representation with single ID
class VarRef implements Term {
  final int varId;
  final bool isReader;
  
  VarRef(this.varId, this.isReader);
  
  @override
  String toString() => isReader ? 'R$varId' : 'W$varId';
  
  Term? dereference(HeapV2 heap) {
    return heap.getValue(varId) ?? this;
  }
}
```

## Step 1.2: Create Compatibility Test
Create file: `refactoring/tests/heap_compatibility_test.dart`

```dart
import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/heap_v2.dart';

void main() {
  group('Heap V1 vs V2 Compatibility', () {
    test('Same logical behavior', () {
      // Test with old heap
      final heap1 = Heap();
      final (w1, r1) = heap1.allocateFreshPair();
      heap1.addWriter(WriterCell(w1, r1));
      heap1.addReader(ReaderCell(r1));
      
      // Test with new heap
      final heap2 = HeapV2();
      final v2 = heap2.allocateFreshVar();
      heap2.addVariable(v2);
      
      // Both should behave identically for binding
      heap1.bindWriter(w1, ConstTerm('test'));
      heap2.bindVariable(v2, ConstTerm('test'));
      
      expect(heap1.isWriterBound(w1), equals(heap2.isBound(v2)));
    });
  });
}
```

Run: `dart test refactoring/tests/heap_compatibility_test.dart`

## Step 1.4: Run Comprehensive Tests with New Heap
```bash
# Test both heaps side-by-side
dart test --name "heap" 2>&1 | tee refactoring/reports/phase1_heap_tests.txt

# Test critical scenarios
dart run bin/glp_repl.dart << 'EOF' > refactoring/reports/phase1_merge_test.txt 2>&1
merge.glp
merge([1,2],[a,b],X).
:quit
EOF
```

## CHECKPOINT 1: Report to Claude Chat

### Generate Phase 1 Report
Create file: `refactoring/reports/phase1_report.md`

```bash
cat > refactoring/reports/phase1_report.md << 'EOF'
# Phase 1 Single ID System + Simplifications Report
Date: $(date)

## Implementation Status
- [x] HeapV2 created with single ID system
- [x] VarRef replaces WriterTerm/ReaderTerm
- [x] Direct lookup replaces O(n) search
- [x] Dereferencing optimizations implemented
- [x] Suspension simplified to single reader
- [ ] All term creation sites updated

## Suspension Simplification Results
- Set<int> Si removed: YES/NO
- Set<int> U removed: YES/NO  
- Single suspendOnReader implemented: YES/NO
- ROQ simplified to 1-to-1: YES/NO
- Goals suspend on first reader only: YES/NO

## Test Results
- Compatibility test: PASS/FAIL
- Heap tests: XX/XX passing
- Dereferencing tests: XX/XX passing
- Single suspension tests: XX/XX passing
- Merge test: PASS/FAIL

## Performance Comparison
Old heap lookup: XXXµs
New heap lookup: XXµs  
Improvement: XX%

Dereferencing chain (10 vars):
- First pass: XXµs
- With compression: XXµs
- Improvement: XX%

Suspension overhead:
- Old (set operations): XXµs
- New (single int): XXµs
- Improvement: XX%

## Memory Impact
- Removed Set<int> per goal: -XX bytes
- Simplified ROQ: -XX% memory

## Code Simplification
- Lines removed: XXX
- Complexity reduction: HIGH/MEDIUM/LOW

## Migration Sites Identified
- WriterTerm(): XX occurrences
- ReaderTerm(): XX occurrences  
- allocateFreshPair(): XX occurrences
- Set<int> si: XX occurrences
- Set<int> U: XX occurrences

## Issues/Blockers
[List any problems encountered]

## Risk Assessment
- [ ] Low risk - isolated changes
- [ ] Medium risk - affects core functionality
- [ ] High risk - widespread impact

## Ready for Phase 2: YES/NO
EOF
```

### What to Report to Claude Chat:
1. **Test results** - both heaps passing same tests?
2. **Performance improvement** - lookup time reduction
3. **Suspension simplification** - working correctly?
4. **Dereferencing performance** - with/without optimization
5. **Memory savings** - from removing sets
6. **Code metrics** - how many files/lines need changing
7. **Issues encountered** - any unexpected complications
8. **Risk assessment** - can we proceed safely?

### Critical Verification Points:
- **Single suspension working** - goals suspend on first reader only
- **No semantic changes** - same programs produce same results
- **Performance improved** - not degraded
- **Memory reduced** - from eliminating sets

### Decision Points for Claude Chat:
- If suspension simplification breaks programs: Investigate or revert?
- If some code still uses sets: Clean up now or Phase 5?
- If performance worse: Profile and fix or accept?
- If too many change sites: Incremental or all at once?

## 🛑 STOP AND WAIT 🛑
**DO NOT PROCEED TO PHASE 2**

**REQUIRED ACTIONS:**
1. Post the Phase 1 report above to Claude Chat
2. Wait for explicit response
3. Claude Chat will respond with one of:
   - "Proceed to Phase 2" → Continue
   - "Fix [issue] first" → Address and re-report
   - "Investigate [concern]" → Provide more details
   - "Rollback Phase 1" → Git revert to baseline

**CRITICAL CHECKS:**
- Dereferencing MUST work correctly
- Performance MUST be better (not worse)
- No semantic changes (same programs produce same results)

**WAIT FOR:** Explicit "Proceed to Phase 2" from Claude Chat

---

# PHASE 2: INSTRUCTION UNIFICATION

```dart
/// Tool to identify all term creation sites that need updating
import 'dart:io';

void main() {
  print('Scanning for term creation sites...\n');
  
  final patterns = [
    'WriterTerm\\(',
    'ReaderTerm\\(',
    'WriterCell\\(',
    'ReaderCell\\(',
    'allocateFreshPair\\(',
  ];
  
  final results = <String, List<String>>{};
  
  for (final pattern in patterns) {
    final process = Process.runSync('grep', [
      '-r',
      pattern,
      'lib/',
      '--include=*.dart',
      '-n'
    ]);
    
    if (process.exitCode == 0) {
      results[pattern] = process.stdout.toString().split('\n')
          .where((line) => line.isNotEmpty)
          .toList();
    }
  }
  
  // Generate report
  final output = StringBuffer();
  output.writeln('# Term Creation Sites to Update\n');
  
  for (final entry in results.entries) {
    output.writeln('## Pattern: ${entry.key}');
    output.writeln('Found ${entry.value.length} occurrences:');
    for (final loc in entry.value.take(5)) {
      output.writeln('  - $loc');
    }
    if (entry.value.length > 5) {
      output.writeln('  ... and ${entry.value.length - 5} more');
    }
    output.writeln('');
  }
  
  File('refactoring/docs/term_creation_sites.md').writeAsStringSync(output.toString());
  print('Report saved to refactoring/docs/term_creation_sites.md');
}
```

Run: `dart refactoring/tools/migrate_terms.dart`

---

# PHASE 2: INSTRUCTION UNIFICATION

## Step 2.1: Create Unified Instruction Set
Create file: `lib/bytecode/opcodes_v2.dart`

```dart
/// Unified instructions with mode flag instead of separate Writer/Reader variants
abstract class OpV2 {
  String get mnemonic;
}

/// Replaces HeadWriter/HeadReader
class HeadVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  
  HeadVariable(this.varIndex, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'head_reader' : 'head_writer';
}

/// Replaces UnifyWriter/UnifyReader  
class UnifyVariable implements OpV2 {
  final int varIndex;
  final bool isReader;
  
  UnifyVariable(this.varIndex, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'unify_reader' : 'unify_writer';
}

/// Replaces PutWriter/PutReader
class PutVariable implements OpV2 {
  final int varIndex;
  final int argSlot;
  final bool isReader;
  
  PutVariable(this.varIndex, this.argSlot, {required this.isReader});
  
  @override
  String get mnemonic => isReader ? 'put_reader' : 'put_writer';
}
```

## Step 2.2: Create Instruction Migration Map
Create file: `refactoring/tools/instruction_map.dart`

```dart
/// Maps old instructions to new unified ones
void main() {
  final migrationMap = {
    'HeadWriter': 'HeadVariable(isReader: false)',
    'HeadReader': 'HeadVariable(isReader: true)',
    'UnifyWriter': 'UnifyVariable(isReader: false)',
    'UnifyReader': 'UnifyVariable(isReader: true)',
    'PutWriter': 'PutVariable(isReader: false)',
    'PutReader': 'PutVariable(isReader: true)',
  };
  
  print('Instruction Migration Map:');
  for (final entry in migrationMap.entries) {
    print('  ${entry.key} -> ${entry.value}');
  }
}
```

## CHECKPOINT 2: Report to Claude Chat

### Generate Report:
```bash
echo "=== PHASE 2 INSTRUCTION UNIFICATION ===" > refactoring/reports/phase2_report.md
echo "Unified instructions created: YES/NO" >> refactoring/reports/phase2_report.md
echo "Migration map complete: YES/NO" >> refactoring/reports/phase2_report.md
echo "Ready for Phase 3: YES/NO" >> refactoring/reports/phase2_report.md
```

## 🛑 STOP - WAIT FOR APPROVAL 🛑
Post report and wait for "Proceed to Phase 3"

---

# PHASE 3: ARRAY-BASED REGISTERS

## Step 3.1: Create New Runner Context
Create file: `lib/bytecode/runner_v2.dart`

```dart
/// Simplified runner with array-based registers
class RunnerContextV2 {
  final GlpRuntime rt;
  final int goalId;
  
  // Array-based registers (like FCP)
  final List<Term?> X = List.filled(256, null);  // Temporary registers
  final List<Term?> A = List.filled(16, null);   // Argument registers
  
  // Simplified state
  int pc = 0;
  UnifyMode mode = UnifyMode.read;
  
  // Tentative bindings (still needed for atomicity)
  final Map<int, Term> sigmaHat = {};
  final Set<int> si = {};
  
  RunnerContextV2({required this.rt, required this.goalId});
  
  void clearClause() {
    X.fillRange(0, X.length, null);
    sigmaHat.clear();
    si.clear();
  }
}
```

## CHECKPOINT 3: Report to Claude Chat

### Generate Report:
```bash
cat > refactoring/reports/phase3_report.md << 'EOF'
# Phase 3 Array Registers Report
Date: $(date)

## Changes Made
- Map-based clauseVars → Array X[256]
- Separate argWriters/argReaders → Single A[16]
- Performance: Map lookup O(log n) → Array O(1)

## Test Results
- Register tests: XX/XX passing
- Full suite: XXX/170 passing

## Issues
[List any]

Ready for Phase 4: YES/NO
EOF
```

## 🛑 STOP - WAIT FOR APPROVAL 🛑
Post report and wait for "Proceed to Phase 4"

---

# PHASE 4: INTEGRATION TESTING

## Step 4.1: Create Side-by-Side Test Runner
Create file: `refactoring/tests/dual_execution_test.dart`

```dart
import 'package:test/test.dart';

/// Runs same program on old and new implementation
class DualExecutor {
  static void compareExecution(String glpSource, String query) {
    // Run on old implementation
    final oldResult = runOldImplementation(glpSource, query);
    
    // Run on new implementation  
    final newResult = runNewImplementation(glpSource, query);
    
    // Compare results
    expect(newResult.success, equals(oldResult.success));
    expect(newResult.bindings, equals(oldResult.bindings));
    
    // Compare performance
    print('Old lookup time: ${oldResult.lookupTime}');
    print('New lookup time: ${newResult.lookupTime}');
    expect(newResult.lookupTime, lessThan(oldResult.lookupTime));
  }
}

void main() {
  test('Merge works identically', () {
    DualExecutor.compareExecution(
      'merge([],[],[])...',
      'merge([1,2],[a,b],X)'
    );
  });
}
```

## CHECKPOINT 4: Report to Claude Chat

### Generate Report:
```bash
cat > refactoring/reports/phase4_report.md << 'EOF'
# Phase 4 Integration Testing Report
Date: $(date)

## Dual Execution Results
Test Case         | Old Result | New Result | Match?
------------------|------------|------------|--------
merge([1,2]...)   | [result]   | [result]   | YES/NO
metainterpreter   | [result]   | [result]   | YES/NO
suspension        | [result]   | [result]   | YES/NO

## Performance Comparison
Operation         | Old Time   | New Time   | Improvement
------------------|------------|------------|-------------
Variable lookup   | XXXµs      | XXµs       | XX%
Dereferencing     | XXXµs      | XXµs       | XX%
Full merge        | XXXms      | XXms       | XX%

## Semantic Equivalence
- All goals produce same results: YES/NO
- Same suspension behavior: YES/NO
- Same failure modes: YES/NO

Ready for Phase 5 (Migration): YES/NO
EOF
```

## 🛑 CRITICAL DECISION POINT 🛑

**This is the point of no return**. Phase 5 will switch to the new implementation.

**REQUIRED FROM CLAUDE CHAT:**
- Explicit approval to proceed
- Or rollback instructions
- Or additional testing requirements

**WAIT FOR:** "Proceed to Phase 5 - switch to new implementation"

---

# PHASE 5: MIGRATION COMPLETION

## Step 5.1: Switch to New Implementation
```bash
cd /Users/udi/GLP/glp_runtime

# Create feature flag
echo "const bool USE_NEW_HEAP = true;" > lib/config.dart

# Update imports
find lib -name "*.dart" -exec sed -i '' 's/heap.dart/heap_v2.dart/g' {} \;

# Run all tests
dart test
```

## Step 5.2: Remove Old Implementation
```bash
# After all tests pass
git rm lib/runtime/heap.dart
git rm lib/runtime/cells.dart
git mv lib/runtime/heap_v2.dart lib/runtime/heap.dart
```

## CHECKPOINT 5: FINAL REPORT

### Generate Final Report:
```bash
cat > refactoring/reports/final_report.md << 'EOF'
# GLP Refactoring Complete - Final Report
Date: $(date)

## Overall Results
- Starting tests passing: XXX/170
- Final tests passing: XXX/170
- Performance improvement: XX% overall

## Key Achievements
- [x] Single ID variable system
- [x] O(1) variable lookup (was O(n))
- [x] Suspend on first reader (removed sets)
- [x] 1-to-1 ROQ mapping (was 1-to-many)
- [x] Unified instruction set
- [x] Array-based registers
- [x] FCP dereferencing optimizations

## Performance Metrics
| Operation | Before | After | Improvement |
|-----------|--------|-------|-------------|
| Variable lookup | XXXµs | XXµs | XX% |
| Dereferencing | XXXµs | XXµs | XX% |
| Register access | XXns | XXns | XX% |
| Full test suite | XXs | XXs | XX% |

## Code Impact
- Files changed: XX
- Lines added: XXXX
- Lines removed: XXXX
- Net reduction: XXXX lines

## Semantic Verification
- All original tests pass: YES/NO
- No behavioral changes: YES/NO
- SRSW constraints maintained: YES/NO

## Recommendations
[Any follow-up work needed]
EOF
```

### Create PR:
```bash
git add -A
git commit -m "refactor: Align GLP with FCP design - single ID variables, unified instructions, array registers"
git push origin refactor/glp-fcp-alignment
```

## 🎉 REFACTORING COMPLETE 🎉

**Report to Claude Chat with:**
1. Final report contents
2. Link to PR/branch
3. Any outstanding issues
4. Recommendations for future work

---

# CHECKPOINT SUMMARY

## Required Checkpoints (DO NOT SKIP)

| Phase | Checkpoint | Wait For | Before Proceeding |
|-------|------------|----------|-------------------|
| 0 | Baseline captured | "Proceed to Phase 1" | Phase 1 |
| 1 | Single ID system tested | "Proceed to Phase 2" | Phase 2 |
| 2 | Instructions unified | "Proceed to Phase 3" | Phase 3 |
| 3 | Array registers done | "Proceed to Phase 4" | Phase 4 |
| 4 | Integration tested | "Proceed to Phase 5" | Phase 5 |
| 5 | Migration complete | "Approved" or fixes | Merge |

## What Claude Chat Needs at Each Checkpoint

### Phase 0
- Exact test counts (passing/failing)
- Performance baseline numbers
- Confirmation of ID pattern (Writer+1=Reader)

### Phase 1  
- Both heaps passing same tests?
- Performance improvement metrics
- Dereferencing working correctly?

### Phase 2
- Instruction unification complete?
- Migration mapping documented?

### Phase 3
- Array performance better than maps?
- No functionality broken?

### Phase 4
- Old vs new produce identical results?
- Performance improvements confirmed?
- Ready for switchover?

### Phase 5
- All tests passing?
- Performance goals met?
- Ready for merge?

## Emergency Procedures

### If Tests Fail
```bash
git stash
git checkout baseline-before-refactor
# Report issue to Claude Chat
```

### If Performance Degrades
```bash
# Profile specific operation
dart compile exe refactoring/tests/performance_baseline.dart
./performance_baseline.exe
# Report metrics to Claude Chat
```

### If Semantic Changes Detected
```bash
# STOP IMMEDIATELY
git diff HEAD~1 > changes.patch
# Send patch to Claude Chat for review
```

---

# END OF INSTRUCTIONS

**START WITH PHASE 0**
**STOP AT EACH CHECKPOINT**  
**WAIT FOR EXPLICIT APPROVAL**
**REPORT ALL ISSUES IMMEDIATELY**