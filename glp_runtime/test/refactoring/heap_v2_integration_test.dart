/// Phase 1.5: HeapV2 Integration Validation
///
/// This test validates that HeapV2 works correctly with the actual
/// bytecode runner by running real GLP programs and comparing results
/// with the existing Heap implementation.
///
/// CRITICAL: This does NOT modify production code - it only tests
/// that HeapV2 can be integrated successfully.

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/heap_v2_adapter.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  group('Phase 1.5: HeapV2 Integration Tests', () {

    test('Test 1: Simple unification - p(a). q(X?) :- p(X).', () {
      print('\n=== INTEGRATION TEST 1: Simple Unification ===');
      print('Program: p(a). q(X?) :- p(X).');
      print('Expected: X binds to a, both succeed\n');

      // Run with HeapV1 (current implementation)
      print('--- Running with HeapV1 (current) ---');
      final resultV1 = _runSimpleUnification(useV2: false);
      print('V1 Result: X = ${resultV1['X']}, success = ${resultV1['success']}\n');

      // Run with HeapV2 (new implementation)
      print('--- Running with HeapV2 (new) ---');
      final resultV2 = _runSimpleUnification(useV2: true);
      print('V2 Result: X = ${resultV2['X']}, success = ${resultV2['success']}\n');

      // Compare results
      expect(resultV2['success'], equals(resultV1['success']),
        reason: 'Both heaps should have same success status');
      expect(resultV2['X'], equals(resultV1['X']),
        reason: 'Both heaps should bind X to same value');
      expect(resultV2['executions'], equals(resultV1['executions']),
        reason: 'Both heaps should execute same number of goals');

      print('✓ PASS: HeapV2 behaves identically to HeapV1\n');
    });

    test('Test 2: Suspension test - r(X) :- s(X?). s(b).', () {
      print('\n=== INTEGRATION TEST 2: Suspension/Reactivation ===');
      print('Program: r(X) :- s(X?). s(b).');
      print('Expected: r suspends on X?, s binds X=b, r reactivates\n');

      // Run with HeapV1
      print('--- Running with HeapV1 (current) ---');
      final resultV1 = _runSuspensionTest(useV2: false);
      print('V1 Result: X = ${resultV1['X']}, suspended = ${resultV1['suspended']}\n');

      // Run with HeapV2
      print('--- Running with HeapV2 (new) ---');
      final resultV2 = _runSuspensionTest(useV2: true);
      print('V2 Result: X = ${resultV2['X']}, suspended = ${resultV2['suspended']}\n');

      // Compare results
      expect(resultV2['success'], equals(resultV1['success']),
        reason: 'Both heaps should have same success status');
      expect(resultV2['X'], equals(resultV1['X']),
        reason: 'Both heaps should bind X to same value');
      expect(resultV2['suspended'], equals(resultV1['suspended']),
        reason: 'Both heaps should suspend same way');
      expect(resultV2['reactivated'], equals(resultV1['reactivated']),
        reason: 'Both heaps should reactivate same goals');

      print('✓ PASS: HeapV2 suspension/reactivation identical to HeapV1\n');
    });

    test('Test 3: Failure test - p(a). q(b). Goal: q(X?), p(X) fails', () {
      print('\n=== INTEGRATION TEST 3: Failure Case ===');
      print('Program: p(a). q(b).');
      print('Goal: q(X?), p(X)');
      print('Expected: X binds to a, q(a) fails (expects b)\n');

      // Run with HeapV1
      print('--- Running with HeapV1 (current) ---');
      final resultV1 = _runFailureTest(useV2: false);
      print('V1 Result: X = ${resultV1['X']}, q_failed = ${resultV1['q_failed']}\n');

      // Run with HeapV2
      print('--- Running with HeapV2 (new) ---');
      final resultV2 = _runFailureTest(useV2: true);
      print('V2 Result: X = ${resultV2['X']}, q_failed = ${resultV2['q_failed']}\n');

      // Compare results
      expect(resultV2['X'], equals(resultV1['X']),
        reason: 'Both heaps should bind X to same value');
      expect(resultV2['q_failed'], equals(resultV1['q_failed']),
        reason: 'Both heaps should fail q in same way');
      expect(resultV2['executions'], equals(resultV1['executions']),
        reason: 'Both heaps should execute same number of goals');

      print('✓ PASS: HeapV2 failure behavior identical to HeapV1\n');
    });
  });
}

// ============================================================================
// Test Program 1: Simple Unification
// ============================================================================

Map<String, dynamic> _runSimpleUnification({required bool useV2}) {
  // Create runtime with chosen heap implementation
  final rt = useV2
    ? GlpRuntime(heap: HeapV2Adapter())
    : GlpRuntime();

  // Create variable X/X? - same API for both heaps
  const wX = 1;
  const rX = 2;
  rt.heap.addWriter(WriterCell(wX, rX));
  rt.heap.addReader(ReaderCell(rX));

  // Program p: p(a).
  final progP = BC.prog([
    BC.L('p/1'),
    BC.TRY(),
    BC.headConst('a', 0),
    BC.COMMIT(),
    BC.PROCEED(),
    BC.L('p_end'),
    BC.SUSP(),
  ]);

  // Program q: q(X?) - trivial clause that succeeds
  final progQ = BC.prog([
    BC.L('q/1'),
    BC.TRY(),
    BC.COMMIT(),  // Accept any argument
    BC.PROCEED(),
    BC.L('q_end'),
    BC.SUSP(),
  ]);

  final runnerP = BytecodeRunner(progP);
  final runnerQ = BytecodeRunner(progQ);
  final sched = Scheduler(rt: rt, runners: {
    'p': runnerP,
    'q': runnerQ,
  });

  // Goal 1: q(X?) - should succeed immediately (trivial clause)
  const goalQ = 100;
  rt.setGoalEnv(goalQ, CallEnv(readers: {0: rX}));
  rt.setGoalProgram(goalQ, 'q');
  rt.gq.enqueue(GoalRef(goalQ, 0));
  sched.drain(maxCycles: 10);

  // Goal 2: p(X) - should bind X to 'a'
  const goalP = 200;
  rt.setGoalEnv(goalP, CallEnv(writers: {0: wX}));
  rt.setGoalProgram(goalP, 'p');
  rt.gq.enqueue(GoalRef(goalP, 0));
  final executions = sched.drain(maxCycles: 10);

  final bound = rt.heap.isWriterBound(wX);
  final value = bound ? rt.heap.valueOfWriter(wX) : null;
  final xValue = value != null && value is ConstTerm ? value.value : null;

  return {
    'success': bound,
    'X': xValue,
    'executions': executions.length,
  };
}

// ============================================================================
// Test Program 2: Suspension/Reactivation
// ============================================================================

Map<String, dynamic> _runSuspensionTest({required bool useV2}) {
  // Create runtime with chosen heap implementation
  final rt = useV2
    ? GlpRuntime(heap: HeapV2Adapter())
    : GlpRuntime();

  // Create variable X/X?
  const wX = 1;
  const rX = 2;
  rt.heap.addWriter(WriterCell(wX, rX));
  rt.heap.addReader(ReaderCell(rX));

  // Program s: s(b).
  final progS = BC.prog([
    BC.L('s/1'),
    BC.TRY(),
    BC.headConst('b', 0),
    BC.COMMIT(),
    BC.PROCEED(),
    BC.L('s_end'),
    BC.SUSP(),
  ]);

  // Program r: r(X) :- s(X?).
  //   r(X) has one clause that spawns s(X?)
  final progR = BC.prog([
    BC.L('r/1'),
    BC.TRY(),
    BC.getVar(wX, 0),  // Get writer X from arg0
    BC.COMMIT(),
    // Body: spawn s(X?)
    BC.putReader(rX, 0),  // arg0 = X?
    BC.spawn('s', 1),
    BC.PROCEED(),
    BC.L('r_end'),
    BC.SUSP(),
  ]);

  final runnerS = BytecodeRunner(progS);
  final runnerR = BytecodeRunner(progR);
  final sched = Scheduler(rt: rt, runners: {
    's': runnerS,
    'r': runnerR,
  });

  // Goal: r(X) where X is unbound
  //   This will spawn s(X?) which suspends on unbound reader
  const goalR = 100;
  rt.setGoalEnv(goalR, CallEnv(writers: {0: wX}));
  rt.setGoalProgram(goalR, 'r');
  rt.gq.enqueue(GoalRef(goalR, 0));

  var executions = sched.drain(maxCycles: 10);
  final suspended = rt.roq.queue(rX).length > 0;

  // At this point: r has spawned s(X?), which suspended on unbound X?
  // Now bind X manually and see if s reactivates
  if (!rt.heap.isWriterBound(wX)) {
    rt.heap.bindWriterConst(wX, 'b');
    final acts = rt.commitWriters([wX]);
    executions = sched.drain(maxCycles: 10);
  }

  final bound = rt.heap.isWriterBound(wX);
  final value = bound ? rt.heap.valueOfWriter(wX) : null;
  final xValue = value != null && value is ConstTerm ? value.value : null;

  return {
    'success': bound,
    'X': xValue,
    'suspended': suspended,
    'reactivated': executions.isNotEmpty,
  };
}

// ============================================================================
// Test Program 3: Failure Case
// ============================================================================

Map<String, dynamic> _runFailureTest({required bool useV2}) {
  // Create runtime with chosen heap implementation
  final rt = useV2
    ? GlpRuntime(heap: HeapV2Adapter())
    : GlpRuntime();

  // Create variable X/X?
  const wX = 1;
  const rX = 2;
  rt.heap.addWriter(WriterCell(wX, rX));
  rt.heap.addReader(ReaderCell(rX));

  // Program p: p(a).
  final progP = BC.prog([
    BC.L('p/1'),
    BC.TRY(),
    BC.headConst('a', 0),
    BC.COMMIT(),
    BC.PROCEED(),
    BC.L('p_end'),
    BC.SUSP(),
  ]);

  // Program q: q(b). - expects 'b', will fail if given 'a'
  final progQ = BC.prog([
    BC.L('q/1'),
    BC.TRY(),
    BC.headConst('b', 0),  // Requires arg0 = 'b'
    BC.COMMIT(),
    BC.PROCEED(),
    BC.L('q_end'),
    BC.SUSP(),
  ]);

  final runnerP = BytecodeRunner(progP);
  final runnerQ = BytecodeRunner(progQ);
  final sched = Scheduler(rt: rt, runners: {
    'p': runnerP,
    'q': runnerQ,
  });

  // Goal 1: q(X?) - will suspend on unbound X?
  const goalQ = 100;
  rt.setGoalEnv(goalQ, CallEnv(readers: {0: rX}));
  rt.setGoalProgram(goalQ, 'q');
  rt.gq.enqueue(GoalRef(goalQ, 0));
  sched.drain(maxCycles: 10);

  // Goal 2: p(X) - binds X to 'a', reactivates q(X?) which will FAIL
  const goalP = 200;
  rt.setGoalEnv(goalP, CallEnv(writers: {0: wX}));
  rt.setGoalProgram(goalP, 'p');
  rt.gq.enqueue(GoalRef(goalP, 0));
  final executions = sched.drain(maxCycles: 10);

  final bound = rt.heap.isWriterBound(wX);
  final value = bound ? rt.heap.valueOfWriter(wX) : null;
  final xValue = value != null && value is ConstTerm ? value.value : null;

  // q should have run and failed (q expects 'b' but got 'a')
  final qFailed = executions.contains(100);

  return {
    'X': xValue,
    'q_failed': qFailed,
    'executions': executions.length,
  };
}
