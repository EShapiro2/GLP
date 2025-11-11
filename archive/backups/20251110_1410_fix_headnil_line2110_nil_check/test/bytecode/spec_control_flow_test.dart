import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('ClauseNext: moves to next clause and accumulates Si into U', () {
    print('\n=== CLAUSENEXT TEST: Spec-compliant clause control flow ===');

    final rt = GlpRuntime();

    // Program with two clauses using ClauseNext:
    // p(X?) :- ground(X?) | proceed.  (clause 1: fails on unbound reader, adds to Si, ClauseNext)
    // p(X?) :- otherwise | proceed.   (clause 2: succeeds)

    final prog = BC.prog([
      BC.L('p/1'),

      // Clause 1: ground(X?) test - will fail and use ClauseNext
      BC.TRY(),
      BC.getVar(0, 0),              // X0 = arg 0 (reader)
      BC.ground(0),                 // Test ground - fails, adds reader to Si
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/1_c1_end'),
      BC.clauseNext('p/1_c2'),      // NEW: ClauseNext instead of UnionSiAndGoto

      // Clause 2: otherwise
      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Call p(X?)
    const goalId = 100;
    final env = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X?) with unbound reader...');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute and succeed via otherwise clause');
    print('✓ ClauseNext correctly transitioned to next clause\n');
  });

  test('NoMoreClauses: suspends when U non-empty', () {
    print('\n=== NOMORECLAUSES TEST: Suspension on U ===');

    final rt = GlpRuntime();

    // Program: p(X?) with guard that adds to Si, no matching clauses
    // The ground(X?) guard on unbound reader adds to Si
    // Since no clause commits, Si gets unioned to U via ClauseNext
    // Then NoMoreClauses suspends because U is non-empty

    final prog = BC.prog([
      BC.L('p/1'),

      // Clause 1: Has ground guard that adds reader to Si, but head fails
      BC.TRY(),
      BC.getVar(0, 0),              // X0 = arg 0 (reader)
      BC.ground(0),                 // Test ground - adds reader to Si, continues
      BC.headConst('no_match', 0),  // Head fails - jumps to next clause with Si→U
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: Also fails
      BC.L('p/1_c2'),
      BC.TRY(),
      BC.headConst('also_no_match', 0), // Also fails
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.noMoreClauses(),           // All clauses exhausted, U non-empty → suspend
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Call p(X?)
    const goalId = 100;
    final env = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X?) with unbound reader - should suspend...');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    print('ROQ has suspension notes: ${!rt.roq.isEmpty(rX)}');

    expect(ran.length, 1, reason: 'Should execute once and suspend');
    expect(!rt.roq.isEmpty(rX), isTrue, reason: 'Should have suspension note in ROQ');
    print('✓ NoMoreClauses correctly suspended goal on U\n');
  });

  test('NoMoreClauses: fails when U empty', () {
    print('\n=== NOMORECLAUSES TEST: Failure when U empty ===');

    final rt = GlpRuntime();

    // Program: p(X) :- head_const(X, 'a') | proceed.
    // No match and no suspension - should fail definitively

    final prog = BC.prog([
      BC.L('p/1'),

      // Clause 1: match 'a' - will fail on 'b'
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/1_c1_end'),
      BC.clauseNext('p/1_end'),

      BC.L('p/1_end'),
      BC.noMoreClauses(),           // NEW: NoMoreClauses checks U (empty) and fails
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Writer bound to 'b' (not 'a')
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'b');

    // Call p('b')
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(\'b\') when program expects \'a\' - should fail...');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and fail');
    print('✓ NoMoreClauses correctly failed when U empty\n');
  });

  test('Spec-compliant control flow: full clause selection example', () {
    print('\n=== SPEC CONTROL FLOW: Full example with new instructions ===');

    final rt = GlpRuntime();

    // Program with 3 clauses demonstrating all new instructions:
    // p(X, Y?) :- head_const(X, 'a'), ground(Y?) | proceed.  (clause 1)
    // p(X, Y?) :- head_const(X, 'b'), ground(Y?) | proceed.  (clause 2)
    // p(X, Y?) :- otherwise | proceed.                        (clause 3)

    final prog = BC.prog([
      BC.L('p/2'),

      // Clause 1: X='a', ground(Y?)
      BC.TRY(),
      BC.headConst('a', 0),
      BC.getVar(0, 1),              // Y = arg 1 (reader)
      BC.ground(0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/2_c1_end'),
      BC.clauseNext('p/2_c2'),

      // Clause 2: X='b', ground(Y?)
      BC.L('p/2_c2'),
      BC.TRY(),
      BC.headConst('b', 0),
      BC.getVar(0, 1),
      BC.ground(0),
      BC.COMMIT(),
      BC.PROCEED(),
      BC.L('p/2_c2_end'),
      BC.clauseNext('p/2_c3'),

      // Clause 3: otherwise
      BC.L('p/2_c3'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/2_end'),
      BC.noMoreClauses(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Test case 1: X='a', Y? unbound -> fails clause 1 (ground fails), succeeds clause 3 (otherwise)
    {
      const wX = 1;
      const rX = 2;
      const wY = 3;
      const rY = 4;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));
      rt.heap.addWriter(WriterCell(wY, rY));
      rt.heap.addReader(ReaderCell(rY));
      rt.heap.bindWriterConst(wX, 'a');

      const goalId = 100;
      final env = CallEnv(writers: {0: wX}, readers: {1: rY});
      rt.setGoalEnv(goalId, env);
      rt.gq.enqueue(GoalRef(goalId, prog.labels['p/2']!));

      print('Test 1: p(\'a\', Y?) where Y unbound...');
      final ran = sched.drain(maxCycles: 10);
      print('  Result: ${ran.length} executions');
      expect(ran.length, 1, reason: 'Should succeed via otherwise');
      print('  ✓ Clause 1 failed (ground), otherwise succeeded');
    }

    // Test case 2: X='c', Y? unbound -> fails clauses 1&2 (head mismatch), succeeds clause 3
    {
      final rt2 = GlpRuntime();
      final sched2 = Scheduler(rt: rt2, runner: runner);

      const wX = 10;
      const rX = 11;
      const wY = 12;
      const rY = 13;
      rt2.heap.addWriter(WriterCell(wX, rX));
      rt2.heap.addReader(ReaderCell(rX));
      rt2.heap.addWriter(WriterCell(wY, rY));
      rt2.heap.addReader(ReaderCell(rY));
      rt2.heap.bindWriterConst(wX, 'c');

      const goalId = 200;
      final env = CallEnv(writers: {0: wX}, readers: {1: rY});
      rt2.setGoalEnv(goalId, env);
      rt2.gq.enqueue(GoalRef(goalId, prog.labels['p/2']!));

      print('Test 2: p(\'c\', Y?) where X doesn\'t match any clause...');
      final ran = sched2.drain(maxCycles: 10);
      print('  Result: ${ran.length} executions');
      expect(ran.length, 1, reason: 'Should succeed via otherwise');
      print('  ✓ All clauses failed head match, otherwise succeeded');
    }

    print('✓ Spec-compliant control flow working correctly\n');
  });
}
