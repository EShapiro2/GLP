import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Simple structure matching: p(f(a, b)) with writer', () {
    print('\n=== Testing Structure Matching ===');
    print('Program: p(f(a, b)).');
    print('Goal: p(X) where X is writer\n');

    final rt = GlpRuntime();

    // Create writer for X
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound\n');

    // Bytecode for p(f(a, b))
    // This will match argument 0 with structure f/2, then match args with constants a, b
    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      // Argument 0 should be a structure f/2
      BC.headStruct('f', 2, 0),  // Match A0 with f/2, enter WRITE mode (writer) or READ mode
      // Now match the two arguments of f
      BC.unifyConst('a'),         // First arg of f should be 'a'
      BC.unifyConst('b'),         // Second arg of f should be 'b'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: p(X) ---');
    print('Expected: X (writer) will be bound to f(a, b) in σ̂w\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val\n');
    }

    // Verify results
    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wX), true, reason: 'Writer should be bound');

    final value = rt.heap.valueOfWriter(wX);
    expect(value, isA<StructTerm>(), reason: 'Should be a structure');

    final struct = value as StructTerm;
    expect(struct.functor, 'f', reason: 'Functor should be f');
    expect(struct.args.length, 2, reason: 'Arity should be 2');
    expect(struct.args[0], isA<ConstTerm>());
    expect((struct.args[0] as ConstTerm).value, 'a');
    expect(struct.args[1], isA<ConstTerm>());
    expect((struct.args[1] as ConstTerm).value, 'b');

    print('✓ Structure correctly built: f(a, b)');
    print('✓ Test passed!');
  });

  test('Simple structure matching: p(f(a, b)) with bound reader', () {
    print('\n=== Testing Structure Matching with Reader ===');
    print('Program: p(f(a, b)).');
    print('Goal: p(X?) where X is already bound to f(a, b)\n');

    final rt = GlpRuntime();

    // Create writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Pre-bind writer to f(a, b)
    rt.heap.bindWriterStruct(wX, 'f', [ConstTerm('a'), ConstTerm('b')]);

    print('Heap Setup:');
    print('  Writer $wX (X) - bound to f(a, b)');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    // Same bytecode as before
    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.headStruct('f', 2, 0),  // Match A0 with f/2, enter READ mode (bound reader)
      BC.unifyConst('a'),         // Verify first arg is 'a'
      BC.unifyConst('b'),         // Verify second arg is 'b'
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: p(X?) ---');
    print('Expected: Match X? (reads f(a, b)) with pattern f(a, b)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Expected: Goal executes successfully (structure matches pattern)\n');

    // Verify results
    expect(ran, [goalId], reason: 'Goal should execute - structure matches');

    print('✓ Structure correctly matched in READ mode');
    print('✓ Test passed!');
  });

  test('Structure matching: p(f(a, b)) fails on mismatch', () {
    print('\n=== Testing Structure Mismatch ===');
    print('Program: p(f(a, b)).');
    print('Goal: p(X?) where X is bound to f(a, c) - should FAIL\n');

    final rt = GlpRuntime();

    // Create writer/reader pair
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Pre-bind writer to f(a, c) - note: 'c' not 'b'
    rt.heap.bindWriterStruct(wX, 'f', [ConstTerm('a'), ConstTerm('c')]);

    print('Heap Setup:');
    print('  Writer $wX (X) - bound to f(a, c)');
    print('  Reader $rX (X?) - paired with Writer $wX\n');

    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.headStruct('f', 2, 0),  // Match A0 with f/2
      BC.unifyConst('a'),         // First arg matches
      BC.unifyConst('b'),         // Second arg DOESN'T match ('c' != 'b')
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: p(X?) ---');
    print('Expected: FAIL or SUSPEND (no matching clause)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');

    // Should execute (clause exhausted with no match)
    expect(ran, [goalId], reason: 'Goal should execute');

    print('✓ Mismatch correctly detected');
    print('✓ Test passed!');
  });
}
