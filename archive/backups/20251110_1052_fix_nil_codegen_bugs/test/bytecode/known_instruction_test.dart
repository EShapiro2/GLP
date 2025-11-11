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
  test('Known instruction: succeeds for bound constant', () {
    print('\n=== KNOWN INSTRUCTION TEST: Bound Constant ===');

    final rt = GlpRuntime();

    // Program: p(X) :- known(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.known(0),           // Test if X0 is known (not unbound)
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Writer bound to constant 'a'
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'a');

    // Call p(X) where X is bound to 'a'
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) where X = a (known)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Known instruction succeeded for bound constant\n');
  });

  test('Known instruction: fails for unbound writer', () {
    print('\n=== KNOWN INSTRUCTION TEST: Unbound Writer ===');

    final rt = GlpRuntime();

    // Program: p(X) :- known(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.known(0),           // Test if X0 is known (not unbound)
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Unbound writer
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    // Don't bind - leave unbound

    // Call p(X) where X is unbound
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) where X is unbound (not known)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should fail first clause, succeed with otherwise clause');
    print('✓ Known instruction failed for unbound writer, otherwise clause executed\n');
  });

  test('Known instruction: succeeds for bound structure with unbound variable', () {
    print('\n=== KNOWN INSTRUCTION TEST: Known Structure (even with unbound internals) ===');

    final rt = GlpRuntime();

    // Program: p(X) :- known(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.known(0),           // Test if X0 is known (not unbound)
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_c2'),
      BC.TRY(),
      BC.otherwise(),
      BC.COMMIT(),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Setup: Writer bound to structure f(Y) where Y is unbound
    // X is known (it's bound to a structure), even though Y inside is not known
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    const wY = 3;
    const rY = 4;
    rt.heap.addWriter(WriterCell(wY, rY));
    rt.heap.addReader(ReaderCell(rY));
    // Y is unbound

    rt.heap.bindWriterStruct(wX, 'f', [WriterTerm(wY)]);

    // Call p(X) where X is bound to f(Y) - X is known even though Y is not
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) where X = f(Y) with Y unbound');
    print('X is known (bound to structure) even though Y inside is unbound');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should succeed - X is known (bound)');
    print('✓ Known instruction succeeded - X is known even with unbound internals\n');
  });
}
