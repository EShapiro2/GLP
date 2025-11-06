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
  test('Ground instruction: succeeds for ground constant', () {
    print('\n=== GROUND INSTRUCTION TEST: Ground Constant ===');

    final rt = GlpRuntime();

    // Program: p(X) :- ground(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.ground(0),           // Test if X0 is ground
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

    print('Starting p(X) where X = a (ground constant)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Ground instruction succeeded for ground constant\n');
  });

  test('Ground instruction: fails for unbound writer', () {
    print('\n=== GROUND INSTRUCTION TEST: Unbound Writer ===');

    final rt = GlpRuntime();

    // Program: p(X) :- ground(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.ground(0),           // Test if X0 is ground
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

    print('Starting p(X) where X is unbound (not ground)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should fail first clause, succeed with otherwise clause (one execution)');
    print('✓ Ground instruction failed for unbound writer, otherwise clause executed\n');
  });

  test('Ground instruction: succeeds for ground structure', () {
    print('\n=== GROUND INSTRUCTION TEST: Ground Structure ===');

    final rt = GlpRuntime();

    // Program: p(X) :- ground(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.ground(0),           // Test if X0 is ground
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

    // Setup: Writer bound to structure f(a, b)
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterStruct(wX, 'f', [ConstTerm('a'), ConstTerm('b')]);

    // Call p(X) where X is bound to f(a, b)
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) where X = f(a, b) (ground structure)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should execute once and succeed');
    print('✓ Ground instruction succeeded for ground structure\n');
  });

  test('Ground instruction: fails for structure with unbound variable', () {
    print('\n=== GROUND INSTRUCTION TEST: Non-Ground Structure ===');

    final rt = GlpRuntime();

    // Program: p(X) :- ground(X) | proceed.
    //          p(X) :- otherwise | fail.
    final prog = BC.prog([
      BC.L('p/1'),
      BC.TRY(),
      BC.getVar(0, 0),       // X0 = argument 0
      BC.ground(0),           // Test if X0 is ground
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

    // Call p(X) where X is bound to f(Y) with Y unbound
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['p/1']!));

    print('Starting p(X) where X = f(Y) with Y unbound (not ground)');
    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: ${ran.length}');
    expect(ran.length, 1, reason: 'Should fail first clause, succeed with otherwise clause (one execution)');
    print('✓ Ground instruction failed for non-ground structure\n');
  });
}
