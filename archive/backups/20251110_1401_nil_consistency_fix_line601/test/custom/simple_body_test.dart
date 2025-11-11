import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Simple BODY test: forward(X) :- p(X)', () {
    print('\n' + '=' * 70);
    print('SIMPLE BODY TEST');
    print('Program: p(a). forward(X) :- p(X).');
    print('Goal: forward(Y) where Y is unbound writer');
    print('Expected: Y binds to a via p(Y)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer Y
    const wY = 1;
    const rY = 2;
    rt.heap.addWriter(WriterCell(wY, rY));
    rt.heap.addReader(ReaderCell(rY));

    print('HEAP SETUP:');
    print('  Writer $wY (Y) paired with Reader $rY (Y?)');
    print('');

    final prog = BC.prog([
      // p(a).
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),

      // forward(X) :- p(X).
      BC.L('forward/1'),
      BC.TRY(),
      BC.getVar(0, 0),            // Get X from arg0 into clause var 0
      BC.COMMIT(),
      // BODY:
      BC.putWriter(0, 0),         // arg0 = X (writer)
      BC.requeue('p/1', 1),       // Tail call p(X)

      BC.L('forward/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    // Goal: forward(Y) where Y is writer wY
    const goalId = 100;
    final env = CallEnv(writers: {0: wY});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['forward/1']!));

    print('Goal: forward(Y) at PC ${prog.labels['forward/1']}');
    print('CallEnv: writers={${env.writerBySlot}}');
    print('');

    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Y (W$wY) bound: ${rt.heap.isWriterBound(wY)}');

    // Check results
    expect(ran.length, lessThan(10), reason: 'Should not hit max cycles (infinite loop)');
    expect(rt.heap.isWriterBound(wY), true, reason: 'Y should be bound');

    if (rt.heap.isWriterBound(wY)) {
      final yValue = rt.heap.valueOfWriter(wY);
      print('Y value: $yValue');
      expect(yValue, isA<ConstTerm>(), reason: 'Y should be ConstTerm');
      if (yValue is ConstTerm) {
        expect(yValue.value, 'a', reason: 'Y should be bound to a');
        print('✓ Y correctly bound to a');
      }
    }

    print('✓ BODY instructions working correctly!');
  });
}
