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
  test('Boot test: boot :- p(X), p(X?)', () {
    print('\n' + '=' * 70);
    print('BOOT TEST');
    print('Program: p(a). boot :- p(X), p(X?).');
    print('Goal: boot');
    print('Expected: X binds to a, both p(X) and p(X?) succeed');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    final prog = BC.prog([
      // p(a).
      BC.L('p/1'),
      BC.TRY(),
      BC.headConst('a', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),

      // boot :- p(X), p(X?).
      BC.L('boot/0'),
      BC.TRY(),
      BC.COMMIT(),
      // BODY: spawn p(X), then tail-call p(X?)
      BC.putWriter(0, 0),         // arg0 = X (fresh writer)
      BC.spawn('p/1', 1),         // Call p(X)
      BC.putReader(0, 0),         // arg0 = X? (reader of X)
      BC.requeue('p/1', 1),       // Tail call p(X?)

      BC.L('boot/0_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    print('PROGRAM OPS:');
    for (var i = 0; i < prog.ops.length; i++) {
      print('  PC $i: ${prog.ops[i].runtimeType}');
    }
    print('');

    // Goal: boot (no arguments)
    const goalId = 100;
    final env = CallEnv();
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['boot/0']!));

    print('Goal: boot at PC ${prog.labels['boot/0']}');
    print('');

    // Enable detailed debugging
    print('Before drain - checking goal queue...');

    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);

    // Check if execution completed without infinite loop
    expect(ran.length, lessThan(10), reason: 'Should not hit max cycles (infinite loop)');
    expect(ran.length, greaterThan(1), reason: 'Should execute multiple goals (boot + spawned goals)');

    // Check if writer 1000 (X) was bound
    if (rt.heap.containsWriter(1000)) {
      final bound = rt.heap.isWriterBound(1000);
      print('Writer 1000 (X) bound: $bound');
      if (bound) {
        final value = rt.heap.valueOfWriter(1000);
        print('Writer 1000 value: $value');
      }
    }

    print('✓ Boot test completed!');
    print('Goals executed: $ran');
  });
}
