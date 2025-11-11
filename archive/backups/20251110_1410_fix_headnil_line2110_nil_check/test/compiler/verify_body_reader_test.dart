import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Verify: p(X) :- q(X?). q(a).', () {
    print('\n' + '=' * 70);
    print('COMPILER VERIFICATION TEST');
    print('Source: p(X) :- q(X?). q(a).');
    print('Goal: p(W) where W is unbound writer');
    print('Expected: p spawns q(W?), q binds W to a');
    print('=' * 70 + '\n');

    // STEP 1: Compile the source
    final compiler = GlpCompiler();
    final source = '''
      p(X) :- q(X?).
      q(a).
    ''';
    final compiledProg = compiler.compile(source);

    print('COMPILED BYTECODE:');
    for (int i = 0; i < compiledProg.ops.length; i++) {
      print('  $i: ${compiledProg.ops[i]}');
    }
    print('\nCOMPILED LABELS:');
    compiledProg.labels.forEach((k, v) => print('  $k => $v'));

    // STEP 2: Verify bytecode structure for p/1
    print('\n' + '-' * 70);
    print('BYTECODE VERIFICATION:');
    print('  p/1 body should use PutReader and Requeue');
    print('  q/1 should match constant a');
    print('-' * 70 + '\n');

    final pLabel = compiledProg.labels['p/1']!;
    expect(compiledProg.ops[pLabel], isA<bc.Label>());
    expect(compiledProg.ops[pLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[pLabel + 2], isA<bc.GetVariable>());
    expect(compiledProg.ops[pLabel + 3], isA<bc.Commit>());
    expect(compiledProg.ops[pLabel + 4], isA<bc.PutReader>());
    expect(compiledProg.ops[pLabel + 5], isA<bc.Requeue>());

    final requeue = compiledProg.ops[pLabel + 5] as bc.Requeue;
    expect(requeue.procedureLabel, 'q/1');
    expect(requeue.arity, 1);

    final qLabel = compiledProg.labels['q/1']!;
    expect(compiledProg.ops[qLabel], isA<bc.Label>());
    expect(compiledProg.ops[qLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[qLabel + 2], isA<bc.HeadConstant>());
    final qHc = compiledProg.ops[qLabel + 2] as bc.HeadConstant;
    expect(qHc.value, 'a');
    expect(qHc.argSlot, 0);

    print('✓ Bytecode structure verified!\n');

    // STEP 3: Run the compiled program
    print('=' * 70);
    print('EXECUTION TEST');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer W
    const wid = 1;
    const rid = 2;
    rt.heap.addWriter(WriterCell(wid, rid));
    rt.heap.addReader(ReaderCell(rid));

    print('HEAP SETUP:');
    print('  Writer $wid (W) paired with Reader $rid (W?)');
    print('');

    final runner = BytecodeRunner(compiledProg);
    final sched = Scheduler(rt: rt, runner: runner);

    // Goal: p(W) where W is unbound writer
    const goalId = 100;
    final env = CallEnv(writers: {0: wid});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, compiledProg.labels['p/1']!));

    print('Goal: p(W) at PC ${compiledProg.labels['p/1']}');
    print('CallEnv: writers={0: $wid}');
    print('');

    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('Writer bound: ${rt.heap.isWriterBound(wid)}');

    if (rt.heap.isWriterBound(wid)) {
      final value = rt.heap.valueOfWriter(wid);
      print('Writer value: $value');
    }
    print('');

    // Check results
    expect(ran.length, greaterThan(0), reason: 'Should execute at least one goal');
    expect(rt.heap.isWriterBound(wid), true, reason: 'W should be bound to a');

    final val = rt.heap.valueOfWriter(wid);
    expect((val as ConstTerm).value, 'a', reason: 'W should be bound to a');

    print('✓ Success! W = a');
    print('✓ p(X) correctly spawned q(X?) and bound X\n');
  });
}
