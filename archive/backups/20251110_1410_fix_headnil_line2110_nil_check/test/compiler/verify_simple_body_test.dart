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
  test('Verify: p(a). forward(X) :- p(X?).', () {
    print('\n' + '=' * 70);
    print('COMPILER VERIFICATION TEST');
    print('Hand-written: p(a). forward(X) :- p(X). [PutWriter - binds X]');
    print('SRSW-compliant: p(a). forward(X) :- p(X?). [PutReader - suspends]');
    print('Goal: forward(Y) where Y is unbound writer');
    print('NOTE: Different semantics - SRSW version will suspend!');
    print('=' * 70 + '\n');

    // STEP 1: Compile the SRSW-compliant source
    final compiler = GlpCompiler();
    final source = '''
      p(a).
      forward(X) :- p(X?).
    ''';
    final compiledProg = compiler.compile(source);

    print('COMPILED BYTECODE:');
    for (int i = 0; i < compiledProg.ops.length; i++) {
      print('  $i: ${compiledProg.ops[i]}');
    }
    print('\nCOMPILED LABELS:');
    compiledProg.labels.forEach((k, v) => print('  $k => $v'));

    // STEP 2: Show bytecode comparison
    print('\n' + '-' * 70);
    print('BYTECODE COMPARISON:');
    print('  Hand-written: Uses PutWriter(0, 0) - forwards X as writer');
    print('  Compiled:     Uses PutReader(0, 0) - passes X? as reader');
    print('');
    print('DIFFERENCE: Hand-written violates SRSW (X appears twice).');
    print('SRSW-compliant version uses X? in body, generating PutReader.');
    print('-' * 70 + '\n');

    // STEP 3: Verify key instructions match
    expect(compiledProg.ops[0], isA<bc.Label>());
    expect(compiledProg.ops[1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[2], isA<bc.HeadConstant>());
    final hc = compiledProg.ops[2] as bc.HeadConstant;
    expect(hc.value, 'a');
    expect(hc.argSlot, 0);

    expect(compiledProg.ops[3], isA<bc.Commit>());
    expect(compiledProg.ops[4], isA<bc.Proceed>());

    // forward/1 should be at label index
    final forwardLabel = compiledProg.labels['forward/1']!;
    expect(compiledProg.ops[forwardLabel], isA<bc.Label>());
    expect(compiledProg.ops[forwardLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[forwardLabel + 2], isA<bc.GetVariable>());
    expect(compiledProg.ops[forwardLabel + 3], isA<bc.Commit>());
    expect(compiledProg.ops[forwardLabel + 4], isA<bc.PutReader>());
    expect(compiledProg.ops[forwardLabel + 5], isA<bc.Requeue>());

    final requeue = compiledProg.ops[forwardLabel + 5] as bc.Requeue;
    expect(requeue.procedureLabel, 'p/1');
    expect(requeue.arity, 1);

    print('✓ Bytecode structure verified!\n');

    // STEP 4: Run the compiled program
    print('=' * 70);
    print('EXECUTION TEST');
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

    final runner = BytecodeRunner(compiledProg);
    final sched = Scheduler(rt: rt, runner: runner);

    // Goal: forward(Y) where Y is writer wY
    const goalId = 100;
    final env = CallEnv(writers: {0: wY});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, compiledProg.labels['forward/1']!));

    print('Goal: forward(Y) at PC ${compiledProg.labels['forward/1']}');
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

    // Check results - SRSW version suspends (different from hand-written!)
    expect(ran.length, lessThan(10), reason: 'Should not hit max cycles (infinite loop)');
    expect(rt.heap.isWriterBound(wY), false, reason: 'Y should remain unbound (suspended)');
    expect(!rt.roq.isEmpty(rY), true, reason: 'Should be suspended on reader Y?');

    print('✓ SRSW version suspends (as expected - different from hand-written)');
    print('✓ Hand-written uses writer forwarding (not valid SRSW)');
    print('✓ Compiled bytecode structure is correct!\n');
  });
}
