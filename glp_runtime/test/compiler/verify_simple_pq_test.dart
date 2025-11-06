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
  test('Verify: p(a). q(a). Goal: q(X?), p(X)', () {
    print('\n' + '=' * 70);
    print('COMPILER VERIFICATION TEST');
    print('Source: p(a). q(a).');
    print('Goal: Run q(X?) first (suspends), then p(X) (binds X=a, wakes q)');
    print('=' * 70 + '\n');

    // STEP 1: Compile the source
    final compiler = GlpCompiler();
    final source = '''
      p(a).
      q(a).
    ''';
    final compiledProg = compiler.compile(source);

    print('COMPILED BYTECODE:');
    for (int i = 0; i < compiledProg.ops.length; i++) {
      print('  $i: ${compiledProg.ops[i]}');
    }
    print('\nCOMPILED LABELS:');
    compiledProg.labels.forEach((k, v) => print('  $k => $v'));

    // STEP 2: Verify bytecode structure
    print('\n' + '-' * 70);
    print('BYTECODE VERIFICATION:');
    print('  Both p/1 and q/1 should have:');
    print('    - Label');
    print('    - ClauseTry');
    print('    - HeadConstant(a, 0)');
    print('    - Commit');
    print('    - Proceed');
    print('    - Label(_end)');
    print('    - NoMoreClauses');
    print('-' * 70 + '\n');

    // Verify p/1
    final pLabel = compiledProg.labels['p/1']!;
    expect(compiledProg.ops[pLabel], isA<bc.Label>());
    expect(compiledProg.ops[pLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[pLabel + 2], isA<bc.HeadConstant>());
    final pHc = compiledProg.ops[pLabel + 2] as bc.HeadConstant;
    expect(pHc.value, 'a');
    expect(pHc.argSlot, 0);
    expect(compiledProg.ops[pLabel + 3], isA<bc.Commit>());
    expect(compiledProg.ops[pLabel + 4], isA<bc.Proceed>());

    // Verify q/1
    final qLabel = compiledProg.labels['q/1']!;
    expect(compiledProg.ops[qLabel], isA<bc.Label>());
    expect(compiledProg.ops[qLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[qLabel + 2], isA<bc.HeadConstant>());
    final qHc = compiledProg.ops[qLabel + 2] as bc.HeadConstant;
    expect(qHc.value, 'a');
    expect(qHc.argSlot, 0);
    expect(compiledProg.ops[qLabel + 3], isA<bc.Commit>());
    expect(compiledProg.ops[qLabel + 4], isA<bc.Proceed>());

    print('✓ Bytecode structure verified!\n');

    // STEP 3: Run the compiled program
    print('=' * 70);
    print('EXECUTION TEST');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer/Reader pair for X/X?
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('HEAP SETUP:');
    print('  Writer $wX (X) paired with Reader $rX (X?)');
    print('');

    final runner = BytecodeRunner(compiledProg);
    final sched = Scheduler(rt: rt, runner: runner);

    // Goal 1: q(X?) - should suspend because X is unbound
    print('Step 1: Run q(X?) with unbound reader');
    const goalQ = 100;
    final envQ = CallEnv(readers: {0: rX});
    rt.setGoalEnv(goalQ, envQ);
    rt.gq.enqueue(GoalRef(goalQ, compiledProg.labels['q/1']!));

    var ran = sched.drain(maxCycles: 10);
    print('  Goals executed: $ran');
    print('  X bound? ${rt.heap.isWriterBound(wX)}');
    print('  Suspended on R$rX? ${rt.roq.queue(rX).length} goals\n');

    expect(rt.heap.isWriterBound(wX), false, reason: 'X should be unbound');
    expect(rt.roq.queue(rX).length, 1, reason: 'q(X?) should be suspended');

    // Goal 2: p(X) - should bind X to 'a' and reactivate q(X?)
    print('Step 2: Run p(X) with unbound writer');
    const goalP = 200;
    final envP = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalP, envP);
    rt.gq.enqueue(GoalRef(goalP, compiledProg.labels['p/1']!));

    ran = sched.drain(maxCycles: 10);
    print('  Goals executed: $ran');
    print('  X bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      print('  X value: ${rt.heap.valueOfWriter(wX)}');
    }
    print('');

    // Check results
    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');
    final val = rt.heap.valueOfWriter(wX);
    expect((val as ConstTerm).value, 'a', reason: 'X should be bound to a');
    expect(ran.length, 2, reason: 'Should run p(X) then reactivated q(X?)');
    expect(ran, [200, 100], reason: 'p runs first, then q is reactivated');

    print('✓ Success! X = a, both goals succeeded');
    print('✓ Compiled program matches hand-written semantics!\n');
  });
}
