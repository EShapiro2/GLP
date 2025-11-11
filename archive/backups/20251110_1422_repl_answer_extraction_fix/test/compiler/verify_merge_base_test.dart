import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Verify: merge([],[],[]).', () {
    print('\n' + '=' * 70);
    print('COMPILER VERIFICATION TEST');
    print('Source: merge([],[],[]).');
    print('Goal: merge([], [], []) - should succeed');
    print('=' * 70 + '\n');

    // STEP 1: Compile the source
    final compiler = GlpCompiler();
    final source = 'merge([],[],[]).';
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
    print('  merge/3 should have:');
    print('    - Label');
    print('    - ClauseTry');
    print('    - HeadNil(0) for first []');
    print('    - HeadNil(1) for second []');
    print('    - HeadNil(2) for third []');
    print('    - Commit');
    print('    - Proceed');
    print('    - Label(_end)');
    print('    - NoMoreClauses');
    print('-' * 70 + '\n');

    // Verify merge/3
    final mergeLabel = compiledProg.labels['merge/3']!;
    expect(compiledProg.ops[mergeLabel], isA<bc.Label>());
    expect(compiledProg.ops[mergeLabel + 1], isA<bc.ClauseTry>());
    expect(compiledProg.ops[mergeLabel + 2], isA<bc.HeadNil>());
    final hn1 = compiledProg.ops[mergeLabel + 2] as bc.HeadNil;
    expect(hn1.argSlot, 0);
    expect(compiledProg.ops[mergeLabel + 3], isA<bc.HeadNil>());
    final hn2 = compiledProg.ops[mergeLabel + 3] as bc.HeadNil;
    expect(hn2.argSlot, 1);
    expect(compiledProg.ops[mergeLabel + 4], isA<bc.HeadNil>());
    final hn3 = compiledProg.ops[mergeLabel + 4] as bc.HeadNil;
    expect(hn3.argSlot, 2);
    expect(compiledProg.ops[mergeLabel + 5], isA<bc.Commit>());
    expect(compiledProg.ops[mergeLabel + 6], isA<bc.Proceed>());

    print('✓ Bytecode structure verified!\n');

    // STEP 3: Run the compiled program
    print('=' * 70);
    print('EXECUTION TEST');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Create bound readers for empty lists
    // Arg 0: []
    const w1 = 1;
    const r1 = 2;
    rt.heap.addWriter(WriterCell(w1, r1));
    rt.heap.addReader(ReaderCell(r1));
    rt.heap.bindWriterConst(w1, null); // [] is null

    // Arg 1: []
    const w2 = 3;
    const r2 = 4;
    rt.heap.addWriter(WriterCell(w2, r2));
    rt.heap.addReader(ReaderCell(r2));
    rt.heap.bindWriterConst(w2, null);

    // Arg 2: []
    const w3 = 5;
    const r3 = 6;
    rt.heap.addWriter(WriterCell(w3, r3));
    rt.heap.addReader(ReaderCell(r3));
    rt.heap.bindWriterConst(w3, null);

    print('HEAP SETUP:');
    print('  Arg0 (Xs?) = []');
    print('  Arg1 (Ys?) = []');
    print('  Arg2 (Zs?) = []');
    print('');

    final runner = BytecodeRunner(compiledProg);
    final sched = Scheduler(rt: rt, runner: runner);

    // Goal: merge([], [], [])
    const goalId = 100;
    final env = CallEnv(readers: {0: r1, 1: r2, 2: r3});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, compiledProg.labels['merge/3']!));

    print('Goal: merge([], [], []) at PC ${compiledProg.labels['merge/3']}');
    print('CallEnv: readers={0: $r1, 1: $r2, 2: $r3}');
    print('');

    final ran = sched.drain(maxCycles: 10);

    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    // Check results
    expect(ran.length, 1, reason: 'Should execute one goal');
    expect(ran, [100], reason: 'Goal should succeed');

    print('✓ Success! merge([],[],[]) completed');
    print('✓ Compiled program matches hand-written semantics!\n');
  });
}
