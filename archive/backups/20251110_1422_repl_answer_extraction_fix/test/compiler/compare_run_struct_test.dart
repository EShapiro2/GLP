import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Compare+Run: struct_test.dart - p(f(a,b))', () {
    print('\n=== STRUCT TEST: p(f(a,b)) ===\n');

    // Compile
    final compiler = GlpCompiler();
    final source = 'p(f(a,b)).';
    final program = compiler.compile(source);

    print('Compiled ${program.ops.length} instructions');

    // Run
    final rt = GlpRuntime();
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    final runner = BytecodeRunner(program);
    final sched = Scheduler(rt: rt, runner: runner);

    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, program.labels['p/1']!));

    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer bound? ${rt.heap.isWriterBound(wX)}');

    expect(rt.heap.isWriterBound(wX), true);
    final value = rt.heap.valueOfWriter(wX);
    expect(value, isA<StructTerm>());

    final struct = value as StructTerm;
    expect(struct.functor, 'f');
    expect(struct.args.length, 2);

    print('✅ p(f(a,b)) works!\n');
  });
}
