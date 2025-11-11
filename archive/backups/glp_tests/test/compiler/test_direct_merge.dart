import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';

void main() {
  test('Direct merge without metainterpreter', () {
    final source = '''
      merge([X|Xs], Ys, [X?|Zs?]) :- merge(Ys?, Xs?, Zs).
      merge(Xs, [Y|Ys], [Y?|Zs?]) :- merge(Xs?, Ys?, Zs).
      merge([], [], []).
    ''';

    final compiler = GlpCompiler();
    final program = compiler.compile(source);

    final rt = GlpRuntime();

    // Zs (unbound result)
    const wZs = 1, rZs = 2;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    // List [a]
    const wListA = 3, rListA = 4;
    rt.heap.addWriter(WriterCell(wListA, rListA));
    rt.heap.addReader(ReaderCell(rListA));
    rt.heap.bindWriterStruct(wListA, '.', [ConstTerm('a'), ConstTerm(null)]);

    // List [b]
    const wListB = 5, rListB = 6;
    rt.heap.addWriter(WriterCell(wListB, rListB));
    rt.heap.addReader(ReaderCell(rListB));
    rt.heap.bindWriterStruct(wListB, '.', [ConstTerm('b'), ConstTerm(null)]);

    final runner = BytecodeRunner(program);
    final sched = Scheduler(rt: rt, runner: runner);

    const goalId = 100;
    rt.setGoalEnv(goalId, CallEnv(writers: {0: wListA, 1: wListB, 2: wZs}));
    rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

    final ran = sched.drain(maxCycles: 500);

    print('Goals executed: ${ran.length}');
    print('Zs bound: ${rt.heap.isWriterBound(wZs)}');
    if (rt.heap.isWriterBound(wZs)) {
      print('Zs value: ${rt.heap.valueOfWriter(wZs)}');
    }

    expect(ran.isNotEmpty, true);
    expect(rt.heap.isWriterBound(wZs), true);
  });
}
