/// The cell InputInjector
/// injects holds its tail's reader, and Dart keeps the writer, so a stream
/// reader's head writer at the tail is assigned the reader (GLP-Spec
/// appendix-term-matching.tex, row "Reader X1?", column "Writer X2") and not
/// failed by a goal writer (row "Writer X1", column "Writer X2": "fail").
/// Red at a1a35748 (drain/1([a | X1]) fails), green with external_io.dart's
/// inject() holding the reader.
import 'package:glp_runtime/bytecode/runner.dart' show CallEnv;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine_v2/interp.dart'
    show ByteRunner, codeImageFromProgram;
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/machine_state.dart' show GoalRef;
import 'package:glp_runtime/runtime/runtime.dart' show GlpRuntime;
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _drain = '''
drain([_ | Xs]) :- drain(Xs?).
drain([]).
''';

void main() {
  test('an injected cell holds the reader of its tail', () {
    final rt = GlpRuntime();
    final (w, _) = rt.heap.allocateVariable();
    final injector = InputInjector(rt.heap, 'user', w);
    injector.inject(ConstTerm('a'));
    final cell = rt.heap.valueOfWriter(w);
    expect(cell, isA<StructTerm>());
    final tail = (cell as StructTerm).args[1];
    expect(tail, isA<VarRef>());
    expect(rt.heap.isReader((tail as VarRef).addr), isTrue,
        reason: 'the tail of an injected cell is a reader; Dart holds its writer');
  });

  test('a stream reader reduces on a cell whose tail is still unbound', () {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(GlpCompiler().compile(_drain));
    final sched = Scheduler(rt: rt, runner: ByteRunner(image));
    final (w, r) = rt.heap.allocateVariable();
    final injector = InputInjector(rt.heap, 'user', w);
    final id = rt.nextGoalId++;
    rt.setGoalEnv(id, CallEnv(args: {0: VarRef(r)}));
    rt.gq.enqueue(GoalRef(id, image.entryOffsetOf('drain/1')!));
    expect(sched.drainWithStatus().status, ExecutionStatus.suspended);

    for (final g in injector.inject(ConstTerm('a'))) {
      rt.gq.enqueue(g);
    }
    final afterA = sched.drainWithStatus().status;
    expect(rt.failedGoals, isEmpty,
        reason: 'drain([_ | Xs]) takes [a | T?] with T unbound');
    expect(afterA, ExecutionStatus.suspended);

    for (final g in injector.close()) {
      rt.gq.enqueue(g);
    }
    expect(sched.drainWithStatus().status, ExecutionStatus.succeeded);
    expect(rt.failedGoals, isEmpty);
  });
}
