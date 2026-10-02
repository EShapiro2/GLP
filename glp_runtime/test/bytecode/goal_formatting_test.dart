import 'package:test/test.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine_v2/code_image.dart';
import 'package:glp_runtime/engine_v2/interp.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';

/// A constant whose text cannot be made: a goal carrying it throws where the
/// goal is formatted, and nowhere else.
class _Unformattable {
  @override
  String toString() => throw StateError('a goal was formatted');
}

/// A goal is formatted only where its text is read (scheduler.dart,
/// drainWithStatus): the trace, when the drain is traced; F, when the goal
/// fails; and the suspended list, when a caller reads it.  Until 2026-10-02
/// every goal was formatted before its reduction, traced or not, so a
/// reduction cost time in the size of its goal's terms.
void main() {
  // p/2 spawns q and calls r last, and both reduce.  w/2 suspends on an
  // unbound first argument and fails on anything but a.
  const source = '''
p(X, Y) :- q(X?), r(Y?).
q(_).
r(_).
w(a, _).
''';

  // The compiled program, with p's last goal made a tail call (Requeue), as
  // hand-assembled code has it; the compiler spawns it.  A run of p then
  // passes the three places a reduction formatted a goal for the trace: the
  // spawn, the tail call and the proceed.
  BytecodeProgram compiled() {
    final ops = List<dynamic>.of(GlpCompiler().compile(source).ops);
    final proceed = ops.indexWhere((op) => op is Proceed);
    final last = ops[proceed - 1] as Spawn;
    ops[proceed - 1] = Requeue(last.procedureLabel, last.arity);
    ops.removeAt(proceed);
    return BytecodeProgram(ops);
  }

  (Scheduler, GlpRuntime, CodeImage) setUp(List<String> trace) {
    final rt = GlpRuntime();
    final image = codeImageFromProgram(compiled());
    final sched = Scheduler(
        rt: rt, runner: ByteRunner(image), traceSink: trace.add);
    return (sched, rt, image);
  }

  // A goal's arguments are heap variables: a value is passed as the reader
  // of a writer bound to it.
  void post(GlpRuntime rt, CodeImage image, int id, String proc,
      List<Term> args) {
    rt.setGoalEnv(id, CallEnv(args: {for (var i = 0; i < args.length; i++) i: args[i]}));
    rt.gq.enqueue(GoalRef(id, image.entryOffsetOf(proc)!));
  }

  Term bound(GlpRuntime rt, Object value) {
    final (writer, reader) = rt.heap.allocateVariable();
    rt.heap.bindWriter(writer, ConstTerm(value));
    return VarRef(reader);
  }

  Term unformattable(GlpRuntime rt) => bound(rt, _Unformattable());

  test('a drain that is not traced formats no goal', () {
    final trace = <String>[];
    final (sched, rt, image) = setUp(trace);
    post(rt, image, 1, 'p/2', [unformattable(rt), unformattable(rt)]);

    final result = sched.drainWithStatus();
    expect(result.status, ExecutionStatus.succeeded);
    expect(result.goalsRun, 2,
        reason: 'p, its tail call r run in its turn, and the q it spawned');
    expect(trace, isEmpty);
  });

  test('a traced drain formats its goals, so the test above is not vacuous',
      () {
    final trace = <String>[];
    final (sched, rt, image) = setUp(trace);
    post(rt, image, 1, 'p/2', [unformattable(rt), unformattable(rt)]);

    expect(() => sched.drainWithStatus(debug: true), throwsStateError);
  });

  test('a suspended goal is formatted when the suspended list is read', () {
    final trace = <String>[];
    final (sched, rt, image) = setUp(trace);
    final (_, reader) = rt.heap.allocateVariable();
    post(rt, image, 1, 'w/2', [VarRef(reader), unformattable(rt)]);

    final result = sched.drainWithStatus();
    expect(result.status, ExecutionStatus.suspended);
    expect(() => result.suspendedGoals, throwsStateError,
        reason: 'formatted on reading, and not before');
  });

  test('a failed and a suspended goal read as they do in a traced drain', () {
    (List<String>, List<String>) run({required bool debug}) {
      final trace = <String>[];
      final (sched, rt, image) = setUp(trace);
      final (_, reader) = rt.heap.allocateVariable();
      post(rt, image, 1, 'w/2', [bound(rt, 'b'), bound(rt, 'c')]);
      post(rt, image, 2, 'w/2', [VarRef(reader), bound(rt, 'c')]);
      final result = sched.drainWithStatus(debug: debug);
      expect(result.status, ExecutionStatus.failed);
      return (List.of(rt.failedGoals), result.suspendedGoals);
    }

    final (tracedFailed, tracedSuspended) = run(debug: true);
    final (failed, suspended) = run(debug: false);
    expect(tracedFailed, ['w/2(b, c)']);
    expect(tracedSuspended, ['w(X1?, c)']);
    expect(failed, tracedFailed);
    expect(suspended, tracedSuspended);
  });
}
