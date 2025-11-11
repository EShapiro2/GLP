import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:test/test.dart';

void main() {
  test('Trace metainterpreter with minimal p/q/r program', () {
    final source = '''
      % Clean test - only the minimal program
      clause(p(X?), (q(Y), r(Y?,X))).
      clause(q(a), true).
      clause(r(a,b), true).

      run(true).
      run((A, B)) :- run(A?), run(B?).
      run(A) :- otherwise | clause(A?, B), run(B?).
    ''';

    final compiler = GlpCompiler();
    final prog = compiler.compile(source);

    final rt = GlpRuntime();
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runners: {'main': runner});

    // Create goal: run(p(X))
    final goalId = rt.nextGoalId++;
    final (xWriter, xReader) = rt.heap.allocateFreshPair();
    rt.heap.addWriterCell(xWriter, xReader);
    rt.heap.addReaderCell(xReader);

    // Create p(X?) term
    final pXTerm = rt.heap.allocateStruct('p', [rt.heap.mkReader(xReader)]);

    // Create run(p(X?)) term
    final runPXTerm = rt.heap.allocateStruct('run', [pXTerm]);

    // Set up environment for run/1 with one argument
    final env = CallEnv(
      writers: {},
      readers: {0: xReader},
    );
    rt.setGoalEnv(goalId, env);
    rt.setGoalProgram(goalId, 'main');

    // Get entry point for run/1
    final entryPc = prog.labels['run/1'];
    expect(entryPc, isNotNull, reason: 'run/1 entry point not found');

    // Enqueue the goal
    rt.gq.enqueue(GoalRef(goalId, entryPc!));

    print('\n=== TRACING EXECUTION ===\n');
    print('Initial goal: run(p(X)) where X = writer $xWriter');
    print('');

    // Run scheduler
    int goalCount = 0;
    while (rt.gq.isNotEmpty && goalCount < 20) {
      goalCount++;
      final result = sched.step();

      if (result == SchedulerStepResult.suspended ||
          result == SchedulerStepResult.failed ||
          result == SchedulerStepResult.noGoals) {
        print('\n>>> Scheduler stopped: $result');
        break;
      }
    }

    print('\n=== FINAL STATE ===');
    print('Total goals executed: $goalCount');
    print('Goals remaining in queue: ${rt.gq.length}');
    print('X (writer $xWriter) value: ${rt.heap.writerValue[xWriter] ?? '<unbound>'}');
    print('');
  });
}
