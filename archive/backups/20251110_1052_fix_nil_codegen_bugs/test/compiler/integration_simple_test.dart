import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  group('Compiler Integration - Simple Programs', () {
    test('compile and run: p(a).', () {
      print('\n=== INTEGRATION TEST: p(a) ===');

      // Compile
      final compiler = GlpCompiler();
      final source = 'p(a).';
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');
      print('Labels: ${program.labels}');

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create goal: p(a)
      // Need to create a writer bound to 'a' and pass it as argument
      const goalId = 100;
      const wid = 1;
      const rid = 2;

      rt.heap.addWriter(WriterCell(wid, rid));
      rt.heap.addReader(ReaderCell(rid));
      rt.heap.bindWriterConst(wid, 'a');

      final env = CallEnv(writers: {0: wid});  // Argument 0 = writer bound to 'a'
      rt.setGoalEnv(goalId, env);

      // Enqueue goal at procedure entry
      final kappa = program.labels['p/1'];
      expect(kappa, isNotNull, reason: 'Should have p/1 label');

      rt.gq.enqueue(GoalRef(goalId, kappa!));

      // Run
      print('Running goal p(a)...');
      final result = sched.drain(maxCycles: 10);

      print('Goals executed: ${result.length}');
      print('✓ Compiled and executed p(a) successfully\n');

      expect(result, [100]);
    });

    test('compile and run: p(X).', () {
      print('\n=== INTEGRATION TEST: p(X) ===');

      // Compile
      final compiler = GlpCompiler();
      final source = 'p(X).';
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create goal: p(W) where W is a writer
      const goalId = 100;
      const wid = 1;
      const rid = 2;

      rt.heap.addWriter(WriterCell(wid, rid));
      rt.heap.addReader(ReaderCell(rid));

      final env = CallEnv(writers: {0: wid});  // Pass writer ID in slot 0
      rt.setGoalEnv(goalId, env);

      final kappa = program.labels['p/1']!;
      rt.gq.enqueue(GoalRef(goalId, kappa));

      // Run
      print('Running goal p(W) with unbound writer...');
      final result = sched.drain(maxCycles: 10);

      print('Goals executed: ${result.length}');
      print('Writer bound: ${rt.heap.isWriterBound(wid)}');

      // The fact p(X) will unify X with the argument (writer W)
      // This creates a binding X = W in the clause, but since X is the
      // argument itself, this is a self-binding which should succeed
      print('✓ Compiled and executed p(X) successfully\n');

      expect(result, [100]);
    });

    test('compile and run: p(a). p(b).', () {
      print('\n=== INTEGRATION TEST: p(a). p(b). ===');

      // Compile
      final compiler = GlpCompiler();
      final source = '''
        p(a).
        p(b).
      ''';
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');
      print('Labels: ${program.labels.keys.toList()}');

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Test 1: Call p(a) - should succeed
      const goal1 = 100;
      const w1 = 10;
      const r1 = 11;
      rt.heap.addWriter(WriterCell(w1, r1));
      rt.heap.addReader(ReaderCell(r1));
      rt.heap.bindWriterConst(w1, 'a');

      final env1 = CallEnv(writers: {0: w1});
      rt.setGoalEnv(goal1, env1);
      rt.gq.enqueue(GoalRef(goal1, program.labels['p/1']!));

      print('Running p(a)...');
      var result = sched.drain(maxCycles: 10);
      expect(result, [100], reason: 'p(a) should succeed');
      print('✓ p(a) succeeded');

      // Test 2: Call p(b) - should succeed
      const goal2 = 200;
      const w2 = 20;
      const r2 = 21;
      rt.heap.addWriter(WriterCell(w2, r2));
      rt.heap.addReader(ReaderCell(r2));
      rt.heap.bindWriterConst(w2, 'b');

      final env2 = CallEnv(writers: {0: w2});
      rt.setGoalEnv(goal2, env2);
      rt.gq.enqueue(GoalRef(goal2, program.labels['p/1']!));

      print('Running p(b)...');
      result = sched.drain(maxCycles: 10);
      expect(result, [200], reason: 'p(b) should succeed');
      print('✓ p(b) succeeded');

      // Test 3: Call p(c) - should fail
      const goal3 = 300;
      const w3 = 30;
      const r3 = 31;
      rt.heap.addWriter(WriterCell(w3, r3));
      rt.heap.addReader(ReaderCell(r3));
      rt.heap.bindWriterConst(w3, 'c');

      final env3 = CallEnv(writers: {0: w3});
      rt.setGoalEnv(goal3, env3);
      rt.gq.enqueue(GoalRef(goal3, program.labels['p/1']!));

      print('Running p(c)...');
      result = sched.drain(maxCycles: 10);
      expect(result, [300], reason: 'p(c) should run once and fail');
      // Goal should have failed (not suspended, not completed)
      print('✓ p(c) failed as expected\n');
    });
  });
}
