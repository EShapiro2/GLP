import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/cells.dart';

void main() {
  group('Compiler Integration - Clauses with Body', () {
    test('compile and run: p(X) :- q(X?). q(a).', () {
      print('\n=== INTEGRATION TEST: p(X) :- q(X?). q(a). ===');

      // Compile
      final compiler = GlpCompiler();
      final source = '''
        p(X) :- q(X?).
        q(a).
      ''';
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');
      print('Labels: ${program.labels.keys.toList()}');

      // Print bytecode for debugging
      for (int i = 0; i < program.ops.length; i++) {
        print('  $i: ${program.ops[i]}');
      }

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create goal: p(W) where W is unbound writer
      const goalId = 100;
      const wid = 1;
      const rid = 2;

      rt.heap.addWriter(WriterCell(wid, rid));
      rt.heap.addReader(ReaderCell(rid));

      final env = CallEnv(writers: {0: wid});
      rt.setGoalEnv(goalId, env);

      final kappa = program.labels['p/1']!;
      rt.gq.enqueue(GoalRef(goalId, kappa));

      // Run
      print('Running p(W) - should spawn q(W?) which binds W to a...');
      final result = sched.drain(maxCycles: 100);

      print('Goals executed: ${result.length}');
      print('Writer bound: ${rt.heap.isWriterBound(wid)}');

      if (rt.heap.isWriterBound(wid)) {
        final value = rt.heap.valueOfWriter(wid);
        print('Writer value: $value');
      }

      // Expect: p runs, spawns q(W?), q binds W to 'a', both complete
      expect(result.length, greaterThan(0));
      expect(rt.heap.isWriterBound(wid), isTrue, reason: 'W should be bound to a');

      print('✓ p(X) :- q(X?). executed successfully\n');
    });

    test('compile and run: p(X, Y) :- q(X?), r(Y?). with spawn', () {
      print('\n=== INTEGRATION TEST: p(X,Y) :- q(X?), r(Y?). ===');

      // Compile
      final compiler = GlpCompiler();
      final source = '''
        p(X, Y) :- q(X?), r(Y?).
        q(a).
        r(b).
      ''';
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create goal: p(W1, W2) with two unbound writers
      const goalId = 100;
      const w1 = 1, r1 = 2;
      const w2 = 3, r2 = 4;

      rt.heap.addWriter(WriterCell(w1, r1));
      rt.heap.addReader(ReaderCell(r1));
      rt.heap.addWriter(WriterCell(w2, r2));
      rt.heap.addReader(ReaderCell(r2));

      final env = CallEnv(writers: {0: w1, 1: w2});
      rt.setGoalEnv(goalId, env);

      final kappa = program.labels['p/2']!;
      rt.gq.enqueue(GoalRef(goalId, kappa));

      // Run
      print('Running p(W1, W2) - should spawn q(W1?) and requeue r(W2?)...');
      final result = sched.drain(maxCycles: 100);

      print('Goals executed: ${result.length}');
      print('W1 bound: ${rt.heap.isWriterBound(w1)}');
      print('W2 bound: ${rt.heap.isWriterBound(w2)}');

      // Both writers should be bound
      expect(rt.heap.isWriterBound(w1), isTrue, reason: 'W1 should be bound to a');
      expect(rt.heap.isWriterBound(w2), isTrue, reason: 'W2 should be bound to b');

      print('✓ p(X,Y) :- q(X?), r(Y?). executed with spawn/requeue\n');
    });

    test('compile and run: suspension on unbound reader', () {
      print('\n=== INTEGRATION TEST: Suspension ===');

      // Compile
      final compiler = GlpCompiler();
      final source = 'p(X?).';  // Just a reader - should suspend
      final program = compiler.compile(source);

      print('Compiled ${program.ops.length} instructions');

      // Setup runtime
      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Create goal: p(R?) where R is unbound reader
      const goalId = 100;
      const wid = 1;
      const rid = 2;

      rt.heap.addWriter(WriterCell(wid, rid));
      rt.heap.addReader(ReaderCell(rid));
      // NOTE: Writer NOT bound

      final env = CallEnv(readers: {0: rid});  // Pass reader
      rt.setGoalEnv(goalId, env);

      final kappa = program.labels['p/1']!;
      rt.gq.enqueue(GoalRef(goalId, kappa));

      // Run
      print('Running p(R?) with unbound reader - should suspend...');
      final result = sched.drain(maxCycles: 100);

      print('Goals executed: ${result.length}');
      print('ROQ has suspension: ${!rt.roq.isEmpty(rid)}');

      // Should suspend, not fail
      expect(!rt.roq.isEmpty(rid), isTrue, reason: 'Should have suspension note');

      print('✓ Suspension works correctly\n');
    });
  });
}
