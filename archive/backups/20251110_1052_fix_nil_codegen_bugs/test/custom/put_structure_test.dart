import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('WAM-style put_structure: Build list [a,b] in BODY', () {
    print('\n' + '=' * 70);
    print('WAM-STYLE STRUCTURE CREATION IN BODY');
    print('Goal: build_list(Result)');
    print('Clause: build_list([a,b]).');
    print('Uses: put_structure, set_constant instructions');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Create result writer
    const wResult = 10;
    const rResult = 11;
    rt.heap.addWriter(WriterCell(wResult, rResult));
    rt.heap.addReader(ReaderCell(rResult));

    print('HEAP SETUP:');
    print('  Result: W$wResult/R$rResult (unbound)');
    print('');

    // Program: build_list(Result) :- Result = [a].
    // [a] = .(a, [])
    final prog = BC.prog([
      BC.L('build_list/1_start'),
      BC.TRY(),
      BC.COMMIT(),

      // BODY: Build structure .(a, [])
      BC.putStructure('.', 2, 0),    // Create structure .(_, _) for A0
      BC.setConst('a'),               // First arg = 'a'
      BC.setConst(null),              // Second arg = [] (null)

      BC.PROCEED(),
    ]);

    final runner = BytecodeRunner(prog);

    print('=' * 70);
    print('EXECUTION TRACE');
    print('=' * 70 + '\n');

    print('--- GOAL 100: build_list(Result) ---');
    const goal1 = 100;
    final env1 = CallEnv(writers: {0: wResult});
    rt.setGoalEnv(goal1, env1);

    final cx1 = RunnerContext(
      rt: rt,
      goalId: goal1,
      kappa: 0,
      env: env1,
      reductionBudget: 100,
    );

    final result = runner.runWithStatus(cx1);
    print('Execution result: $result');
    print('Reductions used: ${cx1.reductionsUsed}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);

    print('Result bound: ${rt.heap.isWriterBound(wResult)}');
    if (rt.heap.isWriterBound(wResult)) {
      final value = rt.heap.valueOfWriter(wResult);
      print('Result value: $value');

      // Verify it's a structure
      expect(value, isA<StructTerm>(), reason: 'Result should be a StructTerm');
      if (value is StructTerm) {
        expect(value.functor, '.', reason: 'Functor should be "."');
        expect(value.args.length, 2, reason: 'Should have 2 arguments');

        // Check first arg is 'a'
        final arg0 = value.args[0];
        expect(arg0, isA<ConstTerm>(), reason: 'First arg should be ConstTerm');
        if (arg0 is ConstTerm) {
          expect(arg0.value, 'a', reason: 'First element should be "a"');
        }

        // Check second arg is []
        final arg1 = value.args[1];
        expect(arg1, isA<ConstTerm>(), reason: 'Second arg should be ConstTerm');
        if (arg1 is ConstTerm) {
          expect(arg1.value, null, reason: 'Tail should be [] (null)');
        }

        print('✓ Structure correctly created: .(a, [])');
      }
    } else {
      fail('Result should be bound after execution');
    }

    print('');
    print('✓ WAM-style put_structure test complete!');
  });

  test('WAM-style put_structure: Build structure with writer variable', () {
    print('\n' + '=' * 70);
    print('STRUCTURE WITH WRITER VARIABLE IN BODY');
    print('Goal: build_pair(Result)');
    print('Clause: build_pair(pair(a, Tail?)).');
    print('Structure: pair(a, WriterTerm)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Create result writer
    const wResult = 10;
    const rResult = 11;
    rt.heap.addWriter(WriterCell(wResult, rResult));
    rt.heap.addReader(ReaderCell(rResult));

    print('HEAP SETUP:');
    print('  Result: W$wResult/R$rResult (unbound)');
    print('');

    // Program: build_pair(Result) :- Result = pair(a, Tail).
    final prog = BC.prog([
      BC.L('build_pair/1_start'),
      BC.TRY(),
      BC.COMMIT(),

      // BODY: Build outer structure pair(a, Tail?)
      BC.putStructure('pair', 2, 0),  // Create pair(_, _) for A0
      BC.setConst('a'),                // First arg = 'a'
      BC.setWriter(0),                 // Second arg = Tail (new writer variable)

      BC.PROCEED(),
    ]);

    final runner = BytecodeRunner(prog);

    print('--- GOAL 200: build_pair(Result) ---');
    const goal2 = 200;
    final env2 = CallEnv(writers: {0: wResult});
    rt.setGoalEnv(goal2, env2);

    final cx2 = RunnerContext(
      rt: rt,
      goalId: goal2,
      kappa: 0,
      env: env2,
      reductionBudget: 100,
    );

    final result2 = runner.runWithStatus(cx2);
    print('Execution result: $result2');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);

    print('Result bound: ${rt.heap.isWriterBound(wResult)}');
    if (rt.heap.isWriterBound(wResult)) {
      final value = rt.heap.valueOfWriter(wResult);
      print('Result value: $value');

      // Verify it's a structure with a writer in second position
      expect(value, isA<StructTerm>(), reason: 'Result should be a StructTerm');
      if (value is StructTerm) {
        expect(value.functor, 'pair', reason: 'Functor should be "pair"');
        expect(value.args.length, 2, reason: 'Should have 2 arguments');

        final arg0 = value.args[0];
        expect(arg0, isA<ConstTerm>(), reason: 'First arg should be ConstTerm');
        if (arg0 is ConstTerm) {
          expect(arg0.value, 'a', reason: 'First element should be "a"');
        }

        final arg1 = value.args[1];
        expect(arg1, isA<WriterTerm>(), reason: 'Second arg should be WriterTerm (unbound tail)');
        if (arg1 is WriterTerm) {
          print('✓ WriterTerm ID: ${arg1.writerId}');
          // Verify the writer exists and is unbound
          final tailWriter = rt.heap.writer(arg1.writerId);
          expect(tailWriter, isNotNull, reason: 'Writer should exist in heap');
          expect(rt.heap.isWriterBound(arg1.writerId), false,
            reason: 'Tail writer should be unbound');
        }

        print('✓ Structure correctly created: pair(a, WriterTerm)');
      }
    }

    print('');
    print('✓ Structure with writer variable test complete!');
  });
}
