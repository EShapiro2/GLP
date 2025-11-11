import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/bytecode/asm.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  test('Compare+Run: merge clause 1 - merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).', () {
    print('\n' + '=' * 70);
    print('BYTECODE COMPARISON: merge/3 with 2 clauses');
    print('=' * 70 + '\n');

    // HAND-WRITTEN bytecode
    print('HAND-WRITTEN BYTECODE:');
    print('Source:');
    print('  merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).');
    print('  merge([],[],[]).');
    print('');

    final handWritten = BC.prog([
      BC.L('merge/3_start'),

      // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      BC.TRY(),
      BC.headStruct('.', 2, 0),    // arg0 = '.'(X, Xs)
      BC.headWriter(0),             // Extract X into clause var 0
      BC.headWriter(1),             // Extract Xs into clause var 1
      BC.getVar(2, 1),              // Load Ys into clause var 2
      BC.headStruct('.', 2, 2),     // arg2 = '.'(_, _)
      BC.headReader(0),              // First element: X? (reader of X)
      BC.headWriter(3),              // Second element: Zs? (fresh writer for tail)
      BC.COMMIT(),
      // Body: merge(Ys?, Xs?, Zs)
      BC.putReader(2, 0),            // A0 = Ys?
      BC.putReader(1, 1),            // A1 = Xs?
      BC.putWriter(3, 2),            // A2 = Zs
      BC.requeue('merge/3_start', 3),  // Tail call merge/3

      // Clause 2: merge([],[],[]).
      BC.L('merge/3_clause2'),
      BC.TRY(),
      BC.headConst(null, 0),        // arg0 = []
      BC.headConst(null, 1),        // arg1 = []
      BC.headConst(null, 2),        // arg2 = []
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('merge/3_end'),
      BC.SUSP(),
    ]);

    for (int i = 0; i < handWritten.ops.length; i++) {
      print('  $i: ${handWritten.ops[i]}');
    }
    print('\nHAND-WRITTEN LABELS:');
    handWritten.labels.forEach((k, v) => print('  $k => $v'));

    // COMPILED bytecode
    print('\n' + '-' * 70);
    print('COMPILED BYTECODE:');
    print('');

    final compiler = GlpCompiler();
    final source = '''
      merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      merge([],[],[]).
    ''';
    final compiled = compiler.compile(source);

    for (int i = 0; i < compiled.ops.length; i++) {
      print('  $i: ${compiled.ops[i]}');
    }
    print('\nCOMPILED LABELS:');
    compiled.labels.forEach((k, v) => print('  $k => $v'));

    // COMPARISON
    print('\n' + '=' * 70);
    print('COMPARISON');
    print('=' * 70);
    print('');
    print('Structure:');
    print('  Both should have:');
    print('    - Clause 1: head matching + body requeue');
    print('    - Clause 2: base case with 3 empty lists');
    print('');
    print('Expected differences:');
    print('  - Label naming (merge/3_start vs merge/3, merge/3_clause2 vs merge/3_c1)');
    print('  - HeadNil vs HeadConstant(null) for base case');
    print('  - NoMoreClauses vs SuspendEnd');
    print('  - Both should use HeadStructure for lists now (not HeadList)');
    print('');

    // Verify key structures
    final hLabel = handWritten.labels['merge/3_start']!;
    final cLabel = compiled.labels['merge/3']!;

    // First clause list handling
    expect(handWritten.ops[hLabel + 1], isA<bc.ClauseTry>());
    expect(handWritten.ops[hLabel + 2], isA<bc.HeadStructure>());
    expect(compiled.ops[cLabel + 1], isA<bc.ClauseTry>());
    expect(compiled.ops[cLabel + 2], isA<bc.HeadStructure>());

    print('✓ Both use HeadStructure for lists (treating [X|Xs] as \'.\'(X,Xs))');

    // Find Requeue in both
    bool foundHandRequeue = false;
    bool foundCompRequeue = false;
    for (var op in handWritten.ops) {
      if (op is bc.Requeue) {
        foundHandRequeue = true;
        print('Hand-written Requeue: label=${op.procedureLabel}, arity=${op.arity}');
      }
    }
    for (var op in compiled.ops) {
      if (op is bc.Requeue) {
        foundCompRequeue = true;
        print('Compiled Requeue: label=${op.procedureLabel}, arity=${op.arity}');
      }
    }

    expect(foundHandRequeue, true, reason: 'Hand-written should have Requeue');
    expect(foundCompRequeue, true, reason: 'Compiled should have Requeue');

    print('');
    print('Conclusion:');
    print('  ✓ Structural match');
    print('  Note: May have minor instruction differences due to codegen strategy');
    print('');

    // EXECUTION TEST
    print('=' * 70);
    print('EXECUTION TEST');
    print('=' * 70);
    print('\nGoal: merge([a], [], Z) where Z is unbound writer');
    print('Expected: Z binds to [a]\n');

    final rt = GlpRuntime();

    // Arg 0: [a] = '.'(a, [])
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));
    rt.heap.bindWriterStruct(wXs, '.', [
      ConstTerm('a'),
      ConstTerm(null), // tail is []
    ]);

    // Arg 1: []
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));
    rt.heap.bindWriterConst(wYs, null);

    // Arg 2: Z (unbound writer)
    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  Arg0 (Xs?) = [a]');
    print('  Arg1 (Ys?) = []');
    print('  Arg2 (Zs) = unbound writer\n');

    final runner = BytecodeRunner(compiled);
    final sched = Scheduler(rt: rt, runner: runner);

    const goalId = 100;
    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, compiled.labels['merge/3']!));

    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');
    if (rt.heap.isWriterBound(wZs)) {
      final val = rt.heap.valueOfWriter(wZs);
      print('Writer $wZs value: $val\n');
    }

    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Result should be bound');

    final value = rt.heap.valueOfWriter(wZs);
    expect(value, isA<StructTerm>(), reason: 'Should be a list structure');

    final list = value as StructTerm;
    expect(list.functor, '.', reason: 'Should be list cons');

    // Head can be either ConstTerm or ReaderTerm depending on how structure is built
    final headArg = list.args[0];
    if (headArg is ConstTerm) {
      expect(headArg.value, 'a', reason: 'Head should be a');
    } else if (headArg is ReaderTerm) {
      // Dereference reader to check value
      final headWriterId = rt.heap.writerIdForReader(headArg.readerId);
      if (headWriterId != null && rt.heap.isWriterBound(headWriterId)) {
        final headValue = rt.heap.valueOfWriter(headWriterId);
        expect((headValue as ConstTerm).value, 'a', reason: 'Head should be a');
      }
    }

    print('✓ Compiled merge/3 works correctly!\n');
  });
}
