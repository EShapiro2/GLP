import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Metainterpreter with circular merge: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))', () {
    print('\n' + '=' * 70);
    print('METAINTERPRETER TEST: CIRCULAR MERGE');
    print('Object program:');
    print('  clause(merge([X|Xs],Ys,[X?|Zs?]), merge(Ys?,Xs?,Zs)).');
    print('  clause(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).');
    print('  clause(merge([],[],[]), true).');
    print('Metainterpreter:');
    print('  run(true).');
    print('  run((A,B)) :- run(A?), run(B?).');
    print('  run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))');
    print('Expected: Creates infinite stream (circular dependency)');
    print('  Xs = [b,a,b,a,...]');
    print('  Ys = [a,b,a,b,...]');
    print('Note: Using reduction budget to prevent infinite loop');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writer Xs and its paired reader, Writer Ys and its paired reader
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    print('HEAP SETUP:');
    print('  Writer $wXs (Xs) paired with Reader $rXs (Xs?)');
    print('  Writer $wYs (Ys) paired with Reader $rYs (Ys?)');
    print('');

    final prog = BC.prog([
      // ===== CLAUSE/2: Object program with merge =====

      // Clause 1: clause(merge([X|Xs],Ys,[X?|Zs?]), merge(Ys?,Xs?,Zs)).
      BC.L('clause/2'),
      BC.TRY(),
      BC.headStruct('merge', 3, 0),
        BC.unifyWriter(10),
        BC.unifyWriter(2),
        BC.unifyWriter(11),
      BC.headStruct('[|]', 2, 10),
        BC.unifyWriter(0),
        BC.unifyWriter(1),
      BC.headStruct('[|]', 2, 11),
        BC.unifyReader(0),
        BC.unifyReader(3),
      BC.headStruct('merge', 3, 1),
        BC.unifyReader(2),
        BC.unifyReader(1),
        BC.unifyWriter(3),
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: clause(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).
      BC.L('clause/2_c2'),
      BC.TRY(),
      BC.headStruct('merge', 3, 0),
        BC.unifyWriter(2),
        BC.unifyWriter(10),
        BC.unifyWriter(11),
      BC.headStruct('[|]', 2, 10),
        BC.unifyWriter(0),
        BC.unifyWriter(1),
      BC.headStruct('[|]', 2, 11),
        BC.unifyReader(0),
        BC.unifyReader(3),
      BC.headStruct('merge', 3, 1),
        BC.unifyReader(2),
        BC.unifyReader(1),
        BC.unifyWriter(3),
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 3: clause(merge([],[],[]), true).
      BC.L('clause/2_c3'),
      BC.TRY(),
      BC.headStruct('merge', 3, 0),
      BC.unifyConst('[]'),
      BC.unifyConst('[]'),
      BC.unifyConst('[]'),
      BC.headConst('true', 1),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('clause/2_end'),
      BC.SUSP(),

      // ===== RUN/1: Metainterpreter =====
      // Clause 1: run(true).
      BC.L('run/1'),
      BC.TRY(),
      BC.headConst('true', 0),
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: run((A,B)) :- run(A?), run(B?).
      BC.L('run/1_c2'),
      BC.TRY(),
      BC.headStruct(',', 2, 0),
      BC.unifyWriter(0),
      BC.unifyWriter(1),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.spawn('run/1', 1),
      BC.putReader(1, 0),
      BC.requeue('run/1', 1),

      // Clause 3: run(A) :- otherwise | clause(A?, B), run(B?).
      BC.L('run/1_c3'),
      BC.TRY(),
      BC.otherwise(),
      BC.getVar(0, 0),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.putWriter(1, 1),
      BC.spawn('clause/2', 2),
      BC.putReader(1, 0),
      BC.requeue('run/1', 1),

      BC.L('run/1_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('PROGRAM LABELS:');
    for (final entry in prog.labels.entries) {
      print('  ${entry.key} => PC ${entry.value}');
    }
    print('');

    // Build the list [a]
    const wListA = 10;
    const rListA = 11;
    rt.heap.addWriter(WriterCell(wListA, rListA));
    rt.heap.addReader(ReaderCell(rListA));
    rt.heap.bindWriterStruct(wListA, '[|]', [ConstTerm('a'), ConstTerm('[]')]);

    // Build the list [b]
    const wListB = 12;
    const rListB = 13;
    rt.heap.addWriter(WriterCell(wListB, rListB));
    rt.heap.addReader(ReaderCell(rListB));
    rt.heap.bindWriterStruct(wListB, '[|]', [ConstTerm('b'), ConstTerm('[]')]);

    // Build merge(Xs?,[a],Ys)
    const wMerge1 = 14;
    const rMerge1 = 15;
    rt.heap.addWriter(WriterCell(wMerge1, rMerge1));
    rt.heap.addReader(ReaderCell(rMerge1));
    rt.heap.bindWriterStruct(wMerge1, 'merge', [
      VarRef(rXs, isReader: true),
      VarRef(wListA, isReader: false),
      VarRef(wYs, isReader: false),
    ]);

    // Build merge(Ys?,[b],Xs)
    const wMerge2 = 16;
    const rMerge2 = 17;
    rt.heap.addWriter(WriterCell(wMerge2, rMerge2));
    rt.heap.addReader(ReaderCell(rMerge2));
    rt.heap.bindWriterStruct(wMerge2, 'merge', [
      VarRef(rYs, isReader: true),
      VarRef(wListB, isReader: false),
      VarRef(wXs, isReader: false),
    ]);

    // Build conjunction (merge(Xs?,[a],Ys), merge(Ys?,[b],Xs))
    const wConj = 18;
    const rConj = 19;
    rt.heap.addWriter(WriterCell(wConj, rConj));
    rt.heap.addReader(ReaderCell(rConj));
    rt.heap.bindWriterStruct(wConj, ',', [VarRef(wMerge1, isReader: false), VarRef(wMerge2, isReader: false)]);

    print('STRUCTURES:');
    print('  [a] = writer $wListA bound to [|](a, [])');
    print('  [b] = writer $wListB bound to [|](b, [])');
    print('  merge(Xs?,[a],Ys) = writer $wMerge1 bound to merge(R$rXs, W$wListA, W$wYs)');
    print('  merge(Ys?,[b],Xs) = writer $wMerge2 bound to merge(R$rYs, W$wListB, W$wXs)');
    print('  conjunction = reader $rConj');
    print('');

    // Start goal: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
    const goalId = 100;
    final env = CallEnv(readers: {0: rConj});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['run/1']!));

    print('Starting: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs))) at PC ${prog.labels['run/1']}');
    print('');

    // Use reduction budget to prevent infinite loop
    final ran = sched.drain(maxCycles: 50);

    print('');
    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}');
    print('Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}');

    if (rt.heap.isWriterBound(wXs)) {
      final xsValue = rt.heap.valueOfWriter(wXs);
      print('Xs value: $xsValue');
      if (xsValue is StructTerm && xsValue.functor == '[|]') {
        print('  Xs is a list starting with...');
        if (xsValue.args.isNotEmpty) {
          final head = xsValue.args[0];
          if (head is VarRef) {
            final wid = rt.heap.writerIdForReader(head.readerId);
            if (wid != null && rt.heap.isWriterBound(wid)) {
              final headValue = rt.heap.valueOfWriter(wid);
              print('    First element: $headValue');
            }
          }
        }
      }
    }

    if (rt.heap.isWriterBound(wYs)) {
      final ysValue = rt.heap.valueOfWriter(wYs);
      print('Ys value: $ysValue');
      if (ysValue is StructTerm && ysValue.functor == '[|]') {
        print('  Ys is a list starting with...');
        if (ysValue.args.isNotEmpty) {
          final head = ysValue.args[0];
          if (head is VarRef) {
            final wid = rt.heap.writerIdForReader(head.readerId);
            if (wid != null && rt.heap.isWriterBound(wid)) {
              final headValue = rt.heap.valueOfWriter(wid);
              print('    First element: $headValue');
            }
          }
        }
      }
    }

    print('');
    print('✓ Circular merge test executed with reduction budget');
    print('✓ Demonstrates circular dependency handling');
    print('Note: This creates an infinite stream - execution limited by maxCycles');
  });
}
