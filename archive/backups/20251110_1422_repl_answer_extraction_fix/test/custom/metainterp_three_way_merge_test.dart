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
  test('Metainterpreter three-way merge: run((merge([1,2],[3],Xs), merge([a,b],[c],Ys?), merge(Xs?,Ys?,Zs)))', () {
    print('\n' + '=' * 70);
    print('METAINTERPRETER TEST: THREE-WAY MERGE');
    print('Object program:');
    print('  clause(merge([X|Xs],Ys,[X?|Zs?]), merge(Ys?,Xs?,Zs)).');
    print('  clause(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).');
    print('  clause(merge([],[],[]), true).');
    print('Metainterpreter:');
    print('  run(true).');
    print('  run((A,B)) :- run(A?), run(B?).');
    print('  run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: run((merge([1,2],[3],Xs), merge([a,b],[c],Ys), merge(Xs?,Ys?,Zs)))');
    print('Expected:');
    print('  Xs = [1,2,3]');
    print('  Ys = [a,b,c]');
    print('  Zs = merge of [1,2,3] and [a,b,c]');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Setup: Writers for Xs, Ys, Zs
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('HEAP SETUP:');
    print('  Writer $wXs (Xs) paired with Reader $rXs (Xs?)');
    print('  Writer $wYs (Ys) paired with Reader $rYs (Ys?)');
    print('  Writer $wZs (Zs) paired with Reader $rZs (Zs?)');
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

    // Build [1,2] = [1|[2|[]]]
    const wList12 = 10;
    const rList12 = 11;
    rt.heap.addWriter(WriterCell(wList12, rList12));
    rt.heap.addReader(ReaderCell(rList12));

    // Build the tail [2|[]]
    const wTail12 = 100;
    const rTail12 = 101;
    rt.heap.addWriter(WriterCell(wTail12, rTail12));
    rt.heap.addReader(ReaderCell(rTail12));
    rt.heap.bindWriterStruct(wTail12, '[|]', [ConstTerm('2'), ConstTerm('[]')]);

    rt.heap.bindWriterStruct(wList12, '[|]', [
      ConstTerm('1'),
      WriterTerm(wTail12),
    ]);

    // Build [3]
    const wList3 = 12;
    const rList3 = 13;
    rt.heap.addWriter(WriterCell(wList3, rList3));
    rt.heap.addReader(ReaderCell(rList3));
    rt.heap.bindWriterStruct(wList3, '[|]', [ConstTerm('3'), ConstTerm('[]')]);

    // Build [a,b]
    const wListAB = 14;
    const rListAB = 15;
    rt.heap.addWriter(WriterCell(wListAB, rListAB));
    rt.heap.addReader(ReaderCell(rListAB));

    // Build the tail [b|[]]
    const wTailAB = 102;
    const rTailAB = 103;
    rt.heap.addWriter(WriterCell(wTailAB, rTailAB));
    rt.heap.addReader(ReaderCell(rTailAB));
    rt.heap.bindWriterStruct(wTailAB, '[|]', [ConstTerm('b'), ConstTerm('[]')]);

    rt.heap.bindWriterStruct(wListAB, '[|]', [
      ConstTerm('a'),
      WriterTerm(wTailAB),
    ]);

    // Build [c]
    const wListC = 16;
    const rListC = 17;
    rt.heap.addWriter(WriterCell(wListC, rListC));
    rt.heap.addReader(ReaderCell(rListC));
    rt.heap.bindWriterStruct(wListC, '[|]', [ConstTerm('c'), ConstTerm('[]')]);

    // Build merge([1,2],[3],Xs)
    const wMerge1 = 18;
    const rMerge1 = 19;
    rt.heap.addWriter(WriterCell(wMerge1, rMerge1));
    rt.heap.addReader(ReaderCell(rMerge1));
    rt.heap.bindWriterStruct(wMerge1, 'merge', [
      WriterTerm(wList12),
      WriterTerm(wList3),
      WriterTerm(wXs),
    ]);

    // Build merge([a,b],[c],Ys)
    const wMerge2 = 20;
    const rMerge2 = 21;
    rt.heap.addWriter(WriterCell(wMerge2, rMerge2));
    rt.heap.addReader(ReaderCell(rMerge2));
    rt.heap.bindWriterStruct(wMerge2, 'merge', [
      WriterTerm(wListAB),
      WriterTerm(wListC),
      WriterTerm(wYs),
    ]);

    // Build merge(Xs?,Ys?,Zs)
    const wMerge3 = 22;
    const rMerge3 = 23;
    rt.heap.addWriter(WriterCell(wMerge3, rMerge3));
    rt.heap.addReader(ReaderCell(rMerge3));
    rt.heap.bindWriterStruct(wMerge3, 'merge', [
      ReaderTerm(rXs),
      ReaderTerm(rYs),
      WriterTerm(wZs),
    ]);

    // Build (merge([1,2],[3],Xs), merge([a,b],[c],Ys))
    const wConj12 = 24;
    const rConj12 = 25;
    rt.heap.addWriter(WriterCell(wConj12, rConj12));
    rt.heap.addReader(ReaderCell(rConj12));
    rt.heap.bindWriterStruct(wConj12, ',', [WriterTerm(wMerge1), WriterTerm(wMerge2)]);

    // Build ((merge([1,2],[3],Xs), merge([a,b],[c],Ys)), merge(Xs?,Ys?,Zs))
    const wConj = 26;
    const rConj = 27;
    rt.heap.addWriter(WriterCell(wConj, rConj));
    rt.heap.addReader(ReaderCell(rConj));
    rt.heap.bindWriterStruct(wConj, ',', [WriterTerm(wConj12), WriterTerm(wMerge3)]);

    print('STRUCTURES:');
    print('  [1,2] = writer $wList12');
    print('  [3] = writer $wList3');
    print('  [a,b] = writer $wListAB');
    print('  [c] = writer $wListC');
    print('  merge([1,2],[3],Xs) = writer $wMerge1');
    print('  merge([a,b],[c],Ys) = writer $wMerge2');
    print('  merge(Xs?,Ys?,Zs) = writer $wMerge3');
    print('  conjunction = reader $rConj');
    print('');

    // Start goal: run((merge([1,2],[3],Xs), merge([a,b],[c],Ys), merge(Xs?,Ys?,Zs)))
    const goalId = 100;
    final env = CallEnv(readers: {0: rConj});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['run/1']!));

    print('Starting goal at PC ${prog.labels['run/1']}');
    print('');

    final ran = sched.drain(maxCycles: 1000);

    print('');
    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}');
    print('Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}');
    print('Zs (W$wZs) bound: ${rt.heap.isWriterBound(wZs)}');

    // Helper function to dereference and print list contents
    void printList(String name, int writerId) {
      if (!rt.heap.isWriterBound(writerId)) {
        print('$name: UNBOUND');
        return;
      }

      final value = rt.heap.valueOfWriter(writerId);
      print('$name value: $value');

      // Walk the list and dereference all elements
      var current = value;
      final elements = <String>[];
      var depth = 0;

      while (current is StructTerm && current.functor == '[|]' && depth < 20) {
        depth++;
        if (current.args.isEmpty) break;

        // Get head element
        final head = current.args[0];
        if (head is ReaderTerm) {
          final wid = rt.heap.writerIdForReader(head.readerId);
          if (wid != null && rt.heap.isWriterBound(wid)) {
            final headValue = rt.heap.valueOfWriter(wid);
            elements.add(headValue.toString());
          } else {
            elements.add('R${head.readerId}(unbound)');
          }
        } else if (head is ConstTerm) {
          elements.add(head.value.toString());
        } else {
          elements.add(head.toString());
        }

        // Get tail
        if (current.args.length > 1) {
          final tail = current.args[1];
          if (tail is ReaderTerm) {
            final wid = rt.heap.writerIdForReader(tail.readerId);
            if (wid != null && rt.heap.isWriterBound(wid)) {
              current = rt.heap.valueOfWriter(wid);
              if (current is ConstTerm && current.value == '[]') {
                break; // End of list
              }
            } else {
              elements.add('... R${tail.readerId}(unbound)');
              break;
            }
          } else if (tail is ConstTerm && tail.value == '[]') {
            break; // End of list
          } else {
            current = tail;
          }
        } else {
          break;
        }
      }

      print('  $name = [${elements.join(', ')}]');
    }

    printList('Xs', wXs);
    printList('Ys', wYs);
    printList('Zs', wZs);

    print('');
    print('✓ Three-way merge test complete');
  });
}
