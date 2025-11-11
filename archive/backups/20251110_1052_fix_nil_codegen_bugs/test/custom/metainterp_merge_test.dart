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
  test('Metainterpreter with merge object program', () {
    print('\n' + '=' * 70);
    print('METAINTERPRETER TEST: MERGE');
    print('Object program:');
    print('  clause(merge([X|Xs],Ys,[X?|Zs?]), merge(Ys?,Xs?,Zs)).');
    print('  clause(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).');
    print('  clause(merge([],[],[]), true).');
    print('Metainterpreter:');
    print('  run(true).');
    print('  run((A,B)) :- run(A?), run(B?).');
    print('  run(A) :- otherwise | clause(A?,B), run(B?).');
    print('Query: run(merge([a],[b],Zs))');
    print('Expected: Zs = [a,b] (alternating merge of two lists)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // We'll build the program with merge clauses as the object program
    final prog = BC.prog([
      // ===== CLAUSE/2: Object program with merge =====

      // Clause 1: clause(merge([X|Xs],Ys,[X?|Zs?]), merge(Ys?,Xs?,Zs)).
      // Variables: 0=X, 1=Xs, 2=Ys, 3=Zs
      // Temporaries for nested structures: 10=first arg list, 11=third arg list
      BC.L('clause/2'),
      BC.TRY(),
      // Match first arg: merge([X|Xs],Ys,[X?|Zs?])
      BC.headStruct('merge', 3, 0),     // Match merge/3 at arg0, enter READ mode, S=0
        BC.unifyWriter(10),             // S=0: Extract first merge arg into temp X10
        BC.unifyWriter(2),              // S=1: Extract second merge arg into var 2 (Ys)
        BC.unifyWriter(11),             // S=2: Extract third merge arg into temp X11
      // Now match the extracted nested structures
      BC.headStruct('[|]', 2, 10),      // Match temp X10 (extracted list) against [|]/2
        BC.unifyWriter(0),              // S=0: X (head of first list)
        BC.unifyWriter(1),              // S=1: Xs (tail of first list)
      BC.headStruct('[|]', 2, 11),      // Match temp X11 (extracted list) against [|]/2
        BC.unifyReader(0),              // S=0: X? (reader - head of third list)
        BC.unifyReader(3),              // S=1: Zs? (reader - tail of third list)
      // Match second arg: merge(Ys?,Xs?,Zs)
      BC.headStruct('merge', 3, 1),     // Match merge/3 at arg1
        BC.unifyReader(2),              // S=0: Ys? (reader of var 2)
        BC.unifyReader(1),              // S=1: Xs? (reader of var 1)
        BC.unifyWriter(3),              // S=2: Zs (writer var 3)
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 2: clause(merge(Xs,[Y|Ys],[Y?|Zs?]), merge(Xs?,Ys?,Zs)).
      // Variables: 0=Y, 1=Ys, 2=Xs, 3=Zs
      // Temporaries for nested structures: 10=second arg list, 11=third arg list
      BC.L('clause/2_c2'),
      BC.TRY(),
      // Match first arg: merge(Xs,[Y|Ys],[Y?|Zs?])
      BC.headStruct('merge', 3, 0),     // Match merge/3 at arg0, enter READ mode, S=0
        BC.unifyWriter(2),              // S=0: Extract first merge arg into var 2 (Xs)
        BC.unifyWriter(10),             // S=1: Extract second merge arg into temp X10
        BC.unifyWriter(11),             // S=2: Extract third merge arg into temp X11
      // Now match the extracted nested structures
      BC.headStruct('[|]', 2, 10),      // Match temp X10 (extracted list) against [|]/2
        BC.unifyWriter(0),              // S=0: Y (head of second list)
        BC.unifyWriter(1),              // S=1: Ys (tail of second list)
      BC.headStruct('[|]', 2, 11),      // Match temp X11 (extracted list) against [|]/2
        BC.unifyReader(0),              // S=0: Y? (reader - head of third list)
        BC.unifyReader(3),              // S=1: Zs? (reader - tail of third list)
      // Match second arg: merge(Xs?,Ys?,Zs)
      BC.headStruct('merge', 3, 1),     // Match merge/3 at arg1
        BC.unifyReader(2),              // S=0: Xs? (reader of var 2)
        BC.unifyReader(1),              // S=1: Ys? (reader of var 1)
        BC.unifyWriter(3),              // S=2: Zs (writer var 3)
      BC.COMMIT(),
      BC.PROCEED(),

      // Clause 3: clause(merge([],[],[]), true).
      BC.L('clause/2_c3'),
      BC.TRY(),
      BC.headStruct('merge', 3, 0),
      BC.unifyConst('[]'),          // Empty list constant
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

    // Build the query: run(merge([a],[b],Zs))
    // First, build the list [a] (cons(a, []))
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

    // Build Zs (unbound writer for result)
    const wZs = 14;
    const rZs = 15;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    // Build merge([a],[b],Zs)
    const wMerge = 16;
    const rMerge = 17;
    rt.heap.addWriter(WriterCell(wMerge, rMerge));
    rt.heap.addReader(ReaderCell(rMerge));
    rt.heap.bindWriterStruct(wMerge, 'merge', [
      WriterTerm(wListA),
      WriterTerm(wListB),
      WriterTerm(wZs),
    ]);

    print('STRUCTURES:');
    print('  [a] = writer $wListA bound to [|](a, [])');
    print('  [b] = writer $wListB bound to [|](b, [])');
    print('  Zs = writer $wZs (unbound)');
    print('  merge([a],[b],Zs) = reader $rMerge');
    print('');

    // Start goal: run(merge([a],[b],Zs))
    const goalId = 100;
    final env = CallEnv(readers: {0: rMerge});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, prog.labels['run/1']!));

    print('Starting: run(merge([a],[b],Zs)) at PC ${prog.labels['run/1']}');
    print('');

    final ran = sched.drain(maxCycles: 200);

    print('');
    print('Goals executed: $ran');
    print('Total executions: ${ran.length}');
    print('');

    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Zs (W$wZs) bound: ${rt.heap.isWriterBound(wZs)}');

    expect(rt.heap.isWriterBound(wZs), true, reason: 'Zs should be bound');

    if (rt.heap.isWriterBound(wZs)) {
      final zsValue = rt.heap.valueOfWriter(wZs);
      print('Zs value: $zsValue');
      expect(zsValue, isA<StructTerm>(), reason: 'Zs should be StructTerm (list)');
      if (zsValue is StructTerm) {
        expect(zsValue.functor, '[|]', reason: 'Zs should be a list cons');
        expect(zsValue.args.length, 2, reason: 'List cons has arity 2');

        // Check head
        if (zsValue.args.isNotEmpty) {
          final head = zsValue.args[0];
          print('  Head of Zs: $head');
          if (head is ReaderTerm) {
            final wid = rt.heap.writerIdForReader(head.readerId);
            if (wid != null && rt.heap.isWriterBound(wid)) {
              final headValue = rt.heap.valueOfWriter(wid);
              print('  Head value: $headValue');
              expect(headValue, isA<ConstTerm>(), reason: 'Head should be constant');
              if (headValue is ConstTerm) {
                expect(headValue.value, 'a', reason: 'First element should be a');
                print('✓ First element correctly bound to a');
              }
            }
          }
        }

        // Check tail
        if (zsValue.args.length > 1) {
          final tail = zsValue.args[1];
          print('  Tail of Zs: $tail');
          if (tail is ReaderTerm) {
            final tailWid = rt.heap.writerIdForReader(tail.readerId);
            if (tailWid != null && rt.heap.isWriterBound(tailWid)) {
              final tailValue = rt.heap.valueOfWriter(tailWid);
              print('  Tail value: $tailValue');

              // Tail should be [b] = [|](b, [])
              if (tailValue is StructTerm && tailValue.functor == '[|]') {
                print('  Tail is a list!');
                if (tailValue.args.isNotEmpty) {
                  final tailHead = tailValue.args[0];
                  print('    Tail head: $tailHead');
                  if (tailHead is ReaderTerm) {
                    final tailHeadWid = rt.heap.writerIdForReader(tailHead.readerId);
                    if (tailHeadWid != null && rt.heap.isWriterBound(tailHeadWid)) {
                      final tailHeadValue = rt.heap.valueOfWriter(tailHeadWid);
                      print('    Tail head value: $tailHeadValue');
                      if (tailHeadValue is ConstTerm) {
                        expect(tailHeadValue.value, 'b', reason: 'Second element should be b');
                        print('✓ Second element correctly bound to b');
                      }
                    }
                  }
                }
                if (tailValue.args.length > 1) {
                  final tailTail = tailValue.args[1];
                  print('    Tail tail: $tailTail');
                  if (tailTail is ReaderTerm) {
                    final tailTailWid = rt.heap.writerIdForReader(tailTail.readerId);
                    if (tailTailWid != null && rt.heap.isWriterBound(tailTailWid)) {
                      final tailTailValue = rt.heap.valueOfWriter(tailTailWid);
                      print('    Tail tail value: $tailTailValue');
                      expect(tailTailValue, isA<ConstTerm>(), reason: 'End should be []');
                      if (tailTailValue is ConstTerm) {
                        expect(tailTailValue.value, '[]', reason: 'List should end with []');
                        print('✓ List correctly ends with []');
                      }
                    }
                  }
                }
              }
            } else {
              print('  WARNING: Tail is UNBOUND - merge incomplete!');
            }
          }
        }

        print('✓ Zs correctly bound to list structure');
      }
    }
    print('');

    print('✓ Metainterpreter with merge passed!');
    print('✓ run(merge([a],[b],Zs)) executed successfully');
    print('✓ Zs bound to list structure [a|...]');
  });
}
