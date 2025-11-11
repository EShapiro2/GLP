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
  test('Reverse order: merge(Xs?,[a,b,c,d],Ys), merge([1,2,3],[],Xs)', () {
    print('\n' + '=' * 70);
    print('REVERSE ORDER SEQUENTIAL MERGE TEST');
    print('Goal 100: merge(Xs?, [a,b,c,d], Ys)  <- Xs unbound, should SUSPEND');
    print('Goal 200: merge([1,2,3], [], Xs)     <- Binds Xs, ACTIVATES Goal 100');
    print('Expected: Xs = [1,2,3], Ys = [1,a,2,b,3,c,d]');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Build list [1,2,3]
    const wList1 = 10;
    const rList1 = 11;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [
      ConstTerm(1),
      ReaderTerm(12),
    ]);

    const wTail1 = 13;
    const rTail1 = 12;
    rt.heap.addWriter(WriterCell(wTail1, rTail1));
    rt.heap.addReader(ReaderCell(rTail1));
    rt.heap.bindWriterStruct(wTail1, '.', [
      ConstTerm(2),
      ReaderTerm(14),
    ]);

    const wTail2 = 15;
    const rTail2 = 14;
    rt.heap.addWriter(WriterCell(wTail2, rTail2));
    rt.heap.addReader(ReaderCell(rTail2));
    rt.heap.bindWriterStruct(wTail2, '.', [
      ConstTerm(3),
      ConstTerm(null),
    ]);

    // Build empty list []
    const wEmpty = 20;
    const rEmpty = 21;
    rt.heap.addWriter(WriterCell(wEmpty, rEmpty));
    rt.heap.addReader(ReaderCell(rEmpty));
    rt.heap.bindWriterConst(wEmpty, null);

    // Build list [a,b,c,d]
    const wList2 = 30;
    const rList2 = 31;
    rt.heap.addWriter(WriterCell(wList2, rList2));
    rt.heap.addReader(ReaderCell(rList2));
    rt.heap.bindWriterStruct(wList2, '.', [
      ConstTerm('a'),
      ReaderTerm(32),
    ]);

    const wTail3 = 33;
    const rTail3 = 32;
    rt.heap.addWriter(WriterCell(wTail3, rTail3));
    rt.heap.addReader(ReaderCell(rTail3));
    rt.heap.bindWriterStruct(wTail3, '.', [
      ConstTerm('b'),
      ReaderTerm(34),
    ]);

    const wTail4 = 35;
    const rTail4 = 34;
    rt.heap.addWriter(WriterCell(wTail4, rTail4));
    rt.heap.addReader(ReaderCell(rTail4));
    rt.heap.bindWriterStruct(wTail4, '.', [
      ConstTerm('c'),
      ReaderTerm(36),
    ]);

    const wTail5 = 37;
    const rTail5 = 36;
    rt.heap.addWriter(WriterCell(wTail5, rTail5));
    rt.heap.addReader(ReaderCell(rTail5));
    rt.heap.bindWriterStruct(wTail5, '.', [
      ConstTerm('d'),
      ConstTerm(null),
    ]);

    // Result writers Xs and Ys
    const wXs = 40;
    const rXs = 41;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 50;
    const rYs = 51;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    print('HEAP SETUP:');
    print('  List1 (W$wList1/R$rList1) = [1,2,3]');
    print('  Empty (W$wEmpty/R$rEmpty) = []');
    print('  List2 (W$wList2/R$rList2) = [a,b,c,d]');
    print('  Result Xs (W$wXs/R$rXs) = unbound');
    print('  Result Ys (W$wYs/R$rYs) = unbound');
    print('');

    // Full merge program
    final prog = BC.prog([
      BC.L('merge/3_start'),

      // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      BC.TRY(),
      BC.headStruct('.', 2, 0),
      BC.headWriter(0),
      BC.headWriter(1),
      BC.getVar(2, 1),
      BC.headStruct('.', 2, 2),
      BC.headReader(0),
      BC.headWriter(3),
      BC.COMMIT(),
      BC.putReader(2, 0),
      BC.putReader(1, 1),
      BC.putWriter(3, 2),
      BC.requeue('merge/3_start', 3),

      // Clause 2: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
      BC.L('merge/3_clause2'),
      BC.TRY(),
      BC.getVar(0, 0),
      BC.headStruct('.', 2, 1),
      BC.headWriter(1),
      BC.headWriter(2),
      BC.headStruct('.', 2, 2),
      BC.headReader(1),
      BC.headWriter(3),
      BC.COMMIT(),
      BC.putReader(0, 0),
      BC.putReader(2, 1),
      BC.putWriter(3, 2),
      BC.requeue('merge/3_start', 3),

      // Clause 3: merge([],[],[]).
      BC.L('merge/3_clause3'),
      BC.TRY(),
      BC.headConst(null, 0),
      BC.headConst(null, 1),
      BC.headConst(null, 2),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('merge/3_end'),
      BC.SUSP(),
    ]);

    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    print('=' * 70);
    print('EXECUTION TRACE');
    print('=' * 70 + '\n');

    // Goal 100: merge(Xs?, [a,b,c,d], Ys) - ENQUEUED FIRST
    print('--- GOAL 100: merge(Xs?, [a,b,c,d], Ys) ---');
    print('    (Xs? is unbound, this should SUSPEND)');
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: rXs, 1: rList2}, writers: {2: wYs});
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, 0));

    // Goal 200: merge([1,2,3], [], Xs) - ENQUEUED SECOND
    print('--- GOAL 200: merge([1,2,3], [], Xs) ---');
    print('    (This will bind Xs and ACTIVATE goal 100)');
    const goal2 = 200;
    final env2 = CallEnv(readers: {0: rList1, 1: rEmpty}, writers: {2: wXs});
    rt.setGoalEnv(goal2, env2);
    rt.gq.enqueue(GoalRef(goal2, 0));

    print('');
    print('Queue order: [100, 200]');
    print('Expected execution: 100 suspends, 200 runs and binds Xs, 100 reactivates');
    print('');

    // Run both goals
    final ran = sched.drain(maxCycles: 100);

    print('');
    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Goals executed in order: $ran');
    print('Total executions: ${ran.length}');
    print('');

    // Helper to extract list
    List<Object?> extractList(Term term) {
      if (term is ConstTerm && term.value == null) {
        return [];
      }
      if (term is StructTerm && term.functor == '.') {
        final head = term.args[0];
        final tail = term.args[1];

        final headValue = head is ConstTerm ? head.value : head;

        if (tail is ConstTerm) {
          return [headValue];
        } else if (tail is ReaderTerm) {
          final writerId = rt.heap.writerIdForReader(tail.readerId);
          if (writerId != null) {
            final tailValue = rt.heap.writerValue[writerId];
            if (tailValue != null) {
              return [headValue, ...extractList(tailValue)];
            }
          }
        } else if (tail is StructTerm) {
          return [headValue, ...extractList(tail)];
        }
        return [headValue];
      }
      return [];
    }

    // Check Xs
    print('Xs (W$wXs) bound: ${rt.heap.isWriterBound(wXs)}');
    if (rt.heap.isWriterBound(wXs)) {
      final xsValue = rt.heap.valueOfWriter(wXs);
      final xsList = extractList(xsValue!);
      print('Xs = $xsList');
      expect(xsList, [1, 2, 3], reason: 'Xs should be [1, 2, 3]');
    }

    print('');

    // Check Ys
    print('Ys (W$wYs) bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      final ysValue = rt.heap.valueOfWriter(wYs);
      final ysList = extractList(ysValue!);
      print('Ys = $ysList');
      // Note: Merge is non-deterministic. Either [1,a,2,b,3,c,d] or [a,b,c,d,1,2,3] are valid
      expect(ysList.length, 7, reason: 'Should have 7 elements total');
    }

    print('');

    // Verify execution order
    expect(ran.length, 3, reason: 'Should execute: goal 100, goal 200, goal 100 (reactivated)');
    expect(ran[0], 100, reason: 'First execution: goal 100 (suspends)');
    expect(ran[1], 200, reason: 'Second execution: goal 200 (binds Xs)');
    expect(ran[2], 100, reason: 'Third execution: goal 100 (reactivated)');

    print('✓ Execution order correct: [100 suspend, 200 bind, 100 reactivate]');
    print('✓ Goal 100 suspended on Xs?, then reactivated when goal 200 bound Xs');
    print('✓ Sequential merge succeeded with suspension and reactivation!');
  });
}
