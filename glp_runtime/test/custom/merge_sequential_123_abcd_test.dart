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
  test('Sequential merge: merge([1,2,3],[],Xs), merge(Xs?,[a,b,c,d],Ys)', () {
    print('\n' + '=' * 70);
    print('SEQUENTIAL MERGE TEST');
    print('Goal 1: merge([1,2,3], [], Xs)');
    print('Goal 2: merge(Xs?, [a,b,c,d], Ys)');
    print('Expected: Xs = [1,2,3], Ys = [1,a,2,b,3,c,d]');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Build list [1,2,3]
    const varList1 = 10;
    
    rt.heap.addWriter(WriterCell(varList1, varList1));
    rt.heap.addReader(ReaderCell(varList1));
    rt.heap.bindWriterStruct(varList1, '.', [
      ConstTerm(1),
      VarRef(13, isReader: true),
    ]);

    const varTail1 = 13;
    
    rt.heap.addWriter(WriterCell(varTail1, varTail1));
    rt.heap.addReader(ReaderCell(varTail1));
    rt.heap.bindWriterStruct(varTail1, '.', [
      ConstTerm(2),
      VarRef(15, isReader: true),
    ]);

    const varTail2 = 15;
    
    rt.heap.addWriter(WriterCell(varTail2, varTail2));
    rt.heap.addReader(ReaderCell(varTail2));
    rt.heap.bindWriterStruct(varTail2, '.', [
      ConstTerm(3),
      ConstTerm(null),
    ]);

    // Build empty list []
    const varEmpty = 20;
    
    rt.heap.addWriter(WriterCell(varEmpty, varEmpty));
    rt.heap.addReader(ReaderCell(varEmpty));
    rt.heap.bindWriterConst(varEmpty, null);

    // Build list [a,b,c,d]
    const varList2 = 30;
    
    rt.heap.addWriter(WriterCell(varList2, varList2));
    rt.heap.addReader(ReaderCell(varList2));
    rt.heap.bindWriterStruct(varList2, '.', [
      ConstTerm('a'),
      VarRef(33, isReader: true),
    ]);

    const varTail3 = 33;
    
    rt.heap.addWriter(WriterCell(varTail3, varTail3));
    rt.heap.addReader(ReaderCell(varTail3));
    rt.heap.bindWriterStruct(varTail3, '.', [
      ConstTerm('b'),
      VarRef(35, isReader: true),
    ]);

    const varTail4 = 35;
    
    rt.heap.addWriter(WriterCell(varTail4, varTail4));
    rt.heap.addReader(ReaderCell(varTail4));
    rt.heap.bindWriterStruct(varTail4, '.', [
      ConstTerm('c'),
      VarRef(37, isReader: true),
    ]);

    const varTail5 = 37;
    
    rt.heap.addWriter(WriterCell(varTail5, varTail5));
    rt.heap.addReader(ReaderCell(varTail5));
    rt.heap.bindWriterStruct(varTail5, '.', [
      ConstTerm('d'),
      ConstTerm(null),
    ]);

    // Result writers Xs and Ys
    const varXs = 40;
    
    rt.heap.addWriter(WriterCell(varXs, varXs));
    rt.heap.addReader(ReaderCell(varXs));

    const varYs = 50;
    
    rt.heap.addWriter(WriterCell(varYs, varYs));
    rt.heap.addReader(ReaderCell(varYs));

    print('HEAP SETUP:');
    print('  List1 (W$varList1/R$varList1) = [1,2,3]');
    print('  Empty (W$varEmpty/R$varEmpty) = []');
    print('  List2 (W$varList2/R$varList2) = [a,b,c,d]');
    print('  Result Xs (W$varXs/R$varXs) = unbound');
    print('  Result Ys (W$varYs/R$varYs) = unbound');
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

    // Goal 1: merge([1,2,3], [], Xs)
    print('--- GOAL 100: merge([1,2,3], [], Xs) ---');
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: varList1, 1: varEmpty}, writers: {2: varXs});
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, 0));

    // Goal 2: merge(Xs?, [a,b,c,d], Ys)
    print('--- GOAL 200: merge(Xs?, [a,b,c,d], Ys) ---');
    print('(Goal 200 will suspend because Xs? is unbound)');
    const goal2 = 200;
    final env2 = CallEnv(readers: {0: varXs, 1: varList2}, writers: {2: varYs});
    rt.setGoalEnv(goal2, env2);
    rt.gq.enqueue(GoalRef(goal2, 0));

    print('');

    // Run both goals
    final ran = sched.drain(maxCycles: 100);

    print('');
    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Goals executed: $ran');
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
        } else if (tail is VarRef) {
          final varId = tail.varId;
          final tailValue = rt.heap.getValue(varId);
          if (tailValue != null) {
            return [headValue, ...extractList(tailValue)];
          }
        } else if (tail is StructTerm) {
          return [headValue, ...extractList(tail)];
        }
        return [headValue];
      }
      return [];
    }

    // Check Xs
    print('Xs (W$varXs) bound: ${rt.heap.isWriterBound(varXs)}');
    if (rt.heap.isWriterBound(varXs)) {
      final xsValue = rt.heap.valueOfWriter(varXs);
      final xsList = extractList(xsValue!);
      print('Xs = $xsList');
      expect(xsList, [1, 2, 3], reason: 'Xs should be [1, 2, 3]');
    }

    print('');

    // Check Ys
    print('Ys (W$varYs) bound: ${rt.heap.isWriterBound(varYs)}');
    if (rt.heap.isWriterBound(varYs)) {
      final ysValue = rt.heap.valueOfWriter(varYs);
      final ysList = extractList(ysValue!);
      print('Ys = $ysList');
      expect(ysList, [1, 'a', 2, 'b', 3, 'c', 'd'],
        reason: 'Ys should be [1, a, 2, b, 3, c, d]');
    }

    print('');
    print('✓ Sequential merge succeeded!');
    print('✓ Goal 200 was suspended, then activated when Goal 100 bound Xs');
  });
}
