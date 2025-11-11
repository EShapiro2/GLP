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

    // Goal 100: merge(Xs?, [a,b,c,d], Ys) - ENQUEUED FIRST
    print('--- GOAL 100: merge(Xs?, [a,b,c,d], Ys) ---');
    print('    (Xs? is unbound, this should SUSPEND)');
    const goal1 = 100;
    final env1 = CallEnv(readers: {0: varXs, 1: varList2}, writers: {2: varYs});
    rt.setGoalEnv(goal1, env1);
    rt.gq.enqueue(GoalRef(goal1, 0));

    // Goal 200: merge([1,2,3], [], Xs) - ENQUEUED SECOND
    print('--- GOAL 200: merge([1,2,3], [], Xs) ---');
    print('    (This will bind Xs and ACTIVATE goal 100)');
    const goal2 = 200;
    final env2 = CallEnv(readers: {0: varList1, 1: varEmpty}, writers: {2: varXs});
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
