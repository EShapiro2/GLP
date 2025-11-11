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
  test('Merge: merge([1,2,3],[a,b],Xs) with full trace', () {
    print('\n' + '=' * 70);
    print('MERGE TEST: merge([1,2,3], [a,b], Xs)');
    print('Expected: Xs = [1, a, 2, b, 3] (alternating elements)');
    print('=' * 70 + '\n');

    final rt = GlpRuntime();

    // Build list [1,2,3] - SINGLE-ID: same varId for writer and reader
    const varList1 = 10;
    rt.heap.addWriter(WriterCell(varList1, varList1));  // Single-ID: writerId == readerId
    rt.heap.addReader(ReaderCell(varList1));
    rt.heap.bindWriterStruct(varList1, '.', [
      ConstTerm(1),
      VarRef(12, isReader: true),  // tail
    ]);

    const varTail1 = 12;
    rt.heap.addWriter(WriterCell(varTail1, varTail1));  // Single-ID
    rt.heap.addReader(ReaderCell(varTail1));
    rt.heap.bindWriterStruct(varTail1, '.', [
      ConstTerm(2),
      VarRef(14, isReader: true),  // tail
    ]);

    const varTail2 = 14;
    rt.heap.addWriter(WriterCell(varTail2, varTail2));  // Single-ID
    rt.heap.addReader(ReaderCell(varTail2));
    rt.heap.bindWriterStruct(varTail2, '.', [
      ConstTerm(3),
      ConstTerm(null),  // end of list
    ]);

    // Build list [a,b] - SINGLE-ID
    const varList2 = 20;
    rt.heap.addWriter(WriterCell(varList2, varList2));  // Single-ID
    rt.heap.addReader(ReaderCell(varList2));
    rt.heap.bindWriterStruct(varList2, '.', [
      ConstTerm('a'),
      VarRef(22, isReader: true),  // tail
    ]);

    const varTail3 = 22;
    rt.heap.addWriter(WriterCell(varTail3, varTail3));  // Single-ID
    rt.heap.addReader(ReaderCell(varTail3));
    rt.heap.bindWriterStruct(varTail3, '.', [
      ConstTerm('b'),
      ConstTerm(null),  // end of list
    ]);

    // Result writer Xs - SINGLE-ID
    const varXs = 30;
    rt.heap.addWriter(WriterCell(varXs, varXs));  // Single-ID
    rt.heap.addReader(ReaderCell(varXs));

    print('HEAP SETUP:');
    print('  List1 (ID=$varList1) = [1,2,3]');
    print('  List2 (ID=$varList2) = [a,b]');
    print('  Result Xs (ID=$varXs) = unbound');
    print('');

    // Full merge program with all 3 clauses
    final prog = BC.prog([
      BC.L('merge/3_start'),

      // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      BC.TRY(),
      BC.headStruct('.', 2, 0),      // arg0 must be cons cell
      BC.headWriter(0),              // X (head)
      BC.headWriter(1),              // Xs (tail)
      BC.getVar(2, 1),               // save Ys to var 2
      BC.headStruct('.', 2, 2),      // arg2 must be cons cell
      BC.headReader(0),              // head = X?
      BC.headWriter(3),              // Zs? (tail)
      BC.COMMIT(),
      BC.putReader(2, 0),            // arg0 = Ys?
      BC.putReader(1, 1),            // arg1 = Xs?
      BC.putWriter(3, 2),            // arg2 = Zs
      BC.requeue('merge/3_start', 3),

      // Clause 2: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
      BC.L('merge/3_clause2'),
      BC.TRY(),
      BC.getVar(0, 0),               // save Xs to var 0
      BC.headStruct('.', 2, 1),      // arg1 must be cons cell
      BC.headWriter(1),              // Y (head)
      BC.headWriter(2),              // Ys (tail)
      BC.headStruct('.', 2, 2),      // arg2 must be cons cell
      BC.headReader(1),              // head = Y?
      BC.headWriter(3),              // Zs? (tail)
      BC.COMMIT(),
      BC.putReader(0, 0),            // arg0 = Xs?
      BC.putReader(2, 1),            // arg1 = Ys?
      BC.putWriter(3, 2),            // arg2 = Zs
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

    // Goal: merge([1,2,3], [a,b], Xs)
    const goalId = 100;
    final env = CallEnv(readers: {0: varList1, 1: varList2}, writers: {2: varXs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    print('Starting goal $goalId: merge(R$varList1, R$varList2, W$varXs)');
    print('');

    // Run with scheduler
    final ran = sched.drain(maxCycles: 50);

    print('');
    print('=' * 70);
    print('RESULTS');
    print('=' * 70);
    print('Goals executed: $ran');
    print('Xs bound: ${rt.heap.isWriterBound(varXs)}');

    if (rt.heap.isWriterBound(varXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(varXs)}');
      print('');

      // Helper to extract list elements by following reader chain
      List<Object?> extractList(Term term) {
        if (term is ConstTerm && term.value == null) {
          return []; // Empty list
        }
        if (term is StructTerm && term.functor == '.') {
          final head = term.args[0];
          final tail = term.args[1];

          final headValue = head is ConstTerm ? head.value : head;

          // Handle tail
          if (tail is ConstTerm) {
            return [headValue];
          } else if (tail is VarRef) {
            // VarRef could be reader or writer - get the value
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

      final value = rt.heap.valueOfWriter(varXs);
      final resultList = extractList(value!);
      print('Extracted list: $resultList');
      print('');
      print('Expected: [1, a, 2, b, 3]');

      expect(resultList, [1, 'a', 2, 'b', 3],
        reason: 'Should alternate: 1, a, 2, b, 3');
    }

    expect(rt.heap.isWriterBound(varXs), true, reason: 'Xs should be bound');
  });
}
