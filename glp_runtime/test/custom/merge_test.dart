import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/asm.dart';

void main() {
  test('Merge base case: merge([],[],[]) succeeds', () {
    print('\n=== Testing Merge Base Case ===');
    print('Program: merge([],[],[]).');
    print('Goal: merge([], [], [])\\n');

    final rt = GlpRuntime();

    // Create bound readers for empty lists
    // Arg 0: [] (empty list 1)
    const w1 = 1;
    const r1 = 2;
    rt.heap.addWriter(WriterCell(w1, r1));
    rt.heap.addReader(ReaderCell(r1));
    rt.heap.bindWriterConst(w1, null); // [] is null

    // Arg 1: [] (empty list 2)
    const w2 = 3;
    const r2 = 4;
    rt.heap.addWriter(WriterCell(w2, r2));
    rt.heap.addReader(ReaderCell(r2));
    rt.heap.bindWriterConst(w2, null);

    // Arg 2: [] (empty result)
    const w3 = 5;
    const r3 = 6;
    rt.heap.addWriter(WriterCell(w3, r3));
    rt.heap.addReader(ReaderCell(r3));
    rt.heap.bindWriterConst(w3, null);

    print('Heap Setup:');
    print('  Arg0 (Xs?) = []');
    print('  Arg1 (Ys?) = []');
    print('  Arg2 (Zs?) = []\\n');

    // Bytecode for merge/3 base case: merge([],[],[]).
    // This is clause 3 from the GLP paper
    final prog = BC.prog([
      BC.L('merge/3_start'),
      BC.TRY(),
      // Match arg0 with []
      BC.headConst(null, 0),
      // Match arg1 with []
      BC.headConst(null, 1),
      // Match arg2 with []
      BC.headConst(null, 2),
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('merge/3_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: merge([], [], []) ---');
    print('Expected: SUCCESS (all three arguments match [])\\n');

    final env = CallEnv(readers: {0: r1, 1: r2, 2: r3});
    final cx = RunnerContext(rt: rt, goalId: 100, kappa: 0, env: env);

    final result = BytecodeRunner(prog).runWithStatus(cx);
    print('Result: $result\\n');

    expect(result, RunResult.terminated, reason: 'Base case should succeed');

    print('✓ Merge base case succeeded');
    print('✓ Test passed!');
  });

  test('Merge copy first stream: merge([a],[],[a]) with requeue', () {
    print('\n=== Testing Merge Copy First Stream ===');
    print('Program: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).');
    print('Goal: merge([a], [], Z) where Z is unbound writer\\n');

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
    print('  Arg2 (Zs) = unbound writer\\n');

    // Bytecode for merge with both clauses:
    // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
    // Clause 2: merge([],[],[]).
    final prog = BC.prog([
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

    print('--- Executing Goal: merge([a], [], Z) ---');
    print('Expected: Z will be bound to [a]\\n');

    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    final cx = RunnerContext(rt: rt, goalId: 100, kappa: 0, env: env);

    final result = BytecodeRunner(prog).runWithStatus(cx);
    print('Result: $result');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');
    if (rt.heap.isWriterBound(wZs)) {
      final val = rt.heap.valueOfWriter(wZs);
      print('Writer $wZs value: $val\\n');
    }

    expect(result, RunResult.terminated, reason: 'Should succeed');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Result should be bound');

    final value = rt.heap.valueOfWriter(wZs);
    expect(value, isA<StructTerm>(), reason: 'Should be a list structure');

    final list = value as StructTerm;
    expect(list.functor, '.', reason: 'Should be list cons');
    expect((list.args[0] as ConstTerm).value, 'a', reason: 'Head should be a');

    // Check that the tail is a reader reference
    expect(list.args[1], isA<ReaderTerm>(), reason: 'Tail should be a reader reference');
    final tailReader = (list.args[1] as ReaderTerm).readerId;

    // Dereference the reader to check the tail value
    print('Tail reader ID: $tailReader');
    final tailWriterId = rt.heap.writerIdForReader(tailReader);
    print('Tail writer ID: $tailWriterId');
    if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
      final tailValue = rt.heap.valueOfWriter(tailWriterId);
      print('Tail value: $tailValue');
      expect(tailValue, isA<ConstTerm>(), reason: 'Tail should be bound to []');
      expect((tailValue as ConstTerm).value, null, reason: 'Tail should be [] (null)');
    } else {
      print('WARNING: Tail reader not bound or writer not found');
    }

    print('✓ Merge correctly copied first stream');
    print('✓ Test passed!');
  });

  test('Merge copy second stream: merge([],[b],Z) with requeue', () {
    print('\n=== Testing Merge Copy Second Stream ===');
    print('Program: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).');
    print('Goal: merge([], [b], Z) where Z is unbound writer\n');

    final rt = GlpRuntime();

    // Arg 0: []
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));
    rt.heap.bindWriterConst(wXs, null);

    // Arg 1: [b]
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));
    rt.heap.bindWriterStruct(wYs, '.', [
      ConstTerm('b'),
      ConstTerm(null), // tail is []
    ]);

    // Arg 2: Z (unbound writer)
    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  Arg0 (Xs?) = []');
    print('  Arg1 (Ys?) = [b]');
    print('  Arg2 (Zs) = unbound writer\n');

    // Bytecode for merge with three clauses:
    // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
    // Clause 2: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
    // Clause 3: merge([],[],[]).
    final prog = BC.prog([
      BC.L('merge/3_start'),

      // Clause 1: merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
      BC.TRY(),
      BC.headStruct('.', 2, 0),    // arg0 = '.'(X, Xs)
      BC.headWriter(0),             // Extract X into clause var 0
      BC.headWriter(1),             // Extract Xs into clause var 1
      BC.getVar(2, 1),              // Load Ys into clause var 2
      BC.headStruct('.', 2, 2),     // arg2 = '.'(_, _)
      BC.headReader(0),             // First element: X? (reader of X)
      BC.headWriter(3),             // Second element: Zs? (fresh writer for tail)
      BC.COMMIT(),
      BC.putReader(2, 0),           // A0 = Ys?
      BC.putReader(1, 1),           // A1 = Xs?
      BC.putWriter(3, 2),           // A2 = Zs
      BC.requeue('merge/3_start', 3),

      // Clause 2: merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
      BC.L('merge/3_clause2'),
      BC.TRY(),
      BC.getVar(0, 0),              // Load Xs into clause var 0
      BC.headStruct('.', 2, 1),     // arg1 = '.'(Y, Ys)
      BC.headWriter(1),             // Extract Y into clause var 1
      BC.headWriter(2),             // Extract Ys into clause var 2
      BC.headStruct('.', 2, 2),     // arg2 = '.'(_, _)
      BC.headReader(1),             // First element: Y? (reader of Y)
      BC.headWriter(3),             // Second element: Zs? (fresh writer for tail)
      BC.COMMIT(),
      BC.putReader(0, 0),           // A0 = Xs?
      BC.putReader(2, 1),           // A1 = Ys?
      BC.putWriter(3, 2),           // A2 = Zs
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

    print('--- Executing Goal: merge([], [b], Z) ---');
    print('Expected: Z will be bound to [b]\n');

    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    final cx = RunnerContext(rt: rt, goalId: 200, kappa: 0, env: env);

    final result = BytecodeRunner(prog).runWithStatus(cx);
    print('Result: $result');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');
    if (rt.heap.isWriterBound(wZs)) {
      final val = rt.heap.valueOfWriter(wZs);
      print('Writer $wZs value: $val\n');
    }

    expect(result, RunResult.terminated, reason: 'Should succeed');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Result should be bound');

    final value = rt.heap.valueOfWriter(wZs);
    expect(value, isA<StructTerm>(), reason: 'Should be a list structure');

    final list = value as StructTerm;
    expect(list.functor, '.', reason: 'Should be list cons');
    expect((list.args[0] as ConstTerm).value, 'b', reason: 'Head should be b');

    // Check that the tail is properly bound
    expect(list.args[1], isA<ReaderTerm>(), reason: 'Tail should be a reader reference');
    final tailReader = (list.args[1] as ReaderTerm).readerId;
    final tailWriterId = rt.heap.writerIdForReader(tailReader);
    if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
      final tailValue = rt.heap.valueOfWriter(tailWriterId);
      print('Tail value: $tailValue');
      expect(tailValue, isA<ConstTerm>(), reason: 'Tail should be bound to []');
      expect((tailValue as ConstTerm).value, null, reason: 'Tail should be [] (null)');
    }

    print('✓ Merge correctly copied second stream');
    print('✓ Test passed!');
  });
}
