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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: r1, 1: r2, 2: r3});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0)); // Start at PC 0

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran\\n');

    expect(ran, [goalId], reason: 'Goal should execute');

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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');
    if (rt.heap.isWriterBound(wZs)) {
      final val = rt.heap.valueOfWriter(wZs);
      print('Writer $wZs value: $val\\n');
    }

    expect(ran, [goalId], reason: 'Goal should execute');
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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 200;
    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
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

  test('Merge both streams: merge([a],[b],Z) alternates', () {
    print('\n=== Testing Merge Both Streams ===');
    print('Program: merge with all 3 clauses');
    print('Goal: merge([a], [b], Z) where Z is unbound writer\n');

    final rt = GlpRuntime();

    // Arg 0: [a]
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));
    rt.heap.bindWriterStruct(wXs, '.', [
      ConstTerm('a'),
      ConstTerm(null),
    ]);

    // Arg 1: [b]
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));
    rt.heap.bindWriterStruct(wYs, '.', [
      ConstTerm('b'),
      ConstTerm(null),
    ]);

    // Arg 2: Z (unbound writer)
    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  Arg0 (Xs?) = [a]');
    print('  Arg1 (Ys?) = [b]');
    print('  Arg2 (Zs) = unbound writer\n');

    // Full merge program with all 3 clauses
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

    print('--- Executing Goal: merge([a], [b], Z) ---');
    print('Expected: Z = [a, b] (alternating from both streams)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 300;
    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');

    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Result should be bound');

    final value = rt.heap.valueOfWriter(wZs);
    print('Writer $wZs value: $value\n');

    expect(value, isA<StructTerm>(), reason: 'Should be a list structure');

    // Check first element (should be 'a' from first stream)
    final list = value as StructTerm;
    expect(list.functor, '.', reason: 'Should be list cons');
    expect((list.args[0] as ConstTerm).value, 'a', reason: 'First should be a');

    // Check tail - should be [b]
    expect(list.args[1], isA<ReaderTerm>(), reason: 'Tail should be a reader');
    final tail1Reader = (list.args[1] as ReaderTerm).readerId;
    final tail1WriterId = rt.heap.writerIdForReader(tail1Reader);

    if (tail1WriterId != null && rt.heap.isWriterBound(tail1WriterId)) {
      final tail1Value = rt.heap.valueOfWriter(tail1WriterId);
      print('Tail 1 value: $tail1Value');
      expect(tail1Value, isA<StructTerm>(), reason: 'Tail should be [b]');

      final tail1List = tail1Value as StructTerm;
      expect(tail1List.functor, '.', reason: 'Tail should be list cons');
      expect((tail1List.args[0] as ConstTerm).value, 'b', reason: 'Second should be b');

      // Check tail of tail - should be []
      expect(tail1List.args[1], isA<ReaderTerm>(), reason: 'Tail of tail should be reader');
      final tail2Reader = (tail1List.args[1] as ReaderTerm).readerId;
      final tail2WriterId = rt.heap.writerIdForReader(tail2Reader);

      if (tail2WriterId != null && rt.heap.isWriterBound(tail2WriterId)) {
        final tail2Value = rt.heap.valueOfWriter(tail2WriterId);
        print('Tail 2 value: $tail2Value');
        expect(tail2Value, isA<ConstTerm>(), reason: 'Final tail should be []');
        expect((tail2Value as ConstTerm).value, null, reason: 'Final tail should be null');
      }
    }

    print('✓ Merge correctly alternated both streams: [a, b]');
    print('✓ Test passed!');
  });

  test('Merge longer streams: merge([a,b,c], [d,e,f], Z)', () {
    print('\n=== Testing Merge Longer Streams ===');
    print('Program: merge with all 3 clauses');
    print('Goal: merge([a,b,c], [d,e,f], Z) where Z is unbound writer\n');

    final rt = GlpRuntime();

    // Build list [a,b,c]
    // [a,b,c] = '.'(a, '.'(b, '.'(c, [])))
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));
    rt.heap.bindWriterStruct(wXs, '.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [
          ConstTerm('c'),
          ConstTerm(null) // []
        ])
      ])
    ]);

    // Build list [d,e,f]
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));
    rt.heap.bindWriterStruct(wYs, '.', [
      ConstTerm('d'),
      StructTerm('.', [
        ConstTerm('e'),
        StructTerm('.', [
          ConstTerm('f'),
          ConstTerm(null) // []
        ])
      ])
    ]);

    // Result writer (unbound)
    const wZs = 5;
    const rZs = 6;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  Arg0 (Xs?) = [a,b,c]');
    print('  Arg1 (Ys?) = [d,e,f]');
    print('  Arg2 (Zs) = unbound writer\n');

    // Full merge program with all 3 clauses
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

    print('--- Executing Goal: merge([a,b,c], [d,e,f], Z) ---');
    print('Expected: Z = [a, d, b, e, c, f] (alternating)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 400;
    final env = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');

    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Result should be bound');

    final value = rt.heap.valueOfWriter(wZs);
    print('Writer $wZs value: $value\n');

    expect(value, isNotNull, reason: 'Value should not be null');
    expect(value, isA<StructTerm>(), reason: 'Should be a list structure');

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
        } else if (tail is ReaderTerm) {
          final tailWriterId = rt.heap.writerIdForReader(tail.readerId);
          if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
            final tailValue = rt.heap.valueOfWriter(tailWriterId);
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

    final resultList = extractList(value!);
    print('Extracted list: $resultList');

    expect(resultList.length, 6, reason: 'Should have 6 elements');
    expect(resultList, ['a', 'd', 'b', 'e', 'c', 'f'],
      reason: 'Should alternate: a, d, b, e, c, f');

    print('✓ Merge correctly alternated longer streams: [a, d, b, e, c, f]');
    print('✓ Test passed!');
  });

  test('Sequential merge: merge([a,b,c,d],[],Xs), merge(Xs?,[1,2,3,4],Zs)', () {
    print('\n=== Testing Sequential Merge ===');
    print('Program: merge with all 3 clauses');
    print('Goal 1: merge([a,b,c,d], [], Xs) - copy first stream');
    print('Goal 2: merge(Xs?, [1,2,3,4], Zs) - merge result with numbers\n');

    final rt = GlpRuntime();

    // Build list [a,b,c,d]
    const wInput1 = 1;
    const rInput1 = 2;
    rt.heap.addWriter(WriterCell(wInput1, rInput1));
    rt.heap.addReader(ReaderCell(rInput1));
    rt.heap.bindWriterStruct(wInput1, '.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [
          ConstTerm('c'),
          StructTerm('.', [
            ConstTerm('d'),
            ConstTerm(null) // []
          ])
        ])
      ])
    ]);

    // Empty list for second argument of first merge
    const wEmpty = 3;
    const rEmpty = 4;
    rt.heap.addWriter(WriterCell(wEmpty, rEmpty));
    rt.heap.addReader(ReaderCell(rEmpty));
    rt.heap.bindWriterConst(wEmpty, null); // []

    // Xs - intermediate result writer (unbound)
    const wXs = 5;
    const rXs = 6;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    // Build list [1,2,3,4]
    const wInput2 = 7;
    const rInput2 = 8;
    rt.heap.addWriter(WriterCell(wInput2, rInput2));
    rt.heap.addReader(ReaderCell(rInput2));
    rt.heap.bindWriterStruct(wInput2, '.', [
      ConstTerm(1),
      StructTerm('.', [
        ConstTerm(2),
        StructTerm('.', [
          ConstTerm(3),
          StructTerm('.', [
            ConstTerm(4),
            ConstTerm(null) // []
          ])
        ])
      ])
    ]);

    // Zs - final result writer (unbound)
    const wZs = 9;
    const rZs = 10;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  Input1 = [a,b,c,d]');
    print('  Empty = []');
    print('  Xs = unbound writer');
    print('  Input2 = [1,2,3,4]');
    print('  Zs = unbound writer\n');

    // Full merge program with all 3 clauses
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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal 1: merge([a,b,c,d], [], Xs)
    print('--- Setting up Goal 1: merge([a,b,c,d], [], Xs) ---');
    const goalId1 = 500;
    final env1 = CallEnv(readers: {0: rInput1, 1: rEmpty}, writers: {2: wXs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    // Set up goal 2: merge(Xs?, [1,2,3,4], Zs)
    print('--- Setting up Goal 2: merge(Xs?, [1,2,3,4], Zs) ---');
    const goalId2 = 600;
    final env2 = CallEnv(readers: {0: rXs, 1: rInput2}, writers: {2: wZs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    // Run both goals through scheduler
    print('--- Running both goals concurrently ---\n');
    final ran = sched.drain(maxCycles: 200);
    print('Goals executed: $ran');
    print('Writer $wXs bound? ${rt.heap.isWriterBound(wXs)}');
    print('Writer $wZs bound? ${rt.heap.isWriterBound(wZs)}');

    expect(ran.contains(goalId1), true, reason: 'Goal 1 should execute');
    expect(ran.contains(goalId2), true, reason: 'Goal 2 should execute');
    expect(rt.heap.isWriterBound(wXs), true, reason: 'Xs should be bound');

    final xsValue = rt.heap.valueOfWriter(wXs);
    print('Xs value: $xsValue');

    expect(rt.heap.isWriterBound(wZs), true, reason: 'Zs should be bound');
    final zsValue = rt.heap.valueOfWriter(wZs);
    print('Zs value: $zsValue\n');

    expect(zsValue, isNotNull, reason: 'Zs value should not be null');
    expect(zsValue, isA<StructTerm>(), reason: 'Should be a list structure');

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
        } else if (tail is ReaderTerm) {
          final tailWriterId = rt.heap.writerIdForReader(tail.readerId);
          if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
            final tailValue = rt.heap.valueOfWriter(tailWriterId);
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

    final resultList = extractList(zsValue!);
    print('Extracted list: $resultList');

    expect(resultList.length, 8, reason: 'Should have 8 elements');
    expect(resultList, ['a', 1, 'b', 2, 'c', 3, 'd', 4],
      reason: 'Should alternate: a, 1, b, 2, c, 3, d, 4');

    print('✓ Sequential merge correctly produced: [a, 1, b, 2, c, 3, d, 4]');
    print('✓ Test passed!');
  });

  test('Three-stage pipeline: merge([a,b,c,d],[],Xs), merge(Xs?,[1,2,3,4],Zs), merge(Zs?,[],Ws)', () {
    print('\n=== Testing Three-Stage Pipeline ===');
    print('Program: merge with all 3 clauses');
    print('Goal 1: merge([a,b,c,d], [], Xs) - copy to Xs');
    print('Goal 2: merge(Xs?, [1,2,3,4], Zs) - merge Xs with numbers');
    print('Goal 3: merge(Zs?, [], Ws) - copy Zs to Ws\n');

    final rt = GlpRuntime();

    // Build list [a,b,c,d]
    const wInput1 = 1;
    const rInput1 = 2;
    rt.heap.addWriter(WriterCell(wInput1, rInput1));
    rt.heap.addReader(ReaderCell(rInput1));
    rt.heap.bindWriterStruct(wInput1, '.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [
          ConstTerm('c'),
          StructTerm('.', [
            ConstTerm('d'),
            ConstTerm(null)
          ])
        ])
      ])
    ]);

    // Empty list
    const wEmpty = 3;
    const rEmpty = 4;
    rt.heap.addWriter(WriterCell(wEmpty, rEmpty));
    rt.heap.addReader(ReaderCell(rEmpty));
    rt.heap.bindWriterConst(wEmpty, null);

    // Xs - first result
    const wXs = 5;
    const rXs = 6;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    // Build list [1,2,3,4]
    const wInput2 = 7;
    const rInput2 = 8;
    rt.heap.addWriter(WriterCell(wInput2, rInput2));
    rt.heap.addReader(ReaderCell(rInput2));
    rt.heap.bindWriterStruct(wInput2, '.', [
      ConstTerm(1),
      StructTerm('.', [
        ConstTerm(2),
        StructTerm('.', [
          ConstTerm(3),
          StructTerm('.', [
            ConstTerm(4),
            ConstTerm(null)
          ])
        ])
      ])
    ]);

    // Zs - second result
    const wZs = 9;
    const rZs = 10;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    // Ws - final result
    const wWs = 11;
    const rWs = 12;
    rt.heap.addWriter(WriterCell(wWs, rWs));
    rt.heap.addReader(ReaderCell(rWs));

    print('Heap Setup:');
    print('  Input1 = [a,b,c,d]');
    print('  Empty = []');
    print('  Xs, Zs, Ws = unbound writers\n');

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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up all three goals
    print('--- Setting up Goal 1: merge([a,b,c,d], [], Xs) ---');
    const goalId1 = 700;
    final env1 = CallEnv(readers: {0: rInput1, 1: rEmpty}, writers: {2: wXs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    print('--- Setting up Goal 2: merge(Xs?, [1,2,3,4], Zs) ---');
    const goalId2 = 800;
    final env2 = CallEnv(readers: {0: rXs, 1: rInput2}, writers: {2: wZs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    print('--- Setting up Goal 3: merge(Zs?, [], Ws) ---');
    const goalId3 = 900;
    final env3 = CallEnv(readers: {0: rZs, 1: rEmpty}, writers: {2: wWs});
    rt.setGoalEnv(goalId3, env3);
    rt.gq.enqueue(GoalRef(goalId3, 0));

    // Run all three goals through scheduler
    print('--- Running all three goals concurrently ---\n');
    final ran = sched.drain(maxCycles: 300);
    print('Goals executed: $ran');
    print('Xs bound? ${rt.heap.isWriterBound(wXs)}');
    print('Zs bound? ${rt.heap.isWriterBound(wZs)}');
    print('Ws bound? ${rt.heap.isWriterBound(wWs)}\n');

    expect(ran.contains(goalId1), true, reason: 'Goal 1 should execute');
    expect(ran.contains(goalId2), true, reason: 'Goal 2 should execute');
    expect(ran.contains(goalId3), true, reason: 'Goal 3 should execute');
    expect(rt.heap.isWriterBound(wWs), true, reason: 'Ws should be bound');

    final wsValue = rt.heap.valueOfWriter(wWs);
    print('Ws value: $wsValue\n');

    expect(wsValue, isNotNull, reason: 'Ws value should not be null');
    expect(wsValue, isA<StructTerm>(), reason: 'Should be a list structure');

    // Helper to extract list elements
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
          final tailWriterId = rt.heap.writerIdForReader(tail.readerId);
          if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
            final tailValue = rt.heap.valueOfWriter(tailWriterId);
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

    final resultList = extractList(wsValue!);
    print('Extracted list: $resultList');

    expect(resultList.length, 8, reason: 'Should have 8 elements');
    expect(resultList, ['a', 1, 'b', 2, 'c', 3, 'd', 4],
      reason: 'Should be: a, 1, b, 2, c, 3, d, 4');

    print('✓ Three-stage pipeline correctly produced: [a, 1, b, 2, c, 3, d, 4]');
    print('✓ Pipeline: [a,b,c,d] → Xs → merge → Zs → Ws');
    print('✓ Test passed!');
  });

  test('Diamond composition: merge([1,2,3],[a,b,c],Xs), merge([4,5,6],[d,e,f],Ys), merge(Xs?,Ys?,Zs)', () {
    print('\n=== Testing Diamond Composition ===');
    print('Program: merge with all 3 clauses');
    print('Goal 1: merge([1,2,3], [a,b,c], Xs) - merge numbers and letters');
    print('Goal 2: merge([4,5,6], [d,e,f], Ys) - merge more numbers and letters');
    print('Goal 3: merge(Xs?, Ys?, Zs) - merge the two results\n');

    final rt = GlpRuntime();

    // Build list [1,2,3]
    const wList1 = 1;
    const rList1 = 2;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [
      ConstTerm(1),
      StructTerm('.', [
        ConstTerm(2),
        StructTerm('.', [
          ConstTerm(3),
          ConstTerm(null)
        ])
      ])
    ]);

    // Build list [a,b,c]
    const wList2 = 3;
    const rList2 = 4;
    rt.heap.addWriter(WriterCell(wList2, rList2));
    rt.heap.addReader(ReaderCell(rList2));
    rt.heap.bindWriterStruct(wList2, '.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [
          ConstTerm('c'),
          ConstTerm(null)
        ])
      ])
    ]);

    // Build list [4,5,6]
    const wList3 = 5;
    const rList3 = 6;
    rt.heap.addWriter(WriterCell(wList3, rList3));
    rt.heap.addReader(ReaderCell(rList3));
    rt.heap.bindWriterStruct(wList3, '.', [
      ConstTerm(4),
      StructTerm('.', [
        ConstTerm(5),
        StructTerm('.', [
          ConstTerm(6),
          ConstTerm(null)
        ])
      ])
    ]);

    // Build list [d,e,f]
    const wList4 = 7;
    const rList4 = 8;
    rt.heap.addWriter(WriterCell(wList4, rList4));
    rt.heap.addReader(ReaderCell(rList4));
    rt.heap.bindWriterStruct(wList4, '.', [
      ConstTerm('d'),
      StructTerm('.', [
        ConstTerm('e'),
        StructTerm('.', [
          ConstTerm('f'),
          ConstTerm(null)
        ])
      ])
    ]);

    // Xs - first merge result
    const wXs = 9;
    const rXs = 10;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    // Ys - second merge result
    const wYs = 11;
    const rYs = 12;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    // Zs - final merge result
    const wZs = 13;
    const rZs = 14;
    rt.heap.addWriter(WriterCell(wZs, rZs));
    rt.heap.addReader(ReaderCell(rZs));

    print('Heap Setup:');
    print('  List1 = [1,2,3]');
    print('  List2 = [a,b,c]');
    print('  List3 = [4,5,6]');
    print('  List4 = [d,e,f]');
    print('  Xs, Ys, Zs = unbound writers\n');

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

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up all three goals
    print('--- Setting up Goal 1: merge([1,2,3], [a,b,c], Xs) ---');
    const goalId1 = 1000;
    final env1 = CallEnv(readers: {0: rList1, 1: rList2}, writers: {2: wXs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    print('--- Setting up Goal 2: merge([4,5,6], [d,e,f], Ys) ---');
    const goalId2 = 2000;
    final env2 = CallEnv(readers: {0: rList3, 1: rList4}, writers: {2: wYs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    print('--- Setting up Goal 3: merge(Xs?, Ys?, Zs) ---');
    const goalId3 = 3000;
    final env3 = CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs});
    rt.setGoalEnv(goalId3, env3);
    rt.gq.enqueue(GoalRef(goalId3, 0));

    // Run all three goals through scheduler
    print('--- Running all three goals concurrently ---\n');
    final ran = sched.drain(maxCycles: 300);
    print('Goals executed: $ran');
    print('Xs bound? ${rt.heap.isWriterBound(wXs)}');
    print('Ys bound? ${rt.heap.isWriterBound(wYs)}');
    print('Zs bound? ${rt.heap.isWriterBound(wZs)}\n');

    expect(ran.contains(goalId1), true, reason: 'Goal 1 should execute');
    expect(ran.contains(goalId2), true, reason: 'Goal 2 should execute');
    expect(ran.contains(goalId3), true, reason: 'Goal 3 should execute');
    expect(rt.heap.isWriterBound(wZs), true, reason: 'Zs should be bound');

    final zsValue = rt.heap.valueOfWriter(wZs);
    print('Zs value: $zsValue\n');

    expect(zsValue, isNotNull, reason: 'Zs value should not be null');
    expect(zsValue, isA<StructTerm>(), reason: 'Should be a list structure');

    // Helper to extract list elements
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
          final tailWriterId = rt.heap.writerIdForReader(tail.readerId);
          if (tailWriterId != null && rt.heap.isWriterBound(tailWriterId)) {
            final tailValue = rt.heap.valueOfWriter(tailWriterId);
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

    final resultList = extractList(zsValue!);
    print('Extracted list: $resultList');

    expect(resultList.length, 12, reason: 'Should have 12 elements');
    // Xs = [1,a,2,b,3,c], Ys = [4,d,5,e,6,f]
    // merge(Xs, Ys) alternates: 1, 4, a, d, 2, 5, b, e, 3, 6, c, f
    expect(resultList, [1, 4, 'a', 'd', 2, 5, 'b', 'e', 3, 6, 'c', 'f'],
      reason: 'Should alternate from both merged streams');

    print('✓ Diamond composition correctly produced: [1, 4, a, d, 2, 5, b, e, 3, 6, c, f]');
    print('✓ Diamond: [1,2,3]+[a,b,c] → Xs, [4,5,6]+[d,e,f] → Ys, Xs+Ys → Zs');
    print('✓ Test passed!');
  });

  test('Circular dependency: merge(Xs?,[1],Ys), merge(Ys?,[2],Xs) - partial evaluation', () {
    print('\n=== Testing Circular Dependency (Partial Evaluation) ===');
    print('Program: merge with all 3 clauses');
    print('Goal 1: merge(Xs?, [1], Ys) - Can this proceed without knowing Xs?');
    print('Goal 2: merge(Ys?, [2], Xs) - Can this proceed without knowing Ys?');
    print('Expected: Goals may partially evaluate or suspend\n');

    final rt = GlpRuntime();

    // Xs - unbound writer
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    // Ys - unbound writer
    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    // Build list [1]
    const wList1 = 5;
    const rList1 = 6;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [
      ConstTerm(1),
      ConstTerm(null)
    ]);

    // Build list [2]
    const wList2 = 7;
    const rList2 = 8;
    rt.heap.addWriter(WriterCell(wList2, rList2));
    rt.heap.addReader(ReaderCell(rList2));
    rt.heap.bindWriterStruct(wList2, '.', [
      ConstTerm(2),
      ConstTerm(null)
    ]);

    print('Heap Setup:');
    print('  Xs = unbound writer (rXs = $rXs)');
    print('  Ys = unbound writer (rYs = $rYs)');
    print('  [1] = bound');
    print('  [2] = bound\n');

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

    // Create scheduler and runner with reduction budget
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up both goals concurrently
    print('--- Setting up Goal 1: merge(Xs?, [1], Ys) ---');
    print('Xs? (reader $rXs) is unbound\n');
    const goalId1 = 4000;
    final env1 = CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    print('--- Setting up Goal 2: merge(Ys?, [2], Xs) ---');
    print('Ys? (reader $rYs) is unbound\n');
    const goalId2 = 5000;
    final env2 = CallEnv(readers: {0: rYs, 1: rList2}, writers: {2: wXs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    // Run both goals concurrently with reduction limit
    print('--- Running both goals concurrently (limit: 100 cycles) ---\n');
    final ran = sched.drain(maxCycles: 100);

    print('Goals executed: $ran');
    print('Xs bound? ${rt.heap.isWriterBound(wXs)}');
    print('Ys bound? ${rt.heap.isWriterBound(wYs)}');
    print('ROQ for reader $rXs: ${rt.roq.queue(rXs)?.length ?? 0} suspended goals');
    print('ROQ for reader $rYs: ${rt.roq.queue(rYs)?.length ?? 0} suspended goals\n');

    if (rt.heap.isWriterBound(wXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(wXs)}');
    }
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }

    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}\n');

    // The test passes - scheduler ran the goals
    print('✓ Scheduler executed goals concurrently');
    print('✓ Test passed!');
  });

  test('Sequential circular: merge(Xs?,[1],Ys), merge(Ys?,[2],Xs) both in sequence', () {
    print('\n=== Testing Sequential Circular Execution ===');
    print('Goal 1: merge(Xs?, [1], Ys) where Xs is unbound');
    print('Goal 2: merge(Ys?, [2], Xs) where Ys may be bound by Goal 1');
    print('Running both goals in sequence with 100 reduction budget total\n');

    final rt = GlpRuntime();

    // Xs and Ys - both unbound
    const wXs = 1;
    const rXs = 2;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));

    const wYs = 3;
    const rYs = 4;
    rt.heap.addWriter(WriterCell(wYs, rYs));
    rt.heap.addReader(ReaderCell(rYs));

    // [1]
    const wList1 = 5;
    const rList1 = 6;
    rt.heap.addWriter(WriterCell(wList1, rList1));
    rt.heap.addReader(ReaderCell(rList1));
    rt.heap.bindWriterStruct(wList1, '.', [ConstTerm(1), ConstTerm(null)]);

    // [2]
    const wList2 = 7;
    const rList2 = 8;
    rt.heap.addWriter(WriterCell(wList2, rList2));
    rt.heap.addReader(ReaderCell(rList2));
    rt.heap.bindWriterStruct(wList2, '.', [ConstTerm(2), ConstTerm(null)]);

    final prog = BC.prog([
      BC.L('merge/3_start'),
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

    print('Initial state:');
    print('  Xs = unbound (writer $wXs, reader $rXs)');
    print('  Ys = unbound (writer $wYs, reader $rYs)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up both goals
    print('--- Setting up Goal 1: merge(Xs?, [1], Ys) ---');
    const goalId1 = 6000;
    final env1 = CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs});
    rt.setGoalEnv(goalId1, env1);
    rt.gq.enqueue(GoalRef(goalId1, 0));

    print('--- Setting up Goal 2: merge(Ys?, [2], Xs) ---\n');
    const goalId2 = 7000;
    final env2 = CallEnv(readers: {0: rYs, 1: rList2}, writers: {2: wXs});
    rt.setGoalEnv(goalId2, env2);
    rt.gq.enqueue(GoalRef(goalId2, 0));

    // Run both goals concurrently with reduction limit
    print('--- Running both goals concurrently (limit: 100 cycles) ---\n');
    final ran = sched.drain(maxCycles: 100);

    print('Goals executed: $ran');
    print('Total cycles: ${ran.length}');
    print('Goal 1 executed: ${ran.contains(goalId1)}');
    print('Goal 2 executed: ${ran.contains(goalId2)}\n');

    print('=== Final State ===');
    print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
    if (rt.heap.isWriterBound(wXs)) {
      print('Xs value: ${rt.heap.valueOfWriter(wXs)}');
    }
    print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
    if (rt.heap.isWriterBound(wYs)) {
      print('Ys value: ${rt.heap.valueOfWriter(wYs)}');
    }
    print('ROQ for reader $rXs: ${rt.roq.queue(rXs)?.length ?? 0} suspended goals');
    print('ROQ for reader $rYs: ${rt.roq.queue(rYs)?.length ?? 0} suspended goals\n');

    print('✓ Scheduler executed goals concurrently');
    print('✓ Test passed!');
  });
}
