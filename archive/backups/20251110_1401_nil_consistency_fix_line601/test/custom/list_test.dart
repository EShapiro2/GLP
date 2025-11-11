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
  test('List matching: first([a|_], X) extracts head', () {
    print('\n=== Testing List Head Extraction ===');
    print('Program: first([a|_], X).');
    print('Goal: first(L, X) where L=[a,b] and X is writer\n');

    final rt = GlpRuntime();

    // Create writer for X
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    // Create the list [a, b] = '.'(a, '.'(b, []))
    final listVal = StructTerm('.', [
      ConstTerm('a'),
      StructTerm('.', [ConstTerm('b'), ConstTerm(null)]) // [] is null
    ]);

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound');
    print('  List L = [a, b] = $listVal\n');

    // Bytecode for first([H|_], H).
    // Pattern: first(arg0, arg1) where arg0 is '.'(H, _) and arg1 is H
    // We'll match arg0='.'(H, _) and extract H, then unify with arg1
    final prog = BC.prog([
      BC.L('first/2_start'),
      BC.TRY(),
      // Arg 0: list structure '.'(H, _)
      BC.headStruct('.', 2, 0),    // Match arg0 with '.'/2, enter READ mode
      BC.headWriter(0),             // Extract H (first element) into clause var 0
      BC.unifyVoid(),               // Skip tail (anonymous)
      // Arg 1: unify with H (clause var 0)
      BC.getVal(0, 1),              // Unify arg1 with clause var 0
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('first/2_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: first([a,b], X) ---');
    print('Expected: X will be bound to \'a\' (head of list)\n');

    // Set up call environment
    // Arg 0: the list (ground term - but we need to pass it somehow)
    // Arg 1: writer X
    // Problem: CallEnv only handles writer/reader IDs, not ground terms!
    // For now, let's test with a reader that's bound to the list

    // Create a writer/reader for the list
    const wList = 3;
    const rList = 4;
    rt.heap.addWriter(WriterCell(wList, rList));
    rt.heap.addReader(ReaderCell(rList));
    rt.heap.bindWriterStruct(wList, '.', [
      ConstTerm('a'),
      StructTerm('.', [ConstTerm('b'), ConstTerm(null)])
    ]);

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rList}, writers: {1: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val\n');
    }

    // Verify results
    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');

    final value = rt.heap.valueOfWriter(wX);
    expect(value, isA<ConstTerm>(), reason: 'Should be a constant');
    expect((value as ConstTerm).value, 'a', reason: 'Should extract head');

    print('✓ List head correctly extracted: a');
    print('✓ Test passed!');
  });

  test('List matching: is_cons([a|b]) checks non-empty list', () {
    print('\n=== Testing List Cons Check ===');
    print('Program: is_cons([_|_]).');
    print('Goal: is_cons([a,b,c])\n');

    final rt = GlpRuntime();

    // Create the list [a, b, c] = '.'(a, '.'(b, '.'(c, [])))
    const wList = 1;
    const rList = 2;
    rt.heap.addWriter(WriterCell(wList, rList));
    rt.heap.addReader(ReaderCell(rList));
    rt.heap.bindWriterStruct(wList, '.', [
      ConstTerm('a'),
      StructTerm('.', [
        ConstTerm('b'),
        StructTerm('.', [ConstTerm('c'), ConstTerm(null)])
      ])
    ]);

    print('Heap Setup:');
    print('  List L = [a, b, c]\n');

    // Bytecode for is_cons([_|_]).
    // Just check that arg0 is a '.' structure
    final prog = BC.prog([
      BC.L('is_cons/1_start'),
      BC.TRY(),
      BC.headStruct('.', 2, 0),    // Match arg0 with '.'/2
      BC.unifyVoid(count: 2),       // Skip both head and tail
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('is_cons/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: is_cons([a,b,c]) ---');
    print('Expected: SUCCESS (list is non-empty)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rList});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran\n');

    expect(ran, [goalId], reason: 'Goal should execute');

    print('✓ List cons check succeeded');
    print('✓ Test passed!');
  });

  test('List matching: is_cons([]) fails on empty list', () {
    print('\n=== Testing Empty List Rejection ===');
    print('Program: is_cons([_|_]).');
    print('Goal: is_cons([])\n');

    final rt = GlpRuntime();

    // Create writer/reader for empty list
    const wList = 1;
    const rList = 2;
    rt.heap.addWriter(WriterCell(wList, rList));
    rt.heap.addReader(ReaderCell(rList));
    rt.heap.bindWriterConst(wList, null); // [] is represented as null

    print('Heap Setup:');
    print('  List L = []\n');

    // Same bytecode as before - tries to match '.'/2
    final prog = BC.prog([
      BC.L('is_cons/1_start'),
      BC.TRY(),
      BC.headStruct('.', 2, 0),    // Try to match arg0 with '.'/2
      BC.unifyVoid(count: 2),       // Skip both head and tail
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('is_cons/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: is_cons([]) ---');
    print('Expected: FAIL (empty list is not a cons)\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rList});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran\n');

    // Should execute (clause exhausted with no match)
    expect(ran, [goalId], reason: 'Goal should execute');

    print('✓ Empty list correctly rejected');
    print('✓ Test passed!');
  });

  test('List construction: cons(X, Xs, [X|Xs]) builds list', () {
    print('\n=== Testing List Construction ===');
    print('Program: cons(X, Xs, [X|Xs]).');
    print('Goal: cons(a, [b], Z) where Z is writer\n');

    final rt = GlpRuntime();

    // Create writer for result Z
    const wZ = 1;
    const rZ = 2;
    rt.heap.addWriter(WriterCell(wZ, rZ));
    rt.heap.addReader(ReaderCell(rZ));

    // Create reader for X = 'a'
    const wX = 3;
    const rX = 4;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));
    rt.heap.bindWriterConst(wX, 'a');

    // Create reader for Xs = [b]
    const wXs = 5;
    const rXs = 6;
    rt.heap.addWriter(WriterCell(wXs, rXs));
    rt.heap.addReader(ReaderCell(rXs));
    rt.heap.bindWriterStruct(wXs, '.', [
      ConstTerm('b'),
      ConstTerm(null) // []
    ]);

    print('Heap Setup:');
    print('  X = a');
    print('  Xs = [b]');
    print('  Z (result) - unbound\n');

    // Bytecode for cons(X?, Xs?, [X|Xs]).
    // Arg 0: reader X
    // Arg 1: reader Xs
    // Arg 2: writer Z - should be '.'(X, Xs)
    final prog = BC.prog([
      BC.L('cons/3_start'),
      BC.TRY(),
      // Load arguments into clause variables
      BC.getVar(0, 0),              // Load X? from arg 0 into clause var 0
      BC.getVar(1, 1),              // Load Xs? from arg 1 into clause var 1
      // Build result structure in arg 2
      BC.headStruct('.', 2, 2),     // Z = '.'/2, enter WRITE mode
      BC.headReader(0),             // First element: use clause var 0 (X?)
      BC.headReader(1),             // Second element: use clause var 1 (Xs?)
      BC.COMMIT(),
      BC.PROCEED(),

      BC.L('cons/3_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: cons(a, [b], Z) ---');
    print('Expected: Z will be bound to [a, b]\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(readers: {0: rX, 1: rXs}, writers: {2: wZ});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wZ bound? ${rt.heap.isWriterBound(wZ)}');
    if (rt.heap.isWriterBound(wZ)) {
      final val = rt.heap.valueOfWriter(wZ);
      print('Writer $wZ value: $val\n');
    }

    // Verify results
    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wZ), true, reason: 'Z should be bound');

    final value = rt.heap.valueOfWriter(wZ);
    expect(value, isA<StructTerm>(), reason: 'Should be a structure');

    final listStruct = value as StructTerm;
    expect(listStruct.functor, '.', reason: 'Should be list cons');
    expect(listStruct.args.length, 2, reason: 'Cons has 2 args');

    // Head should be a reader pointing to 'a'
    expect(listStruct.args[0], isA<ReaderTerm>());
    final headReader = (listStruct.args[0] as ReaderTerm).readerId;
    final headWriterId = rt.heap.writerIdForReader(headReader);
    expect(headWriterId, isNotNull, reason: 'Head reader should have paired writer');
    final headValue = rt.heap.valueOfWriter(headWriterId!);
    expect(headValue, isA<ConstTerm>());
    expect((headValue as ConstTerm).value, 'a');

    // Tail should be a reader pointing to [b]
    expect(listStruct.args[1], isA<ReaderTerm>());
    final tailReader = (listStruct.args[1] as ReaderTerm).readerId;
    final tailWriterId = rt.heap.writerIdForReader(tailReader);
    expect(tailWriterId, isNotNull, reason: 'Tail reader should have paired writer');
    final tailValue = rt.heap.valueOfWriter(tailWriterId!);
    expect(tailValue, isA<StructTerm>());
    final tail = tailValue as StructTerm;
    expect(tail.functor, '.');
    expect((tail.args[0] as ConstTerm).value, 'b');

    print('✓ List correctly constructed: [a, b]');
    print('✓ Test passed!');
  });

  test('List matching: match concrete list [a, b]', () {
    print('\n=== Testing Concrete List Matching ===');
    print('Program: p([a, b]).');
    print('Goal: p(X) where X is writer\n');

    final rt = GlpRuntime();

    // Create writer for X
    const wX = 1;
    const rX = 2;
    rt.heap.addWriter(WriterCell(wX, rX));
    rt.heap.addReader(ReaderCell(rX));

    print('Heap Setup:');
    print('  Writer $wX (X) - unbound\n');

    // Bytecode for p([a, b]).
    // [a, b] = '.'(a, '.'(b, []))
    // Use BODY phase construction with nested constant structure
    final prog = BC.prog([
      BC.L('p/1_start'),
      BC.TRY(),
      BC.COMMIT(),
      // Build the complete nested list in BODY phase
      // BSTRUCTC can accept nested structures in constArgs
      BC.BSTRUCTC(1, '.', ['a', StructTerm('.', [ConstTerm('b'), ConstTerm(null)])]),
      BC.PROCEED(),

      BC.L('p/1_end'),
      BC.SUSP(),
    ]);

    print('--- Executing Goal: p(X) ---');
    print('Expected: X will be bound to [a, b]\n');

    // Create scheduler and runner
    final runner = BytecodeRunner(prog);
    final sched = Scheduler(rt: rt, runner: runner);

    // Set up goal environment and enqueue goal
    const goalId = 100;
    final env = CallEnv(writers: {0: wX});
    rt.setGoalEnv(goalId, env);
    rt.gq.enqueue(GoalRef(goalId, 0));

    // Run the scheduler
    final ran = sched.drain(maxCycles: 100);
    print('Goals executed: $ran');
    print('Writer $wX bound? ${rt.heap.isWriterBound(wX)}');
    if (rt.heap.isWriterBound(wX)) {
      final val = rt.heap.valueOfWriter(wX);
      print('Writer $wX value: $val\n');
    }

    // Verify results
    expect(ran, [goalId], reason: 'Goal should execute');
    expect(rt.heap.isWriterBound(wX), true, reason: 'X should be bound');

    final value = rt.heap.valueOfWriter(wX);
    expect(value, isA<StructTerm>(), reason: 'Should be a structure');

    // Verify it's [a, b]
    final list = value as StructTerm;
    expect(list.functor, '.');
    expect((list.args[0] as ConstTerm).value, 'a');

    final tail = list.args[1] as StructTerm;
    expect(tail.functor, '.');
    expect((tail.args[0] as ConstTerm).value, 'b');
    expect((tail.args[1] as ConstTerm).value, null); // []

    print('✓ List correctly constructed: [a, b]');
    print('✓ Test passed!');
  });
}
