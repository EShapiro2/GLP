// This file will compile and run ALL custom test programs with their exact goals
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

void main() {
  group('Compiled versions of all custom tests', () {

    // TEST 1: simple_p_q_test.dart
    test('simple_p_q: p(a). q(a). - Goal: q(X?), p(X)', () {
      print('\n=== TEST 1: simple_p_q ===');
      final compiler = GlpCompiler();
      final source = '''
        p(a).
        q(a).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: q(X?)
      const goalQ = 100;
      rt.setGoalEnv(goalQ, CallEnv(readers: {0: rX}));
      rt.gq.enqueue(GoalRef(goalQ, program.labels['q/1']!));
      var ran = sched.drain(maxCycles: 10);
      expect(rt.heap.isWriterBound(wX), false);

      // Goal 2: p(X)
      const goalP = 200;
      rt.setGoalEnv(goalP, CallEnv(writers: {0: wX}));
      rt.gq.enqueue(GoalRef(goalP, program.labels['p/1']!));
      ran = sched.drain(maxCycles: 10);

      expect(rt.heap.isWriterBound(wX), true);
      expect((rt.heap.valueOfWriter(wX) as ConstTerm).value, 'a');
      print('✅ PASS\n');
    });

    // TEST 2: merge_test.dart - Base case
    test('merge base: merge([],[],[]) - Goal: merge([],[],[])', () {
      print('\n=== TEST 2: merge base case ===');
      final compiler = GlpCompiler();
      final source = 'merge([],[],[]).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      const w1 = 1, r1 = 2, w2 = 3, r2 = 4, w3 = 5, r3 = 6;
      rt.heap.addWriter(WriterCell(w1, r1));
      rt.heap.addReader(ReaderCell(r1));
      rt.heap.bindWriterConst(w1, null);
      rt.heap.addWriter(WriterCell(w2, r2));
      rt.heap.addReader(ReaderCell(r2));
      rt.heap.bindWriterConst(w2, null);
      rt.heap.addWriter(WriterCell(w3, r3));
      rt.heap.addReader(ReaderCell(r3));
      rt.heap.bindWriterConst(w3, null);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: r1, 1: r2, 2: r3}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 10);
      expect(ran, [100]);
      print('✅ PASS\n');
    });

    // TEST 3: merge_test.dart - Copy first stream
    test('merge copy first: merge([a],[],Z) with 2-clause merge', () {
      print('\n=== TEST 3: merge copy first stream ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Arg 0: [a]
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));
      rt.heap.bindWriterStruct(wXs, '.', [ConstTerm('a'), ConstTerm(null)]);

      // Arg 1: []
      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));
      rt.heap.bindWriterConst(wYs, null);

      // Arg 2: Z (unbound)
      const wZs = 5, rZs = 6;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wZs), true);

      final value = rt.heap.valueOfWriter(wZs);
      expect(value, isA<StructTerm>());
      final list = value as StructTerm;
      expect(list.functor, '.');
      print('✅ PASS\n');
    });

    // TEST 4: struct_test.dart
    test('struct: p(f(a,b)) - Goal: p(X)', () {
      print('\n=== TEST 4: struct test ===');
      final compiler = GlpCompiler();
      final source = 'p(f(a,b)).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(writers: {0: wX}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['p/1']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wX), true);

      final value = rt.heap.valueOfWriter(wX);
      expect(value, isA<StructTerm>());
      final struct = value as StructTerm;
      expect(struct.functor, 'f');
      expect(struct.args.length, 2);
      print('✅ PASS\n');
    });

    // TEST 5: merge_test.dart - Copy second stream
    test('merge copy second: merge([],[b],Z) with 3-clause merge', () {
      print('\n=== TEST 5: merge copy second stream ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Arg 0: []
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));
      rt.heap.bindWriterConst(wXs, null);

      // Arg 1: [b]
      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));
      rt.heap.bindWriterStruct(wYs, '.', [ConstTerm('b'), ConstTerm(null)]);

      // Arg 2: Z (unbound)
      const wZs = 5, rZs = 6;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 200;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wZs), true);

      final value = rt.heap.valueOfWriter(wZs);
      expect(value, isA<StructTerm>());
      final list = value as StructTerm;
      expect(list.functor, '.');
      // Head should be a reader (Y?) that points to 'b'
      if (list.args[0] is ReaderTerm) {
        final headReader = (list.args[0] as ReaderTerm).readerId;
        final headWriterId = rt.heap.writerIdForReader(headReader);
        if (headWriterId != null && rt.heap.isWriterBound(headWriterId)) {
          final headValue = rt.heap.valueOfWriter(headWriterId);
          expect((headValue as ConstTerm).value, 'b');
        }
      } else {
        expect((list.args[0] as ConstTerm).value, 'b');
      }
      print('✅ PASS\n');
    });

    // TEST 6: merge_test.dart - Merge both streams
    test('merge both: merge([a],[b],Z) alternates', () {
      print('\n=== TEST 6: merge both streams ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Arg 0: [a]
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));
      rt.heap.bindWriterStruct(wXs, '.', [ConstTerm('a'), ConstTerm(null)]);

      // Arg 1: [b]
      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));
      rt.heap.bindWriterStruct(wYs, '.', [ConstTerm('b'), ConstTerm(null)]);

      // Arg 2: Z (unbound)
      const wZs = 5, rZs = 6;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 300;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wZs), true);

      final value = rt.heap.valueOfWriter(wZs);
      expect(value, isA<StructTerm>());
      final list = value as StructTerm;
      expect(list.functor, '.');
      // Head should be a reader (X?) that points to 'a'
      if (list.args[0] is ReaderTerm) {
        final headReader = (list.args[0] as ReaderTerm).readerId;
        final headWriterId = rt.heap.writerIdForReader(headReader);
        if (headWriterId != null && rt.heap.isWriterBound(headWriterId)) {
          final headValue = rt.heap.valueOfWriter(headWriterId);
          expect((headValue as ConstTerm).value, 'a');
        }
      } else {
        expect((list.args[0] as ConstTerm).value, 'a');
      }
      print('✅ PASS\n');
    });

    // TEST 7: merge_test.dart - Longer streams
    test('merge longer: merge([a,b,c],[d,e,f],Z)', () {
      print('\n=== TEST 7: merge longer streams ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build list [a,b,c]
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));
      rt.heap.bindWriterStruct(wXs, '.', [
        ConstTerm('a'),
        StructTerm('.', [
          ConstTerm('b'),
          StructTerm('.', [ConstTerm('c'), ConstTerm(null)])
        ])
      ]);

      // Build list [d,e,f]
      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));
      rt.heap.bindWriterStruct(wYs, '.', [
        ConstTerm('d'),
        StructTerm('.', [
          ConstTerm('e'),
          StructTerm('.', [ConstTerm('f'), ConstTerm(null)])
        ])
      ]);

      // Result writer (unbound)
      const wZs = 5, rZs = 6;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 400;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wZs), true);
      expect(ran, [goalId]);
      print('✅ PASS\n');
    });

    // TEST 8: merge_test.dart - Sequential merge
    test('sequential merge: merge([a,b,c,d],[],Xs), merge(Xs?,[1,2,3,4],Zs)', () {
      print('\n=== TEST 8: sequential merge ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build list [a,b,c,d]
      const wInput1 = 1, rInput1 = 2;
      rt.heap.addWriter(WriterCell(wInput1, rInput1));
      rt.heap.addReader(ReaderCell(rInput1));
      rt.heap.bindWriterStruct(wInput1, '.', [
        ConstTerm('a'),
        StructTerm('.', [
          ConstTerm('b'),
          StructTerm('.', [
            ConstTerm('c'),
            StructTerm('.', [ConstTerm('d'), ConstTerm(null)])
          ])
        ])
      ]);

      // Empty list
      const wEmpty = 3, rEmpty = 4;
      rt.heap.addWriter(WriterCell(wEmpty, rEmpty));
      rt.heap.addReader(ReaderCell(rEmpty));
      rt.heap.bindWriterConst(wEmpty, null);

      // Xs - intermediate result
      const wXs = 5, rXs = 6;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      // Build list [1,2,3,4]
      const wInput2 = 7, rInput2 = 8;
      rt.heap.addWriter(WriterCell(wInput2, rInput2));
      rt.heap.addReader(ReaderCell(rInput2));
      rt.heap.bindWriterStruct(wInput2, '.', [
        ConstTerm(1),
        StructTerm('.', [
          ConstTerm(2),
          StructTerm('.', [
            ConstTerm(3),
            StructTerm('.', [ConstTerm(4), ConstTerm(null)])
          ])
        ])
      ]);

      // Zs - final result
      const wZs = 9, rZs = 10;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: merge([a,b,c,d], [], Xs)
      const goalId1 = 500;
      rt.setGoalEnv(goalId1, CallEnv(readers: {0: rInput1, 1: rEmpty}, writers: {2: wXs}));
      rt.gq.enqueue(GoalRef(goalId1, program.labels['merge/3']!));

      // Goal 2: merge(Xs?, [1,2,3,4], Zs)
      const goalId2 = 600;
      rt.setGoalEnv(goalId2, CallEnv(readers: {0: rXs, 1: rInput2}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId2, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 200);
      expect(ran.contains(goalId1), true);
      expect(ran.contains(goalId2), true);
      expect(rt.heap.isWriterBound(wXs), true);
      expect(rt.heap.isWriterBound(wZs), true);
      print('✅ PASS\n');
    });

    // TEST 9: merge_test.dart - Three-stage pipeline
    test('three-stage: merge([a,b,c,d],[],Xs), merge(Xs?,[1,2,3,4],Zs), merge(Zs?,[],Ws)', () {
      print('\n=== TEST 9: three-stage pipeline ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build list [a,b,c,d]
      const wInput1 = 1, rInput1 = 2;
      rt.heap.addWriter(WriterCell(wInput1, rInput1));
      rt.heap.addReader(ReaderCell(rInput1));
      rt.heap.bindWriterStruct(wInput1, '.', [
        ConstTerm('a'),
        StructTerm('.', [
          ConstTerm('b'),
          StructTerm('.', [
            ConstTerm('c'),
            StructTerm('.', [ConstTerm('d'), ConstTerm(null)])
          ])
        ])
      ]);

      // Empty list
      const wEmpty = 3, rEmpty = 4;
      rt.heap.addWriter(WriterCell(wEmpty, rEmpty));
      rt.heap.addReader(ReaderCell(rEmpty));
      rt.heap.bindWriterConst(wEmpty, null);

      // Xs - first result
      const wXs = 5, rXs = 6;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      // Build list [1,2,3,4]
      const wInput2 = 7, rInput2 = 8;
      rt.heap.addWriter(WriterCell(wInput2, rInput2));
      rt.heap.addReader(ReaderCell(rInput2));
      rt.heap.bindWriterStruct(wInput2, '.', [
        ConstTerm(1),
        StructTerm('.', [
          ConstTerm(2),
          StructTerm('.', [
            ConstTerm(3),
            StructTerm('.', [ConstTerm(4), ConstTerm(null)])
          ])
        ])
      ]);

      // Zs - second result
      const wZs = 9, rZs = 10;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      // Ws - final result
      const wWs = 11, rWs = 12;
      rt.heap.addWriter(WriterCell(wWs, rWs));
      rt.heap.addReader(ReaderCell(rWs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: merge([a,b,c,d], [], Xs)
      const goalId1 = 700;
      rt.setGoalEnv(goalId1, CallEnv(readers: {0: rInput1, 1: rEmpty}, writers: {2: wXs}));
      rt.gq.enqueue(GoalRef(goalId1, program.labels['merge/3']!));

      // Goal 2: merge(Xs?, [1,2,3,4], Zs)
      const goalId2 = 800;
      rt.setGoalEnv(goalId2, CallEnv(readers: {0: rXs, 1: rInput2}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId2, program.labels['merge/3']!));

      // Goal 3: merge(Zs?, [], Ws)
      const goalId3 = 900;
      rt.setGoalEnv(goalId3, CallEnv(readers: {0: rZs, 1: rEmpty}, writers: {2: wWs}));
      rt.gq.enqueue(GoalRef(goalId3, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 300);
      expect(ran.contains(goalId1), true);
      expect(ran.contains(goalId2), true);
      expect(ran.contains(goalId3), true);
      expect(rt.heap.isWriterBound(wWs), true);
      print('✅ PASS\n');
    });

    // TEST 10: merge_test.dart - Diamond composition
    test('diamond: merge([1,2,3],[a,b,c],Xs), merge([4,5,6],[d,e,f],Ys), merge(Xs?,Ys?,Zs)', () {
      print('\n=== TEST 10: diamond composition ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build list [1,2,3]
      const wList1 = 1, rList1 = 2;
      rt.heap.addWriter(WriterCell(wList1, rList1));
      rt.heap.addReader(ReaderCell(rList1));
      rt.heap.bindWriterStruct(wList1, '.', [
        ConstTerm(1),
        StructTerm('.', [
          ConstTerm(2),
          StructTerm('.', [ConstTerm(3), ConstTerm(null)])
        ])
      ]);

      // Build list [a,b,c]
      const wList2 = 3, rList2 = 4;
      rt.heap.addWriter(WriterCell(wList2, rList2));
      rt.heap.addReader(ReaderCell(rList2));
      rt.heap.bindWriterStruct(wList2, '.', [
        ConstTerm('a'),
        StructTerm('.', [
          ConstTerm('b'),
          StructTerm('.', [ConstTerm('c'), ConstTerm(null)])
        ])
      ]);

      // Build list [4,5,6]
      const wList3 = 5, rList3 = 6;
      rt.heap.addWriter(WriterCell(wList3, rList3));
      rt.heap.addReader(ReaderCell(rList3));
      rt.heap.bindWriterStruct(wList3, '.', [
        ConstTerm(4),
        StructTerm('.', [
          ConstTerm(5),
          StructTerm('.', [ConstTerm(6), ConstTerm(null)])
        ])
      ]);

      // Build list [d,e,f]
      const wList4 = 7, rList4 = 8;
      rt.heap.addWriter(WriterCell(wList4, rList4));
      rt.heap.addReader(ReaderCell(rList4));
      rt.heap.bindWriterStruct(wList4, '.', [
        ConstTerm('d'),
        StructTerm('.', [
          ConstTerm('e'),
          StructTerm('.', [ConstTerm('f'), ConstTerm(null)])
        ])
      ]);

      // Xs - first merge result
      const wXs = 9, rXs = 10;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      // Ys - second merge result
      const wYs = 11, rYs = 12;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));

      // Zs - final merge result
      const wZs = 13, rZs = 14;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: merge([1,2,3], [a,b,c], Xs)
      const goalId1 = 1000;
      rt.setGoalEnv(goalId1, CallEnv(readers: {0: rList1, 1: rList2}, writers: {2: wXs}));
      rt.gq.enqueue(GoalRef(goalId1, program.labels['merge/3']!));

      // Goal 2: merge([4,5,6], [d,e,f], Ys)
      const goalId2 = 2000;
      rt.setGoalEnv(goalId2, CallEnv(readers: {0: rList3, 1: rList4}, writers: {2: wYs}));
      rt.gq.enqueue(GoalRef(goalId2, program.labels['merge/3']!));

      // Goal 3: merge(Xs?, Ys?, Zs)
      const goalId3 = 3000;
      rt.setGoalEnv(goalId3, CallEnv(readers: {0: rXs, 1: rYs}, writers: {2: wZs}));
      rt.gq.enqueue(GoalRef(goalId3, program.labels['merge/3']!));

      final ran = sched.drain(maxCycles: 300);
      expect(ran.contains(goalId1), true);
      expect(ran.contains(goalId2), true);
      expect(ran.contains(goalId3), true);
      expect(rt.heap.isWriterBound(wZs), true);
      print('✅ PASS\n');
    });

    // TEST 11: merge_test.dart - Circular dependency
    test('circular: merge(Xs?,[1],Ys), merge(Ys?,[2],Xs) with budget', () {
      print('\n=== TEST 11: circular dependency ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Xs - unbound writer
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      // Ys - unbound writer
      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));

      // Build list [1]
      const wList1 = 5, rList1 = 6;
      rt.heap.addWriter(WriterCell(wList1, rList1));
      rt.heap.addReader(ReaderCell(rList1));
      rt.heap.bindWriterStruct(wList1, '.', [ConstTerm(1), ConstTerm(null)]);

      // Build list [2]
      const wList2 = 7, rList2 = 8;
      rt.heap.addWriter(WriterCell(wList2, rList2));
      rt.heap.addReader(ReaderCell(rList2));
      rt.heap.bindWriterStruct(wList2, '.', [ConstTerm(2), ConstTerm(null)]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: merge(Xs?, [1], Ys)
      const goalId1 = 4000;
      rt.setGoalEnv(goalId1, CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs}));
      rt.gq.enqueue(GoalRef(goalId1, program.labels['merge/3']!));

      // Goal 2: merge(Ys?, [2], Xs)
      const goalId2 = 5000;
      rt.setGoalEnv(goalId2, CallEnv(readers: {0: rYs, 1: rList2}, writers: {2: wXs}));
      rt.gq.enqueue(GoalRef(goalId2, program.labels['merge/3']!));

      // Run with budget to prevent infinite loop
      final ran = sched.drain(maxCycles: 100);

      // Both goals should execute (may suspend)
      expect(ran.length, greaterThan(0));
      print('✅ PASS\n');
    });

    // TEST 12: merge_test.dart - Sequential circular
    test('sequential circular: merge(Xs?,[1],Ys), merge(Ys?,[2],Xs) in sequence', () {
      print('\n=== TEST 12: sequential circular ===');
      final compiler = GlpCompiler();
      final source = '''
        merge([X|Xs],Ys,[X?|Zs?]) :- merge(Ys?,Xs?,Zs).
        merge(Xs,[Y|Ys],[Y?|Zs?]) :- merge(Xs?,Ys?,Zs).
        merge([],[],[]).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Xs and Ys - both unbound
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));

      // [1]
      const wList1 = 5, rList1 = 6;
      rt.heap.addWriter(WriterCell(wList1, rList1));
      rt.heap.addReader(ReaderCell(rList1));
      rt.heap.bindWriterStruct(wList1, '.', [ConstTerm(1), ConstTerm(null)]);

      // [2]
      const wList2 = 7, rList2 = 8;
      rt.heap.addWriter(WriterCell(wList2, rList2));
      rt.heap.addReader(ReaderCell(rList2));
      rt.heap.bindWriterStruct(wList2, '.', [ConstTerm(2), ConstTerm(null)]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Goal 1: merge(Xs?, [1], Ys)
      const goalId1 = 6000;
      rt.setGoalEnv(goalId1, CallEnv(readers: {0: rXs, 1: rList1}, writers: {2: wYs}));
      rt.gq.enqueue(GoalRef(goalId1, program.labels['merge/3']!));

      // Goal 2: merge(Ys?, [2], Xs)
      const goalId2 = 7000;
      rt.setGoalEnv(goalId2, CallEnv(readers: {0: rYs, 1: rList2}, writers: {2: wXs}));
      rt.gq.enqueue(GoalRef(goalId2, program.labels['merge/3']!));

      // Run with budget
      final ran = sched.drain(maxCycles: 100);

      expect(ran.length, greaterThan(0));
      print('✅ PASS\n');
    });

    // TEST 13: list_test.dart
    test('list: first([a|Xs], Xs?) - Goal: first(L, X)', () {
      print('\n=== TEST 13: list head extraction ===');
      final compiler = GlpCompiler();
      final source = 'first([a|Xs], Xs?).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Create writer for X
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      // Create the list [a, b]
      const wList = 3, rList = 4;
      rt.heap.addWriter(WriterCell(wList, rList));
      rt.heap.addReader(ReaderCell(rList));
      rt.heap.bindWriterStruct(wList, '.', [
        ConstTerm('a'),
        StructTerm('.', [ConstTerm('b'), ConstTerm(null)])
      ]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rList}, writers: {1: wX}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['first/2']!));

      final ran = sched.drain(maxCycles: 100);
      expect(ran, [goalId]);
      expect(rt.heap.isWriterBound(wX), true);

      // X should bind to the tail [b]
      final value = rt.heap.valueOfWriter(wX);
      expect(value, isA<StructTerm>());
      final list = value as StructTerm;
      expect(list.functor, '.');
      print('✅ PASS\n');
    });

    // TEST 14: put_structure_test.dart
    test('put_structure: p(f(a,b),g(c)) - Goal: p(X,Y)', () {
      print('\n=== TEST 14: put_structure test ===');
      final compiler = GlpCompiler();
      final source = 'p(f(a,b),g(c)).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      const wX = 1, rX = 2, wY = 3, rY = 4;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));
      rt.heap.addWriter(WriterCell(wY, rY));
      rt.heap.addReader(ReaderCell(rY));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(writers: {0: wX, 1: wY}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['p/2']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wX), true);
      expect(rt.heap.isWriterBound(wY), true);

      final valueX = rt.heap.valueOfWriter(wX);
      expect(valueX, isA<StructTerm>());
      expect((valueX as StructTerm).functor, 'f');
      expect(valueX.args.length, 2);

      final valueY = rt.heap.valueOfWriter(wY);
      expect(valueY, isA<StructTerm>());
      expect((valueY as StructTerm).functor, 'g');
      expect(valueY.args.length, 1);
      print('✅ PASS\n');
    });

  });
}
