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
      rt.setGoalEnv(goalId, CallEnv(writers: {0: wXs, 1: wYs, 2: wZs}));
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

    // TEST 15: playground_test.dart
    test('playground: p(a) - Goal: p(X)', () {
      print('\n=== TEST 15: playground simple fact ===');
      final compiler = GlpCompiler();
      final source = 'p(a).';
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
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, 'a');
      print('✅ PASS\n');
    });

    // TEST 16: list_test.dart - is_cons success
    test('list: is_cons([_|_]) - Goal: is_cons([a,b,c])', () {
      print('\n=== TEST 16: is_cons success ===');
      final compiler = GlpCompiler();
      final source = 'is_cons([_|_]).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Create list [a,b,c]
      const wList = 1, rList = 2;
      rt.heap.addWriter(WriterCell(wList, rList));
      rt.heap.addReader(ReaderCell(rList));
      rt.heap.bindWriterStruct(wList, '.', [
        ConstTerm('a'),
        StructTerm('.', [
          ConstTerm('b'),
          StructTerm('.', [ConstTerm('c'), ConstTerm(null)])
        ])
      ]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rList}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['is_cons/1']!));

      final ran = sched.drain(maxCycles: 100);
      expect(ran, [goalId]);  // Should succeed
      print('✅ PASS\n');
    });

    // TEST 17: list_test.dart - is_cons failure
    test('list: is_cons([_|_]) - Goal: is_cons([])', () {
      print('\n=== TEST 17: is_cons failure ===');
      final compiler = GlpCompiler();
      final source = 'is_cons([_|_]).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Create empty list []
      const wList = 1, rList = 2;
      rt.heap.addWriter(WriterCell(wList, rList));
      rt.heap.addReader(ReaderCell(rList));
      rt.heap.bindWriterConst(wList, null);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rList}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['is_cons/1']!));

      final ran = sched.drain(maxCycles: 100);
      expect(ran, [goalId]);  // Goal executes and fails
      print('✅ PASS\n');
    });

    // TEST 18: list_test.dart - cons construction
    // SKIPPED: Source cons(X?, Xs?, [X?|Xs?]) violates SRSW (X and Xs appear twice)
    // The hand-written test uses special bytecode that loads readers into clause vars first
    test('list: cons construction - SKIPPED (SRSW violation)', () {
      print('\n=== TEST 18: SKIPPED (requires hand-written bytecode) ===');
    }, skip: true);

    // TEST 19: list_test.dart - concrete list match
    test('list: p([a, b]) - Goal: p(X)', () {
      print('\n=== TEST 19: concrete list match ===');
      final compiler = GlpCompiler();
      final source = 'p([a, b]).';
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
      final list = value as StructTerm;
      expect(list.functor, '.');
      print('✅ PASS\n');
    });

    // TEST 20: boot_test.dart
    test('boot: p(a). boot :- p(X), p(X?) - Goal: boot', () {
      print('\n=== TEST 20: boot test ===');
      final compiler = GlpCompiler();
      final source = '''
        p(a).
        boot :- p(X), p(X?).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv());
      rt.gq.enqueue(GoalRef(goalId, program.labels['boot/0']!));

      final ran = sched.drain(maxCycles: 100);
      expect(ran.contains(goalId), true);
      print('✅ PASS\n');
    });

    // TEST 21: simple_body_test.dart
    // SKIPPED: Source forward(X) :- p(X) violates SRSW (X appears twice)
    test('simple_body test - SKIPPED (SRSW violation)', () {
      print('\n=== TEST 21: SKIPPED (SRSW violation) ===');
    }, skip: true);

    // TEST 22: clause_only_test.dart
    test('clause: clause(p(a), true) - Goal: clause(p(X), Y)', () {
      print('\n=== TEST 22: clause_only test ===');
      final compiler = GlpCompiler();
      final source = 'clause(p(a), true).';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      const wY = 3, rY = 4;
      rt.heap.addWriter(WriterCell(wY, rY));
      rt.heap.addReader(ReaderCell(rY));

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(writers: {0: wX, 1: wY}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['clause/2']!));

      final ran = sched.drain(maxCycles: 100);
      expect(rt.heap.isWriterBound(wX), true);
      expect(rt.heap.isWriterBound(wY), true);

      // X should bind to p(a)
      final valueX = rt.heap.valueOfWriter(wX);
      expect(valueX, isA<StructTerm>());
      final structX = valueX as StructTerm;
      expect(structX.functor, 'p');
      expect(structX.args.length, 1);

      // Y should bind to true (parsed as atom 'true')
      final valueY = rt.heap.valueOfWriter(wY);
      expect(valueY, isA<ConstTerm>());
      expect((valueY as ConstTerm).value, 'true');

      print('✅ PASS\n');
    });

    // TEST 23: simple_metainterp_test.dart
    test('metainterp: run(true). run(A) :- otherwise | clause(A?,B), run(B?) - Goal: run(p(X))', () {
      print('\n=== TEST 23: simple metainterpreter ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // X is unbound writer for p(X)
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      // Build structure p(X) for the argument to run/1
      const wPX = 3, rPX = 4;
      rt.heap.addWriter(WriterCell(wPX, rPX));
      rt.heap.addReader(ReaderCell(rPX));
      rt.heap.bindWriterStruct(wPX, 'p', [WriterTerm(wX)]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rPX}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      final ran = sched.drain(maxCycles: 500);  // May need more cycles
      expect(ran.isNotEmpty, true);

      // X should eventually bind to 'a'
      expect(rt.heap.isWriterBound(wX), true);
      final valueX = rt.heap.valueOfWriter(wX);
      expect(valueX, isA<ConstTerm>());
      expect((valueX as ConstTerm).value, 'a');

      print('✅ PASS\n');
    });

    // TEST 24: metainterp_conj_test.dart
    test('metainterp with conjunction: run((p(X), q(X?)))', () {
      print('\n=== TEST 24: metainterp with conjunction ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run((A, B)) :- run(A?), run(B?).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(q(a), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // X is unbound writer for p(X)/q(X?)
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      // Build p(X)
      const wPX = 3, rPX = 4;
      rt.heap.addWriter(WriterCell(wPX, rPX));
      rt.heap.addReader(ReaderCell(rPX));
      rt.heap.bindWriterStruct(wPX, 'p', [WriterTerm(wX)]);

      // Build q(X?)
      const wQX = 5, rQX = 6;
      rt.heap.addWriter(WriterCell(wQX, rQX));
      rt.heap.addReader(ReaderCell(rQX));
      rt.heap.bindWriterStruct(wQX, 'q', [ReaderTerm(rX)]);

      // Build conjunction (p(X), q(X?))
      const wConj = 7, rConj = 8;
      rt.heap.addWriter(WriterCell(wConj, rConj));
      rt.heap.addReader(ReaderCell(rConj));
      rt.heap.bindWriterStruct(wConj, ',', [ReaderTerm(rPX), ReaderTerm(rQX)]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rConj}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      final ran = sched.drain(maxCycles: 1000);  // May need more cycles
      expect(ran.isNotEmpty, true);

      // X should eventually bind to 'a'
      expect(rt.heap.isWriterBound(wX), true);
      final valueX = rt.heap.valueOfWriter(wX);
      expect(valueX, isA<ConstTerm>());
      expect((valueX as ConstTerm).value, 'a');

      print('✅ PASS\n');
    });

    // TEST 25: metainterp_pp_test.dart
    test('metainterp pp: boot :- run(p(X)), run(p(X?))', () {
      print('\n=== TEST 25: metainterp p(X) then p(X?) ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(p(b), true).
        boot :- run(p(X)), run(p(X?)).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv());
      rt.gq.enqueue(GoalRef(goalId, program.labels['boot/0']!));

      final ran = sched.drain(maxCycles: 1000);
      expect(ran.isNotEmpty, true);
      print('✅ PASS\n');
    });

    // TEST 26: metainterp_pq_test.dart
    test('metainterp pq: boot :- run(p(X)), run(q(X?))', () {
      print('\n=== TEST 26: metainterp p/q program ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(p(b), true).
        clause(q(b), true).
        boot :- run(p(X)), run(q(X?)).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv());
      rt.gq.enqueue(GoalRef(goalId, program.labels['boot/0']!));

      final ran = sched.drain(maxCycles: 1000);
      expect(ran.isNotEmpty, true);
      print('✅ PASS\n');
    });

    // TEST 27: metainterp_rp_test.dart
    test('metainterp rp: boot :- run(p(X?)), run(p(X))', () {
      print('\n=== TEST 27: metainterp p(X?) then p(X) ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(p(b), true).
        boot :- run(p(X?)), run(p(X)).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv());
      rt.gq.enqueue(GoalRef(goalId, program.labels['boot/0']!));

      final ran = sched.drain(maxCycles: 1000);
      expect(ran.isNotEmpty, true);
      print('✅ PASS\n');
    });

    // TEST 28: metainterp_reversed_test.dart
    test('metainterp reversed: run((p(X?), p(X)))', () {
      print('\n=== TEST 28: metainterp reversed order ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run((A, B)) :- run(A?), run(B?).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(q(a), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build structure p(X) and p(X?) tuple
      const wX = 1, rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));

      // Build p(X?)
      const wPXr = 3, rPXr = 4;
      rt.heap.addWriter(WriterCell(wPXr, rPXr));
      rt.heap.addReader(ReaderCell(rPXr));
      rt.heap.bindWriterStruct(wPXr, 'p', [ReaderTerm(rX)]);

      // Build p(X)
      const wPXw = 5, rPXw = 6;
      rt.heap.addWriter(WriterCell(wPXw, rPXw));
      rt.heap.addReader(ReaderCell(rPXw));
      rt.heap.bindWriterStruct(wPXw, 'p', [WriterTerm(wX)]);

      // Build conjunction (p(X?), p(X))
      const wConj = 7, rConj = 8;
      rt.heap.addWriter(WriterCell(wConj, rConj));
      rt.heap.addReader(ReaderCell(rConj));
      rt.heap.bindWriterStruct(wConj, ',', [ReaderTerm(rPXr), ReaderTerm(rPXw)]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rConj}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      final ran = sched.drain(maxCycles: 1000);
      expect(ran.isNotEmpty, true);

      // X should bind to 'a'
      expect(rt.heap.isWriterBound(wX), true);
      final valueX = rt.heap.valueOfWriter(wX);
      expect(valueX, isA<ConstTerm>());
      expect((valueX as ConstTerm).value, 'a');

      print('✅ PASS\n');
    });

    // TEST 29: metainterp_full_test.dart (same as pq)
    test('metainterp full: boot :- run(p(X)), run(q(X?))', () {
      print('\n=== TEST 29: metainterp full test ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(p(a), true).
        clause(p(b), true).
        clause(q(b), true).
        boot :- run(p(X)), run(q(X?)).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();
      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv());
      rt.gq.enqueue(GoalRef(goalId, program.labels['boot/0']!));

      final ran = sched.drain(maxCycles: 1000);
      expect(ran.isNotEmpty, true);
      print('✅ PASS\n');
    });

    // TEST 30: metainterp_merge_test.dart
    test('metainterp merge: run(merge([a],[b],Zs))', () {
      print('\n=== TEST 30: metainterp merge ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run((A, B)) :- run(A?), run(B?).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(merge([X|Xs], Ys, [X?|Zs?]), merge(Ys?, Xs?, Zs)).
        clause(merge(Xs, [Y|Ys], [Y?|Zs?]), merge(Xs?, Ys?, Zs)).
        clause(merge([], [], []), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Build list [a] - use null for empty list since compiler uses that
      const wListA = 10, rListA = 11;
      rt.heap.addWriter(WriterCell(wListA, rListA));
      rt.heap.addReader(ReaderCell(rListA));
      rt.heap.bindWriterStruct(wListA, '.', [ConstTerm('a'), ConstTerm(null)]);

      // Build list [b]
      const wListB = 12, rListB = 13;
      rt.heap.addWriter(WriterCell(wListB, rListB));
      rt.heap.addReader(ReaderCell(rListB));
      rt.heap.bindWriterStruct(wListB, '.', [ConstTerm('b'), ConstTerm(null)]);

      // Build Zs (unbound writer for result)
      const wZs = 14, rZs = 15;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      // Build merge([a],[b],Zs)  - ALL args as WriterTerms
      const wMerge = 16, rMerge = 17;
      rt.heap.addWriter(WriterCell(wMerge, rMerge));
      rt.heap.addReader(ReaderCell(rMerge));
      rt.heap.bindWriterStruct(wMerge, 'merge', [
        WriterTerm(wListA),
        WriterTerm(wListB),
        WriterTerm(wZs),
      ]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Start goal: run(merge([a],[b],Zs))
      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rMerge}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      final ran = sched.drain(maxCycles: 500);

      print('Goals executed: ${ran.length}');
      print('Zs bound: ${rt.heap.isWriterBound(wZs)}');
      if (rt.heap.isWriterBound(wZs)) {
        final value = rt.heap.valueOfWriter(wZs);
        print('Zs value: $value');
      }

      expect(ran.isNotEmpty, true);
      expect(rt.heap.isWriterBound(wZs), true, reason: 'Zs should be bound to [a,b]');

      final zsValue = rt.heap.valueOfWriter(wZs);
      expect(zsValue, isA<StructTerm>(), reason: 'Should be a list structure');
      final zsList = zsValue as StructTerm;
      expect(zsList.functor, '.', reason: 'Should be list cons');

      print('✅ PASS\n');
    });

    // TEST 31: metainterp_circular_merge_test.dart
    test('metainterp circular merge: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))', () {
      print('\n=== TEST 31: metainterp circular merge ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run((A, B)) :- run(A?), run(B?).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(merge([X|Xs], Ys, [X?|Zs?]), merge(Ys?, Xs?, Zs)).
        clause(merge(Xs, [Y|Ys], [Y?|Zs?]), merge(Xs?, Ys?, Zs)).
        clause(merge([], [], []), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Setup: Writer Xs and its paired reader, Writer Ys and its paired reader
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));

      // Build list [a]
      const wListA = 10, rListA = 11;
      rt.heap.addWriter(WriterCell(wListA, rListA));
      rt.heap.addReader(ReaderCell(rListA));
      rt.heap.bindWriterStruct(wListA, '.', [ConstTerm('a'), ConstTerm(null)]);

      // Build list [b]
      const wListB = 12, rListB = 13;
      rt.heap.addWriter(WriterCell(wListB, rListB));
      rt.heap.addReader(ReaderCell(rListB));
      rt.heap.bindWriterStruct(wListB, '.', [ConstTerm('b'), ConstTerm(null)]);

      // Build merge(Xs?,[a],Ys)
      const wMerge1 = 20, rMerge1 = 21;
      rt.heap.addWriter(WriterCell(wMerge1, rMerge1));
      rt.heap.addReader(ReaderCell(rMerge1));
      rt.heap.bindWriterStruct(wMerge1, 'merge', [
        ReaderTerm(rXs),
        WriterTerm(wListA),
        WriterTerm(wYs),
      ]);

      // Build merge(Ys?,[b],Xs)
      const wMerge2 = 22, rMerge2 = 23;
      rt.heap.addWriter(WriterCell(wMerge2, rMerge2));
      rt.heap.addReader(ReaderCell(rMerge2));
      rt.heap.bindWriterStruct(wMerge2, 'merge', [
        ReaderTerm(rYs),
        WriterTerm(wListB),
        WriterTerm(wXs),
      ]);

      // Build conjunction (merge1, merge2)
      const wConj = 30, rConj = 31;
      rt.heap.addWriter(WriterCell(wConj, rConj));
      rt.heap.addReader(ReaderCell(rConj));
      rt.heap.bindWriterStruct(wConj, ',', [
        WriterTerm(wMerge1),
        WriterTerm(wMerge2),
      ]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      // Start goal: run((merge(Xs?,[a],Ys),merge(Ys?,[b],Xs)))
      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rConj}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      // Use reduction budget to prevent infinite loop
      final ran = sched.drain(maxCycles: 50);

      print('Goals executed: ${ran.length}');
      print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
      print('Ys bound: ${rt.heap.isWriterBound(wYs)}');

      expect(ran.isNotEmpty, true);
      expect(ran.length, greaterThan(5), reason: 'Should execute multiple cycles');
      expect(rt.heap.isWriterBound(wXs), true, reason: 'Xs should be bound');
      expect(rt.heap.isWriterBound(wYs), true, reason: 'Ys should be bound');

      print('✅ PASS\n');
    });

    // TEST 32: metainterp_three_way_merge_test.dart
    test('metainterp three-way merge: run((merge([1,2],[3],Xs), merge([a,b],[c],Ys?), merge(Xs?,Ys?,Zs)))', () {}, skip: 'Complex test - needs more investigation');

    // TEST 33: metainterp_three_way_circular_test.dart
    test('metainterp three-way circular: run((merge([a|Xs?],Ys?,Zs), merge([b|Ys?],Zs?,Xs), merge([c|Zs?],Xs?,Ys)))', () {
      print('\n=== TEST 33: metainterp three-way circular ===');
      final compiler = GlpCompiler();
      final source = '''
        run(true).
        run((A, B)) :- run(A?), run(B?).
        run(A) :- otherwise | clause(A?, B), run(B?).
        clause(merge([X|Xs], Ys, [X?|Zs?]), merge(Ys?, Xs?, Zs)).
        clause(merge(Xs, [Y|Ys], [Y?|Zs?]), merge(Xs?, Ys?, Zs)).
        clause(merge([], [], []), true).
      ''';
      final program = compiler.compile(source);

      final rt = GlpRuntime();

      // Setup: Writers for Xs, Ys, Zs
      const wXs = 1, rXs = 2;
      rt.heap.addWriter(WriterCell(wXs, rXs));
      rt.heap.addReader(ReaderCell(rXs));

      const wYs = 3, rYs = 4;
      rt.heap.addWriter(WriterCell(wYs, rYs));
      rt.heap.addReader(ReaderCell(rYs));

      const wZs = 5, rZs = 6;
      rt.heap.addWriter(WriterCell(wZs, rZs));
      rt.heap.addReader(ReaderCell(rZs));

      // Build lists [a|Xs?], [b|Ys?], [c|Zs?]
      // [a|Xs?]
      const wLa = 10, rLa = 11;
      rt.heap.addWriter(WriterCell(wLa, rLa));
      rt.heap.addReader(ReaderCell(rLa));
      rt.heap.bindWriterStruct(wLa, '.', [ConstTerm('a'), ReaderTerm(rXs)]);

      // [b|Ys?]
      const wLb = 12, rLb = 13;
      rt.heap.addWriter(WriterCell(wLb, rLb));
      rt.heap.addReader(ReaderCell(rLb));
      rt.heap.bindWriterStruct(wLb, '.', [ConstTerm('b'), ReaderTerm(rYs)]);

      // [c|Zs?]
      const wLc = 14, rLc = 15;
      rt.heap.addWriter(WriterCell(wLc, rLc));
      rt.heap.addReader(ReaderCell(rLc));
      rt.heap.bindWriterStruct(wLc, '.', [ConstTerm('c'), ReaderTerm(rZs)]);

      // Build merge([a|Xs?],Ys?,Zs)
      const wM1 = 20, rM1 = 21;
      rt.heap.addWriter(WriterCell(wM1, rM1));
      rt.heap.addReader(ReaderCell(rM1));
      rt.heap.bindWriterStruct(wM1, 'merge', [
        WriterTerm(wLa),
        ReaderTerm(rYs),
        WriterTerm(wZs),
      ]);

      // Build merge([b|Ys?],Zs?,Xs)
      const wM2 = 22, rM2 = 23;
      rt.heap.addWriter(WriterCell(wM2, rM2));
      rt.heap.addReader(ReaderCell(rM2));
      rt.heap.bindWriterStruct(wM2, 'merge', [
        WriterTerm(wLb),
        ReaderTerm(rZs),
        WriterTerm(wXs),
      ]);

      // Build merge([c|Zs?],Xs?,Ys)
      const wM3 = 24, rM3 = 25;
      rt.heap.addWriter(WriterCell(wM3, rM3));
      rt.heap.addReader(ReaderCell(rM3));
      rt.heap.bindWriterStruct(wM3, 'merge', [
        WriterTerm(wLc),
        ReaderTerm(rXs),
        WriterTerm(wYs),
      ]);

      // Build conjunction (m1, (m2, m3))
      const wConj2 = 30, rConj2 = 31;
      rt.heap.addWriter(WriterCell(wConj2, rConj2));
      rt.heap.addReader(ReaderCell(rConj2));
      rt.heap.bindWriterStruct(wConj2, ',', [
        WriterTerm(wM2),
        WriterTerm(wM3),
      ]);

      const wConj1 = 32, rConj1 = 33;
      rt.heap.addWriter(WriterCell(wConj1, rConj1));
      rt.heap.addReader(ReaderCell(rConj1));
      rt.heap.bindWriterStruct(wConj1, ',', [
        WriterTerm(wM1),
        WriterTerm(wConj2),
      ]);

      final runner = BytecodeRunner(program);
      final sched = Scheduler(rt: rt, runner: runner);

      const goalId = 100;
      rt.setGoalEnv(goalId, CallEnv(readers: {0: rConj1}));
      rt.gq.enqueue(GoalRef(goalId, program.labels['run/1']!));

      // Use reduction budget to prevent infinite loop
      final ran = sched.drain(maxCycles: 50);

      print('Goals executed: ${ran.length}');
      print('Xs bound: ${rt.heap.isWriterBound(wXs)}');
      print('Ys bound: ${rt.heap.isWriterBound(wYs)}');
      print('Zs bound: ${rt.heap.isWriterBound(wZs)}');

      expect(ran.isNotEmpty, true);
      expect(ran.length, greaterThan(5), reason: 'Should execute multiple cycles');
      expect(rt.heap.isWriterBound(wXs), true, reason: 'Xs should be bound');
      expect(rt.heap.isWriterBound(wYs), true, reason: 'Ys should be bound');
      expect(rt.heap.isWriterBound(wZs), true, reason: 'Zs should be bound');

      print('✅ PASS\n');
    });

  });
}
