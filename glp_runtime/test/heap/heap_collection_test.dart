/// Heap collection (a PROPOSAL of 2026-10-02, off unless asked for):
/// [HeapFCP.collect] keeps every cell a root reaches and reclaims the rest,
/// without moving a cell or reusing an address; [GlpRuntime.collectHeap]
/// gathers the roots --- the argument registers of every goal in the queue or
/// suspended, the readers goals wait on, the waits on when_idle and wait/1,
/// and the REPL goal's variables --- and the scheduler collects between goals
/// once enough cells have been allocated ([GlpRuntime.maybeCollectHeap]).
///
/// IGLP's heap (app:in-heap) says nothing of reclaiming a cell, and until
/// 2026-10-02 none was ever reclaimed: a year of sGLP's 100-agent social graph
/// ended with 46 million cells and 5 GB live, and its 1000-agent run ran out
/// of memory 19 simulated days in.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// gen/2 writes the stream N, ..., 1 and total/3 sums a stream; fill/2
/// appends N, ..., 1 to a mutual reference and closes it, which mr/2 sums;
/// tick/2 counts down by when_idle, one step each time the machine is idle.
const String _source = r'''
procedure gen(Integer?, Stream(Integer)).
gen(0, []).
gen(N, [N?|Xs?]) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs).

procedure total(Stream(Integer)?, Integer?, Integer).
total([X|Xs], A, S?) :- A1 := A? + X?, total(Xs?, A1?, S).
total([], A, A?).

procedure run(Integer?, Integer).
run(N, S?) :- gen(N?, Xs), total(Xs?, 0, S).

procedure fill(Integer?, MutualRef?).
fill(0, Ref) :- close_mutual_reference(Ref?).
fill(N, Ref) :- N? > 0 | stream_append(N?, Ref?, Ref1), N1 := N? - 1, fill(N1?, Ref1?).

procedure mr(Integer?, Integer).
mr(N, S?) :- allocate_mutual_reference(Ref, Xs), fill(N?, Ref?), total(Xs?, 0, S).

procedure tick(Integer?, Integer).
tick(0, 0).
tick(N, Z?) :- N? > 0, when_idle | N1 := N? - 1, tick(N1?, Z).
''';

/// The stream a reader stands for, its elements as Dart values.
List<Object?> _read(HeapFCP heap, int addr) {
  final out = <Object?>[];
  Object t = heap.derefAddr(addr);
  while (t is StructTerm && t.functor == '.') {
    final h = heap.dereference(t.args[0]);
    out.add(h is ConstTerm ? h.value : h);
    final tail = t.args[1];
    t = tail is VarRef ? heap.derefAddr(tail.addr) : tail;
  }
  return out;
}

void main() {
  group('HeapFCP.collect', () {
    test(
        'with no root every cell is reclaimed, every segment but the one '
        'allocation is filling is dropped, and a reclaimed cell throws when '
        'read', () {
      final heap = HeapFCP();
      for (var i = 0; i < 3 * HeapCells.segmentSize + 10; i++) {
        heap.allocateVariable();
      }
      final allocated = heap.HP;
      final c = heap.collect(rootAddrs: const [], rootTerms: const []);
      expect(c.liveCells, 0);
      expect(c.reclaimedCells, allocated);
      expect(heap.cells.heldSegments, 1);
      expect(heap.cells.isHeld(0), isFalse);
      expect(() => heap.cells[0], throwsStateError);
      expect(() => heap.cells[allocated - 1], throwsStateError);
      // Allocation goes on at fresh addresses: none is handed out twice.
      final (w, r) = heap.allocateVariable();
      expect(w, allocated);
      expect(r, allocated + 1);
      expect(heap.isWriter(w), isTrue);
      expect(heap.readerForWriter(w), r);
    });

    test(
        'what a root reaches is kept --- a stream, its elements, a bound '
        "writer's reader --- and everything else is reclaimed", () {
      final heap = HeapFCP();
      for (var i = 0; i < 1000; i++) {
        heap.allocateVariable();
      }
      final (w0, r0) = heap.allocateVariable();
      var w = w0;
      for (final v in [1, 2, 3]) {
        final (we, re) = heap.allocateVariable();
        heap.bindWriterConst(we, v);
        final (wt, rt) = heap.allocateVariable();
        heap.bindWriterStruct(w, '.', [VarRef(re), VarRef(rt)]);
        w = wt;
      }
      heap.bindWriterConst(w, 'nil');
      for (var i = 0; i < 1000; i++) {
        final (gw, _) = heap.allocateVariable();
        heap.bindWriterConst(gw, i);
      }
      final allocated = heap.HP;
      final c = heap.collect(rootAddrs: [r0], rootTerms: const []);
      // r0 and w0, and for each element its pair and its tail's pair.
      expect(c.liveCells, 14);
      expect(c.reclaimedCells, allocated - 14);
      expect(_read(heap, r0), [1, 2, 3]);
      expect(heap.pairedReaderAddr(w0), r0);
      // The garbage writer at address 0: its cell and its pair entry are gone.
      expect(heap.cells.isHeld(0), isFalse);
      expect(() => heap.pairedReaderAddr(0), throwsStateError);
    });

    test('a root term reaches the addresses inside it', () {
      final heap = HeapFCP();
      final (we, re) = heap.allocateVariable();
      heap.bindWriterConst(we, 7);
      final (_, unrelated) = heap.allocateVariable();
      final c = heap.collect(
          rootAddrs: const [],
          rootTerms: [
            StructTerm('f', [ConstTerm(1), VarRef(re)])
          ]);
      expect(c.liveCells, 2);
      expect(heap.getReaderValue(re)?.toString(), 'Const(7)');
      expect(heap.cells.isHeld(unrelated), isFalse);
    });

    test(
        "a mutual reference keeps its stream's current tail writer, and that "
        "writer's reader", () {
      final heap = HeapFCP();
      final (tw, tr) = heap.allocateVariable();
      final ref = MutualRefTerm(tw);
      final (rw, rr) = heap.allocateVariable();
      heap.bindWriter(rw, ref);
      for (var i = 0; i < 100; i++) {
        heap.allocateVariable();
      }
      final c = heap.collect(rootAddrs: [rr], rootTerms: const []);
      expect(c.liveCells, 4);
      expect(heap.cells.isHeld(tw), isTrue);
      expect(heap.cells.isHeld(tr), isTrue);
      expect(heap.isFullyBound(tw), isFalse);
    });

    test('an address a root holds that an earlier collection reclaimed fails '
        'the collection loudly', () {
      final heap = HeapFCP();
      final (_, r) = heap.allocateVariable();
      heap.allocateVariable();
      heap.collect(rootAddrs: const [], rootTerms: const []);
      expect(() => heap.collect(rootAddrs: [r], rootTerms: const []),
          throwsStateError);
    });
  });

  group('collection during a run', () {
    Future<(String, int, int)> run(String goal, {required bool collect}) async {
      final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
      engine.maxCycles = 10000000;
      engine.runtime.heapCollection = collect;
      engine.runtime.heapCollectionMinInterval = 1024;
      final r = await engine.runGoal(goal);
      final answer = '${r.status} ${r.bindings}';
      return (
        answer,
        engine.runtime.heapCollections,
        engine.runtime.lastHeapCollection?.liveCells ?? -1,
      );
    }

    for (final goal in ['run(20000, S)', 'mr(20000, S)', 'tick(3000, Z)']) {
      test(
          '$goal collected every thousand cells or so computes what it '
          'computes uncollected, and keeps few cells', () async {
        final off = await run(goal, collect: false);
        final on = await run(goal, collect: true);
        expect(on.$1, off.$1);
        expect(off.$2, 0);
        expect(on.$2, greaterThan(20));
        expect(on.$3, lessThan(2000));
      });
    }

    test("the REPL goal's variables are kept by every collection", () async {
      final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
      engine.maxCycles = 10000000;
      engine.runtime.heapCollection = true;
      engine.runtime.heapCollectionMinInterval = 1024;
      final r = await engine.runGoal('run(5000, S)');
      expect(engine.runtime.heapCollections, greaterThan(5));
      expect('${r.bindings['S']}', 'Const(12502500)');
      expect(engine.runtime.pinnedAddrs, isNotEmpty);
      engine.runtime.collectHeap();
      for (final a in engine.runtime.pinnedAddrs) {
        expect(engine.runtime.heap.cells.isHeld(a), isTrue);
      }
    });

    test('collection is off unless asked for', () {
      final engine = GlpEngine(rootSelfGlpPath: _root);
      expect(engine.runtime.heapCollection,
          Platform.environment['GLP_HEAP_GC'] == '1');
    });
  });
}
