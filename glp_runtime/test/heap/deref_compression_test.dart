/// Path compression ([HeapFCP.derefAddr]), IGLP app:in-heap, "Dereferencing"
/// (IGLP df254d9): "Path compression then rewrites the pointer of the starting
/// cell when it is a writer, and never of a reader, whose pointer names its
/// paired writer: a writer whose chain ends at a value is pointed at the
/// value; a writer whose chain ends at an unbound writer is pointed at that
/// writer's reader, the last reader of the chain, so the invariant holds after
/// compression as before."  These tests hold a dereference to that: the
/// starting writer rewritten, on chains short and longer than the cycle
/// check's short chain; a reader never; and no other cell, value or binding
/// touched.
library;

import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/suspension.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// [n] variable pairs, the writer of each bound to the reader of the next, the
/// last writer left unbound.
List<(HeapCell, HeapCell)> _chain(HeapFCP heap, int n) {
  final pairs = [for (var i = 0; i < n; i++) heap.allocateVariable()];
  for (var i = 0; i + 1 < n; i++) {
    heap.bindWriterToReader(pairs[i].$1, pairs[i + 1].$2);
  }
  return pairs;
}

/// Each cell of [pairs] as it stands: its tag and the content it holds.
List<(CellTag, Object?)> _snapshot(List<(HeapCell, HeapCell)> pairs) => [
      for (final (w, r) in pairs) ...[(w.tag, w.content), (r.tag, r.content)]
    ];

/// That every cell of [pairs] holds what [before] recorded, the same tag and
/// the same content object, but for the cell [except].
void _unchangedBut(List<(HeapCell, HeapCell)> pairs,
    List<(CellTag, Object?)> before, HeapCell? except) {
  final cells = [for (final (w, r) in pairs) ...[w, r]];
  for (var i = 0; i < cells.length; i++) {
    if (identical(cells[i], except)) continue;
    expect(cells[i].tag, before[i].$1, reason: 'the tag of cell ${cells[i]}');
    expect(identical(cells[i].content, before[i].$2), isTrue,
        reason: 'the content of cell ${cells[i]}');
  }
}

/// The cell a writer's pointer names.
HeapCell _target(HeapCell writer) => (writer.content as Pointer).targetAddr;

void main() {
  for (final n in [3, 5, 17, 40]) {
    test('a writer chain of $n variables to a value, dereferenced once, leaves '
        'the writer pointing at the value, the next dereference one hop', () {
      final heap = HeapFCP();
      final pairs = _chain(heap, n);
      final valueCell = pairs.last.$1;
      heap.bindWriterConst(valueCell, 'v');
      final start = pairs.first.$1;
      final before = _snapshot(pairs);

      final v = heap.derefAddr(start);
      expect(v, isA<ConstTerm>());
      expect((v as ConstTerm).value, 'v');
      // The starting writer, still a writer, points at the value cell: the
      // next dereference follows that one pointer.
      expect(start.tag, CellTag.WrtTag);
      expect(identical(_target(start), valueCell), isTrue);
      expect(valueCell.tag, CellTag.ValueTag);
      _unchangedBut(pairs, before, start);

      // The next dereference gives the same value and rewrites nothing.
      final pointer = start.content;
      final again = heap.derefAddr(start);
      expect((again as ConstTerm).value, 'v');
      expect(identical(start.content, pointer), isTrue);
      expect(heap.getValue(start), isA<ConstTerm>());
    });
  }

  for (final n in [3, 5, 17, 40]) {
    test('a writer chain of $n variables ending at an unbound writer leaves the '
        'starting writer pointing at that writer\'s reader, and the SRSW check '
        'never fires on it', () {
      final heap = HeapFCP();
      final pairs = _chain(heap, n);
      final (lastWriter, lastReader) = pairs.last;
      final start = pairs.first.$1;
      final before = _snapshot(pairs);

      final end = heap.derefAddr(start);
      expect(end, isA<VarRef>());
      expect(identical((end as VarRef).addr, lastWriter), isTrue);
      // The starting writer points at the last reader of the chain, the
      // unbound writer's paired reader, never at a writer.
      expect(start.tag, CellTag.WrtTag);
      expect(identical(_target(start), lastReader), isTrue);
      expect(identical(heap.pairedReaderAddr(lastWriter), lastReader), isTrue);
      _unchangedBut(pairs, before, start);

      // Dereferenced again: the same end, the SRSW check passing, nothing
      // rewritten.
      final pointer = start.content;
      final again = heap.derefAddr(start);
      expect(identical((again as VarRef).addr, lastWriter), isTrue);
      expect(identical(start.content, pointer), isTrue);

      // The chain extended past the old end: the starting writer is pointed
      // at the new last reader, and then, the chain ending at a value, at it.
      final (w2, r2) = heap.allocateVariable();
      heap.bindWriterToReader(lastWriter, r2);
      final end2 = heap.derefAddr(start);
      expect(identical((end2 as VarRef).addr, w2), isTrue);
      expect(identical(_target(start), r2), isTrue);
      heap.bindWriterConst(w2, 7);
      expect((heap.derefAddr(start) as ConstTerm).value, 7);
      expect(identical(_target(start), w2), isTrue);
      // Every reader of the chain still names its paired writer and reaches
      // the value.
      for (final (w, r) in pairs) {
        expect(identical(heap.tryWriterForReader(r), w), isTrue);
        expect((heap.derefAddr(r) as ConstTerm).value, 7);
      }
    });
  }

  test('a writer chain ending at an unbound writer with suspensions leaves the '
      'starting writer pointing at that writer\'s reader, the suspensions where '
      'they were', () {
    final heap = HeapFCP();
    final pairs = _chain(heap, 6);
    final (lastWriter, lastReader) = pairs.last;
    heap.suspendOnWriter(lastWriter, SuspensionRecord(1, 0));
    final suspended = lastWriter.content;
    expect(suspended, isA<WriterContent>());
    final start = pairs.first.$1;
    final before = _snapshot(pairs);

    final end = heap.derefAddr(start);
    expect(identical((end as VarRef).addr, lastWriter), isTrue);
    expect(identical(_target(start), lastReader), isTrue);
    expect(identical(lastWriter.content, suspended), isTrue);
    _unchangedBut(pairs, before, start);

    // Binding the end activates the suspended goal once, as before.
    final activations = heap.bindWriterConst(lastWriter, 1);
    expect(activations.map((g) => g.id), [1]);
    expect((heap.derefAddr(start) as ConstTerm).value, 1);
  });

  test('a writer chain of one hop is not rewritten', () {
    final heap = HeapFCP();
    final pairs = _chain(heap, 2);
    final start = pairs.first.$1;
    final pointer = start.content;
    final end = heap.derefAddr(start);
    expect(identical((end as VarRef).addr, pairs.last.$1), isTrue);
    expect(identical(start.content, pointer), isTrue);

    heap.bindWriterConst(pairs.last.$1, 'x');
    heap.derefAddr(start);
    // Now two hops, through the reader to the value cell: pointed at it.
    expect(identical(_target(start), pairs.last.$1), isTrue);
  });

  for (final n in [1, 5, 40]) {
    test('a reader start of a chain of $n variables is not rewritten, nor any '
        'other cell', () {
      final heap = HeapFCP();
      final pairs = _chain(heap, n);
      final reader = pairs.first.$2;
      var before = _snapshot(pairs);

      final end = heap.derefAddr(reader);
      expect(identical((end as VarRef).addr, pairs.last.$1), isTrue);
      _unchangedBut(pairs, before, null);
      expect(identical(heap.tryWriterForReader(reader), pairs.first.$1),
          isTrue);

      heap.bindWriterConst(pairs.last.$1, 3);
      before = _snapshot(pairs);
      expect((heap.derefAddr(reader) as ConstTerm).value, 3);
      _unchangedBut(pairs, before, null);
      expect(identical(heap.tryWriterForReader(reader), pairs.first.$1),
          isTrue);
    });
  }

  test('an unbound writer and a value cell, dereferenced, are left as they '
      'are', () {
    final heap = HeapFCP();
    final (w, r) = heap.allocateVariable();
    final pointer = w.content;
    expect(identical((heap.derefAddr(w) as VarRef).addr, w), isTrue);
    expect(identical(w.content, pointer), isTrue);
    expect(identical(r.content is Pointer ? _target(r) : null, w), isTrue);

    heap.bindWriterConst(w, 9);
    final value = w.content;
    expect((heap.derefAddr(w) as ConstTerm).value, 9);
    expect(w.tag, CellTag.ValueTag);
    expect(identical(w.content, value), isTrue);
  });

  test('a structure\'s value, reached through a compressed writer, is the '
      'same term', () {
    final heap = HeapFCP();
    final pairs = _chain(heap, 4);
    final (hw, hr) = heap.allocateVariable();
    final struct = StructTerm('f', [VarRef(hr), ConstTerm(1)]);
    heap.bindWriter(pairs.last.$1, struct);
    final start = pairs.first.$1;
    final v = heap.derefAddr(start);
    expect(identical(v, struct), isTrue);
    expect(identical(heap.derefAddr(start), struct), isTrue);
    // The structure's own variable is as it was: unbound, its reader naming
    // its writer.
    expect(identical((heap.derefAddr(hr) as VarRef).addr, hw), isTrue);
  });
}
