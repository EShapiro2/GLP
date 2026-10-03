/// Dereferencing ([HeapFCP.derefAddr]) follows a chain to its end, a value or
/// an unbound writer, and checks the chain for a cycle and for a writer bound
/// to a writer (IGLP app:in-heap, "Dereferencing").  It keeps the cells it
/// passes, for the cycle check, only once a chain is longer than a short one;
/// until 2026-10-02 every dereference made that set, most of them following
/// one or two pointers.  These tests hold it to the chain's end on short and
/// long chains, and to the two checks on chains of either length.
library;

import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// A chain of [n] variables: the writer of each bound to the reader of the
/// next, the last writer left unbound.  The first pair's reader, and the last
/// pair's writer.
(int, int) _chain(HeapFCP heap, int n) {
  final pairs = [for (var i = 0; i < n; i++) heap.allocateVariable()];
  for (var i = 0; i + 1 < n; i++) {
    heap.bindWriterToReader(pairs[i].$1, pairs[i + 1].$2);
  }
  return (pairs.first.$2, pairs.last.$1);
}

void main() {
  for (final n in [1, 2, 8, 16, 17, 100, 5000]) {
    test('a chain of $n variables dereferences to its unbound end, and then '
        'to the value its end is bound to', () {
      final heap = HeapFCP();
      final (first, last) = _chain(heap, n);
      final end = heap.derefAddr(first);
      expect(end, isA<VarRef>());
      expect((end as VarRef).addr, last);
      heap.bindWriterConst(last, n);
      final v = heap.derefAddr(first);
      expect(v, isA<ConstTerm>());
      expect((v as ConstTerm).value, n);
    });
  }

  for (final n in [2, 8, 16, 17, 40]) {
    test('a cycle of $n variables is found, whatever its length', () {
      final heap = HeapFCP();
      final pairs = [for (var i = 0; i < n; i++) heap.allocateVariable()];
      // Each writer points to the next pair's reader, the last to the first's:
      // a chain that never ends.
      for (var i = 0; i < n; i++) {
        heap.cells[pairs[i].$1].content = Pointer(pairs[(i + 1) % n].$2);
      }
      expect(
          () => heap.derefAddr(pairs.first.$2),
          throwsA(isA<StateError>().having(
              (e) => e.message, 'message', contains('Cycle detected'))));
    });
  }

  for (final n in [1, 20]) {
    test('a writer bound to a writer is refused at hop ${n + 1}, naming both',
        () {
      final heap = HeapFCP();
      final (first, last) = _chain(heap, n);
      final (w, _) = heap.allocateVariable();
      // The chain's last writer made to point at a writer, which no binding
      // does: the SRSW invariant the dereference checks.
      heap.cells[last].content = Pointer(w);
      expect(
          () => heap.derefAddr(first),
          throwsA(isA<StateError>().having((e) => e.message, 'message',
              'SRSW violation: writer at $last points to writer at $w')));
    });
  }
}
