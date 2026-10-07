/// A reader cell always points to its writer (IGLP app:in-heap, Variable
/// pairs: a variable is a writer cell and a reader cell, the reader holding a
/// pointer to its writer), whatever becomes of the writer --- bound to a
/// value, to a structure, to another reader, to another writer, its chain
/// compressed by a dereference.  So [HeapFCP.tryWriterForReader] finds the
/// writer of every reader, and is null only for a cell that is no reader.
///
/// The runner rests on this: its two "imported reader" branches, for a reader
/// with no local writer, which the heap has not represented since
/// VariableEntry went (ae141816), went on 2026-10-07 (runner.dart,
/// execGetValue and the put of a clause variable holding a reader).  The
/// branches they guarded are exercised by every reader passed through a
/// clause variable and by goal_writer_head_writer_test's s4.
library;

import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

void main() {
  test('unbound, its reader names it', () {
    final h = HeapFCP();
    final (w, r) = h.allocateVariable();
    expect(h.tryWriterForReader(r), same(w));
    expect(h.tryWriterForReader(w), isNull, reason: 'a writer is no reader');
  });

  test('bound to a constant and to a structure, still', () {
    final h = HeapFCP();
    final (w1, r1) = h.allocateVariable();
    final (w2, r2) = h.allocateVariable();
    h.bindWriterConst(w1, 'a');
    h.bindWriterStruct(w2, 'f', [ConstTerm(nil)]);
    expect(h.tryWriterForReader(r1), same(w1));
    expect(h.tryWriterForReader(r2), same(w2));
  });

  test('bound to another reader, and the chain dereferenced, still', () {
    final h = HeapFCP();
    final (w1, r1) = h.allocateVariable();
    final (w2, r2) = h.allocateVariable();
    final (w3, r3) = h.allocateVariable();
    h.bindWriterToReader(w1, r2);
    h.bindWriterToReader(w2, r3);
    h.dereference(VarRef(r1));
    h.bindWriterConst(w3, 7);
    expect(h.dereference(VarRef(r1)), isA<ConstTerm>());
    expect(h.tryWriterForReader(r1), same(w1));
    expect(h.tryWriterForReader(r2), same(w2));
    expect(h.tryWriterForReader(r3), same(w3));
  });
}
