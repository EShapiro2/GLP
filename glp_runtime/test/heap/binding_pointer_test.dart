/// Tests for binding operations with Pointer Architecture Heap
///
/// For spec: docs/heap-pointer-architecture-spec.md v3.0
///
/// Tests the various binding scenarios:
/// - bindWriter: bind writer to ground value
/// - bindWriterToReader: bind writer to another variable's reader
/// - WxW violation detection
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/suspension.dart';
import 'package:glp_runtime/runtime/machine_state.dart';

void main() {
  group('bindWriter - Ground Values', () {
    test('bind to ConstTerm integer', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(42));

      expect(heap.cells[writerAddr].tag, equals(CellTag.ValueTag));
      expect(heap.cells[writerAddr].content, isA<ConstTerm>());
      expect((heap.cells[writerAddr].content as ConstTerm).value, equals(42));
    });

    test('bind to ConstTerm string', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('hello'));

      final value = heap.getValue(writerAddr);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals('hello'));
    });

    test('bind to ConstTerm double', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(3.14159));

      final value = heap.getValue(writerAddr);
      expect((value as ConstTerm).value, equals(3.14159));
    });

    test('bind to ConstTerm null', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(null));

      final value = heap.getValue(writerAddr);
      expect((value as ConstTerm).value, isNull);
    });

    test('bind to ConstTerm boolean', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(true));

      final value = heap.getValue(writerAddr);
      expect((value as ConstTerm).value, equals(true));
    });

    test('bind to StructTerm', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final struct = StructTerm('point', [ConstTerm(10), ConstTerm(20)]);
      heap.bindWriter(writerAddr, struct);

      final value = heap.getValue(writerAddr);
      expect(value, isA<StructTerm>());
      expect((value as StructTerm).functor, equals('point'));
      expect(value.args.length, equals(2));
    });

    test('bind to nested StructTerm', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final inner = StructTerm('inner', [ConstTerm('x')]);
      final outer = StructTerm('outer', [inner, ConstTerm('y')]);
      heap.bindWriter(writerAddr, outer);

      final value = heap.getValue(writerAddr) as StructTerm;
      expect(value.functor, equals('outer'));
      expect(value.args[0], isA<StructTerm>());
      expect((value.args[0] as StructTerm).functor, equals('inner'));
    });

    test('bind to StructTerm containing VarRef', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (_, r2) = heap.allocateVariable();

      // Bind w1 to f(X?) where X? is r2
      final struct = StructTerm('f', [VarRef(r2)]);
      heap.bindWriter(w1, struct);

      final value = heap.getValue(w1) as StructTerm;
      expect(value.args[0], isA<VarRef>());
      expect((value.args[0] as VarRef).addr, equals(r2));
    });
  });

  group('bindWriterToReader - Variable Chains', () {
    test('basic binding creates pointer', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (_, r2) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);

      // w1 should contain Pointer to r2
      expect(heap.cells[w1].tag, equals(CellTag.WrtTag)); // Still WrtTag
      expect(heap.cells[w1].content, isA<Pointer>());
      expect((heap.cells[w1].content as Pointer).targetAddr, equals(r2));
    });

    test('chain of bindings', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();
      final (w3, _) = heap.allocateVariable();

      // w1 -> r2 -> w2 -> r3 -> w3
      // But wait, we can only bind writer to reader, so:
      // w1 -> r2, w2 -> r3 is not valid (can't bind w2 after it's pointed to)
      // Actually the chain works differently...

      // Let's do: w1 -> r2, then bind w2 to ground
      heap.bindWriterToReader(w1, r2);
      heap.bindWriter(w2, ConstTerm('end'));

      // Dereference from r1 should find 'end'
      final result = heap.derefAddr(r1);
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('end'));
    });

    test('long chain dereferences correctly', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();
      final (w3, r3) = heap.allocateVariable();
      final (w4, _) = heap.allocateVariable();

      // Build chain: w1 -> r2, w2 -> r3, w3 -> r4 (conceptually)
      // Actually: w1 -> r2, w2 -> r3, w3 -> value
      heap.bindWriterToReader(w1, r2);
      heap.bindWriterToReader(w2, r3);
      heap.bindWriter(w3, ConstTerm('final'));

      // Dereference from any point in chain should find 'final'
      expect((heap.derefAddr(r1) as ConstTerm).value, equals('final'));
      expect((heap.derefAddr(r2) as ConstTerm).value, equals('final'));
      expect((heap.derefAddr(r3) as ConstTerm).value, equals('final'));
    });

    test('unbound chain returns final writer VarRef', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();
      final (w3, _) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);
      heap.bindWriterToReader(w2, r2); // w2 also points to r2? No, that's wrong
      // Let me redo: w1 -> r2, w2 remains unbound

      // Actually let's keep it simple:
      // w1 -> r2, w2 unbound
      // Deref r1 -> w1 -> r2 -> w2 (unbound) -> VarRef(w2)

      final heap2 = HeapFCP();
      final (wa, ra) = heap2.allocateVariable();
      final (wb, rb) = heap2.allocateVariable();

      heap2.bindWriterToReader(wa, rb);
      // wb is unbound

      final result = heap2.derefAddr(ra);
      expect(result, isA<VarRef>());
      expect((result as VarRef).addr, equals(wb));
    });
  });

  group('WxW Violation Detection', () {
    test('bindWriterToWriter throws StateError', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, _) = heap.allocateVariable();

      expect(
        () => heap.bindWriterToWriter(w1, w2),
        throwsStateError,
      );
    });

    test('indirect WxW through deref detected', () {
      // Per spec v3.3 Section 4.5: WxW detection during deref is mandatory
      // This provides defense-in-depth even if a bug allows WxW binding
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, _) = heap.allocateVariable();

      // Manually corrupt to create w1 -> w2 (simulates a bug that bypassed binding check)
      heap.cells[w1].content = Pointer(w2);

      // Dereference should detect and report the WxW violation
      expect(
        () => heap.derefAddr(w1),
        throwsStateError,
      );
    });
  });

  group('Binding with Suspensions', () {
    test('binding ground value activates all suspensions', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      // Add multiple suspensions
      heap.suspendOnWriter(writerAddr, SuspensionRecord(1, 100));
      heap.suspendOnWriter(writerAddr, SuspensionRecord(2, 200));
      heap.suspendOnWriter(writerAddr, SuspensionRecord(3, 300));

      final activations = heap.bindWriter(writerAddr, ConstTerm('value'));

      expect(activations.length, equals(3));
      final ids = activations.map((a) => a.id).toSet();
      expect(ids, equals({1, 2, 3}));
    });

    test('binding to variable forwards suspensions without activation', () {
      // Per spec v3.3 Section 2.3: Writer with suspensions has WriterContent
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Suspend on w1
      heap.suspendOnWriter(w1, SuspensionRecord(10, 1000));

      // Bind w1 to r2 (forward, no activation)
      final acts1 = heap.bindWriterToReader(w1, r2);
      expect(acts1, isEmpty);

      // Suspension should be on w2 now (as WriterContent per spec)
      expect(heap.cells[w2].content, isA<WriterContent>());
      expect((heap.cells[w2].content as WriterContent).suspensions, isA<SuspensionListNode>());

      // Binding w2 activates
      final acts2 = heap.bindWriter(w2, ConstTerm('done'));
      expect(acts2.length, equals(1));
      expect(acts2.first.id, equals(10));
    });

    test('disarmed suspensions not activated', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final r1 = SuspensionRecord(1, 100);
      final r2 = SuspensionRecord(2, 200);

      heap.suspendOnWriter(writerAddr, r1);
      heap.suspendOnWriter(writerAddr, r2);

      // Disarm r1
      r1.disarm();
      expect(r1.armed, isFalse);

      final activations = heap.bindWriter(writerAddr, ConstTerm('x'));

      // Only r2 should activate
      expect(activations.length, equals(1));
      expect(activations.first.id, equals(2));
    });
  });

  group('Binding State Transitions', () {
    test('unbound writer has Pointer to reader (FCP bidirectional)', () {
      // Per spec v3.3 Section 2.3: WrtTag unbound without suspensions has Pointer(readerAddr)
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      expect(heap.cells[writerAddr].tag, equals(CellTag.WrtTag));
      expect(heap.cells[writerAddr].content, isA<Pointer>());
      expect((heap.cells[writerAddr].content as Pointer).targetAddr, equals(readerAddr));
    });

    test('writer with suspension has WriterContent with reader addr and suspensions', () {
      // Per spec v3.3 Section 2.3: WrtTag with suspensions has WriterContent(readerAddr, SuspensionListNode)
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      heap.suspendOnWriter(writerAddr, SuspensionRecord(1, 100));

      expect(heap.cells[writerAddr].tag, equals(CellTag.WrtTag));
      expect(heap.cells[writerAddr].content, isA<WriterContent>());
      final wc = heap.cells[writerAddr].content as WriterContent;
      expect(wc.readerAddr, equals(readerAddr));
      expect(wc.suspensions, isA<SuspensionListNode>());
    });

    test('writer bound to variable has Pointer content', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (_, r2) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);

      expect(heap.cells[w1].tag, equals(CellTag.WrtTag));
      expect(heap.cells[w1].content, isA<Pointer>());
    });

    test('writer bound to ground has ValueTag and Term content', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(42));

      expect(heap.cells[writerAddr].tag, equals(CellTag.ValueTag));
      expect(heap.cells[writerAddr].content, isA<ConstTerm>());
    });
  });

  group('isFullyBound and getValue', () {
    test('isFullyBound false for unbound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      expect(heap.isFullyBound(writerAddr), isFalse);
    });

    test('isFullyBound true for bound to ground', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('x'));

      expect(heap.isFullyBound(writerAddr), isTrue);
    });

    test('isFullyBound follows chain', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);
      // w2 unbound, so chain is unbound
      expect(heap.isFullyBound(w1), isFalse);

      heap.bindWriter(w2, ConstTerm('end'));
      // Now chain resolves to ground
      expect(heap.isFullyBound(w1), isTrue);
    });

    test('getValue returns null for unbound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      expect(heap.getValue(writerAddr), isNull);
    });

    test('getValue returns term for bound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(42));

      final value = heap.getValue(writerAddr);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals(42));
    });

    test('getValue follows chain', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);
      heap.bindWriter(w2, ConstTerm('chain_end'));

      final value = heap.getValue(w1);
      expect((value as ConstTerm).value, equals('chain_end'));
    });
  });
}
