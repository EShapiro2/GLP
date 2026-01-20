/// Tests for VarRef structure with Pointer Architecture
///
/// For spec: docs/heap-pointer-architecture-spec.md v3.0
///
/// In the new architecture, VarRef has only an addr field.
/// The cell's tag determines whether it's a reader or writer.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';

void main() {
  group('VarRef Structure - Pointer Architecture', () {
    test('VarRef has only addr field', () {
      final ref = VarRef(42);
      expect(ref.addr, equals(42));
    });

    test('VarRef equality based on addr only', () {
      final ref1 = VarRef(100);
      final ref2 = VarRef(100);
      final ref3 = VarRef(101);

      expect(ref1, equals(ref2));
      expect(ref1, isNot(equals(ref3)));
    });

    test('VarRef hashCode consistent with equality', () {
      final ref1 = VarRef(100);
      final ref2 = VarRef(100);

      expect(ref1.hashCode, equals(ref2.hashCode));
    });

    test('Determine reader/writer by checking heap cell', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final writerRef = VarRef(writerAddr);
      final readerRef = VarRef(readerAddr);

      // Determine type by checking cell tag
      expect(heap.isWriter(writerRef.addr), isTrue);
      expect(heap.isReader(writerRef.addr), isFalse);

      expect(heap.isReader(readerRef.addr), isTrue);
      expect(heap.isWriter(readerRef.addr), isFalse);
    });

    test('VarRef can be used as struct argument', () {
      final heap = HeapFCP();
      final (_, readerAddr) = heap.allocateVariable();

      final struct = StructTerm('f', [
        ConstTerm('a'),
        VarRef(readerAddr),
        ConstTerm('b'),
      ]);

      expect(struct.args[1], isA<VarRef>());
      expect((struct.args[1] as VarRef).addr, equals(readerAddr));
    });

    test('VarRef in nested structures', () {
      final heap = HeapFCP();
      final (_, r1) = heap.allocateVariable();
      final (_, r2) = heap.allocateVariable();

      final inner = StructTerm('inner', [VarRef(r1)]);
      final outer = StructTerm('outer', [inner, VarRef(r2)]);

      expect(outer.args[0], isA<StructTerm>());
      expect((outer.args[0] as StructTerm).args[0], isA<VarRef>());
      expect(outer.args[1], isA<VarRef>());
    });
  });

  group('VarRef Dereferencing', () {
    test('Dereference VarRef to writer returns VarRef when unbound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final result = heap.derefAddr(writerAddr);

      expect(result, isA<VarRef>());
      expect((result as VarRef).addr, equals(writerAddr));
    });

    test('Dereference VarRef to reader returns VarRef to writer when unbound', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final result = heap.derefAddr(readerAddr);

      expect(result, isA<VarRef>());
      // Reader dereferences to writer (following pointer)
      expect((result as VarRef).addr, equals(writerAddr));
    });

    test('Dereference VarRef returns value when bound', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(42));

      // From writer
      final result1 = heap.derefAddr(writerAddr);
      expect(result1, isA<ConstTerm>());
      expect((result1 as ConstTerm).value, equals(42));

      // From reader
      final result2 = heap.derefAddr(readerAddr);
      expect(result2, isA<ConstTerm>());
      expect((result2 as ConstTerm).value, equals(42));
    });

    test('dereference method on heap works with VarRef term', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('hello'));

      // Dereference a VarRef term
      final ref = VarRef(readerAddr);
      final result = heap.dereference(ref);

      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('hello'));
    });

    test('dereference non-VarRef term returns itself', () {
      final heap = HeapFCP();

      final term = ConstTerm(123);
      final result = heap.dereference(term);

      expect(result, same(term));
    });

    test('dereference StructTerm returns itself (no deep deref)', () {
      final heap = HeapFCP();
      final (_, readerAddr) = heap.allocateVariable();

      // Struct containing VarRef
      final struct = StructTerm('f', [VarRef(readerAddr)]);
      final result = heap.dereference(struct);

      // Should return same struct (shallow deref)
      expect(result, same(struct));
    });
  });

  group('VarRef in Collections', () {
    test('VarRef can be used in Set', () {
      final ref1 = VarRef(10);
      final ref2 = VarRef(10);
      final ref3 = VarRef(20);

      final set = <VarRef>{ref1, ref2, ref3};

      // ref1 and ref2 are equal, so set should have 2 elements
      expect(set.length, equals(2));
      expect(set.contains(VarRef(10)), isTrue);
      expect(set.contains(VarRef(20)), isTrue);
    });

    test('VarRef can be used as Map key', () {
      final map = <VarRef, String>{};

      map[VarRef(10)] = 'first';
      map[VarRef(20)] = 'second';
      map[VarRef(10)] = 'updated'; // Should update, not add

      expect(map.length, equals(2));
      expect(map[VarRef(10)], equals('updated'));
      expect(map[VarRef(20)], equals('second'));
    });
  });
}
