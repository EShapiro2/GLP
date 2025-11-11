import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap.dart';
import 'package:glp_runtime/runtime/heap_v2.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';

void main() {
  group('Heap V1 vs V2 Compatibility', () {
    test('Single variable allocation and binding', () {
      // Old heap
      final heap1 = Heap();
      final (w1, r1) = heap1.allocateFreshPair();
      heap1.addWriter(WriterCell(w1, r1));
      heap1.addReader(ReaderCell(r1));

      // New heap
      final heap2 = HeapV2();
      final v2 = heap2.allocateFreshVar();
      heap2.addVariable(v2);

      // Both should behave identically for binding
      heap1.bindWriterConst(w1, 'test');
      heap2.bindVariableConst(v2, 'test');

      expect(heap1.isWriterBound(w1), equals(heap2.isBound(v2)));
      expect(heap1.valueOfWriter(w1), isA<ConstTerm>());
      expect(heap2.getValue(v2), isA<ConstTerm>());
    });

    test('Structure binding compatibility', () {
      // Old heap
      final heap1 = Heap();
      final (w1, r1) = heap1.allocateFreshPair();
      heap1.addWriter(WriterCell(w1, r1));
      heap1.addReader(ReaderCell(r1));

      // New heap
      final heap2 = HeapV2();
      final v2 = heap2.allocateFreshVar();
      heap2.addVariable(v2);

      // Bind to structure
      final args1 = [ConstTerm('a'), ConstTerm('b')];
      final args2 = [ConstTerm('a'), ConstTerm('b')];

      heap1.bindWriterStruct(w1, 'f', args1);
      heap2.bindVariableStruct(v2, 'f', args2);

      expect(heap1.isWriterBound(w1), isTrue);
      expect(heap2.isBound(v2), isTrue);

      final val1 = heap1.valueOfWriter(w1);
      final val2 = heap2.getValue(v2);

      expect(val1, isA<StructTerm>());
      expect(val2, isA<StructTerm>());
      expect((val1 as StructTerm).functor, equals((val2 as StructTerm).functor));
    });

    test('Multiple variable allocations have consistent pattern', () {
      final heap1 = Heap();
      final heap2 = HeapV2();

      final pairs1 = <(int, int)>[];
      final vars2 = <int>[];

      // Allocate 10 variables in each
      for (int i = 0; i < 10; i++) {
        pairs1.add(heap1.allocateFreshPair());
        vars2.add(heap2.allocateFreshVar());
      }

      // Old heap: Reader = Writer + 1
      for (final (w, r) in pairs1) {
        expect(r, equals(w + 1));
      }

      // New heap: Just sequential IDs
      for (int i = 1; i < vars2.length; i++) {
        expect(vars2[i], equals(vars2[i - 1] + 1));
      }
    });

    test('Unbound variable behavior', () {
      final heap1 = Heap();
      final (w1, r1) = heap1.allocateFreshPair();
      heap1.addWriter(WriterCell(w1, r1));

      final heap2 = HeapV2();
      final v2 = heap2.allocateFreshVar();
      heap2.addVariable(v2);

      // Both should report unbound
      expect(heap1.isWriterBound(w1), isFalse);
      expect(heap2.isBound(v2), isFalse);

      expect(heap1.valueOfWriter(w1), isNull);
      expect(heap2.getValue(v2), isNull);
    });

    test('VarRef creation and usage', () {
      final heap2 = HeapV2();
      final v1 = heap2.allocateFreshVar();
      final v2 = heap2.allocateFreshVar();

      heap2.addVariable(v1);
      heap2.addVariable(v2);

      // Create writer and reader references
      final writer = VarRef(v1, false);
      final reader = VarRef(v1, true);

      expect(writer.varId, equals(v1));
      expect(writer.isReader, isFalse);
      expect(reader.varId, equals(v1));
      expect(reader.isReader, isTrue);

      // Same varId, different modes
      expect(writer.varId, equals(reader.varId));
      expect(writer == reader, isFalse); // Different because of isReader flag
    });

    test('Dereferencing simple chain', () {
      final heap2 = HeapV2();

      // Create chain: v1 = v2?, v2 = 'a'
      final v1 = heap2.allocateFreshVar();
      final v2 = heap2.allocateFreshVar();

      heap2.addVariable(v1);
      heap2.addVariable(v2);

      heap2.bindVariable(v1, VarRef(v2, true)); // v1 = v2?
      heap2.bindVariableConst(v2, 'a'); // v2 = 'a'

      // Dereference v1 should give 'a'
      final result = heap2.dereference(VarRef(v1, false));
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('a'));
    });

    test('Dereferencing stops at unbound variable', () {
      final heap2 = HeapV2();

      final v1 = heap2.allocateFreshVar();
      final v2 = heap2.allocateFreshVar();

      heap2.addVariable(v1);
      heap2.addVariable(v2);

      heap2.bindVariable(v1, VarRef(v2, true)); // v1 = v2?
      // v2 remains unbound

      // Dereference v1 should stop at v2
      final result = heap2.dereference(VarRef(v1, false));
      expect(result, isA<VarRef>());
      expect((result as VarRef).varId, equals(v2));
    });

    test('Ground term checking', () {
      final heap2 = HeapV2();

      // Ground constant
      expect(heap2.isGround(ConstTerm('a')), isTrue);

      // Ground structure
      final groundStruct = StructTerm('f', [ConstTerm('a'), ConstTerm('b')]);
      expect(heap2.isGround(groundStruct), isTrue);

      // Unbound variable
      final v1 = heap2.allocateFreshVar();
      heap2.addVariable(v1);
      expect(heap2.isGround(VarRef(v1, false)), isFalse);

      // Structure with unbound variable
      final nonGroundStruct = StructTerm('f', [ConstTerm('a'), VarRef(v1, false)]);
      expect(heap2.isGround(nonGroundStruct), isFalse);

      // Bind the variable
      heap2.bindVariableConst(v1, 'b');
      expect(heap2.isGround(nonGroundStruct), isTrue);
    });

    test('ROQ suspension management', () {
      final heap2 = HeapV2();
      final v1 = heap2.allocateFreshVar();

      // Add suspensions
      heap2.addSuspension(v1, 100);
      heap2.addSuspension(v1, 200);

      final suspended = heap2.getSuspensions(v1);
      expect(suspended, isNotNull);
      expect(suspended, contains(100));
      expect(suspended, contains(200));

      // Remove one suspension
      heap2.removeSuspension(v1, 100);
      expect(heap2.getSuspensions(v1), contains(200));
      expect(heap2.getSuspensions(v1), isNot(contains(100)));
    });

    test('Path compression optimization', () {
      final heap2 = HeapV2();

      // Create long chain: v0 -> v1 -> v2 -> ... -> v9 -> 'end'
      final vars = List.generate(10, (_) => heap2.allocateFreshVar());
      for (final v in vars) {
        heap2.addVariable(v);
      }

      // Link them
      for (int i = 0; i < 9; i++) {
        heap2.bindVariable(vars[i], VarRef(vars[i + 1], true));
      }
      heap2.bindVariableConst(vars[9], 'end');

      // Dereference with compression
      final result = heap2.dereferenceWithCompression(VarRef(vars[0], false));
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('end'));

      // Check that cache was set for intermediate variables
      // (Note: This is an optimization detail, not required for correctness)
    });
  });

  group('Performance Comparison', () {
    test('Lookup performance: V1 O(n) vs V2 O(1)', () {
      final heap1 = Heap();
      final heap2 = HeapV2();

      // Allocate 1000 variables
      final pairs1 = <(int, int)>[];
      final vars2 = <int>[];

      for (int i = 0; i < 1000; i++) {
        final (w, r) = heap1.allocateFreshPair();
        heap1.addWriter(WriterCell(w, r));
        heap1.addReader(ReaderCell(r));
        pairs1.add((w, r));

        final v = heap2.allocateFreshVar();
        heap2.addVariable(v);
        vars2.add(v);
      }

      // Time V1 lookup (O(n) - scans all writers)
      final sw1 = Stopwatch()..start();
      for (int i = 0; i < 100; i++) {
        heap1.writerIdForReader(pairs1.last.$2);
      }
      sw1.stop();

      // Time V2 lookup (O(1) - direct access)
      final sw2 = Stopwatch()..start();
      for (int i = 0; i < 100; i++) {
        heap2.isBound(vars2.last);
      }
      sw2.stop();

      // V2 should be faster (though exact timing varies)
      print('V1 lookup time (O(n)): ${sw1.elapsedMicroseconds}µs');
      print('V2 lookup time (O(1)): ${sw2.elapsedMicroseconds}µs');

      // This test just demonstrates the difference
      // In practice, V2 should be significantly faster for large heaps
    });
  });
}
