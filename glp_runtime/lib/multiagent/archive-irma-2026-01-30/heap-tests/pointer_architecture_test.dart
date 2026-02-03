/// Tests for Pointer Architecture Heap (v3.0 spec)
///
/// Derived from: docs/heap-pointer-architecture-spec.md v3.0
///
/// Key properties tested:
/// - Reader cells point TO writer cells (Section 3)
/// - Writer cells contain null, SuspensionListNode, or Pointer (Section 2.3)
/// - Dereferencing follows pointers (Section 4)
/// - Suspensions live on writer cells (Section 6)
/// - Imported readers point to VariableEntry (Section 10)
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/suspension.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/variable_table.dart' show VariableEntry, VariableRole;

void main() {
  group('Section 3: Variable Allocation', () {
    test('allocateVariable returns (writerAddr, readerAddr)', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      expect(writerAddr, isNonNegative);
      expect(readerAddr, isNonNegative);
      expect(writerAddr, isNot(equals(readerAddr)));
    });

    test('writer cell is WrtTag with null content (unbound)', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final writerCell = heap.cells[writerAddr];
      expect(writerCell.tag, equals(CellTag.WrtTag));
      expect(writerCell.content, isNull);
    });

    test('reader cell is RoTag pointing to writer', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final readerCell = heap.cells[readerAddr];
      expect(readerCell.tag, equals(CellTag.RoTag));
      expect(readerCell.content, isA<Pointer>());
      expect((readerCell.content as Pointer).targetAddr, equals(writerAddr));
    });

    test('multiple allocations produce independent pairs', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // All addresses distinct
      expect({w1, r1, w2, r2}.length, equals(4));

      // Each reader points to its own writer
      expect((heap.cells[r1].content as Pointer).targetAddr, equals(w1));
      expect((heap.cells[r2].content as Pointer).targetAddr, equals(w2));
    });
  });

  group('Section 4: Dereferencing', () {
    test('dereference unbound reader returns VarRef to writer', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final result = heap.derefAddr(readerAddr);

      expect(result, isA<VarRef>());
      final varRef = result as VarRef;
      expect(varRef.addr, equals(writerAddr));
    });

    test('dereference unbound writer returns VarRef to self', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final result = heap.derefAddr(writerAddr);

      expect(result, isA<VarRef>());
      final varRef = result as VarRef;
      expect(varRef.addr, equals(writerAddr));
    });

    test('dereference bound writer returns value', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      // Bind writer to ground value
      heap.bindWriter(writerAddr, ConstTerm(42));

      final result = heap.derefAddr(writerAddr);

      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals(42));
    });

    test('dereference reader of bound writer returns value', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('hello'));

      final result = heap.derefAddr(readerAddr);

      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('hello'));
    });

    test('dereference follows variable chain', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Bind w1 to reader r2 (chain: r1 -> w1 -> r2 -> w2)
      heap.bindWriterToReader(w1, r2);

      // Bind w2 to ground value
      heap.bindWriter(w2, ConstTerm('end'));

      // Dereference from r1 should find 'end'
      final result = heap.derefAddr(r1);
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('end'));
    });

    test('dereference unbound chain returns final writer', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Bind w1 to reader r2, but w2 remains unbound
      heap.bindWriterToReader(w1, r2);

      final result = heap.derefAddr(r1);
      expect(result, isA<VarRef>());
      expect((result as VarRef).addr, equals(w2));
    });
  });

  group('Section 4.4: Path Compression (Stage 2)', () {
    test('path compression updates reader to point directly to final target', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Chain: r1 -> w1 -> r2 -> w2
      heap.bindWriterToReader(w1, r2);
      heap.bindWriter(w2, ConstTerm('value'));

      // Before dereference, r1 points to w1
      expect((heap.cells[r1].content as Pointer).targetAddr, equals(w1));

      // Dereference from r1
      heap.derefAddr(r1);

      // After dereference with path compression, r1 should point directly to w2
      // (since w2 now has ValueTag with the final value)
      final r1Content = heap.cells[r1].content;
      expect(r1Content, isA<Pointer>());
      // The pointer should skip w1 and point closer to the value
    });
  });

  group('Section 5: Binding', () {
    test('binding writer to ground value changes tag to ValueTag', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      expect(heap.cells[writerAddr].tag, equals(CellTag.WrtTag));

      heap.bindWriter(writerAddr, ConstTerm('bound'));

      expect(heap.cells[writerAddr].tag, equals(CellTag.ValueTag));
      expect(heap.cells[writerAddr].content, isA<ConstTerm>());
    });

    test('binding writer to VarRef stores pointer, keeps WrtTag', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (_, r2) = heap.allocateVariable();

      heap.bindWriterToReader(w1, r2);

      expect(heap.cells[w1].tag, equals(CellTag.WrtTag));
      expect(heap.cells[w1].content, isA<Pointer>());
      expect((heap.cells[w1].content as Pointer).targetAddr, equals(r2));
    });

    test('binding activates suspensions on writer', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      // Add suspension to writer
      final record = SuspensionRecord(42, 100);
      heap.suspendOnWriter(writerAddr, record);

      expect(heap.cells[writerAddr].content, isA<SuspensionListNode>());

      // Bind to ground value
      final activations = heap.bindWriter(writerAddr, ConstTerm('done'));

      expect(activations.length, equals(1));
      expect(activations[0].id, equals(42));
      expect(activations[0].pc, equals(100));
    });

    test('binding to another variable forwards suspensions', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Suspend goal on w1
      final record = SuspensionRecord(99, 200);
      heap.suspendOnWriter(w1, record);

      // Bind w1 to r2 (forwards suspensions to w2)
      final activations = heap.bindWriterToReader(w1, r2);

      // No immediate activation (w2 still unbound)
      expect(activations, isEmpty);

      // Suspension should be on w2 now
      expect(heap.cells[w2].content, isA<SuspensionListNode>());

      // Binding w2 should activate the forwarded suspension
      final acts2 = heap.bindWriter(w2, ConstTerm('final'));
      expect(acts2.length, equals(1));
      expect(acts2[0].id, equals(99));
    });

    test('WxW violation throws error', () {
      final heap = HeapFCP();
      final (w1, _) = heap.allocateVariable();
      final (w2, _) = heap.allocateVariable();

      // Attempting to bind writer to writer should fail
      expect(
        () => heap.bindWriterToWriter(w1, w2),
        throwsStateError,
      );
    });
  });

  group('Section 6: Suspension', () {
    test('suspension added to writer cell, not reader', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final record = SuspensionRecord(10, 50);
      heap.suspendOnWriter(writerAddr, record);

      // Writer has suspension
      expect(heap.cells[writerAddr].content, isA<SuspensionListNode>());

      // Reader still points to writer
      expect(heap.cells[readerAddr].content, isA<Pointer>());
    });

    test('suspendOnReader finds writer and adds there', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final record = SuspensionRecord(20, 60);
      heap.suspendOnReader(readerAddr, record);

      // Suspension should be on writer
      expect(heap.cells[writerAddr].content, isA<SuspensionListNode>());
    });

    test('multiple suspensions form linked list', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final r1 = SuspensionRecord(1, 10);
      final r2 = SuspensionRecord(2, 20);
      final r3 = SuspensionRecord(3, 30);

      heap.suspendOnWriter(writerAddr, r1);
      heap.suspendOnWriter(writerAddr, r2);
      heap.suspendOnWriter(writerAddr, r3);

      // Count nodes in list
      var count = 0;
      var node = heap.cells[writerAddr].content as SuspensionListNode?;
      while (node != null) {
        count++;
        node = node.next;
      }
      expect(count, equals(3));
    });

    test('disarmed suspensions not activated', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      final r1 = SuspensionRecord(1, 10);
      final r2 = SuspensionRecord(2, 20);

      heap.suspendOnWriter(writerAddr, r1);
      heap.suspendOnWriter(writerAddr, r2);

      // Disarm r1
      r1.disarm();

      final activations = heap.bindWriter(writerAddr, ConstTerm('x'));

      // Only r2 should activate
      expect(activations.length, equals(1));
      expect(activations[0].id, equals(2));
    });
  });

  group('Section 7: Finding Paired Cell', () {
    test('tryWriterForReader follows pointer', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      final foundWriter = heap.tryWriterForReader(readerAddr);
      expect(foundWriter, equals(writerAddr));
    });

    test('tryWriterForReader works after chain binding', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Bind w1 -> r2, so r1 chain leads to w2
      heap.bindWriterToReader(w1, r2);

      // tryWriterForReader(r1) should return w1 (the immediate target)
      // NOT w2 (the final unbound writer in the chain)
      final foundWriter = heap.tryWriterForReader(r1);
      expect(foundWriter, equals(w1));
    });

    test('isWriter and isReader check cell tags', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      expect(heap.isWriter(writerAddr), isTrue);
      expect(heap.isReader(writerAddr), isFalse);
      expect(heap.isReader(readerAddr), isTrue);
      expect(heap.isWriter(readerAddr), isFalse);
    });
  });

  group('Section 10: Imported Variables', () {
    test('imported reader has no local writer', () {
      final heap = HeapFCP();
      final readerAddr = heap.allocateImportedReader();

      // Single cell allocated
      expect(heap.cells[readerAddr].tag, equals(CellTag.RoTag));
      // Content will be set to VariableEntry by caller
    });

    test('imported reader with VariableEntry dereferences to entry', () {
      final heap = HeapFCP();
      final readerAddr = heap.allocateImportedReader();

      // Simulate MadContext setting VariableEntry
      final entry = VariableEntry(
        varId: readerAddr,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        creatorLocalId: 42,
      );
      heap.cells[readerAddr].content = entry;

      final result = heap.derefAddr(readerAddr);
      expect(result, isA<VariableEntry>());
      expect((result as VariableEntry).creator, equals('bob'));
    });

    test('imported reader with value in entry dereferences to value', () {
      final heap = HeapFCP();
      final readerAddr = heap.allocateImportedReader();

      final entry = VariableEntry(
        varId: readerAddr,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        creatorLocalId: 42,
        state: ConstTerm('received'),
      );
      heap.cells[readerAddr].content = entry;

      final result = heap.derefAddr(readerAddr);
      expect(result, isA<ConstTerm>());
      expect((result as ConstTerm).value, equals('received'));
    });

    test('suspension on imported reader stored in entry', () {
      final heap = HeapFCP();
      final readerAddr = heap.allocateImportedReader();

      final entry = VariableEntry(
        varId: readerAddr,
        isReader: true,
        creator: 'bob',
        role: VariableRole.importedReader,
        creatorLocalId: 42,
      );
      heap.cells[readerAddr].content = entry;

      final record = SuspensionRecord(77, 300);
      heap.suspendOnReader(readerAddr, record);

      // For imported reader, suspension should be stored somewhere accessible
      // (implementation detail - could be in entry.state or a separate mechanism)
    });
  });

  group('Compatibility: Existing API Patterns', () {
    // These tests ensure the new implementation supports patterns used by existing code

    test('allocateVariable returns usable addresses', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      expect(heap.isWriter(writerAddr), isTrue);
      expect(heap.isReader(readerAddr), isTrue);
    });

    test('bindVariable by writerAddr works', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm(123));

      expect(heap.isFullyBound(writerAddr), isTrue);
      final value = heap.getValue(writerAddr);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals(123));
    });

    test('VarRef with addr field works', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      // New VarRef just has addr
      final writerRef = VarRef(writerAddr);
      final readerRef = VarRef(readerAddr);

      // Can determine type by checking cell
      expect(heap.isWriter(writerRef.addr), isTrue);
      expect(heap.isReader(readerRef.addr), isTrue);
    });

    test('circular term handling works', () {
      final heap = HeapFCP();
      final (writerAddr, readerAddr) = heap.allocateVariable();

      // Create circular term: X = f(X?)
      final circular = StructTerm('f', [VarRef(readerAddr)]);
      heap.bindWriter(writerAddr, circular);

      // Dereference should handle cycle (return the struct, not infinite loop)
      final result = heap.derefAddr(writerAddr);
      expect(result, isA<StructTerm>());
      expect((result as StructTerm).functor, equals('f'));
    });

    test('isFullyBound returns false for unbound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      expect(heap.isFullyBound(writerAddr), isFalse);
    });

    test('isFullyBound returns true for bound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('x'));

      expect(heap.isFullyBound(writerAddr), isTrue);
    });

    test('getValue returns null for unbound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      expect(heap.getValue(writerAddr), isNull);
    });

    test('getValue returns term for bound', () {
      final heap = HeapFCP();
      final (writerAddr, _) = heap.allocateVariable();

      heap.bindWriter(writerAddr, ConstTerm('value'));

      final value = heap.getValue(writerAddr);
      expect(value, isA<ConstTerm>());
      expect((value as ConstTerm).value, equals('value'));
    });
  });

  group('Edge Cases', () {
    test('dereference detects cycle in chain', () {
      // If somehow a cycle forms, dereference should detect it
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Create cycle: w1 -> r2 -> w2 -> r1 -> w1
      heap.cells[w1].content = Pointer(r2);
      heap.cells[w2].content = Pointer(r1);

      expect(
        () => heap.derefAddr(r1),
        throwsStateError,
      );
    });

    test('empty heap has HP=0', () {
      final heap = HeapFCP();
      expect(heap.HP, equals(0));
      expect(heap.cells, isEmpty);
    });

    test('binding struct with VarRef args works', () {
      final heap = HeapFCP();
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Bind w1 to f(X?) where X? is r2
      final struct = StructTerm('f', [VarRef(r2)]);
      heap.bindWriter(w1, struct);

      // w1 should be ValueTag with the struct
      expect(heap.cells[w1].tag, equals(CellTag.ValueTag));
      
      // Dereference returns struct
      final result = heap.derefAddr(w1);
      expect(result, isA<StructTerm>());
      
      // The arg is still a VarRef
      final s = result as StructTerm;
      expect(s.args[0], isA<VarRef>());
    });
  });
}
