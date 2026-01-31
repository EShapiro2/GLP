/// Tests for suspension and reactivation with Pointer Architecture Heap
///
/// Adapted from: test/conformance/restart_clause1_test.dart
/// For spec: docs/heap-pointer-architecture-spec.md v3.0
///
/// Key changes from original:
/// - Suspensions now live on WRITER cells (not reader cells)
/// - suspendOnWriter/suspendOnReader API
/// - bindWriter replaces bindVariable
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/suspension.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapFCP, Pointer, SuspensionListNode, WriterContent;
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  group('Suspension and Reactivation - Pointer Architecture', () {
    test('On wake, activation pc equals kappa (restart at clause 1)', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      const GoalId g = 77;
      const Pc kappa = 1;

      // Allocate variable - returns (writerAddr, readerAddr)
      final (writerAddr, readerAddr) = heap.allocateVariable();

      // Suspend goal g on the variable with restart point kappa
      // In pointer architecture, we suspend on reader but it stores on writer
      final record = SuspensionRecord(g, kappa);
      heap.suspendOnReader(readerAddr, record);

      // Verify suspension is on the WRITER cell (not reader)
      expect(heap.cells[writerAddr].content, isA<WriterContent>());
      expect(heap.cells[readerAddr].content, isA<Pointer>()); // Reader still points to writer

      // Binding the writer should activate the goal
      final activations = heap.bindWriter(writerAddr, ConstTerm('ground'));

      expect(activations, hasLength(1));
      expect(activations.first.id, g);
      expect(activations.first.pc, kappa);
    });

    test('Multiple suspensions on same variable all activate', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (writerAddr, readerAddr) = heap.allocateVariable();

      // Suspend multiple goals
      final r1 = SuspensionRecord(10, 100);
      final r2 = SuspensionRecord(20, 200);
      final r3 = SuspensionRecord(30, 300);

      heap.suspendOnReader(readerAddr, r1);
      heap.suspendOnReader(readerAddr, r2);
      heap.suspendOnReader(readerAddr, r3);

      // Bind writer
      final activations = heap.bindWriter(writerAddr, ConstTerm('value'));

      // All three should activate
      expect(activations, hasLength(3));
      final goalIds = activations.map((a) => a.id).toSet();
      expect(goalIds, containsAll([10, 20, 30]));
    });

    test('Disarmed suspensions do not activate', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (writerAddr, readerAddr) = heap.allocateVariable();

      final r1 = SuspensionRecord(10, 100);
      final r2 = SuspensionRecord(20, 200);

      heap.suspendOnReader(readerAddr, r1);
      heap.suspendOnReader(readerAddr, r2);

      // Disarm r1 (as if goal was cancelled or restarted elsewhere)
      r1.disarm();

      final activations = heap.bindWriter(writerAddr, ConstTerm('value'));

      // Only r2 should activate
      expect(activations, hasLength(1));
      expect(activations.first.id, equals(20));
    });

    test('Suspension forwarding when binding to another variable', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();

      // Suspend goal on variable 1
      final record = SuspensionRecord(42, 500);
      heap.suspendOnReader(r1, record);

      // Bind w1 to r2 (forwards suspensions to w2)
      final acts1 = heap.bindWriterToReader(w1, r2);

      // No immediate activation (w2 still unbound)
      expect(acts1, isEmpty);

      // Suspension should now be on w2
      expect(heap.cells[w2].content, isA<WriterContent>());

      // Binding w2 should activate the forwarded suspension
      final acts2 = heap.bindWriter(w2, ConstTerm('final'));

      expect(acts2, hasLength(1));
      expect(acts2.first.id, equals(42));
      expect(acts2.first.pc, equals(500));
    });

    test('Chain of variable bindings forwards suspensions correctly', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();
      final (w3, r3) = heap.allocateVariable();

      // Suspend on variable 1
      final record = SuspensionRecord(99, 999);
      heap.suspendOnReader(r1, record);

      // Chain: w1 -> r2, w2 -> r3
      heap.bindWriterToReader(w1, r2);
      heap.bindWriterToReader(w2, r3);

      // Suspension should have been forwarded to w3
      expect(heap.cells[w3].content, isA<WriterContent>());

      // Binding w3 activates the suspension
      final activations = heap.bindWriter(w3, ConstTerm('end'));

      expect(activations, hasLength(1));
      expect(activations.first.id, equals(99));
    });

    test('suspendOnWriter adds directly to writer cell', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (writerAddr, _) = heap.allocateVariable();

      final record = SuspensionRecord(55, 550);
      heap.suspendOnWriter(writerAddr, record);

      // Suspension should be on writer (via WriterContent)
      expect(heap.cells[writerAddr].content, isA<WriterContent>());
      final wc = heap.cells[writerAddr].content as WriterContent;
      expect(wc.suspensions!.record.goalId, equals(55));
    });

    test('suspendOnReader follows pointer to find writer', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      final (writerAddr, readerAddr) = heap.allocateVariable();

      // Initially reader points to writer
      expect((heap.cells[readerAddr].content as Pointer).targetAddr, equals(writerAddr));

      final record = SuspensionRecord(66, 660);
      heap.suspendOnReader(readerAddr, record);

      // Suspension should be on writer (found by following reader's pointer)
      // Per FCP pattern, suspensions are stored in WriterContent to preserve reader pointer
      expect(heap.cells[writerAddr].content, isA<WriterContent>());
      expect((heap.cells[writerAddr].content as WriterContent).suspensions, isNotNull);

      // Reader should still point to writer
      expect(heap.cells[readerAddr].content, isA<Pointer>());
    });
  });

  group('Fairness Budget - Pointer Architecture', () {
    test('26-step tail recursion budget yields and resets', () {
      final rt = GlpRuntime();
      const GoalId g = 123;

      // First 25 tail reductions: do not yield
      for (var i = 0; i < 25; i++) {
        final y = rt.tailReduce(g);
        expect(y, isFalse, reason: 'should not yield on step ${i + 1}');
      }
      // 26th: yield and reset
      final y26 = rt.tailReduce(g);
      expect(y26, isTrue, reason: 'should yield on step 26');
      expect(rt.budgetOf(g), 26, reason: 'budget resets after yielding');

      // One more step after reset: budget decrements, no yield
      final y1 = rt.tailReduce(g);
      expect(y1, isFalse);
      expect(rt.budgetOf(g), 25);
    });
  });

  group('CommitOps Equivalent - Pointer Architecture', () {
    test('applySigmaHat binds multiple variables and collects activations', () {
      final rt = GlpRuntime();
      final heap = rt.heap as HeapFCP;

      // Allocate several variables
      final (w1, r1) = heap.allocateVariable();
      final (w2, r2) = heap.allocateVariable();
      final (w3, r3) = heap.allocateVariable();

      // Suspend goals on each
      heap.suspendOnReader(r1, SuspensionRecord(1, 10));
      heap.suspendOnReader(r2, SuspensionRecord(2, 20));
      heap.suspendOnReader(r3, SuspensionRecord(3, 30));

      // Simulate σ̂ (tentative substitution) - map of writerAddr -> value
      final sigmaHat = <int, Term>{
        w1: ConstTerm('a'),
        w2: ConstTerm('b'),
        w3: ConstTerm('c'),
      };

      // Apply all bindings and collect activations
      final allActivations = <GoalRef>[];
      for (final entry in sigmaHat.entries) {
        final acts = heap.bindWriter(entry.key, entry.value);
        allActivations.addAll(acts);
      }

      // All three suspensions should activate
      expect(allActivations, hasLength(3));
      final goalIds = allActivations.map((a) => a.id).toSet();
      expect(goalIds, equals({1, 2, 3}));
    });
  });
}
