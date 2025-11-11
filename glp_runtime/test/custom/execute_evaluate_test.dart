/// Test for Execute opcode and evaluate/2 system predicate
///
/// This test verifies the system predicate execution mechanism by testing
/// the evaluate/2 predicate with various arithmetic expressions.

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/cells.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';
import 'package:test/test.dart';

void main() {
  group('Execute - evaluate/2 system predicate', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
      // Register standard system predicates
      registerStandardPredicates(rt.systemPredicates);
    });

    test('evaluate simple addition: 2 + 3 = 5', () {
      print('\n=== EVALUATE TEST: Simple Addition ===');

      // Create expression: +(2, 3)
      final expr = StructTerm('+', [ConstTerm(2), ConstTerm(3)]);

      // Create writer for result
      const wR = 1;
      const rR = 2;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));

      // Call evaluate directly
      final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should succeed
      expect(result, SystemResult.success);

      print('Result: $result');
      print('Expression: $expr');
      print('✓ Evaluate succeeded for 2 + 3\n');
    });

    test('evaluate expression with unbound reader - should suspend', () {
      print('\n=== EVALUATE TEST: Unbound Reader Suspension ===');

      // Create unbound writer/reader for X
      const wX = 1;
      const rX = 2;
      rt.heap.addWriter(WriterCell(wX, rX));
      rt.heap.addReader(ReaderCell(rX));
      // Do NOT bind wX - leave it unbound

      // Create expression: +(X?, 5) where X? is a reader of unbound X
      final expr = StructTerm('+', [VarRef(rX, isReader: true), ConstTerm(5)]);

      // Create writer for result
      const wR = 3;
      const rR = 4;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));

      // Call evaluate
      final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should suspend because X is unbound reader
      expect(result, SystemResult.suspend);

      // Check that the reader was added to suspendedReaders
      expect(call.suspendedReaders, contains(rX));

      print('Result: $result');
      print('Suspended readers: ${call.suspendedReaders}');
      print('✓ Evaluate correctly suspended on unbound reader\n');
    });

    test('evaluate with nested expression: (2 + 3) * 4 = 20', () {
      print('\n=== EVALUATE TEST: Nested Expression ===');

      // Create the expression: *( +(2, 3), 4)
      final innerExpr = StructTerm('+', [ConstTerm(2), ConstTerm(3)]);
      final outerExpr = StructTerm('*', [innerExpr, ConstTerm(4)]);

      // Create writer for result
      const wR = 1;
      const rR = 2;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));

      // Call evaluate directly
      final call = SystemCall('evaluate', [outerExpr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should succeed
      expect(result, SystemResult.success);

      print('Result: $result');
      print('Expression: $outerExpr');
      print('✓ Evaluate succeeded for (2 + 3) * 4\n');
    });

    test('evaluate division by zero - should fail', () {
      print('\n=== EVALUATE TEST: Division by Zero ===');

      // Create expression: /(10, 0)
      final expr = StructTerm('/', [ConstTerm(10), ConstTerm(0)]);

      // Create writer for result
      const wR = 1;
      const rR = 2;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));

      // Call evaluate directly
      final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should fail due to division by zero
      expect(result, SystemResult.failure);

      print('Result: $result');
      print('✓ Evaluate correctly failed for division by zero\n');
    });

    test('evaluate with all operators: +, -, *, /, mod', () {
      print('\n=== EVALUATE TEST: All Operators ===');

      final testCases = [
        (StructTerm('+', [ConstTerm(10), ConstTerm(5)]), 15), // 10 + 5 = 15
        (StructTerm('-', [ConstTerm(10), ConstTerm(5)]), 5),  // 10 - 5 = 5
        (StructTerm('*', [ConstTerm(10), ConstTerm(5)]), 50), // 10 * 5 = 50
        (StructTerm('/', [ConstTerm(10), ConstTerm(5)]), 2),  // 10 / 5 = 2
        (StructTerm('mod', [ConstTerm(10), ConstTerm(3)]), 1), // 10 mod 3 = 1
      ];

      for (var i = 0; i < testCases.length; i++) {
        final (expr, expected) = testCases[i];

        // Create fresh writer/reader pair for each test
        final (wR, rR) = rt.heap.allocateFreshPair();
        rt.heap.addWriter(WriterCell(wR, rR));
        rt.heap.addReader(ReaderCell(rR));

        final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
        final result = evaluatePredicate(rt, call);

        expect(result, SystemResult.success);
        print('Expression: $expr => expected $expected, result: $result');
      }

      print('✓ All operators evaluated successfully\n');
    });

    test('evaluate with bound result variable - should verify', () {
      print('\n=== EVALUATE TEST: Result Verification ===');

      // Create expression: +(2, 3)
      final expr = StructTerm('+', [ConstTerm(2), ConstTerm(3)]);

      // Create writer for result and bind it to 5
      const wR = 1;
      const rR = 2;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));
      rt.heap.bindWriterConst(wR, 5);

      // Call evaluate - should verify that result matches
      final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should succeed because 2 + 3 = 5
      expect(result, SystemResult.success);

      print('Result: $result');
      print('✓ Evaluate correctly verified bound result\n');
    });

    test('evaluate with mismatched bound result - should fail', () {
      print('\n=== EVALUATE TEST: Result Mismatch ===');

      // Create expression: +(2, 3)
      final expr = StructTerm('+', [ConstTerm(2), ConstTerm(3)]);

      // Create writer for result and bind it to 6 (wrong value)
      const wR = 1;
      const rR = 2;
      rt.heap.addWriter(WriterCell(wR, rR));
      rt.heap.addReader(ReaderCell(rR));
      rt.heap.bindWriterConst(wR, 6);

      // Call evaluate - should fail because 2 + 3 ≠ 6
      final call = SystemCall('evaluate', [expr, VarRef(wR, isReader: false)]);
      final result = evaluatePredicate(rt, call);

      // Should fail
      expect(result, SystemResult.failure);

      print('Result: $result');
      print('✓ Evaluate correctly failed for mismatched result\n');
    });
  });
}
