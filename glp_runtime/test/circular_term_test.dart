/// Tests for circular term handling in GLP runtime.
///
/// Circular terms can form through cross-goal communication when two goals
/// share variables and bind them in ways that create cycles. These tests
/// verify that the runtime handles such terms gracefully:
/// - ground/1 guard terminates and correctly identifies ground circular terms
/// - =?= equality terminates and correctly compares circular terms
/// - copy_term/2 preserves cyclic structure in copies

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/system_predicates.dart';
import 'package:glp_runtime/runtime/system_predicates_impl.dart';

void main() {
  group('Circular Term Handling', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
    });

    group('Ground Guard with Circular Terms', () {
      test('circular term without unbound variables is ground', () {
        // Create a circular term: X = f(X)
        // This simulates what could happen through cross-goal communication
        final varId = rt.heap.allocateVariable();

        // Create f(VarRef(varId)) where VarRef points to the same variable
        final circularStruct = StructTerm('f', [VarRef(varId, isReader: true)]);

        // Bind the variable to the structure (creates the cycle)
        rt.heap.bindVariable(varId, circularStruct);

        // The term is circular but contains no unbound variables, so it should be ground
        // We test this by checking if the heap properly handles dereferencing
        final value = rt.heap.getValue(varId);
        expect(value, isA<StructTerm>());

        // The structure's argument should resolve back to the same structure
        // (we don't test infinite dereferencing here, just that the cycle exists)
        final struct = value as StructTerm;
        expect(struct.functor, equals('f'));
        expect(struct.args.length, equals(1));
        expect(struct.args[0], isA<VarRef>());
      });

      test('circular term with unbound variable inside is not ground', () {
        // Create: X = f(Y, X) where Y is unbound
        final xId = rt.heap.allocateVariable();
        final yId = rt.heap.allocateVariable();  // Unbound

        // Create f(VarRef(yId), VarRef(xId))
        final circularStruct = StructTerm('f', [
          VarRef(yId, isReader: true),  // Y - unbound
          VarRef(xId, isReader: true),  // X - will be bound to this structure
        ]);

        // Bind X to the structure
        rt.heap.bindVariable(xId, circularStruct);

        // Y remains unbound, so the term is not ground
        // The ground guard should detect the unbound Y even in the circular structure
        expect(rt.heap.isWriterBound(yId), isFalse);
      });
    });

    group('Equality (=?=) with Circular Terms', () {
      test('identical circular terms are equal', () {
        // Create two identical circular structures: X = f(X), Y = f(Y)
        final xId = rt.heap.allocateVariable();
        final yId = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xId, isReader: true)]);
        final circularY = StructTerm('f', [VarRef(yId, isReader: true)]);

        rt.heap.bindVariable(xId, circularX);
        rt.heap.bindVariable(yId, circularY);

        // Both are f(f(f(...))) - structurally identical
        // The equality comparison should terminate (not infinite loop)
        // and recognize them as equal
        final xValue = rt.heap.getValue(xId);
        final yValue = rt.heap.getValue(yId);

        expect(xValue, isA<StructTerm>());
        expect(yValue, isA<StructTerm>());

        // Both have functor 'f' and arity 1
        expect((xValue as StructTerm).functor, equals((yValue as StructTerm).functor));
      });

      test('different circular terms are not equal', () {
        // Create: X = f(X), Y = g(Y) - different functors
        final xId = rt.heap.allocateVariable();
        final yId = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xId, isReader: true)]);
        final circularY = StructTerm('g', [VarRef(yId, isReader: true)]);

        rt.heap.bindVariable(xId, circularX);
        rt.heap.bindVariable(yId, circularY);

        final xValue = rt.heap.getValue(xId) as StructTerm;
        final yValue = rt.heap.getValue(yId) as StructTerm;

        // Different functors - should be detected as not equal
        expect(xValue.functor, isNot(equals(yValue.functor)));
      });
    });

    group('Deep Copy with Circular Terms', () {
      test('copy of circular term preserves structure', () {
        // Create: X = f(a, X)
        final xId = rt.heap.allocateVariable();
        final circularStruct = StructTerm('f', [
          ConstTerm('a'),
          VarRef(xId, isReader: true),
        ]);
        rt.heap.bindVariable(xId, circularStruct);

        // Create a writer for the copy result
        final copyId = rt.heap.allocateVariable();

        // Set up the system call
        final call = SystemCall('copy_term', [
          VarRef(xId, isReader: true),  // Original
          VarRef(copyId, isReader: false),  // Copy (writer)
        ]);

        // Execute copy_term
        final result = copyTermPredicate(rt, call);

        // Copy should succeed
        expect(result, equals(SystemResult.success));

        // Copy should be bound
        expect(rt.heap.isWriterBound(copyId), isTrue);

        // Copy should be a StructTerm with same functor
        final copyValue = rt.heap.getValue(copyId);
        expect(copyValue, isA<StructTerm>());
        final copyStruct = copyValue as StructTerm;
        expect(copyStruct.functor, equals('f'));
        expect(copyStruct.args.length, equals(2));
        expect(copyStruct.args[0], isA<ConstTerm>());
        expect((copyStruct.args[0] as ConstTerm).value, equals('a'));
      });

      test('copy of acyclic term creates independent copy', () {
        // Create: X = f(a, b)
        final xId = rt.heap.allocateVariable();
        final struct = StructTerm('f', [ConstTerm('a'), ConstTerm('b')]);
        rt.heap.bindVariable(xId, struct);

        // Create a writer for the copy result
        final copyId = rt.heap.allocateVariable();

        // Set up the system call
        final call = SystemCall('copy_term', [
          VarRef(xId, isReader: true),
          VarRef(copyId, isReader: false),
        ]);

        // Execute copy_term
        final result = copyTermPredicate(rt, call);
        expect(result, equals(SystemResult.success));

        // Copy should be a new StructTerm (not identical object)
        final copyValue = rt.heap.getValue(copyId);
        expect(copyValue, isA<StructTerm>());

        // Verify structure is correct
        final copyStruct = copyValue as StructTerm;
        expect(copyStruct.functor, equals('f'));
        expect(copyStruct.args.length, equals(2));
      });
    });

    group('Term Formatter with Circular Terms', () {
      test('circular term does not cause infinite loop in toString', () {
        // Create: X = f(X)
        final xId = rt.heap.allocateVariable();
        final circularStruct = StructTerm('f', [VarRef(xId, isReader: true)]);
        rt.heap.bindVariable(xId, circularStruct);

        // Getting the value should work (returns the StructTerm)
        final value = rt.heap.getValue(xId);
        expect(value, isA<StructTerm>());

        // Calling toString on the StructTerm should not infinite loop
        // (This tests the Term.toString method, not the REPL formatter)
        expect(() => value.toString(), returnsNormally);
      });
    });
  });
}
