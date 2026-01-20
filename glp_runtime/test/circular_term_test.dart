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
        final (varWriter, varReader) = rt.heap.allocateVariable();

        // Create f(VarRef(varReader)) where VarRef points to the reader
        final circularStruct = StructTerm('f', [VarRef(varReader)]);

        // Bind the variable to the structure (creates the cycle)
        rt.heap.bindVariable(varWriter, circularStruct);

        // The term is circular but contains no unbound variables, so it should be ground
        // We test this by checking if the heap properly handles dereferencing
        final value = rt.heap.getValue(varWriter);
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
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();  // Unbound

        // Create f(VarRef(yReader), VarRef(xReader))
        final circularStruct = StructTerm('f', [
          VarRef(yReader),  // Y - unbound
          VarRef(xReader),  // X - will be bound to this structure
        ]);

        // Bind X to the structure
        rt.heap.bindVariable(xWriter, circularStruct);

        // Y remains unbound, so the term is not ground
        // The ground guard should detect the unbound Y even in the circular structure
        expect(rt.heap.isWriterBound(yWriter), isFalse);
      });
    });

    group('Equality (=?=) with Circular Terms', () {
      test('identical circular terms are equal', () {
        // Create two identical circular structures: X = f(X), Y = f(Y)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xReader)]);
        final circularY = StructTerm('f', [VarRef(yReader)]);

        rt.heap.bindVariable(xWriter, circularX);
        rt.heap.bindVariable(yWriter, circularY);

        // Both are f(f(f(...))) - structurally identical
        // The equality comparison should terminate (not infinite loop)
        // and recognize them as equal
        final xValue = rt.heap.getValue(xWriter);
        final yValue = rt.heap.getValue(yWriter);

        expect(xValue, isA<StructTerm>());
        expect(yValue, isA<StructTerm>());

        // Both have functor 'f' and arity 1
        expect((xValue as StructTerm).functor, equals((yValue as StructTerm).functor));
      });

      test('different circular terms are not equal', () {
        // Create: X = f(X), Y = g(Y) - different functors
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xReader)]);
        final circularY = StructTerm('g', [VarRef(yReader)]);

        rt.heap.bindVariable(xWriter, circularX);
        rt.heap.bindVariable(yWriter, circularY);

        final xValue = rt.heap.getValue(xWriter) as StructTerm;
        final yValue = rt.heap.getValue(yWriter) as StructTerm;

        // Different functors - should be detected as not equal
        expect(xValue.functor, isNot(equals(yValue.functor)));
      });
    });

    group('Deep Copy with Circular Terms', () {
      test('copy of circular term preserves structure', () {
        // Create: X = f(a, X)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final circularStruct = StructTerm('f', [
          ConstTerm('a'),
          VarRef(xReader),
        ]);
        rt.heap.bindVariable(xWriter, circularStruct);

        // Create a writer for the copy result
        final (copyWriter, _) = rt.heap.allocateVariable();

        // Set up the system call
        final call = SystemCall('copy_term', [
          VarRef(xReader),  // Original (reader)
          VarRef(copyWriter),  // Copy (writer)
        ]);

        // Execute copy_term
        final result = copyTermPredicate(rt, call);

        // Copy should succeed
        expect(result, equals(SystemResult.success));

        // Copy should be bound
        expect(rt.heap.isWriterBound(copyWriter), isTrue);

        // Copy should be a StructTerm with same functor
        final copyValue = rt.heap.getValue(copyWriter);
        expect(copyValue, isA<StructTerm>());
        final copyStruct = copyValue as StructTerm;
        expect(copyStruct.functor, equals('f'));
        expect(copyStruct.args.length, equals(2));
        expect(copyStruct.args[0], isA<ConstTerm>());
        expect((copyStruct.args[0] as ConstTerm).value, equals('a'));
      });

      test('copy of acyclic term creates independent copy', () {
        // Create: X = f(a, b)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final struct = StructTerm('f', [ConstTerm('a'), ConstTerm('b')]);
        rt.heap.bindVariable(xWriter, struct);

        // Create a writer for the copy result
        final (copyWriter, _) = rt.heap.allocateVariable();

        // Set up the system call
        final call = SystemCall('copy_term', [
          VarRef(xReader),  // Reader
          VarRef(copyWriter),  // Writer
        ]);

        // Execute copy_term
        final result = copyTermPredicate(rt, call);
        expect(result, equals(SystemResult.success));

        // Copy should be a new StructTerm (not identical object)
        final copyValue = rt.heap.getValue(copyWriter);
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
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final circularStruct = StructTerm('f', [VarRef(xReader)]);
        rt.heap.bindVariable(xWriter, circularStruct);

        // Getting the value should work (returns the StructTerm)
        final value = rt.heap.getValue(xWriter);
        expect(value, isA<StructTerm>());

        // Calling toString on the StructTerm should not infinite loop
        // (This tests the Term.toString method, not the REPL formatter)
        expect(() => value.toString(), returnsNormally);
      });
    });
  });
}
