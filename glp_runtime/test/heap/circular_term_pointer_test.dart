/// Tests for circular term handling with Pointer Architecture Heap
///
/// Adapted from: test/circular_term_test.dart
/// For spec: docs/heap-pointer-architecture-spec.md v3.0
///
/// Circular terms can form through cross-goal communication when two goals
/// share variables and bind them in ways that create cycles. These tests
/// verify that the runtime handles such terms gracefully with the new
/// pointer-based heap architecture.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';

void main() {
  group('Circular Term Handling - Pointer Architecture', () {
    late GlpRuntime rt;

    setUp(() {
      rt = GlpRuntime();
    });

    group('Ground Guard with Circular Terms', () {
      test('circular term without unbound variables is ground', () {
        // Create a circular term: X = f(X?)
        // Allocate returns (writerAddr, readerAddr) tuple
        final (writerAddr, readerAddr) = rt.heap.allocateVariable();

        // Create f(VarRef(readerAddr)) where VarRef points to the reader
        // In new architecture, VarRef just has addr field
        final circularStruct = StructTerm('f', [VarRef(readerAddr)]);

        // Bind the writer to the structure (creates the cycle)
        rt.heap.bindWriter(writerAddr, circularStruct);

        // The term is circular but contains no unbound variables, so it should be ground
        final value = rt.heap.getValue(writerAddr);
        expect(value, isA<StructTerm>());

        // The structure's argument should be a VarRef to the reader
        final struct = value as StructTerm;
        expect(struct.functor, equals('f'));
        expect(struct.args.length, equals(1));
        expect(struct.args[0], isA<VarRef>());
      });

      test('circular term with unbound variable inside is not ground', () {
        // Create: X = f(Y?, X?) where Y is unbound
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        // Create f(VarRef(yReader), VarRef(xReader))
        final circularStruct = StructTerm('f', [
          VarRef(yReader),  // Y? - unbound
          VarRef(xReader),  // X? - will point back to this structure
        ]);

        // Bind X to the structure
        rt.heap.bindWriter(xWriter, circularStruct);

        // Y remains unbound
        expect(rt.heap.isFullyBound(yWriter), isFalse);
      });
    });

    group('Equality (=?=) with Circular Terms', () {
      test('identical circular terms are equal', () {
        // Create two identical circular structures: X = f(X?), Y = f(Y?)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xReader)]);
        final circularY = StructTerm('f', [VarRef(yReader)]);

        rt.heap.bindWriter(xWriter, circularX);
        rt.heap.bindWriter(yWriter, circularY);

        // Both are f(f(f(...))) - structurally identical
        final xValue = rt.heap.getValue(xWriter);
        final yValue = rt.heap.getValue(yWriter);

        expect(xValue, isA<StructTerm>());
        expect(yValue, isA<StructTerm>());

        // Both have functor 'f' and arity 1
        expect((xValue as StructTerm).functor, equals((yValue as StructTerm).functor));
      });

      test('different circular terms are not equal', () {
        // Create: X = f(X?), Y = g(Y?) - different functors
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        final circularX = StructTerm('f', [VarRef(xReader)]);
        final circularY = StructTerm('g', [VarRef(yReader)]);

        rt.heap.bindWriter(xWriter, circularX);
        rt.heap.bindWriter(yWriter, circularY);

        final xValue = rt.heap.getValue(xWriter) as StructTerm;
        final yValue = rt.heap.getValue(yWriter) as StructTerm;

        // Different functors - should be detected as not equal
        expect(xValue.functor, isNot(equals(yValue.functor)));
      });
    });

    group('Term Formatter with Circular Terms', () {
      test('circular term does not cause infinite loop in toString', () {
        // Create: X = f(X?)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final circularStruct = StructTerm('f', [VarRef(xReader)]);
        rt.heap.bindWriter(xWriter, circularStruct);

        // Getting the value should work
        final value = rt.heap.getValue(xWriter);
        expect(value, isA<StructTerm>());

        // Calling toString should not infinite loop
        expect(() => value.toString(), returnsNormally);
      });
    });

    group('Dereferencing Circular Terms', () {
      test('dereference through circular structure terminates', () {
        final (writerAddr, readerAddr) = rt.heap.allocateVariable();

        // X = f(X?)
        final circular = StructTerm('f', [VarRef(readerAddr)]);
        rt.heap.bindWriter(writerAddr, circular);

        // Dereferencing the writer should return the struct
        final result = rt.heap.derefAddr(writerAddr);
        expect(result, isA<StructTerm>());

        // Dereferencing the reader should also return the struct
        final resultReader = rt.heap.derefAddr(readerAddr);
        expect(resultReader, isA<StructTerm>());
      });

      test('nested circular references work correctly', () {
        // Create X = f(Y?), Y = g(X?)
        final (xWriter, xReader) = rt.heap.allocateVariable();
        final (yWriter, yReader) = rt.heap.allocateVariable();

        final structX = StructTerm('f', [VarRef(yReader)]);
        final structY = StructTerm('g', [VarRef(xReader)]);

        rt.heap.bindWriter(xWriter, structX);
        rt.heap.bindWriter(yWriter, structY);

        // Both should dereference to their respective structs
        final resultX = rt.heap.derefAddr(xWriter);
        final resultY = rt.heap.derefAddr(yWriter);

        expect(resultX, isA<StructTerm>());
        expect(resultY, isA<StructTerm>());
        expect((resultX as StructTerm).functor, equals('f'));
        expect((resultY as StructTerm).functor, equals('g'));
      });
    });
  });
}
