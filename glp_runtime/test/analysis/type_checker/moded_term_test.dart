/// Tests for ModedTerm construction and operations
/// Specification: docs/modules/moded-term.md
/// Paper Reference: Definition 4.2

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('ModedTerm', () {
    group('consumedTerm', () {
      // Positive controls
      test('creates consumed moded term with all modes consume', () {
        // Term: foo(1, bar(2))
        final term = Compound('foo', [
          Constant(1),
          Compound('bar', [Constant(2)])
        ]);

        final moded = consumedTerm(term);

        expect(moded.mode, equals(Mode.consume));
        expect(isConsumed(moded), isTrue);
        expect(isProduced(moded), isFalse);
      });

      test('consumed term with variables preserves reader/writer status', () {
        // Term: foo(X, Y?)
        final term = Compound('foo', [
          Variable('X', isReader: false),
          Variable('Y', isReader: true)
        ]);

        final moded = consumedTerm(term);

        expect(moded.mode, equals(Mode.consume));
        // Variables don't get explicit mode annotations
      });
    });

    group('producedTerm', () {
      // Positive controls
      test('creates produced moded term with all modes produce', () {
        // Term: foo(1, bar(2))
        final term = Compound('foo', [
          Constant(1),
          Compound('bar', [Constant(2)])
        ]);

        final moded = producedTerm(term);

        expect(moded.mode, equals(Mode.produce));
        expect(isProduced(moded), isTrue);
        expect(isConsumed(moded), isFalse);
      });
    });

    group('complement', () {
      // Positive controls
      test('complement flips all modes', () {
        // ↓foo(↓1) -> ↑foo(↑1)
        final original = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [ModedConstant(Mode.consume, 1)]
        );

        final complemented = original.flip;

        expect(complemented.mode, equals(Mode.produce));
        expect((complemented as ModedCompound).args[0].mode, equals(Mode.produce));
      });

      test('complement flips variables: X becomes X?, X? becomes X', () {
        final original = ModedCompound(
          Mode.consume,
          'foo',
          2,
          [
            ModedVariable('X', isReader: false),  // writer
            ModedVariable('Y', isReader: true)    // reader
          ]
        );

        final complemented = original.flip as ModedCompound;

        expect((complemented.args[0] as ModedVariable).isReader, isTrue);  // X -> X?
        expect((complemented.args[1] as ModedVariable).isReader, isFalse); // Y? -> Y
      });

      test('complement is involution: (T?)? == T', () {
        final original = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [ModedVariable('X', isReader: false)]
        );

        final doubleComplement = original.flip.flip;

        expect(doubleComplement.mode, equals(original.mode));
        expect(
          (doubleComplement as ModedCompound).args[0],
          equals(original.args[0])
        );
      });
    });

    group('paths extraction', () {
      // Positive controls
      test('extracts paths from simple term', () {
        // ↓foo(↓1)
        final term = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [ModedConstant(Mode.consume, 1)]
        );

        final paths = term.paths;

        expect(paths.length, equals(1));
        expect(paths.first.isInput, isTrue);
        expect(paths.first.steps.length, equals(2)); // root + leaf
      });

      test('extracts multiple paths from compound term', () {
        // ↓foo(↓1, ↓2)
        final term = ModedCompound(
          Mode.consume,
          'foo',
          2,
          [
            ModedConstant(Mode.consume, 1),
            ModedConstant(Mode.consume, 2)
          ]
        );

        final paths = term.paths;

        expect(paths.length, equals(2));
      });

      test('paths reflect mode inversions in I/O term', () {
        // ↓foo(↓1, ↑2) - mode inversion at arg 2
        final term = ModedCompound(
          Mode.consume,
          'foo',
          2,
          [
            ModedConstant(Mode.consume, 1),
            ModedConstant(Mode.produce, 2)
          ]
        );

        final paths = term.paths.toList();

        // Path to arg1 should have all consume
        final path1 = paths.firstWhere(
          (p) => p.steps.last.symbol == '1'
        );
        expect(path1.steps[1].mode, equals(Mode.consume));

        // Path to arg2 should have produce
        final path2 = paths.firstWhere(
          (p) => p.steps.last.symbol == '2'
        );
        expect(path2.steps[1].mode, equals(Mode.produce));
      });
    });

    group('I/O moded term', () {
      // Positive controls
      test('isIO returns true for valid I/O term', () {
        // ↓foo(↓1, ↑2) - one inversion, valid
        final term = ModedCompound(
          Mode.consume,
          'foo',
          2,
          [
            ModedConstant(Mode.consume, 1),
            ModedConstant(Mode.produce, 2)
          ]
        );

        expect(isIO(term), isTrue);
      });

      test('isIO returns true for consumed term (zero inversions)', () {
        final term = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [ModedConstant(Mode.consume, 1)]
        );

        expect(isIO(term), isTrue);
      });

      // Negative controls
      test('isIO returns false for produced term (root is not consume)', () {
        final term = ModedCompound(
          Mode.produce,
          'foo',
          1,
          [ModedConstant(Mode.produce, 1)]
        );

        expect(isIO(term), isFalse);
      });

      test('isIO returns false for term with produce-to-consume inversion', () {
        // ↓foo(↑bar(↓1)) - has ↑→↓ inversion, invalid
        final term = ModedCompound(
          Mode.consume,
          'foo',
          1,
          [
            ModedCompound(
              Mode.produce,
              'bar',
              1,
              [ModedConstant(Mode.consume, 1)]  // ↑→↓ inversion!
            )
          ]
        );

        expect(isIO(term), isFalse);
      });
    });

    group('ioModedTerm construction', () {
      // Negative controls - these should throw
      test('throws InvalidIOModeError for multiple inversions on path', () {
        // Type that would require ↓→↑→↓→↑
        // This test requires a TypeDFA that forces multiple inversions
        // Implementation-specific setup needed
        skip('Requires TypeDFA fixture');
      });

      test('throws InvalidIOModeError for produce-to-consume inversion', () {
        // Type that would require ↑→↓
        skip('Requires TypeDFA fixture');
      });
    });
  });
}
