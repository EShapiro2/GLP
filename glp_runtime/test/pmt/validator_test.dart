import 'package:test/test.dart';
import 'package:glp_runtime/compiler/pmt/validator.dart';
import 'package:glp_runtime/compiler/pmt/errors.dart';

void main() {
  group('PmtValidator', () {
    group('validateSource - valid programs', () {
      test('merge/3 with correct SRSW', () {
        final source = '''
Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys, Zs).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'Valid merge program');
      });

      test('append/3 with correct SRSW', () {
        final source = '''
Append(A) := append(List(A)?, List(A)?, List(A)).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'Valid append program');
      });

      test('program without mode declarations is valid (not checked)', () {
        final source = '''
merge([], Ys, Ys).
merge([X|Xs], Ys, [X|Zs]) :- merge(Xs, Ys, Zs).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'No mode declarations = no checking');
      });

      test('grounded variable allows multiple readers', () {
        // Simple case: use variable multiple times in head with ground guard
        final source = '''
Triple := triple(Num?, Num, Num).

triple(X, X, X) :- ground(X) | true.
''';
        // X: writer (head arg 0, reader mode) + reader (head arg 1, writer mode) + reader (head arg 2, writer mode)
        // X has 1 writer, 2 readers, but is grounded
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'X is grounded by ground/1');
      });
    });

    group('validateSource - invalid programs', () {
      test('eq(X, X) with mode (Num?, Num?) - two writers', () {
        final source = '''
Eq := eq(Num?, Num?).

eq(X, X).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors.length, greaterThanOrEqualTo(1));
        expect(errors.any((e) => e.message.contains('writer')), isTrue);
      });

      test('dup(X, X, X) with mode (Num?, Num, Num) - multiple readers without ground', () {
        final source = '''
Dup := dup(Num?, Num, Num).

dup(X, X, X).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors.length, greaterThanOrEqualTo(1));
        expect(errors.any((e) => e.message.contains('reader')), isTrue);
      });

      test('bad(X, Y) with mode (Num?, Num) - orphan variables', () {
        final source = '''
Bad := bad(Num?, Num).

bad(X, Y).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors.length, 2);
        expect(errors.any((e) => e.message.contains('X') && e.message.contains('no reader')), isTrue);
        expect(errors.any((e) => e.message.contains('Y') && e.message.contains('no writer')), isTrue);
      });
    });

    group('isValid', () {
      test('returns true for valid program', () {
        final source = '''
Merge(A) := merge(List(A)?, List(A)?, List(A)).
merge([], Ys, Ys).
''';
        expect(PmtValidator.isValid(source), isTrue);
      });

      test('returns false for invalid program', () {
        final source = '''
Eq := eq(Num?, Num?).
eq(X, X).
''';
        expect(PmtValidator.isValid(source), isFalse);
      });
    });

    group('assertValid', () {
      test('does not throw for valid program', () {
        final source = '''
Merge(A) := merge(List(A)?, List(A)?, List(A)).
merge([], Ys, Ys).
''';
        expect(() => PmtValidator.assertValid(source), returnsNormally);
      });

      test('throws PmtErrors for invalid program', () {
        final source = '''
Eq := eq(Num?, Num?).
eq(X, X).
''';
        expect(() => PmtValidator.assertValid(source), throwsA(isA<PmtErrors>()));
      });
    });

    group('multiple procedures', () {
      test('validates all procedures with mode declarations', () {
        final source = '''
Eq := eq(Num?, Num?).
Bad := bad(Num?, Num).

eq(X, X).
bad(A, B).
''';
        final errors = PmtValidator.validateSource(source);
        // eq: 2 writers, 0 readers → 2 errors
        // bad: A no reader, B no writer → 2 errors
        expect(errors.length, 4);
      });

      test('skips procedures without mode declarations', () {
        final source = '''
Merge(A) := merge(List(A)?, List(A)?, List(A)).

merge([], Ys, Ys).
foo(X, X).
''';
        // merge is checked and valid
        // foo has no mode declaration, so not checked
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty);
      });
    });

    group('complex programs', () {
      test('half_adder with guards', () {
        final source = '''
HalfAdder := half_adder(Num?, Num?, Num, Num).
Xor := xor(Num?, Num?, Num).
And := and(Num?, Num?, Num).

half_adder(A, B, Sum, Carry) :- ground(A), ground(B) | xor(A, B, Sum), and(A, B, Carry).
xor(0, 0, 0).
xor(0, 1, 1).
xor(1, 0, 1).
xor(1, 1, 0).
and(0, 0, 0).
and(0, 1, 0).
and(1, 0, 0).
and(1, 1, 1).
''';
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'A and B are grounded');
      });

      test('max with comparison guard', () {
        // Simple test case with comparison guard grounding variables
        final source = '''
Max := max(Num?, Num?, Num).

max(A, B, A) :- A > B | true.
max(A, B, B) :- B >= A | true.
''';
        // In first clause:
        // - A: writer (head arg 0, reader mode) + reader (guard >) + reader (head arg 2, writer mode)
        // - B: writer (head arg 1, reader mode) + reader (guard >)
        // A and B are grounded by comparison guard
        final errors = PmtValidator.validateSource(source);
        expect(errors, isEmpty, reason: 'A and B are grounded by comparison guard');
      });
    });
  });
}
