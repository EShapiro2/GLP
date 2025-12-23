// test/analysis/type_checker/primitive_mode_coverage_test.dart
//
// Tests for primitive mode types (_, _?, Any) and coverage requirements

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive Mode Coverage', () {
    // =========================================================================
    // POSITIVE CONTROLS - Should PASS
    // =========================================================================

    group('Positive Controls', () {
      test('Any position with both modes covered passes', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure echo(Any?, Any).
          echo(X, Y?) :- Y = X?.
          echo(X?, Y) :- Y? = X.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both writer and reader modes covered for Any');
      });

      test('Single _ position needs only writer', () {
        final result = checkTypes('''
          Out ::= _.
          procedure produce(Out).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output-only type needs only writer clause');
      });

      test('Single _? position needs only reader', () {
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input-only type needs only reader clause');
      });

      test('List with _ elements needs only writer mode', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          procedure copy(List?, List).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List elements are _ (output only), two clauses suffice');
      });
    });

    // =========================================================================
    // NEGATIVE CONTROLS - Should FAIL
    // =========================================================================

    group('Negative Controls', () {
      test('Any position with only writer mode fails', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure echo(Any?, Any).
          echo(X, Y?) :- Y = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing reader mode for Any');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage')),
            isTrue);
      });

      test('Any position with only reader mode fails', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure sink(Any?).
          sink(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing writer mode for Any');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage')),
            isTrue);
      });
    });

    // =========================================================================
    // NEGATIVE CONTROLS - Currently PASS erroneously, should FAIL
    // =========================================================================

    group('Erroneous Passes (should fail after fix)', () {
      test('ERRONEOUS: AnyList copy with only one element mode', () {
        // This currently passes but should fail!
        // AnyList elements are Any, requiring both modes
        final result = checkTypes('''
          Any ::= _ ; _?.
          AnyList ::= [] ; [Any | AnyList].
          procedure copy(AnyList?, AnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        // CURRENT (wrong): passes
        // EXPECTED (correct): fails - missing copy([X? | In], [X | Out]) clause
        expect(result.isWellTyped, isFalse,
            reason: 'AnyList requires both element modes covered');
      });

      test('ERRONEOUS: Nested Any in struct not checked', () {
        // This currently passes but should fail!
        final result = checkTypes('''
          Any ::= _ ; _?.
          Pair ::= pair(Any, Any).
          procedure swap(Pair?, Pair).
          swap(pair(X, Y), pair(Y?, X?)).
        ''');
        // CURRENT (wrong): passes
        // EXPECTED (correct): fails - only one mode for each Any position
        expect(result.isWellTyped, isFalse,
            reason: 'Nested Any positions require mode coverage');
      });
    });
  });
}
