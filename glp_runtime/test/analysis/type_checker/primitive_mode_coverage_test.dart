// test/analysis/type_checker/primitive_mode_coverage_test.dart
//
// Tests for primitive mode types (_, _?) and coverage requirements

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive Mode Coverage', () {
    // =========================================================================
    // POSITIVE CONTROLS - Should PASS
    // =========================================================================

    group('Positive Controls', () {
      test('Bimodal position with both modes covered passes', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          procedure echo(Bimodal?, Bimodal).
          echo(X, Y?) :- Y = X?.
          echo(X?, Y) :- Y? = X.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both writer and reader modes covered for Bimodal');
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
          MyList ::= [] ; [_ | MyList].
          procedure copy(MyList?, MyList).
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
      test('Bimodal position with only writer mode fails', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          procedure echo(Bimodal?, Bimodal).
          echo(X, Y?) :- Y = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing reader mode for Bimodal');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage') || e.message.contains('Coverage')),
            isTrue);
      });

      test('Bimodal position with only reader mode fails', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          procedure sink(Bimodal?).
          sink(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing writer mode for Bimodal');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage') || e.message.contains('Coverage')),
            isTrue);
      });
    });

    // =========================================================================
    // NEGATIVE CONTROLS - Nested coverage
    // =========================================================================

    group('Nested Mode Coverage', () {
      test('BimodalList copy with only one element mode fails', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          BimodalList ::= [] ; [Bimodal | BimodalList].
          procedure copy(BimodalList?, BimodalList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'BimodalList requires both element modes covered');
      });

      test('Nested Bimodal in struct not checked', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          Pair ::= pair(Bimodal, Bimodal).
          procedure swap(Pair?, Pair).
          swap(pair(X, Y), pair(Y?, X?)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Nested Bimodal positions require mode coverage');
      });
    });
  });
}
