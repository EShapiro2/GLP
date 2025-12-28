// test/analysis/type_checker/primitive_mode_coverage_test.dart
//
// Tests for primitive mode types (_, _?, Any) and coverage requirements
//
// FIXED: Tests now use user-defined MyAny instead of redefining predefined Any

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Primitive Mode Coverage', () {
    // =========================================================================
    // POSITIVE CONTROLS - Should PASS
    // =========================================================================

    group('Positive Controls', () {
      test('MyAny position with both modes covered passes', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure echo(MyAny?, MyAny).
          echo(X, Y?) :- Y = X?.
          echo(X?, Y) :- Y? = X.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both writer and reader modes covered for MyAny');
      });

      test('Single _ position needs only writer', () {
        final result = checkTypes('''
          Out ::= _.
          procedure produce(Out).
          produce(X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output-only type needs only writer clause');
      });

      test('Single _? position needs only reader', () {
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X).
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
      test('MyAny position with only writer mode fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure echo(MyAny?, MyAny).
          echo(X, Y?) :- Y = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing reader mode for MyAny');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage') ||
                                     e.message.contains('do not cover')),
            isTrue);
      });

      test('MyAny position with only reader mode fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure sink(MyAny?).
          sink(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing writer mode for MyAny');
        expect(
            result.errors.any((e) => e.message.contains('mode coverage') ||
                                     e.message.contains('do not cover')),
            isTrue);
      });
    });

    // =========================================================================
    // NESTED MODE COVERAGE - Tests for nested Any positions
    // =========================================================================

    group('Nested Mode Coverage', () {
      test('NEGATIVE: MyAnyList copy with only one element mode', () {
        // MyAnyList elements are MyAny, requiring both modes
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAnyList ::= [] ; [MyAny | MyAnyList].
          procedure copy(MyAnyList?, MyAnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        // Should fail - missing copy([X? | In], [X | Out]) clause
        expect(result.isWellTyped, isFalse,
            reason: 'MyAnyList requires both element modes covered');
      });

      test('POSITIVE: MyAnyList copy with both element modes', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAnyList ::= [] ; [MyAny | MyAnyList].
          procedure copy(MyAnyList?, MyAnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
          copy([X? | In], [X | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both element modes covered for MyAnyList');
      });

      test('NEGATIVE: Nested MyAny in struct not covered', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          Pair ::= pair(MyAny, MyAny).
          procedure swap(Pair?, Pair).
          swap(pair(X, Y), pair(Y?, X?)).
        ''');
        // Should fail - only one mode for each MyAny position
        expect(result.isWellTyped, isFalse,
            reason: 'Nested MyAny positions require mode coverage');
      });

      test('POSITIVE: Nested MyAny in struct with both modes', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          Pair ::= pair(MyAny, MyAny).
          procedure swap(Pair?, Pair).
          swap(pair(X, Y), pair(Y?, X?)).
          swap(pair(X?, Y?), pair(Y, X)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both modes covered for nested MyAny positions');
      });
    });

    // =========================================================================
    // OUTPUT-ONLY ELEMENT TYPES - Simpler coverage requirements
    // =========================================================================

    group('Output-Only Element Types', () {
      test('SimpleList (output-only elements) needs only one mode', () {
        final result = checkTypes('''
          SimpleList ::= [] ; [_ | SimpleList].
          procedure copy(SimpleList?, SimpleList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'SimpleList elements are _ (output only), one mode suffices');
      });
    });
  });
}
