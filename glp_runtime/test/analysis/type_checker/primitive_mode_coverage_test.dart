// test/analysis/type_checker/primitive_mode_coverage_test.dart
//
// Tests for primitive mode types (_, _?, Any) and coverage requirements
//
// v3: Corrected based on full algorithm trace:
// - calleeView complements declaration annotation (Out → Out?, In? → In)
// - Type complement affects primitives (_ → _?, _? → _)
// - At _? (input) primitive: writer X matches
// - At _ (output) primitive: reader X? matches

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

      test('Output-only primitive at output declaration', () {
        final result = checkTypes('''
          Out ::= _.
          procedure produce(Out).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Writer at complemented output position (becomes input)');
      });

      test('Input-only primitive at input declaration', () {
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Writer at input position');
      });

      test('Output-only primitive at input declaration needs reader', () {
        final result = checkTypes('''
          Out ::= _.
          procedure read_out(Out?).
          read_out(X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Reader at output position');
      });

      test('List with output-only elements needs only one mode', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output-only elements: writer in input arg, reader in output arg');
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
            reason: 'Missing second mode for MyAny at input position');
      });

      test('Wrong variable mode at output position fails', () {
        final result = checkTypes('''
          Out ::= _.
          procedure produce(Out).
          produce(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Reader at input primitive position is wrong');
      });

      test('Wrong variable mode at input position fails', () {
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Reader at input primitive position is wrong');
      });
    });

    // =========================================================================
    // NESTED MODE COVERAGE - Tests for nested bi-moded positions
    // =========================================================================

    group('Nested Mode Coverage', () {
      test('NEGATIVE: MyAnyList copy with only one element mode', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAnyList ::= [] ; [MyAny | MyAnyList].
          procedure copy(MyAnyList?, MyAnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
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
