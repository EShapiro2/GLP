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
      test('Single _ position needs only writer', () {
        // Use _ directly in procedure declaration (no alias type needed)
        final result = checkTypes('''
          procedure produce(_).
          produce(X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output-only type needs only reader clause (after flip)');
      });

      test('Single _? position needs only reader', () {
        // Use _? directly in procedure declaration (no alias type needed)
        final result = checkTypes('''
          procedure consume(_?).
          consume(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input-only type needs only writer clause (after flip)');
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
      test('Wrong mode at output position fails', () {
        // _ = output, expects reader X? (after flip)
        // Writer X is wrong
        final result = checkTypes('''
          procedure produce(_).
          produce(X).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Writer X at output _ position is wrong mode (needs reader)');
      });

      test('Wrong mode at input position fails', () {
        // _? = input, expects writer X (after flip)
        // Reader X? is wrong
        final result = checkTypes('''
          procedure consume(_?).
          consume(X?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Reader X? at input _? position is wrong mode (needs writer)');
      });
    });

    // =========================================================================
    // Single-mode types (no coverage requirement)
    // =========================================================================

    group('Single-mode Types', () {
      test('List copy with _ elements - only one mode needed', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List elements are _ - single mode, no coverage requirement');
      });

      test('Nested struct with single-mode primitives', () {
        final result = checkTypes('''
          Pair ::= pair(_, _).
          procedure swap(Pair?, Pair).
          swap(pair(X, Y), pair(Y?, X?)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Pair with _ elements - single mode per position');
      });
    });
  });
}
