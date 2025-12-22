// test/analysis/type_checker/fixpoint_check_test.dart
//
// Tests for YS fixpoint checking: T_P^α(S) = S

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Fixpoint Checking', () {
    // =========================================================================
    // Complete coverage (inferred = declared)
    // =========================================================================

    group('Complete Coverage', () {
      test('POSITIVE: Nat fully covered', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure isNat(Nat).
          isNat(0).
          isNat(s(N)) :- isNat(N?).
        ''');
        expect(result.isWellTyped, isTrue);
      });

      test('POSITIVE: Binary tree fully covered', () {
        final result = checkTypes('''
          Tree ::= leaf ; node(Tree, Any, Tree).
          procedure size(Tree?, Number).
          size(leaf, 0).
          size(node(L, _, R), N) :-
              size(L?, NL), size(R?, NR), N := NL? + NR? + 1.
        ''');
        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // Incomplete coverage (inferred ⊂ declared)
    // =========================================================================

    group('Incomplete Coverage', () {
      test('NEGATIVE: Missing base case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure isNat(Nat).
          isNat(s(N)) :- isNat(N?).
        ''');
        expect(result.isWellTyped, isFalse, reason: 'Missing case for 0');
        expect(
            result.errors.any((e) =>
                e.message.contains('incomplete') ||
                e.message.contains('not cover')),
            isTrue);
      });

      test('NEGATIVE: Missing recursive case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure isNat(Nat).
          isNat(0).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing case for s(Nat)');
      });

      test('NEGATIVE: Missing middle alternative', () {
        final result = checkTypes('''
          Color ::= red ; green ; blue.
          procedure isColor(Color).
          isColor(red).
          isColor(blue).
        ''');
        expect(result.isWellTyped, isFalse, reason: 'Missing case for green');
      });
    });

    // =========================================================================
    // Over-coverage (inferred ⊃ declared)
    // =========================================================================

    group('Over Coverage', () {
      test('NEGATIVE: Clause produces value outside type', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure makeNat(Nat).
          makeNat(0).
          makeNat(s(N)) :- makeNat(N?).
          makeNat(foo).
        ''');
        expect(result.isWellTyped, isFalse, reason: 'foo is not in Nat');
      });

      test('NEGATIVE: Wrong constructor in recursive case', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure double(Nat?, Nat).
          double(0, 0).
          double(s(N), p(M)) :- double(N?, M).
        ''');
        expect(result.isWellTyped, isFalse, reason: 'p(M) is not in Nat');
      });
    });

    // =========================================================================
    // ERRONEOUS - Currently pass but should fail after fixpoint check
    // =========================================================================

    group('Erroneous Passes', () {
      test('ERRONEOUS: Incomplete Nat not detected', () {
        // Current checker may not do full fixpoint check
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure pred(Nat?, Nat).
          pred(s(N), N?).
        ''');
        // CURRENT (wrong): may pass (only checks what's there)
        // EXPECTED (correct): fails - does not cover pred(0, _)
        expect(result.isWellTyped, isFalse,
            reason: 'pred does not handle 0 case');
      });

      test('ERRONEOUS: Multi-alternative type incomplete', () {
        final result = checkTypes('''
          Expr ::= num(Number) ; add(Expr, Expr) ; mul(Expr, Expr).
          procedure eval(Expr?, Number).
          eval(num(N), N?).
          eval(add(A, B), R) :- eval(A?, RA), eval(B?, RB), R := RA? + RB?.
        ''');
        // CURRENT (wrong): may pass
        // EXPECTED (correct): fails - missing mul case
        expect(result.isWellTyped, isFalse, reason: 'Missing mul(Expr, Expr) case');
      });
    });
  });
}
