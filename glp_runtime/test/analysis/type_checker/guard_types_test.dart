// test/analysis/type_checker/guard_types_test.dart
//
// Tests for guard type constraints and ground guards

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Guard Type Constraints', () {
    // =========================================================================
    // Type constraints from guards
    // =========================================================================

    group('Type Constraints', () {
      test('POSITIVE: number guard constrains to Number', () {
        final result = checkTypes('''
          procedure double_it(Any?, Number).
          double_it(X, Y) :- number(X?) | Y := X? * 2.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Guard constrains X to Number');
      });

      test('POSITIVE: string guard constrains to String', () {
        final result = checkTypes('''
          procedure process(Any?, String).
          process(X, Y) :- string(X?) | Y = X?.
        ''');
        expect(result.isWellTyped, isTrue);
      });

      test('POSITIVE: arithmetic guard constrains both args to Number', () {
        final result = checkTypes('''
          procedure max(Any?, Any?, Number).
          max(X, Y, X) :- X? >= Y? | true.
          max(X, Y, Y) :- X? < Y? | true.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Comparison guards constrain to Number');
      });

      test('NEGATIVE: guard inconsistent with head type fails', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure bad(Nat).
          bad(X) :- string(X?) | fail.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Nat and String have empty intersection');
      });

      test('NEGATIVE: number guard on non-number type fails', () {
        final result = checkTypes('''
          Sym ::= a ; b ; c.
          procedure bad(Sym?).
          bad(X) :- number(X?) | fail.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Sym and Number have empty intersection');
      });
    });

    // =========================================================================
    // Ground guards and mode coverage
    // =========================================================================

    group('Ground Guards', () {
      test('POSITIVE: ground guard allows multiple readers', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure broadcast(Any?, Any, Any).
          broadcast(X, Y, Z) :- ground(X?) | Y = X?, Z = X?.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'ground(X?) certifies X has no unbound vars');
      });

      test('POSITIVE: ground guard covers all mode alternatives', () {
        // Single clause with ground guard should cover both modes
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure echo(Any?, Any).
          echo(X, Y) :- ground(X?) | Y = X?.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'ground(X?) satisfies both _ and _? coverage');
      });

      test('POSITIVE: number guard implies ground', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure use_twice(Any?, Any, Any).
          use_twice(X, Y, Z) :- number(X?) | Y := X? + 1, Z := X? * 2.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'number(X?) implies X is ground');
      });

      test('NEGATIVE: multiple readers without ground guard fails', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure bad_broadcast(Any?, Any, Any).
          bad_broadcast(X, Y, Z) :- Y = X?, Z = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Multiple readers without ground guard is SRSW violation');
      });

      test('POSITIVE: ground on nested structure covers nested modes', () {
        final result = checkTypes('''
          Any ::= _ ; _?.
          AnyList ::= [] ; [Any | AnyList].
          procedure process_list(AnyList?, Any).
          process_list(L, X) :- ground(L?) | member(X?, L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'ground(L?) covers all nested Any positions');
      });
    });

    // =========================================================================
    // ERRONEOUS - Currently pass but should fail
    // =========================================================================

    group('Erroneous Passes', () {
      test('ERRONEOUS: known guard should NOT imply ground', () {
        // known(X?) only checks top-level binding, not nested groundness
        final result = checkTypes('''
          Any ::= _ ; _?.
          procedure bad(Any?, Any, Any).
          bad(X, Y, Z) :- known(X?) | Y = X?, Z = X?.
        ''');
        // CURRENT (wrong): may pass
        // EXPECTED (correct): fails - known does not imply ground
        expect(result.isWellTyped, isFalse,
            reason: 'known(X?) does not guarantee groundness');
      });
    });
  });
}
