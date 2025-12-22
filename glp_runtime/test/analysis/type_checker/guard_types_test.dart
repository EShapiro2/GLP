// test/analysis/type_checker/guard_types_test.dart
//
// Tests for guard type checking

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Guard Type Checking', () {

    // =========================================================================
    // Type Constraints from Guards
    // =========================================================================

    group('Type Constraints', () {

      test('number(X?) constrains X to Number', () {
        final result = checkTypes('''
          procedure process(Any?, Number).
          process(X, Y) :- number(X?) | Y := X? * 2.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Guard constrains X to Number, arithmetic valid');
      });

      test('string(X?) constrains X to String', () {
        final result = checkTypes('''
          procedure process(Any?, String).
          process(X, Y?) :- string(X?) | Y = X?.
        ''');
        expect(result.errors, isEmpty);
      });

      test('arithmetic guards constrain to Number', () {
        final result = checkTypes('''
          procedure max(Any?, Any?, Number).
          max(X, Y, X?) :- X? >= Y? | true.
          max(X, Y, Y?) :- X? < Y? | true.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Comparison guards constrain both args to Number');
      });

      test('guard inconsistent with head type fails', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).
          procedure bad(Nat?).
          bad(X) :- string(X?) | true.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'Nat and String have empty intersection');
        expect(result.errors.any((e) =>
            e.message.contains('inconsistent')), isTrue);
      });

      test('number guard on non-number type fails', () {
        final result = checkTypes('''
          Sym ::= a ; b ; c.
          procedure bad(Sym?).
          bad(X) :- number(X?) | true.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'Sym and Number have empty intersection');
      });

    });

    // =========================================================================
    // Ground Guards and Mode Coverage
    // =========================================================================

    group('Ground Guards', () {

      test('ground(X?) allows multiple readers', () {
        final result = checkTypes('''
          procedure broadcast(Any?, Any, Any).
          broadcast(X, Y?, Z?) :- ground(X?) | Y = X?, Z = X?.
        ''');
        expect(result.errors, isEmpty,
            reason: 'ground(X?) certifies X has no unbound vars');
      });

      test('number guard implies ground for multiple use', () {
        final result = checkTypes('''
          procedure compute(Any?, Number, Number).
          compute(X, Y?, Z?) :- number(X?) | Y := X? + 1, Z := X? * 2.
        ''');
        expect(result.errors, isEmpty,
            reason: 'number(X?) implies X is ground');
      });

      test('known(X?) does NOT imply ground', () {
        // known only checks top-level binding, not recursive groundness
        final result = checkTypes('''
          procedure bad(Every?, Every).
          bad(X, Y?) :- known(X?) | Y = X?.
        ''');
        // This should still require mode coverage for Every
        // because known does not imply ground
        expect(result.errors, isNotEmpty,
            reason: 'known does not satisfy mode coverage');
      });

      test('POSITIVE: ground on type without mode complementations', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].

          procedure broadcast(MyList?, Any, Any).

          broadcast(X, Y?, Z?) :- ground(X?) | Y = ok, Z = ok.
        ''');
        expect(result.errors, isEmpty,
            reason: 'MyList has no mode complementations, ground(X?) is WMT');
      });

      test('NEGATIVE: ground on type with mode complementations', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyDiffList ::= MyList \\ MyList?.

          procedure bad(MyDiffList?, Any).

          bad(X, Y?) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'MyDiffList has mode complementation (MyList?), ground(X?) is not WMT');
      });

      test('POSITIVE: ground on Nat (no mode complementations)', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?, Any).

          check(X, Y?) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Nat has no mode complementations');
      });

      test('NEGATIVE: ground on Channel (has mode complementations)', () {
        final result = checkTypes('''
          MyStream ::< Any.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure bad(MyChannel?, Any).

          bad(X, Y?) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'MyChannel has mode complementation (MyStream?)');
      });

    });

    // =========================================================================
    // Defined Guards
    // =========================================================================

    group('Defined Guards', () {

      test('defined guard constrains type', () {
        final result = checkTypes('''
          Pair ::= pair(Any, Any).

          procedure is_pair(Pair?).
          is_pair(pair(_, _)).

          procedure first(Any?, Any).
          first(X, A?) :- is_pair(X?) | X = pair(A, _).
        ''');
        expect(result.errors, isEmpty,
            reason: 'is_pair constrains X to Pair');
      });

    });

  });
}
