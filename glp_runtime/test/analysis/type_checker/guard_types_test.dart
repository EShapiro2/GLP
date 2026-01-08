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
          procedure process(_?, Number).
          process(X, Y) :- number(X?) | Y := X? * 2.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Guard constrains X to Number, arithmetic valid');
      });

      test('string(X?) constrains X to String', () {
        final result = checkTypes('''
          procedure process(_?, String).
          process(X, Y?) :- string(X?) | Y = X?.
        ''');
        expect(result.errors, isEmpty);
      });

      test('arithmetic guards constrain to Number', () {
        final result = checkTypes('''
          procedure max(_?, _?, Number).
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
          procedure broadcast(_?, _, _).
          broadcast(X, Y?, Z?) :- ground(X?) | Y = X?, Z = X?.
        ''');
        expect(result.errors, isEmpty,
            reason: 'ground(X?) certifies X has no unbound vars');
      });

      test('number guard implies ground for multiple use', () {
        final result = checkTypes('''
          procedure compute(_?, Number, Number).
          compute(X, Y?, Z?) :- number(X?) | Y := X? + 1, Z := X? * 2.
        ''');
        expect(result.errors, isEmpty,
            reason: 'number(X?) implies X is ground');
      });

      test('known(X?) does NOT imply ground', () {
        // known only checks top-level binding, not recursive groundness
        final result = checkTypes('''
          procedure bad(_?, _).
          bad(X, Y?) :- known(X?) | Y = X?.
        ''');
        // This should still require mode coverage
        // because known does not imply ground
        expect(result.errors, isNotEmpty,
            reason: 'known does not satisfy mode coverage');
      });

      test('POSITIVE: ground on type without mode complementations', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].

          procedure check(MyList?, _).

          check(X, Y) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isEmpty,
            reason: 'MyList has no mode complementations, ground(X?) is WMT');
      });

      test('NEGATIVE: ground on type with mode complementations', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyDiffList ::= MyList \\ MyList?.

          procedure bad(MyDiffList?, _).

          bad(X, Y?) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'MyDiffList has mode complementation (MyList?), ground(X?) is not WMT');
      });

      test('POSITIVE: ground on Nat (no mode complementations)', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?, _).

          check(X, Y) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Nat has no mode complementations');
      });

      test('NEGATIVE: ground on Channel (has mode complementations)', () {
        final result = checkTypes('''
          MyStream ::= [] ; [_ | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure bad(MyChannel?, _).

          bad(X, Y?) :- ground(X?) | Y = ok.
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'MyChannel has mode complementation (MyStream?)');
      });

    });

    // =========================================================================
    // Input Argument Error Detection (verify fixpoint skip doesn't hide errors)
    // =========================================================================

    group('Input Argument Errors', () {

      test('NEGATIVE: type mismatch at input position', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?).

          check(foo).
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'Atom foo does not match Nat type');
      });

      test('NEGATIVE: constructor mismatch at input position', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?).

          check([H | T]).
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'List constructor does not match Nat type');
      });

      test('NEGATIVE: mode mismatch at input position (reader where writer expected)', () {
        final result = checkTypes('''
          InputOnly ::= _?.

          procedure check(InputOnly?).

          check(X?).
        ''');
        expect(result.errors, isNotEmpty,
            reason: 'Reader X? at InputOnly? position: after complementation expects writer');
      });

      test('POSITIVE: correct pattern at input position', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?).

          check(0).
          check(s(N)) :- check(N?).
        ''');
        expect(result.errors, isEmpty,
            reason: 'Pattern 0 and s(N) match Nat type');
      });

      test('POSITIVE: variable at input position accepts all', () {
        final result = checkTypes('''
          Nat ::= 0 ; s(Nat).

          procedure check(Nat?).

          check(X) :- ground(X?) | true.
        ''');
        expect(result.errors, isEmpty,
            reason: 'Variable X accepts any Nat value');
      });

    });

    // =========================================================================
    // Defined Guards
    // =========================================================================

    group('Defined Guards', () {

      test('defined guard constrains type', () {
        final result = checkTypes('''
          Pair ::= pair(_, _).

          procedure is_pair(Pair?).
          is_pair(pair(_, _)).

          procedure first(_?, _).
          first(X, A?) :- is_pair(X?) | X = pair(A, _).
        ''');
        expect(result.errors, isEmpty,
            reason: 'is_pair constrains X to Pair');
      });

    });

  });
}
