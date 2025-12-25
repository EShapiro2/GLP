// test/analysis/type_checker/clause_contribution_nfa_test.dart
//
// Tests for NFA-based clause contribution (spec v1.10 Section 5.4)
// Per Yardeni-Shapiro: P is well-typed by S iff T_P^α(S) = S (fixpoint)

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Clause Contribution via NFA Pipeline', () {

    test('Simple list copy - well-typed', () {
      // Use predefined List, don't redefine
      final source = '''
procedure copy(List?, List).
copy([], []).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'Simple list copy should be well-typed: ${result.errors}');
    });

    test('succ with precise output type s(Nat) - well-typed', () {
      // Per YS: declared type must equal inferred type (fixpoint)
      // succ only produces s(Nat), so declare s(Nat) not Nat
      final source = '''
Nat ::= 0 ; s(Nat).
PosNat ::= s(Nat).

procedure succ(Nat?, PosNat).
succ(N, s(N?)).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'succ with precise type should be well-typed: ${result.errors}');
    });

    test('pred with complete coverage - well-typed', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure pred(Nat?, Nat).
pred(s(N), N?).
pred(0, 0).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'pred with complete coverage should be well-typed: ${result.errors}');
    });

    test('DiffList type - well-typed dl_append', () {
      // Use predefined List and DiffList
      final source = '''
procedure dl_append(DiffList?, DiffList?, DiffList).
dl_append(A\\B?, B\\C?, A?\\C).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'dl_append should be well-typed: ${result.errors}');
    });

    test('swap with complete coverage - well-typed', () {
      final source = '''
Nat ::= 0 ; s(Nat).
Pair ::= pair(Nat, Nat).

procedure swap(Pair?, Pair).
swap(pair(X, Y), pair(Y?, X?)).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'swap should be well-typed: ${result.errors}');
    });

    test('Constant in clause with complete coverage', () {
      final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.

procedure is_zero(Nat?, Bool).
is_zero(0, true).
is_zero(s(_), false).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'is_zero should be well-typed: ${result.errors}');
    });

    test('Underscore matches any - uses Every', () {
      final source = '''
Nat ::= 0 ; s(Nat).
Unit ::= unit.

procedure ignore(Nat?, Unit).
ignore(_, unit).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'ignore with underscore should be well-typed: ${result.errors}');
    });

    test('Subtype ::< does not require coverage', () {
      // Stream ::< List means partial coverage is OK
      final source = '''
procedure echo(Stream?, Stream).
echo([H | T], [H? | Out?]) :- echo(T?, Out).
''';
      // Missing: echo([], []). but Stream ::< means no coverage required
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'Stream ::< should not require [] coverage: ${result.errors}');
    });
  });

  group('Negative Controls - Should Fail (per YS fixpoint requirement)', () {

    test('succ with imprecise type Nat - SHOULD FAIL', () {
      // Per YS: T_P^α(S) must equal S
      // succ produces only s(Nat), not full Nat
      // So Nat is NOT a fixpoint - this should fail
      final source = '''
Nat ::= 0 ; s(Nat).

procedure succ(Nat?, Nat).
succ(N, s(N?)).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'succ(Nat?, Nat) should fail: output s(Nat) ≠ Nat');
    });

    test('Missing constructor coverage - SHOULD FAIL', () {
      // half is missing cases for 0 and s(0)
      final source = '''
Nat ::= 0 ; s(Nat).

procedure half(Nat?, Nat).
half(s(s(N)), s(M?)) :- half(N?, M).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'half should fail: missing 0 and s(0) coverage');
    });

    test('Type mismatch - wrong constructor', () {
      final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.

procedure bad(Nat?, Bool).
bad(0, 0).
''';
      // 0 is Nat, not Bool
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should reject 0 where Bool expected');
    });
  });
}
