// test/analysis/type_checker/clause_contribution_nfa_test.dart
//
// Tests for NFA-based clause contribution (spec v1.10 Section 5.4)
// Per Yardeni-Shapiro: P is well-typed by S iff T_P^α(S) = S (fixpoint equality)

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Clause Contribution via NFA Pipeline - Positive Controls', () {

    test('List copy with predefined List - well-typed', () {
      final source = '''
procedure copy(List?, List).
copy([], []).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'List copy should be well-typed: ${result.errors}');
    });

    test('succ with precise output type PosNat - well-typed', () {
      // Per YS: declared type must equal inferred type (fixpoint)
      // succ only produces s(Nat), so declare s(Nat) as output type
      final source = '''
Nat ::= 0 ; s(Nat).
PosNat ::= s(Nat).

procedure succ(Nat?, PosNat).
succ(N, s(N?)).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'succ with precise PosNat output should be well-typed: ${result.errors}');
    });

    test('pred with complete Nat coverage - well-typed', () {
      // pred covers both 0 and s(N) cases, so Nat output is correct
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

    test('DiffList my_dl_append - well-typed', () {
      // Use different name to avoid conflict with predefined dl_append
      final source = '''
procedure my_dl_append(DiffList?, DiffList?, DiffList).
my_dl_append(A\\B?, B\\C?, A?\\C).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'my_dl_append should be well-typed: ${result.errors}');
    });

    test('swap with Pair type - well-typed', () {
      // swap covers the single constructor pair/2
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

    test('is_zero with Bool output - well-typed', () {
      // Covers both Nat constructors (0 and s), produces both Bool constructors
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

    test('ignore with Unit output - well-typed', () {
      // Underscore matches any Nat, produces single Unit constructor
      final source = '''
Nat ::= 0 ; s(Nat).
Unit ::= unit.

procedure ignore(Nat?, Unit).
ignore(_, unit).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'ignore should be well-typed: ${result.errors}');
    });

    test('Stream echo - partial coverage OK (Stream has no nil)', () {
      // Stream ::= [Any | Stream] means no [] case exists
      final source = '''
procedure echo(Stream?, Stream).
echo([H | T], [H? | Out?]) :- echo(T?, Out).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'Stream has no [] constructor: ${result.errors}');
    });

    test('double with precise output type - well-typed', () {
      // double(N) = s(s(...N times...)) which is PosNat for N>0, but 0 maps to 0
      // So we need Nat output since we produce both 0 and s(...)
      final source = '''
Nat ::= 0 ; s(Nat).

procedure double(Nat?, Nat).
double(0, 0).
double(s(N), s(s(M?))) :- double(N?, M).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'double should be well-typed: ${result.errors}');
    });
  });

  group('Clause Contribution - Negative Controls (Should Fail)', () {

    test('succ with imprecise Nat output - FAILS structural coverage', () {
      // Per YS: T_P^α(S) must equal S
      // succ produces only s(Nat), not full Nat (missing 0)
      // So Nat output violates fixpoint equality
      final source = '''
Nat ::= 0 ; s(Nat).

procedure succ(Nat?, Nat).
succ(N, s(N?)).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'succ(Nat?, Nat) should FAIL: output s(Nat) does not cover full Nat');
    });

    test('half missing base cases - FAILS structural coverage', () {
      // half is missing cases for 0 and s(0)
      final source = '''
Nat ::= 0 ; s(Nat).

procedure half(Nat?, Nat).
half(s(s(N)), s(M?)) :- half(N?, M).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'half should FAIL: missing coverage for 0 and s(0)');
    });

    test('wrong constructor in output - FAILS type check', () {
      // Produces Nat constant 0 where Bool expected
      final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.

procedure bad(Nat?, Bool).
bad(0, 0).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should FAIL: 0 is Nat, not Bool');
    });

    test('missing constructor alternative - FAILS structural coverage', () {
      // Only produces true, never false
      final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.

procedure always_true(Nat?, Bool).
always_true(_, true).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should FAIL: only produces true, missing false');
    });
  });

  group('Hypothesis Tests', () {

    test('Hypothesis 1: double with EvenNat output type', () {
      // Test if double with precise EvenNat type passes
      final source = '''
Nat ::= 0 ; s(Nat).
EvenNat ::= 0 ; s(s(EvenNat)).

procedure double(Nat?, EvenNat).
double(0, 0).
double(s(N), s(s(M?))) :- double(N?, M).
''';
      final result = checkTypes(source);

      print('Test 1 - double with EvenNat:');
      print('  Well-typed: ${result.isWellTyped}');
      print('  Errors: ${result.errors}');
      print('  Warnings: ${result.warnings}');
    });

    test('Hypothesis 2: always_true with ground guard', () {
      // Test if strict SRSW with ground guard detects coverage error
      final source = '''
Nat ::= 0 ; s(Nat).
Bool ::= true ; false.

procedure always_true(Nat?, Bool).
always_true(X, true) :- ground(X?) | true.
''';
      final result = checkTypes(source);

      print('Test 2 - always_true with ground guard:');
      print('  Well-typed: ${result.isWellTyped}');
      print('  Errors: ${result.errors}');
      print('  Warnings: ${result.warnings}');
    });
  });
}
