// test/analysis/type_checker/clause_contribution_nfa_test.dart
//
// Tests for NFA-based clause contribution (spec v1.10 Section 5.4)

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Clause Contribution via NFA Pipeline', () {

    test('Simple list copy - well-typed', () {
      final source = '''
List ::= [] ; [_ | List].

procedure copy(List?, List).
copy([], []).
copy([H | In], [H? | Out?]) :- copy(In?, Out).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'Simple list copy should be well-typed: ${result.errors}');
    });

    test('Variable mode encoded correctly - writer as output', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure succ(Nat?, Nat).
succ(N, s(N?)).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'succ should be well-typed: ${result.errors}');
    });

    test('Variable mode encoded correctly - reader as input', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure pred(Nat?, Nat).
pred(s(N), N?).
pred(0, 0).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'pred should be well-typed: ${result.errors}');
    });

    test('DiffList type - well-typed dl_append', () {
      final source = '''
List ::= [] ; [_ | List].
DiffList ::= List \\ List?.

procedure dl_append(DiffList?, DiffList?, DiffList).
dl_append(A\\B?, B\\C?, A?\\C).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'dl_append should be well-typed: ${result.errors}');
    });

    test('Channel type with mode-distinguished alternatives', () {
      final source = '''
List ::= [] ; [_ | List].
Stream ::< List.
Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

procedure new_channel(Channel, Channel).
new_channel(ch(AtoB?, BtoA), ch(BtoA?, AtoB)).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'new_channel should be well-typed: ${result.errors}');
    });

    test('Coverage check - missing constructor alternative', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure half(Nat?, Nat).
half(s(s(N)), s(M?)) :- half(N?, M).
''';
      // Missing: half(0, 0). and half(s(0), 0).
      final result = checkTypes(source);

      // Should report incomplete coverage for ::= type
      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should report missing coverage for 0 and s(0) cases');
    });

    test('Subtype ::< does not require coverage', () {
      final source = '''
List ::= [] ; [_ | List].
Stream ::< List.

procedure echo(Stream?, Stream).
echo([H | T], [H? | Out?]) :- echo(T?, Out).
''';
      // Missing: echo([], []). but Stream ::< means no coverage required
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'Stream ::< should not require [] coverage: ${result.errors}');
    });

    test('Nested structure contribution', () {
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

    test('Constant in pattern contributes singleton', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure is_zero(Nat?, Nat).
is_zero(0, s(0)).
is_zero(s(_), 0).
''';
      final result = checkTypes(source);

      expect(result.isWellTyped, isTrue,
          reason: 'is_zero should be well-typed: ${result.errors}');
    });
  });

  group('Negative Controls - Should Fail', () {

    test('Wrong mode on variable - writer where reader expected', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure bad(Nat?, Nat).
bad(N?, s(N)).
''';
      // N? is reader but appears at output position in second arg
      // N is writer but appears at input position in first arg
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should reject wrong variable modes');
    });

    test('Type mismatch - String where Nat expected', () {
      final source = '''
Nat ::= 0 ; s(Nat).

procedure bad(Nat?, Nat).
bad("hello", 0).
''';
      final result = checkTypes(source);

      expect(result.errors.isNotEmpty, isTrue,
          reason: 'Should reject String constant where Nat expected');
    });
  });
}
