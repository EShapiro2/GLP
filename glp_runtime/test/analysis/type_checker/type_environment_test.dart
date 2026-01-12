// test/analysis/type_checker/type_environment_test.dart
//
// Tests for type environment validation per spec: type-environment.md v0.5

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment_builder.dart';
import 'test_helpers.dart';

void main() {
  group('Predefined types in TypeRef.builtins', () {
    test('Integer is a builtin type', () {
      expect(TypeRef.builtins.contains('Integer'), isTrue);
    });

    test('Real is a builtin type', () {
      expect(TypeRef.builtins.contains('Real'), isTrue);
    });

    test('Number is a builtin type', () {
      expect(TypeRef.builtins.contains('Number'), isTrue);
    });

    test('String is a builtin type', () {
      expect(TypeRef.builtins.contains('String'), isTrue);
    });

    test('builtins contains exactly 4 types', () {
      expect(TypeRef.builtins.length, equals(4));
    });
  });

  group('Type alias prohibition', () {
    test('NEGATIVE: alias to primitive _ is rejected', () {
      final source = 'Output ::= _.';
      expect(() => checkTypes(source), throwsA(isA<TypeAliasError>()));
    });

    test('NEGATIVE: alias to primitive _? is rejected', () {
      final source = 'Input ::= _?.';
      expect(() => checkTypes(source), throwsA(isA<TypeAliasError>()));
    });

    test('NEGATIVE: alias to defined type is rejected', () {
      final source = '''
        MyList ::= [] ; [_ | MyList].
        AliasList ::= MyList.
      ''';
      expect(() => checkTypes(source), throwsA(isA<TypeAliasError>()));
    });

    test('NEGATIVE: alias to complement type is rejected', () {
      final source = '''
        MyList ::= [] ; [_ | MyList].
        MyInput ::= MyList?.
      ''';
      expect(() => checkTypes(source), throwsA(isA<TypeAliasError>()));
    });

    test('POSITIVE: type with single compound alternative is valid', () {
      final source = '''
        Wrapper ::= wrap(_).
        procedure test(Wrapper).
        test(wrap(X)).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: type with multiple alternatives is valid', () {
      final source = '''
        Bool ::= true ; false.
        procedure test(Bool).
        test(true).
        test(false).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: type with single constant alternative is valid', () {
      final source = '''
        Unit ::= unit.
        procedure test(Unit).
        test(unit).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: type with list nil alternative is valid', () {
      final source = '''
        Empty ::= [].
        procedure test(Empty).
        test([]).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });
  });

  group('Determinism check', () {
    test('NEGATIVE: overlapping wildcards _ and _? rejected', () {
      final source = 'Any ::= _ ; _?.';
      expect(() => checkTypes(source), throwsA(isA<NonDeterministicTypeError>()));
    });

    test('NEGATIVE: _ overlaps with Integer rejected', () {
      final source = 'Ambiguous ::= _ ; Integer.';
      expect(() => checkTypes(source), throwsA(isA<NonDeterministicTypeError>()));
    });

    test('NEGATIVE: Number overlaps with Integer rejected', () {
      final source = 'BadNumeric ::= Number ; Integer.';
      expect(() => checkTypes(source), throwsA(isA<NonDeterministicTypeError>()));
    });

    test('NEGATIVE: duplicate functor same arity rejected', () {
      final source = 'BadTree ::= leaf(Integer) ; leaf(String).';
      expect(() => checkTypes(source), throwsA(isA<NonDeterministicTypeError>()));
    });

    test('NEGATIVE: duplicate constant rejected', () {
      final source = 'Bad ::= 0 ; 0.';
      expect(() => checkTypes(source), throwsA(isA<NonDeterministicTypeError>()));
    });

    test('POSITIVE: different functors same arity is valid', () {
      final source = '''
        Tree ::= leaf(Integer) ; node(Tree, Tree).
        procedure test(Tree).
        test(leaf(1)).
        test(node(leaf(1), leaf(2))).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: same functor different arity is valid', () {
      final source = '''
        Tree ::= leaf ; node(Tree, Tree).
        procedure test(Tree).
        test(leaf).
        test(node(leaf, leaf)).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: disjoint primitives Integer and String is valid', () {
      final source = '''
        Constant ::= Integer ; String.
        procedure test(Constant).
        test(42).
        test("hello").
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });

    test('POSITIVE: [] and [|] are distinguishable', () {
      final source = '''
        MyList ::= [] ; [_ | MyList].
        procedure test(MyList).
        test([]).
        test([X]).
      ''';
      final result = checkTypes(source);
      expect(result.isWellTyped, isTrue);
    });
  });
}
