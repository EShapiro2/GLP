// test/analysis/type_checker/defined_guards_test.dart
//
// Tests for user-defined guards (unit clauses callable in guard position)

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Defined Guards', () {
    test('POSITIVE: Defined guard with procedure declaration', () {
      // Note: Type aliasing is not allowed, use base types directly
      final result = checkTypes('''
        MyList ::= [] ; [_ | MyList].
        MyChannel ::= ch(MyList?, MyList) ; ch(MyList, MyList?).

        procedure channel(MyChannel?).
        channel(ch(_, _)).

        procedure handle(_?).
        procedure process(_?, String).
        process(X, ok) :- channel(X?) | handle(X?).
        process(_, error) :- otherwise | true.
      ''');
      expect(result.isWellTyped, isTrue);
    }, skip: 'Defined guard type checking not yet implemented');

    test('NEGATIVE: Defined guard without declaration fails', () {
      final result = checkTypes('''
        my_guard(foo).

        procedure use_guard(_?).
        use_guard(X) :- my_guard(X?) | handle(X?).
      ''');
      expect(result.isWellTyped, isFalse,
          reason: 'my_guard needs procedure declaration');
    });

    test('POSITIVE: Defined guard constrains type', () {
      final result = checkTypes('''
        Pair ::= pair(_, _).

        procedure is_pair(Pair?).
        is_pair(pair(_, _)).

        procedure swap(_?, Pair).
        swap(X, pair(B?, A?)) :- is_pair(X?) | X = pair(A, B).
      ''');
      expect(result.isWellTyped, isTrue,
          reason: 'is_pair constrains X to Pair');
    }, skip: 'Defined guard type checking not yet implemented');
  });
}
