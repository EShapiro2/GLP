// test/analysis/type_checker/system_types_test.dart
//
// Tests for system types: List, Stream, InvStream

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('System Types', () {
    // =========================================================================
    // List (exact semantics, _ elements)
    // =========================================================================

    group('List', () {
      test('POSITIVE: Complete list procedure passes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure length(MyList?, Number).
          length([], 0).
          length([_ | Xs], N) :- length(Xs?, M), N := M? + 1.
        ''');
        expect(result.isWellTyped, isTrue);
      });

      test('NEGATIVE: Missing [] case fails', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure length(MyList?, Number).
          length([_ | Xs], N) :- length(Xs?, M), N := M? + 1.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing base case for []');
      });

      test('NEGATIVE: Missing cons case fails', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure length(MyList?, Number).
          length([], 0).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing recursive case for [_|MyList]');
      });
    });

    // =========================================================================
    // Stream (subtype semantics, may not close)
    // =========================================================================

    group('Stream', () {
      test('POSITIVE: Stream handler without [] case passes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyStream ::= [Any | MyStream].
          procedure handle(Any?).
          procedure process(MyStream?).
          process([X | Xs]) :- handle(X?), process(Xs?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'MyStream has no [] constructor');
      });

      test('POSITIVE: Stream handler with [] case also passes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyStream ::= [Any | MyStream].
          procedure handle(Any?).
          procedure process(MyStream?).
          process([]).
          process([X | Xs]) :- handle(X?), process(Xs?).
        ''');
        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // InvStream (input mode elements)
    // =========================================================================

    group('InvStream', () {
      test('POSITIVE: InvStream with reader elements', () {
        final result = checkTypes('''
          InvStream ::= [] ; [_? | InvStream].
          procedure fill_slots(InvStream?).
          fill_slots([]).
          fill_slots([Slot? | Rest]) :- Slot = value, fill_slots(Rest?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'InvStream elements are _? (input), filled by writer');
      });

      test('NEGATIVE: InvStream with wrong element mode fails', () {
        final result = checkTypes('''
          InvStream ::= [] ; [_? | InvStream].
          procedure handle(Any?).
          procedure fill_slots(InvStream?).
          fill_slots([]).
          fill_slots([Slot | Rest]) :- handle(Slot?), fill_slots(Rest?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Slot should be reader (Slot?) not writer (Slot)');
      });
    });

    // =========================================================================
    // AnyList (theoretical, requires full mode coverage)
    // =========================================================================

    group('AnyList', () {
      test('POSITIVE: AnyList copy with three clauses passes', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAnyList ::= [] ; [MyAny | MyAnyList].
          procedure copy(MyAnyList?, MyAnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
          copy([X? | In], [X | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'All three clauses cover both modes at element position');
      });

      test('NEGATIVE: AnyList copy with two clauses fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAnyList ::= [] ; [MyAny | MyAnyList].
          procedure copy(MyAnyList?, MyAnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing reverse mode clause');
      });
    });
  });
}
