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
          procedure fill_slots(InvStream?).
          fill_slots([]).
          fill_slots([Slot | Rest]) :- handle(Slot?), fill_slots(Rest?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Slot should be reader (Slot?) not writer (Slot)');
      });
    });

    // =========================================================================
    // BimodalList (both _ and _? required)
    // =========================================================================

    group('BimodalList', () {
      test('POSITIVE: BimodalList copy with three clauses passes', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          BimodalList ::= [] ; [Bimodal | BimodalList].
          procedure copy(BimodalList?, BimodalList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
          copy([X? | In], [X | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'All three clauses cover both modes at element position');
      });

      test('NEGATIVE: BimodalList copy with two clauses fails', () {
        final result = checkTypes('''
          Bimodal ::= _ ; _?.
          BimodalList ::= [] ; [Bimodal | BimodalList].
          procedure copy(BimodalList?, BimodalList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing reverse mode clause');
      });
    });
  });
}
