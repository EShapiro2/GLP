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
      }, skip: 'Nested primitive mode checking in lists not yet implemented');

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
    // Stream (same as List in new type system)
    // =========================================================================

    group('Stream', () {
      test('POSITIVE: Stream handler with complete coverage', () {
        // Note: Type aliasing (MyStream ::= MyList) is not allowed
        // because it creates epsilon transitions. Use the base type directly.
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure handle(_?).
          procedure process(MyList?).
          process([]).
          process([X | Xs]) :- handle(X?), process(Xs?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both cases covered for List');
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
    // Single-mode List (standard case, no extra coverage)
    // =========================================================================

    group('Single-mode List', () {
      test('POSITIVE: List copy with two clauses passes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Two clauses cover all cases for List with _ elements');
      }, skip: 'Nested primitive mode checking in lists not yet implemented');
    });
  });
}
