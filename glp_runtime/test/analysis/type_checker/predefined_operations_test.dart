// test/analysis/type_checker/predefined_operations_test.dart
//
// Tests for predefined type operations: DiffList and Channel
// Note: Any/Every types removed - use _ and _? primitives directly

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Predefined Operations', () {
    // =========================================================================
    // Primitive mode types
    // =========================================================================

    group('Primitive Types', () {
      test('POSITIVE: Output position with writer', () {
        final result = checkTypes('''
          Output ::= _.
          procedure produce(Output).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output _ accepts writer');
      });

      test('POSITIVE: Input position with reader', () {
        final result = checkTypes('''
          Input ::= _?.
          procedure consume(Input?).
          consume(X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input _? accepts reader');
      });

      test('POSITIVE: Writer at _ position is valid', () {
        final result = checkTypes('''
          Output ::= _.
          procedure produce(Output).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue, reason: '_ accepts writer');
      });

      test('POSITIVE: Reader at _? position is valid', () {
        final result = checkTypes('''
          Input ::= _?.
          procedure consume(Input?).
          consume(X?).
        ''');
        expect(result.isWellTyped, isTrue, reason: '_? accepts reader');
      });

      test('POSITIVE: List with _ elements needs only two clauses', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          procedure copy(List?, List).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: '_ has single mode, no coverage requirement');
      });
    });

    // =========================================================================
    // DiffList operations
    // =========================================================================

    group('DiffList', () {
      test('POSITIVE: dl_append is well-moded', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_append(DiffList?, DiffList?, DiffList).
          dl_append(A\\B?, B\\C?, A?\\C).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append has correct mode annotations');
      });

      test('POSITIVE: dl_to_list is well-moded', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_to_list(DiffList?, List).
          dl_to_list(L\\[], L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes the hole correctly');
      });

      test('NEGATIVE: dl_append with wrong modes fails', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_append(DiffList?, DiffList?, DiffList).
          dl_append(A?\\B, B?\\C, A\\C?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Modes are inverted incorrectly');
      });

      test('POSITIVE: dl_append demonstrates O(1) concatenation', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_append(DiffList?, DiffList?, DiffList).
          dl_append(A\\B?, B\\C?, A?\\C).

          procedure use_append(List?, List?, List).
          use_append(L1, L2, Result) :-
              dl1(L1?, DL1),
              dl2(L2?, DL2),
              dl_append(DL1?, DL2?, DL3),
              dl_to_list(DL3?, Result).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append used to concatenate lists efficiently');
      });
    });

    // =========================================================================
    // Channel operations
    // =========================================================================

    group('Channel', () {
      test('POSITIVE: new_channel is well-moded', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure new_channel(Channel, Channel).
          new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'new_channel swaps streams correctly');
      });

      test('POSITIVE: send is well-moded', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure send(_, Channel?, Channel).
          send(X, ch(In, [X?|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'send adds message to output stream');
      });

      test('POSITIVE: receive is well-moded', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure receive(_?, Channel?, Channel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive takes message from input stream');
      });

      test('NEGATIVE: send with wrong message mode fails', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure send(_, Channel?, Channel).
          send(X?, ch(In, [X|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'X should be writer in first arg, reader in stream');
      });

      test('POSITIVE: Producer-consumer pattern', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure send(_, Channel?, Channel).
          send(X, ch(In, [X?|Out?]), ch(In?, Out)).

          procedure receive(_?, Channel?, Channel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure producer(Channel?).
          producer(Ch) :-
              send(hello, Ch?, Ch2),
              send(world, Ch2?, Ch3),
              done(Ch3?).

          procedure consumer(Channel?).
          consumer(Ch) :- receive(Msg, Ch?, Ch2) |
              process(Msg?),
              consumer(Ch2?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Producer-consumer pattern is well-typed');
      });
    });

    // =========================================================================
    // Usage as defined guards
    // =========================================================================

    group('Defined Guards', () {
      test('POSITIVE: dl_append usable in guard position', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_append(DiffList?, DiffList?, DiffList).
          dl_append(A\\B?, B\\C?, A?\\C).

          procedure process(DiffList?, DiffList?, DiffList).
          process(DL1, DL2, Result) :- dl_append(DL1?, DL2?, Result) |
              continue(Result?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Unit clause can be used as guard');
      });

      test('POSITIVE: receive usable in guard position', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          Stream ::= List.
          Channel ::= ch(Stream?, Stream).

          procedure receive(_?, Channel?, Channel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure handler(Channel?).
          handler(Ch) :- receive(Msg, Ch?, Ch2) |
              process(Msg?),
              handler(Ch2?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive in guard position suspends until message');
      });

      test('POSITIVE: dl_to_list in guard for closing difference list', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].
          DiffList ::= List \\ List?.

          procedure dl_to_list(DiffList?, List).
          dl_to_list(L\\[], L?).

          procedure finalize(DiffList?, List).
          finalize(DL, L) :- dl_to_list(DL?, L) |
              output(L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes difference list in guard');
      });
    });

    // =========================================================================
    // List with single mode elements (no coverage requirement)
    // =========================================================================

    group('Single-mode List', () {
      test('POSITIVE: List copy with two clauses passes', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].

          procedure copy(List?, List).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List with _ elements only needs two clauses');
      });

      test('POSITIVE: List (with _ elements) simple copy', () {
        final result = checkTypes('''
          List ::= [_ | List] ; [].

          procedure copy_list(List?, List).
          copy_list([], []).
          copy_list([X | In], [X? | Out]) :- copy_list(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List (_ elements) simpler than lists with multiple modes');
      });
    });
  });
}
