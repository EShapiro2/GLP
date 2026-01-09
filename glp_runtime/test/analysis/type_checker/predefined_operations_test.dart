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
          Out ::= _.
          procedure produce(Out).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Output _ accepts writer');
      });

      test('POSITIVE: Input position with writer (flips to reader)', () {
        // Per spec: Input positions expect HEAD WRITERS (which flip to readers)
        // X in head → X? after flip (serves as input)
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Input _? accepts writer in head (flips to reader)');
      });

      test('POSITIVE: Writer at _ position is valid', () {
        final result = checkTypes('''
          Out ::= _.
          procedure produce(Out).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue, reason: '_ accepts writer');
      });

      test('POSITIVE: Writer at _? (input) position is valid', () {
        // Per spec: Input positions expect HEAD WRITERS (which flip to readers)
        // Head writer X → reader X? after flip (serves as input)
        final result = checkTypes('''
          In ::= _?.
          procedure consume(In?).
          consume(X).
        ''');
        expect(result.isWellTyped, isTrue, reason: '_? accepts writer (flips to reader)');
      });

      test('POSITIVE: List with _ elements needs only two clauses', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          procedure copy(MyList?, MyList).
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
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A\\B?, B\\C?, A?\\C).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append has correct mode annotations');
      });

      test('POSITIVE: dl_to_list is well-moded', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_to_list(MyDiffList?, MyList).
          my_dl_to_list(L\\[], L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes the hole correctly');
      });

      test('NEGATIVE: dl_append with wrong modes fails', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A?\\B, B?\\C, A\\C?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Modes are inverted incorrectly');
      });

      test('POSITIVE: dl_append demonstrates O(1) concatenation', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A\\B?, B\\C?, A?\\C).

          procedure my_dl_to_list(MyDiffList?, MyList).
          my_dl_to_list(L\\[], L?).

          procedure use_append(MyList?, MyList?, MyList).
          use_append(L1, L2, Result) :-
              dl1(L1?, DL1),
              dl2(L2?, DL2),
              my_dl_append(DL1?, DL2?, DL3),
              my_dl_to_list(DL3?, Result).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append used to concatenate lists efficiently');
      });
    });

    // =========================================================================
    // Channel operations
    // =========================================================================

    group('Channel', () {
      // Note: Type aliasing (Stream ::= List) is not allowed.
      // Use base types directly in type definitions.

      test('POSITIVE: new_channel is well-moded', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure my_new_channel(MyChan, MyChan).
          my_new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'new_channel swaps streams correctly');
      });

      test('POSITIVE: send is well-moded', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure my_send(_, MyChan?, MyChan).
          my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'send adds message to output stream');
      });

      test('POSITIVE: receive is well-moded', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure my_receive(_?, MyChan?, MyChan).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive takes message from input stream');
      });

      test('NEGATIVE: send with wrong message mode fails', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure my_send(_, MyChan?, MyChan).
          my_send(X?, ch(In, [X|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'X should be writer in first arg, reader in stream');
      });

      test('POSITIVE: Producer-consumer pattern', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure done(MyChan?).
          procedure process(_?).
          procedure my_send(_, MyChan?, MyChan).
          my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).

          procedure my_receive(_?, MyChan?, MyChan).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure producer(MyChan?).
          producer(Ch) :-
              my_send(hello, Ch?, Ch2),
              my_send(world, Ch2?, Ch3),
              done(Ch3?).

          procedure consumer(MyChan?).
          consumer(Ch) :- my_receive(Msg, Ch?, Ch2) |
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
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A\\B?, B\\C?, A?\\C).

          procedure process(MyDiffList?, MyDiffList?, MyDiffList).
          process(DL1, DL2, Result) :- my_dl_append(DL1?, DL2?, Result) |
              continue(Result?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Unit clause can be used as guard');
      });

      test('POSITIVE: receive usable in guard position', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyChan ::= ch(MyList?, MyList).

          procedure process(_?).
          procedure my_receive(_?, MyChan?, MyChan).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure handler(MyChan?).
          handler(Ch) :- my_receive(Msg, Ch?, Ch2) |
              process(Msg?),
              handler(Ch2?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive in guard position suspends until message');
      });

      test('POSITIVE: dl_to_list in guard for closing difference list', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_to_list(MyDiffList?, MyList).
          my_dl_to_list(L\\[], L?).

          procedure finalize(MyDiffList?, MyList).
          finalize(DL, L) :- my_dl_to_list(DL?, L) |
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
          MyList ::= [_ | MyList] ; [].

          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List with _ elements only needs two clauses');
      });

      test('POSITIVE: List (with _ elements) simple copy', () {
        final result = checkTypes('''
          MyList ::= [_ | MyList] ; [].

          procedure copy_list(MyList?, MyList).
          copy_list([], []).
          copy_list([X | In], [X? | Out]) :- copy_list(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'List (_ elements) simpler than lists with multiple modes');
      });
    });
  });
}
