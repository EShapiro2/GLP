// test/analysis/type_checker/predefined_operations_test.dart
//
// Tests for predefined type operations: Every/Any self-duality, DiffList, and Channel

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Predefined Operations', () {
    // =========================================================================
    // Self-duality of Every and Any
    // =========================================================================

    group('Self-Duality', () {
      test('POSITIVE: Any and Any? are equivalent', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          procedure foo(MyAny, MyAny?).
          foo(X, X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Any = Any? by self-duality');
      });

      test('POSITIVE: Every and Every? are equivalent', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          procedure foo(MyEvery, MyEvery?).
          foo(X, X?).
          foo(X?, X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Every = Every? but still needs both modes covered');
      });

      test('POSITIVE: Writer at Any position is valid', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          procedure produce(MyAny).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue, reason: 'Any accepts writer');
      });

      test('POSITIVE: Reader at Any position is valid', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          procedure consume(MyAny).
          consume(X?).
        ''');
        expect(result.isWellTyped, isTrue, reason: 'Any accepts reader');
      });

      test('POSITIVE: List with Any elements needs only two clauses', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Any has no coverage requirement');
      });

      test('NEGATIVE: Every needs both modes despite self-duality', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          procedure echo(MyEvery?, MyEvery).
          echo(X, Y?) :- Y = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Every ::= requires both _ and _? covered');
      });

      test('POSITIVE: Every with both modes covered', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          procedure echo(MyEvery?, MyEvery).
          echo(X, Y?) :- Y = X?.
          echo(X?, Y) :- Y? = X.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both modes covered for Every');
      });
    });

    // =========================================================================
    // DiffList operations
    // =========================================================================

    group('DiffList', () {
      test('POSITIVE: dl_append is well-moded', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          dl_append(A\\B?, B\\C?, A?\\C).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append has correct mode annotations');
      });

      test('POSITIVE: dl_to_list is well-moded', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_to_list(MyDiffList?, MyList).
          dl_to_list(L\\[], L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes the hole correctly');
      });

      test('NEGATIVE: dl_append with wrong modes fails', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          dl_append(A?\\B, B?\\C, A\\C?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Modes are inverted incorrectly');
      });

      test('POSITIVE: dl_append demonstrates O(1) concatenation', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          dl_append(A\\B?, B\\C?, A?\\C).

          procedure use_append(MyList?, MyList?, MyList).
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
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure new_channel(MyChannel, MyChannel).
          new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'new_channel swaps streams correctly');
      });

      test('POSITIVE: send is well-moded', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure send(MyAny, MyChannel?, MyChannel).
          send(X, ch(In, [X?|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'send adds message to output stream');
      });

      test('POSITIVE: receive is well-moded', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure receive(MyAny, MyChannel?, MyChannel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive takes message from input stream');
      });

      test('NEGATIVE: send with wrong message mode fails', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure send(MyAny, MyChannel?, MyChannel).
          send(X?, ch(In, [X|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'X should be writer in first arg, reader in stream');
      });

      test('POSITIVE: Producer-consumer pattern', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure send(MyAny, MyChannel?, MyChannel).
          send(X, ch(In, [X?|Out?]), ch(In?, Out)).

          procedure receive(MyAny, MyChannel?, MyChannel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure producer(MyChannel?).
          producer(Ch) :-
              send(hello, Ch?, Ch2),
              send(world, Ch2?, Ch3),
              done(Ch3?).

          procedure consumer(MyChannel?).
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
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          dl_append(A\\B?, B\\C?, A?\\C).

          procedure process(MyDiffList?, MyDiffList?, MyDiffList).
          process(DL1, DL2, Result) :- dl_append(DL1?, DL2?, Result) |
              continue(Result?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Unit clause can be used as guard');
      });

      test('POSITIVE: receive usable in guard position', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::< MyList.
          MyChannel ::= ch(MyStream?, MyStream).

          procedure receive(MyAny, MyChannel?, MyChannel).
          receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure handler(MyChannel?).
          handler(Ch) :- receive(Msg, Ch?, Ch2) |
              process(Msg?),
              handler(Ch2?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive in guard position suspends until message');
      });

      test('POSITIVE: dl_to_list in guard for closing difference list', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure dl_to_list(MyDiffList?, MyList).
          dl_to_list(L\\[], L?).

          procedure finalize(MyDiffList?, MyList).
          finalize(DL, L) :- dl_to_list(DL?, L) |
              output(L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes difference list in guard');
      });
    });

    // =========================================================================
    // EveryList theoretical example
    // =========================================================================

    group('EveryList', () {
      test('POSITIVE: EveryList copy with three clauses', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          EveryList ::= [MyEvery | EveryList] ; [].

          procedure copy(EveryList?, EveryList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
          copy([X? | In], [X | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'EveryList requires both element mode alternatives');
      });

      test('NEGATIVE: EveryList copy with only two clauses fails', () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          EveryList ::= [MyEvery | EveryList] ; [].

          procedure copy(EveryList?, EveryList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing second element mode');
      });

      test('POSITIVE: List (with Any) needs only two clauses vs EveryList three',
          () {
        final result = checkTypes('''
          MyEvery ::= _ ; _?.
          MyAny ::< MyEvery.
          MyList ::= [MyAny | MyList] ; [].
          EveryList ::= [MyEvery | EveryList] ; [].

          procedure copy_list(MyList?, MyList).
          copy_list([], []).
          copy_list([X | In], [X? | Out]) :- copy_list(In?, Out).

          procedure copy_everylist(EveryList?, EveryList).
          copy_everylist([], []).
          copy_everylist([X | In], [X? | Out]) :- copy_everylist(In?, Out).
          copy_everylist([X? | In], [X | Out]) :- copy_everylist(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason:
                'List (Any elements) simpler than EveryList (Every elements)');
      });
    });
  });
}
