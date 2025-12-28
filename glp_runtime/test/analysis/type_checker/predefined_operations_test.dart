// test/analysis/type_checker/predefined_operations_test.dart
//
// Tests for predefined type operations: Any self-duality, DiffList, and Channel

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Predefined Operations', () {
    // =========================================================================
    // Self-duality of Any
    // =========================================================================

    group('Self-Duality', () {
      test('POSITIVE: Any and Any? are equivalent', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          procedure foo(MyAny, MyAny?).
          foo(X, X?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Any = Any? by self-duality');
      });

      test('POSITIVE: Any and Any? are equivalent (both modes)', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure foo(MyAny, MyAny?).
          foo(X, X?).
          foo(X?, X).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Any = Any? but still needs both modes covered');
      });

      test('POSITIVE: Writer at Any position is valid', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          procedure produce(MyAny).
          produce(X).
        ''');
        expect(result.isWellTyped, isTrue, reason: 'Any accepts writer');
      });

      test('POSITIVE: Reader at Any position is valid', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          procedure consume(MyAny).
          consume(X?).
        ''');
        expect(result.isWellTyped, isTrue, reason: 'Any accepts reader');
      });

      test('POSITIVE: List with Any elements needs only two clauses', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          procedure copy(MyList?, MyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Any has no coverage requirement');
      });

      test('NEGATIVE: Any needs both modes despite self-duality', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure echo(MyAny?, MyAny).
          echo(X, Y?) :- Y = X?.
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Any ::= requires both _ and _? covered');
      });

      test('POSITIVE: Any with both modes covered', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          procedure echo(MyAny?, MyAny).
          echo(X, Y?) :- Y = X?.
          echo(X?, Y) :- Y? = X.
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both modes covered for Any');
      });
    });

    // =========================================================================
    // DiffList operations
    // =========================================================================

    group('DiffList', () {
      test('POSITIVE: dl_append is well-moded', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A\\B?, B\\C?, A?\\C).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_append has correct mode annotations');
      });

      test('POSITIVE: dl_to_list is well-moded', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_to_list(MyDiffList?, MyList).
          my_dl_to_list(L\\[], L?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'dl_to_list closes the hole correctly');
      });

      test('NEGATIVE: dl_append with wrong modes fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A?\\B, B?\\C, A\\C?).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Modes are inverted incorrectly');
      });

      test('POSITIVE: dl_append demonstrates O(1) concatenation', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyDiffList ::= MyList \\ MyList?.

          procedure my_dl_append(MyDiffList?, MyDiffList?, MyDiffList).
          my_dl_append(A\\B?, B\\C?, A?\\C).

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
      test('POSITIVE: new_channel is well-moded', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_new_channel(MyChannel, MyChannel).
          my_new_channel(ch(Xs?, Ys), ch(Ys?, Xs)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'new_channel swaps streams correctly');
      });

      test('POSITIVE: send is well-moded', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_send(MyAny, MyChannel?, MyChannel).
          my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'send adds message to output stream');
      });

      test('POSITIVE: receive is well-moded', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_receive(MyAny, MyChannel?, MyChannel).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive takes message from input stream');
      });

      test('NEGATIVE: send with wrong message mode fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_send(MyAny, MyChannel?, MyChannel).
          my_send(X?, ch(In, [X|Out?]), ch(In?, Out)).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'X should be writer in first arg, reader in stream');
      });

      test('POSITIVE: Producer-consumer pattern', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_send(MyAny, MyChannel?, MyChannel).
          my_send(X, ch(In, [X?|Out?]), ch(In?, Out)).

          procedure my_receive(MyAny, MyChannel?, MyChannel).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure producer(MyChannel?).
          producer(Ch) :-
              my_send(hello, Ch?, Ch2),
              my_send(world, Ch2?, Ch3),
              done(Ch3?).

          procedure consumer(MyChannel?).
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
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
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
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
          MyStream ::= [MyAny | MyStream].
          MyChannel ::= ch(MyStream?, MyStream).

          procedure my_receive(MyAny, MyChannel?, MyChannel).
          my_receive(X?, ch([X|In], Out?), ch(In?, Out)).

          procedure handler(MyChannel?).
          handler(Ch) :- my_receive(Msg, Ch?, Ch2) |
              process(Msg?),
              handler(Ch2?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'receive in guard position suspends until message');
      });

      test('POSITIVE: dl_to_list in guard for closing difference list', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          MyAny ::= _ ; _?.
          MyList ::= [MyAny | MyList] ; [].
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
    // AnyList theoretical example (requires both element modes covered)
    // =========================================================================

    group('AnyList', () {
      test('POSITIVE: AnyList copy with three clauses', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          AnyList ::= [MyAny | AnyList] ; [].

          procedure copy(AnyList?, AnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
          copy([X? | In], [X | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'AnyList requires both element mode alternatives');
      });

      test('NEGATIVE: AnyList copy with only two clauses fails', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          AnyList ::= [MyAny | AnyList] ; [].

          procedure copy(AnyList?, AnyList).
          copy([], []).
          copy([X | In], [X? | Out]) :- copy(In?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing second element mode');
      });

      test('POSITIVE: List (with Any) needs both modes vs SimpleList one', () {
        final result = checkTypes('''
          MyAny ::= _ ; _?.
          AnyList ::= [MyAny | AnyList] ; [].
          SimpleList ::= [_ | SimpleList] ; [].

          procedure copy_anylist(AnyList?, AnyList).
          copy_anylist([], []).
          copy_anylist([X | In], [X? | Out]) :- copy_anylist(In?, Out).
          copy_anylist([X? | In], [X | Out]) :- copy_anylist(In?, Out).

          procedure copy_simplelist(SimpleList?, SimpleList).
          copy_simplelist([], []).
          copy_simplelist([X | In], [X? | Out]) :- copy_simplelist(In?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason:
                'SimpleList (output only elements) simpler than AnyList (both modes)');
      });
    });
  });
}
