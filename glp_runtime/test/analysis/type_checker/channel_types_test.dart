// test/analysis/type_checker/channel_types_test.dart
//
// Tests for bidirectional channels and complementary modes

import 'package:test/test.dart';
import 'test_helpers.dart';

void main() {
  group('Channel Types', () {
    // =========================================================================
    // Channel creation and mode duality
    // =========================================================================

    group('Channel Creation', () {
      test('POSITIVE: create_channel with complementary modes', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

          procedure create_channel(Channel, Channel).
          create_channel(ch(AtoB?, BtoA), ch(BtoA?, AtoB)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Complementary channel endpoints are well-moded');
      });

      test('POSITIVE: channel sender uses correct modes', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

          procedure send(Channel?, Any?).
          send(ch(_, Out), Msg) :- Out = [Msg? | Rest?], send(ch(_, Rest?), done).
        ''');
        // This is a simplified sender - real one would be more complex
        expect(result.isWellTyped, isTrue);
      });
    });

    // =========================================================================
    // Bounded buffers with InvStream
    // =========================================================================

    group('Bounded Buffer', () {
      test('POSITIVE: bounded buffer consumes slots', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          InvStream ::= [] ; [_? | InvStream].

          procedure bounded_buffer(Stream?, InvStream?, Stream).
          bounded_buffer([], _, []).
          bounded_buffer([X | In], [Slot? | Slots], [X? | Out]) :-
              Slot = taken,
              bounded_buffer(In?, Slots?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Buffer fills slots (Slot?) while copying stream');
      });

      test('NEGATIVE: bounded buffer with wrong slot mode fails', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          InvStream ::= [] ; [_? | InvStream].

          procedure bounded_buffer(Stream?, InvStream?, Stream).
          bounded_buffer([], _, []).
          bounded_buffer([X | In], [Slot | Slots], [X? | Out]) :-
              handle(Slot?),
              bounded_buffer(In?, Slots?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Slot should be reader not writer');
      });
    });

    // =========================================================================
    // Channel type coverage
    // =========================================================================

    group('Channel Coverage', () {
      test('POSITIVE: handler covers both channel orientations', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

          procedure handle_channel(Channel?).
          handle_channel(ch(In?, Out)) :- process_read(In?), process_write(Out).
          handle_channel(ch(Out, In?)) :- process_write(Out), process_read(In?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both channel alternatives covered');
      });

      test('NEGATIVE: handler missing one channel orientation fails', () {
        final result = checkTypes('''
          List ::= [] ; [_ | List].
          Stream ::< List.
          Channel ::= ch(Stream?, Stream) ; ch(Stream, Stream?).

          procedure handle_channel(Channel?).
          handle_channel(ch(In?, Out)) :- process_read(In?), process_write(Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing ch(Stream, Stream?) case');
      });
    });
  });
}
