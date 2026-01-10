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
      // Note: Type aliasing (MyStream ::= MyList) is not allowed.
      // Use base types directly in type definitions.

      test('POSITIVE: create_channel with complementary modes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyChannel ::= ch(MyList?, MyList) ; ch(MyList, MyList?).

          procedure create_channel(MyChannel, MyChannel).
          create_channel(ch(AtoB?, BtoA), ch(BtoA?, AtoB)).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Complementary channel endpoints are well-moded');
      }, skip: 'Nested type mode handling not yet implemented for compound types like ch(MyList?, MyList)');

      test('POSITIVE: channel sender uses correct modes', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyChannel ::= ch(MyList?, MyList) ; ch(MyList, MyList?).

          procedure my_send(MyChannel?, _?).
          my_send(ch(_, Out), Msg) :- Out = [Msg? | Rest?], my_send(ch(_, Rest?), done).
        ''');
        // This is a simplified sender - real one would be more complex
        expect(result.isWellTyped, isTrue);
      }, skip: 'Nested type mode handling not yet implemented for compound types like ch(MyList?, MyList)');
    });

    // =========================================================================
    // Bounded buffers with InvStream
    // =========================================================================

    group('Bounded Buffer', () {
      test('POSITIVE: bounded buffer consumes slots', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyInvStream ::= [] ; [_? | MyInvStream].

          procedure bounded_buffer(MyList?, MyInvStream?, MyList).
          bounded_buffer([], _, []).
          bounded_buffer([X | In], [Slot? | Slots], [X? | Out]) :-
              Slot = taken,
              bounded_buffer(In?, Slots?, Out).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Buffer fills slots (Slot?) while copying stream');
      }, skip: 'Complex nested mode inference not yet implemented');

      test('NEGATIVE: bounded buffer with wrong slot mode fails', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyInvStream ::= [] ; [_? | MyInvStream].

          procedure handle(_?).
          procedure bounded_buffer(MyList?, MyInvStream?, MyList).
          bounded_buffer([], _, []).
          bounded_buffer([X | In], [Slot | Slots], [X? | Out]) :-
              handle(Slot?),
              bounded_buffer(In?, Slots?, Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Slot should be reader not writer');
      }, skip: 'Complex nested mode inference not yet implemented');
    });

    // =========================================================================
    // Channel type coverage
    // =========================================================================

    group('Channel Coverage', () {
      test('POSITIVE: handler covers both channel orientations', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyChannel ::= ch(MyList?, MyList) ; ch(MyList, MyList?).

          procedure process_read(MyList?).
          procedure process_write(MyList).
          procedure handle_channel(MyChannel?).
          handle_channel(ch(In?, Out)) :- process_read(In?), process_write(Out).
          handle_channel(ch(Out, In?)) :- process_write(Out), process_read(In?).
        ''');
        expect(result.isWellTyped, isTrue,
            reason: 'Both channel alternatives covered');
      }, skip: 'Nested type mode handling not yet implemented for compound types like ch(MyList?, MyList)');

      test('NEGATIVE: handler missing one channel orientation fails', () {
        final result = checkTypes('''
          MyList ::= [] ; [_ | MyList].
          MyChannel ::= ch(MyList?, MyList) ; ch(MyList, MyList?).

          procedure process_read(MyList?).
          procedure process_write(MyList).
          procedure handle_channel(MyChannel?).
          handle_channel(ch(In?, Out)) :- process_read(In?), process_write(Out).
        ''');
        expect(result.isWellTyped, isFalse,
            reason: 'Missing ch(MyList, MyList?) case');
      }, skip: 'Subtype fixpoint semantics not yet implemented');
    });
  });
}
