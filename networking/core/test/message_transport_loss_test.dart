import 'dart:async';
import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/protocol/message_transport.dart';

/// Spec §Message Transport / §Networking Assumptions (Unordered delivery):
/// "No order is kept or promised across messages; a lost fragment delays
/// only its own message." This drives the sender with two concurrent large
/// messages to one peer and loses a fragment of the first.
///
/// ONE fragment of A is lost — index 0, dropped until the clean message has
/// completed — and the assertion is that B completes anyway. There is no
/// clock in it: A cannot finish, because the fragment it is missing is never
/// acknowledged until after the assertion, and B either completes or the test
/// times out. An earlier version healed A after three attempts and asserted
/// that B won the resulting race; under load A sometimes won, and a test that
/// fails once in nine is worse than no test.
///
/// Losing one fragment rather than all of them is deliberate, and it is what
/// the specification says: "a LOST FRAGMENT delays only its own message". A
/// message whose every fragment is stalled is a different claim, and a weaker
/// one holds for it — see the note on window occupancy below.
void main() {
  final peer = 'aa' * 32;
  const maxChunk = 100;

  test('a lost fragment does not delay another message to the same peer',
      () async {
    // A fake carrier: it "receives" every fragment sent and acks it — except
    // fragment [lossyIndex] of [lossyMessageId], which is dropped until
    // [healed], so that message can never complete while the other flows.
    late MessageTransportSender sender;
    String? lossyMessageId;
    const lossyIndex = 0;
    var healed = false;
    final dropCounts = <String, int>{};

    // Deliver an ack back to the sender for a fragment that "arrived".
    void ackFragment(String messageId, int index) {
      // Hop off the send stack so the sender's in-flight bookkeeping for
      // this fragment is in place before its ack lands.
      Timer(const Duration(milliseconds: 5), () {
        sender.handleFragmentAck(peer, messageId, index);
      });
    }

    sender = MessageTransportSender(
      maxChunk: maxChunk,
      windowPerPeer: 4,
      initialBackoff: const Duration(milliseconds: 20),
      maxBackoff: const Duration(milliseconds: 40),
      // Generous: A is retransmitted for as long as B takes, and how long
      // that is depends on the machine. Exhausting the attempt limit is a
      // different behaviour and not what this test is about.
      maxAttempts: 100000,
      sendPacket: (peerHex, packet) async {
        expect(peerHex, peer);
        final fragment = FragmentHandler.decodeFragment(packet.payload);
        final id = fragment.messageId;
        if (id == lossyMessageId && fragment.index == lossyIndex && !healed) {
          dropCounts.update('$id:${fragment.index}', (n) => n + 1,
              ifAbsent: () => 1);
          return true; // "sent", but lost
        }
        ackFragment(id, fragment.index);
        return true;
      },
    );
    addTearDown(sender.dispose);

    // Two ~2 KB messages: ~21 fragments each at 100-byte chunks. Both are
    // larger than the 4-fragment per-peer window, so the window is contended
    // and round-robin across the two messages is what lets B through.
    //
    // The lost fragment holds one of the four slots for the whole test, and
    // B completes on the other three. That is the bound worth knowing: the
    // window is released on acknowledgment, so an unacknowledged fragment
    // occupies its slot, and a message with EVERY fragment stalled would take
    // the whole window and starve the other. What keeps that from mattering
    // in the layer is the attempt limit — 3 in the coordinator — which fails
    // such a message and releases its slots, not the round-robin.
    Uint8List body(int seed) =>
        Uint8List.fromList(List.generate(2000, (i) => (i * seed) % 256));

    const idA = '00000000-0000-4000-8000-00000000000a'; // stalled
    const idB = '00000000-0000-4000-8000-00000000000b'; // clean
    lossyMessageId = idA;

    var aCompleted = false;
    final aDone = Completer<void>();
    final bDone = Completer<void>();

    unawaited(sender.sendMessage(peer, idA, body(3)).then((ok) {
      expect(ok, isTrue, reason: 'message A must still eventually complete');
      aCompleted = true;
      aDone.complete();
    }));
    unawaited(sender.sendMessage(peer, idB, body(5)).then((ok) {
      expect(ok, isTrue);
      bDone.complete();
    }));

    // B runs to completion while A's fragment is still being dropped. If a
    // lost fragment could delay a message it does not belong to, this would
    // time out — that is the whole assertion, and it has no clock in it.
    await bDone.future.timeout(
      const Duration(seconds: 10),
      onTimeout: () => fail(
        'the clean message never completed while a fragment of the other was '
        'lost: a lost fragment delayed a message it does not belong to',
      ),
    );

    // A cannot have completed: the fragment it is missing has never been
    // acknowledged. Asserted before healing, so it is a fact and not a race.
    expect(aCompleted, isFalse);
    expect(dropCounts.keys, ['$idA:$lossyIndex'],
        reason: 'exactly one fragment was lost, and it was A\'s');
    expect(dropCounts.values.single, greaterThan(1),
        reason: 'the lost fragment must actually have been retransmitted');

    // Heal the carrier: A now completes, which is the other half of "delays
    // only its own message" — delayed, not failed.
    healed = true;
    await aDone.future.timeout(const Duration(seconds: 10));
    expect(aCompleted, isTrue);
  });

  test('a whole-message ack settles all outstanding fragments', () async {
    late MessageTransportSender sender;
    sender = MessageTransportSender(
      maxChunk: maxChunk,
      windowPerPeer: 4,
      initialBackoff: const Duration(seconds: 30), // never fires in the test
      sendPacket: (peerHex, packet) async => true, // nothing acks fragments
    );
    addTearDown(sender.dispose);

    const id = '00000000-0000-4000-8000-00000000000c';
    final done =
        sender.sendMessage(peer, id, Uint8List.fromList(List.filled(500, 1)));
    // No per-fragment acks arrive; a delivery ACK settles the message.
    Timer(const Duration(milliseconds: 20),
        () => sender.handleMessageAck(id));
    expect(await done.timeout(const Duration(seconds: 5)), isTrue);
  });

  test('abandonPeer fails outstanding messages so fair delivery re-queues',
      () async {
    late MessageTransportSender sender;
    sender = MessageTransportSender(
      maxChunk: maxChunk,
      initialBackoff: const Duration(seconds: 30),
      sendPacket: (peerHex, packet) async => true,
    );
    addTearDown(sender.dispose);
    const id = '00000000-0000-4000-8000-00000000000d';
    final done =
        sender.sendMessage(peer, id, Uint8List.fromList(List.filled(500, 1)));
    sender.abandonPeer(peer);
    expect(await done.timeout(const Duration(seconds: 5)), isFalse);
  });

  group('backoff schedule', () {
    // The exponential backoff used to be `initialBackoff * (1 << (attempts -
    // 1))`. At attempts 63 the shift is large enough that the Duration
    // multiplication wraps to ZERO, so every retransmit timer fired
    // immediately: a hot loop flooding the carrier, and invisible as anything
    // but a spinning CPU. The attempt limits in use (3 in the coordinator, 8
    // by default) never reached it, so raising either would have found it.
    final sender = MessageTransportSender(
      maxChunk: maxChunk,
      initialBackoff: const Duration(milliseconds: 20),
      maxBackoff: const Duration(milliseconds: 40),
      sendPacket: (peerHex, packet) async => true,
    );

    test('doubles from the initial delay and clamps at the maximum', () {
      expect(sender.backoffForAttempt(1), const Duration(milliseconds: 20));
      expect(sender.backoffForAttempt(2), const Duration(milliseconds: 40));
      expect(sender.backoffForAttempt(3), const Duration(milliseconds: 40));
    });

    test('never collapses to zero, at any attempt count', () {
      for (final attempts in [1, 2, 3, 10, 62, 63, 64, 65, 100, 100000]) {
        final delay = sender.backoffForAttempt(attempts);
        expect(delay, greaterThan(Duration.zero),
            reason: 'attempt $attempts backed off by nothing at all');
        expect(delay, lessThanOrEqualTo(const Duration(milliseconds: 40)),
            reason: 'attempt $attempts exceeded maxBackoff');
      }
    });
  });
}
