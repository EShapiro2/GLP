import 'dart:async';
import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/protocol/message_transport.dart';

/// Spec §Message Transport / §Networking Assumptions (Unordered delivery):
/// "No order is kept or promised across messages; a lost fragment delays
/// only its own message." This drives the sender with two concurrent large
/// messages to one peer, injects loss into the first message's fragments,
/// and asserts the second message completes before the first — the lost
/// fragment must not head-of-line-block the other message.
void main() {
  final peer = 'aa' * 32;
  const maxChunk = 100;

  test('a lost fragment delays only its own message; the other completes '
      'first', () async {
    // A fake carrier: it "receives" every fragment sent and would ack it —
    // except fragments of [lossyMessageId] are dropped until [healAfter]
    // wall-clock attempts, so that message stalls while the other flows.
    late MessageTransportSender sender;
    String? lossyMessageId;
    final dropCounts = <String, int>{};
    const dropFirstNAttempts = 3;

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
      maxAttempts: 50,
      sendPacket: (peerHex, packet) async {
        expect(peerHex, peer);
        final fragment = FragmentHandler.decodeFragment(packet.payload);
        final id = fragment.messageId;
        if (id == lossyMessageId) {
          final dropped = dropCounts.update(
              '$id:${fragment.index}', (n) => n + 1,
              ifAbsent: () => 1);
          // Drop this fragment's first few transmissions — simulated loss.
          if (dropped <= dropFirstNAttempts) return true; // "sent", but lost
        }
        ackFragment(id, fragment.index);
        return true;
      },
    );
    addTearDown(sender.dispose);

    // Two ~2 KB messages: ~21 fragments each at 100-byte chunks.
    Uint8List body(int seed) =>
        Uint8List.fromList(List.generate(2000, (i) => (i * seed) % 256));

    const idA = '00000000-0000-4000-8000-00000000000a'; // lossy
    const idB = '00000000-0000-4000-8000-00000000000b'; // clean
    lossyMessageId = idA;

    final aDone = Completer<DateTime>();
    final bDone = Completer<DateTime>();

    unawaited(sender.sendMessage(peer, idA, body(3)).then((ok) {
      expect(ok, isTrue, reason: 'message A must still eventually complete');
      aDone.complete(DateTime.now());
    }));
    unawaited(sender.sendMessage(peer, idB, body(5)).then((ok) {
      expect(ok, isTrue);
      bDone.complete(DateTime.now());
    }));

    final bAt = await bDone.future.timeout(const Duration(seconds: 10));
    // B finished; A should still be retransmitting its dropped fragments.
    expect(aDone.isCompleted, isFalse,
        reason: 'the clean message must complete before the lossy one, '
            'proving no cross-message head-of-line blocking');

    final aAt = await aDone.future.timeout(const Duration(seconds: 10));
    expect(aAt.isAfter(bAt) || aAt.isAtSameMomentAs(bAt), isTrue);
    // The lossy fragments really were dropped and retransmitted.
    expect(dropCounts.values.any((n) => n > 1), isTrue);
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
}
