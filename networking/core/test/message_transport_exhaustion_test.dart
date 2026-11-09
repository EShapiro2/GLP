import 'dart:async';
import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/protocol/message_transport.dart';

/// Spec §Message Protocol: "When a message's attempts are exhausted the
/// session is torn down and the message queues for the next path." The sender
/// signals exhaustion, and only exhaustion, so the layer can tear the session
/// down; the message's failure is still reported to its caller as `false`.
void main() {
  final peer = 'bb' * 32;
  final payload = Uint8List.fromList([1, 2, 3]);
  const _id1 = '00000000-0000-4000-8000-000000000001';
  const _id2 = '00000000-0000-4000-8000-000000000002';
  const _id3 = '00000000-0000-4000-8000-000000000003';

  test('a fragment that is never acknowledged exhausts its message: every '
      'message to the peer fails, and the peer is signalled once', () async {
    final signals = <String>[];
    final sender = MessageTransportSender(
      maxChunk: 100,
      initialBackoff: const Duration(milliseconds: 1),
      maxBackoff: const Duration(milliseconds: 2),
      maxAttempts: 3,
      // The carrier takes every packet and no acknowledgment ever comes.
      sendPacket: (peerHex, packet) async => true,
      onAttemptsExhausted: signals.add,
    );
    final first = sender.sendMessage(peer, _id1, payload);
    final second = sender.sendMessage(peer, _id2, payload);
    expect(await first, isFalse);
    expect(await second, isFalse,
        reason: 'the message that rode the same session fails with it');
    expect(signals, [peer], reason: 'signalled once, for the peer');
  }, timeout: Timeout.none);

  test('a message the carrier refuses on its first transmission fails without '
      'signalling exhaustion', () async {
    final signals = <String>[];
    final sender = MessageTransportSender(
      maxChunk: 100,
      maxAttempts: 3,
      // No session, no path: the carrier refuses every packet.
      sendPacket: (peerHex, packet) async => false,
      onAttemptsExhausted: signals.add,
    );
    expect(await sender.sendMessage(peer, _id2, payload), isFalse);
    expect(signals, isEmpty);
  }, timeout: Timeout.none);

  test('a peer abandoned with a message outstanding fails it without '
      'signalling exhaustion', () async {
    final signals = <String>[];
    final sent = Completer<void>();
    final sender = MessageTransportSender(
      maxChunk: 100,
      initialBackoff: const Duration(seconds: 60),
      maxBackoff: const Duration(seconds: 60),
      maxAttempts: 3,
      sendPacket: (peerHex, packet) async {
        if (!sent.isCompleted) sent.complete();
        return true;
      },
      onAttemptsExhausted: signals.add,
    );
    final delivered = sender.sendMessage(peer, _id3, payload);
    await sent.future;
    sender.abandonPeer(peer);
    expect(await delivered, isFalse);
    expect(signals, isEmpty);
  }, timeout: Timeout.none);
}
