import 'dart:async';
import 'dart:typed_data';

import 'package:test/test.dart';

import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/protocol/fragment_handler.dart';
import 'package:grassroots_networking_core/src/protocol/message_transport.dart';
import 'package:grassroots_networking_core/src/routing/message_router.dart';

/// F07: a received message is identified by its sender and the sender's wire
/// messageId together, never by the wire messageId alone (spec api.tex:188-189,
/// GNA §2.9). Two senders may use the same wire id; their fragments must not
/// merge, their deliveries must not share an id, and a fragment
/// acknowledgment from one peer must not settle a message sent to another.
void main() {
  const maxChunk = 100;
  const wireId = '00000000-0000-4000-8000-0000000000f7';
  final alice = 'aa' * 32;
  final bob = 'bb' * 32;

  test('fragments of two senders with the same wire id do not merge', () {
    final handler = FragmentHandler();
    addTearDown(handler.dispose);
    final aliceMsg = Uint8List.fromList(List.filled(150, 1));
    final bobMsg = Uint8List.fromList(List.filled(150, 2));
    final aliceFrags = handler.fragment(
        payload: aliceMsg, messageId: wireId, maxChunk: maxChunk);
    final bobFrags = handler.fragment(
        payload: bobMsg, messageId: wireId, maxChunk: maxChunk);
    Fragment decode(GrassrootsPacket p) =>
        FragmentHandler.decodeFragment(p.payload);

    // One fragment from each sender, then the second fragment of each: neither
    // message is complete until its own two fragments have arrived.
    expect(
        handler.addFragment(decode(aliceFrags.fragments[0]), senderHex: alice),
        isNull);
    expect(handler.addFragment(decode(bobFrags.fragments[1]), senderHex: bob),
        isNull);
    final aliceDone =
        handler.addFragment(decode(aliceFrags.fragments[1]), senderHex: alice);
    expect(aliceDone, isNotNull);
    expect(aliceDone!.payload, aliceMsg);
    final bobDone =
        handler.addFragment(decode(bobFrags.fragments[0]), senderHex: bob);
    expect(bobDone, isNotNull);
    expect(bobDone!.payload, bobMsg);
  });

  test('the delivery id is layer-assigned, stable per sender and id', () {
    final pk = Uint8List.fromList(List.generate(32, (i) => i));
    final other = Uint8List.fromList(List.generate(32, (i) => 255 - i));
    final id = MessageRouter.deliveryIdOf(pk, wireId);

    expect(id, matches(RegExp(r'^[0-9a-f]{32}$')));
    expect(id, isNot(wireId));
    // A re-send of the same message from the same sender maps to the same id.
    expect(MessageRouter.deliveryIdOf(pk, wireId), id);
    // Two senders using the same wire id get different delivery ids.
    expect(MessageRouter.deliveryIdOf(other, wireId), isNot(id));
    // The same sender's different message gets a different id.
    expect(
        MessageRouter.deliveryIdOf(pk, '00000000-0000-4000-8000-0000000000f8'),
        isNot(id));
  });

  test(
      'fragment acknowledgments from a peer other than the recipient settle '
      'nothing', () async {
    late MessageTransportSender sender;
    sender = MessageTransportSender(
      maxChunk: maxChunk,
      initialBackoff: const Duration(seconds: 30), // never fires in the test
      sendPacket: (peerHex, packet) async => true, // nothing acks fragments
    );
    addTearDown(sender.dispose);

    // 500 bytes in fragments of 100, all in flight within the window.
    const fragments = 5;
    final done = sender.sendMessage(
        alice, wireId, Uint8List.fromList(List.filled(500, 1)));
    var settled = false;
    unawaited(done.then((_) => settled = true));

    // The wrong peer acknowledges every fragment: the message stays
    // outstanding on the recipient's lane.
    for (var i = 0; i < fragments; i++) {
      sender.handleFragmentAck(bob, wireId, i);
    }
    await Future<void>.value(); // flush microtasks; nothing has settled
    expect(settled, isFalse);
    expect(sender.hasOutstanding(alice), isTrue);

    // The recipient acknowledges every fragment: it settles.
    for (var i = 0; i < fragments; i++) {
      sender.handleFragmentAck(alice, wireId, i);
    }
    expect(await done, isTrue);
  }, timeout: Timeout.none);
}
