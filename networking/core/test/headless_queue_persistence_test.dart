import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'helpers/event_waits.dart';

/// Fair delivery across the termination of the application (GNA §2.10,
/// Networking Assumptions: "Messages to temporarily-unreachable peers are
/// queued and delivered when connectivity is restored; the queue survives the
/// termination of the application"), in the headless profile, whose queue is
/// kept in a file its embedder names.
///
/// A message `send` queues is saved before `send` returns and removed once
/// acknowledged; the saved queue is read back when a layer with the same
/// identity and store is instantiated, and the heartbeat replays it.  A
/// duplicate after a restart is allowed (at-least-once, GNA §2.9).
///
/// Needs a native libsodium, as `headless_e2e_test.dart` does; self-skips
/// when absent.
void main() {
  SodiumSumo? sodium;
  String? sodiumUnavailable;

  setUpAll(() async {
    try {
      sodium = await initHeadlessSodium();
    } on Object catch (e) {
      sodiumUnavailable = 'libsodium unavailable, skipping: $e';
    }
  });

  bool skip() {
    if (sodiumUnavailable == null) return false;
    markTestSkipped(sodiumUnavailable!);
    return true;
  }

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  Future<Directory> tempDir() async {
    final dir = await Directory.systemTemp.createTemp('gna-queue');
    addTearDown(() => dir.delete(recursive: true));
    return dir;
  }

  /// A UDP port nothing listens on, until the test binds it.
  Future<int> freePort() async {
    final socket =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = socket.port;
    socket.close();
    return port;
  }

  test(
      'a message queued to an unreachable peer survives the layer, and is '
      'delivered by a new layer with the same identity and store once the '
      'peer is reachable', () async {
    if (skip()) return;
    final dir = await tempDir();
    final queueFile = '${dir.path}/queue.json';
    final knownPeersFile = '${dir.path}/peers.json';
    final senderId = await identityFromSeed(0x61);
    final recipientId = await identityFromSeed(0x62);
    final senderHex = _hex(senderId.publicKey);
    final port = await freePort();
    final recipientAddress = '127.0.0.1:$port';

    HeadlessGrassrootsNetwork senderLayer(OutboundQueueStore queue) =>
        HeadlessGrassrootsNetwork(
          identity: senderId,
          sodium: sodium!,
          knownPeersStore: FileKnownPeersStore(knownPeersFile),
          outboundQueueStore: queue,
        );

    // The first layer: the recipient's address is known, and nothing
    // listens there.
    final first = senderLayer(FileOutboundQueueStore(queueFile));
    expect(await first.start(), isTrue);
    first.wan.putPeerAddress(recipientId.publicKey, recipientAddress);
    final messageId = await first.send(
      recipientId.publicKey,
      Uint8List.fromList(utf8.encode('queued before termination')),
    );
    expect(messageId, isNotNull);
    final saved = await FileOutboundQueueStore(queueFile).load(senderHex);
    expect(saved.map((e) => e.messageId), [messageId],
        reason: 'the message is saved before send returns');
    expect(utf8.decode(saved.single.payload), 'queued before termination');
    expect(saved.single.recipientHex, _hex(recipientId.publicKey));
    await first.dispose();

    // The recipient comes up at that address, knowing the sender.
    final recipient = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: recipientId,
      sodium: sodium!,
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: recipientAddress),
    );
    addTearDown(recipient.dispose);
    final received = <(String, String, String)>[];
    recipient.onMessageReceived = (sender, payload, id, underlay) =>
        received.add((_hex(sender), utf8.decode(payload), id));
    recipient.putKnownPeer(senderId.publicKey);
    expect(await recipient.start(), isTrue);

    // A new layer with the same identity and stores: the heartbeat replays
    // the queue, dialing the recipient at its recorded address.
    final secondQueue = WatchedQueueStore(FileOutboundQueueStore(queueFile));
    final second = senderLayer(secondQueue);
    addTearDown(second.dispose);
    expect(await second.start(), isTrue);

    // The restored queue is delivered once the peer is reachable.
    await until([recipient], () => received.isNotEmpty);
    expect(received.first,
        (senderHex, 'queued before termination', _deliveryId(senderId, messageId!)));
    expect(
        received.every((r) => r.$2 == 'queued before termination'), isTrue,
        reason: 'a duplicate is allowed; nothing else is delivered');

    // Removed once acknowledged: the save that removes it, then the file.
    await secondQueue.untilSaved(senderHex, (entries) => entries.isEmpty);
    final left = await FileOutboundQueueStore(queueFile).load(senderHex);
    expect(left, isEmpty,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: Timeout.none);

  test('a message delivered while the peer is reachable is saved until its '
      'acknowledgment, and removed then', () async {
    if (skip()) return;
    final store = WatchedQueueStore(MemoryOutboundQueueStore());
    final senderId = await identityFromSeed(0x63);
    final recipientId = await identityFromSeed(0x64);
    final sender = HeadlessGrassrootsNetwork(
      identity: senderId,
      sodium: sodium!,
      outboundQueueStore: store,
    );
    final recipient = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: recipientId,
      sodium: sodium!,
    );
    addTearDown(() async {
      await sender.dispose();
      await recipient.dispose();
    });
    final received = <String>[];
    recipient.onMessageReceived =
        (s, payload, id, u) => received.add(utf8.decode(payload));
    recipient.putKnownPeer(senderId.publicKey);
    expect(await recipient.start(), isTrue);
    expect(await sender.start(), isTrue);
    sender.wan.putPeerAddress(
        recipientId.publicKey, '127.0.0.1:${recipient.boundPort}');

    final id = await sender.send(
        recipientId.publicKey, Uint8List.fromList(utf8.encode('one')));
    expect((await store.load(_hex(senderId.publicKey))).map((e) => e.messageId),
        [id],
        reason: 'the message is saved before send returns');
    await until([recipient], () => received.contains('one'));
    await store.untilSaved(
        _hex(senderId.publicKey), (entries) => entries.isEmpty);
    expect(await store.load(_hex(senderId.publicKey)), isEmpty,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: Timeout.none);

  test(
      'a message delivered and not acknowledged is delivered again after a '
      'restart of the layer, and one acknowledged is not (GNA §2.9, §2.11)',
      () async {
    if (skip()) return;
    final dir = await tempDir();
    final queueFile = '${dir.path}/recipient.json';
    final senderId = await identityFromSeed(0x67);
    final recipientId = await identityFromSeed(0x68);
    final recipientHex = _hex(recipientId.publicKey);

    HeadlessGrassrootsNetwork recipientLayer() => HeadlessGrassrootsNetwork(
          identity: recipientId,
          sodium: sodium!,
          outboundQueueStore: FileOutboundQueueStore(queueFile),
        );

    final sender = HeadlessGrassrootsNetwork(
      identity: senderId,
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    final first = recipientLayer();
    addTearDown(sender.dispose);
    final both = Completer<Map<String, String>>();
    final received = <String, String>{};
    first.onMessageReceived = (s, payload, id, u) {
      received[utf8.decode(payload)] = id;
      if (received.length == 2 && !both.isCompleted) both.complete(received);
    };
    first.putKnownPeer(senderId.publicKey);
    expect(await first.start(), isTrue);
    expect(await sender.start(), isTrue);
    sender.wan.putPeerAddress(
        recipientId.publicKey, '127.0.0.1:${first.boundPort}');

    final keptId = await sender.send(
        recipientId.publicKey, Uint8List.fromList(utf8.encode('kept')));
    final ackedId = await sender.send(
        recipientId.publicKey, Uint8List.fromList(utf8.encode('acked')));
    final ids = await both.future;
    final keptDelivery = _deliveryId(senderId, keptId!);
    final ackedDelivery = _deliveryId(senderId, ackedId!);
    expect(ids, {'kept': keptDelivery, 'acked': ackedDelivery},
        reason: 'each is delivered under the layer-assigned id of its sender '
            'and wire id');
    expect(
        (await FileOutboundQueueStore(queueFile).loadDelivered(recipientHex))
            .map((e) => e.messageId)
            .toSet(),
        {keptDelivery, ackedDelivery},
        reason: 'a message is kept before it is delivered');

    // The runtime acknowledges one of the two; then the layer terminates.
    await first.acknowledge(ackedDelivery);
    await first.dispose();
    final kept =
        await FileOutboundQueueStore(queueFile).loadDelivered(recipientHex);
    expect(kept.map((e) => e.messageId), [keptDelivery],
        reason: 'an acknowledged message is kept no longer');
    expect(kept.single.senderHex, _hex(senderId.publicKey));
    expect(kept.single.underlay, Underlay.wan);

    // The restart: a layer with the same identity and store delivers the
    // unacknowledged message again, as it was delivered, and nothing else.
    final second = recipientLayer();
    addTearDown(second.dispose);
    final again = <(String, String, String, Underlay)>[];
    final firstAgain = Completer<void>();
    second.onMessageReceived = (s, payload, id, u) {
      again.add((_hex(s), utf8.decode(payload), id, u));
      if (!firstAgain.isCompleted) firstAgain.complete();
    };
    // The kept messages are delivered again in one turn of the event loop,
    // which ends before the wait on the first of them resumes.
    await firstAgain.future;
    expect(again,
        [(_hex(senderId.publicKey), 'kept', keptDelivery, Underlay.wan)]);

    // Acknowledged now, it is not delivered at the next restart.
    await second.acknowledge(keptDelivery);
    await second.dispose();
    expect(
        await FileOutboundQueueStore(queueFile).loadDelivered(recipientHex),
        isEmpty);
  }, timeout: Timeout.none);

  test(
      'a layer constructed without a store of its queue is refused: the '
      'store is a required argument, and each embedder names its file',
      () async {
    if (skip()) return;
    final id = await identityFromSeed(0x66);
    expect(
        () => Function.apply(HeadlessGrassrootsNetwork.new, const [], {
              #identity: id,
              #sodium: sodium!,
            }),
        throwsNoSuchMethodError,
        reason: 'no layer is made whose queue the termination of the '
            'application would lose (GNA §2.11)');
    final layer = Function.apply(HeadlessGrassrootsNetwork.new, const [], {
      #identity: id,
      #sodium: sodium!,
      #outboundQueueStore: MemoryOutboundQueueStore(),
    }) as HeadlessGrassrootsNetwork;
    await layer.dispose();
  });

  group('FileOutboundQueueStore', () {
    final owner = 'ab' * 32;
    final recipient = 'cd' * 32;

    test('round-trips a queue in order, and an empty one', () async {
      final dir = await tempDir();
      final store = FileOutboundQueueStore('${dir.path}/q.json');
      expect(await store.load(owner), isEmpty);
      final entries = [
        for (var i = 0; i < 3; i++)
          OutboundQueueEntry(
            recipientHex: recipient,
            messageId: 'm$i',
            payload: Uint8List.fromList([i, 255, 0]),
          ),
      ];
      await store.save(owner, entries);
      final loaded =
          await FileOutboundQueueStore('${dir.path}/q.json').load(owner);
      expect(loaded.map((e) => e.messageId), ['m0', 'm1', 'm2']);
      expect(loaded.map((e) => e.payload), [
        [0, 255, 0],
        [1, 255, 0],
        [2, 255, 0],
      ]);
      await store.save(owner, const []);
      expect(await store.load(owner), isEmpty);
    });

    test('keeps the delivered messages beside the sent ones, each part '
        'saved without the other changing', () async {
      final dir = await tempDir();
      final store = FileOutboundQueueStore('${dir.path}/q.json');
      expect(await store.loadDelivered(owner), isEmpty);
      final sent = OutboundQueueEntry(
          recipientHex: recipient,
          messageId: 's',
          payload: Uint8List.fromList([1]));
      final delivered = DeliveredQueueEntry(
          senderHex: recipient,
          messageId: 'd',
          payload: Uint8List.fromList([2, 3]),
          underlay: Underlay.lan);
      await store.save(owner, [sent]);
      await store.saveDelivered(owner, [delivered]);
      final reread = FileOutboundQueueStore('${dir.path}/q.json');
      expect((await reread.load(owner)).map((e) => e.messageId), ['s']);
      final loaded = await reread.loadDelivered(owner);
      expect(loaded.map((e) => (e.senderHex, e.messageId, e.underlay)),
          [(recipient, 'd', Underlay.lan)]);
      expect(loaded.single.payload, [2, 3]);
      await store.save(owner, const []);
      expect((await reread.loadDelivered(owner)).map((e) => e.messageId),
          ['d']);
      await store.saveDelivered(owner, const []);
      expect(await reread.loadDelivered(owner), isEmpty);
    });

    test("refuses another agent's queue", () async {
      final dir = await tempDir();
      final store = FileOutboundQueueStore('${dir.path}/q.json');
      await store.save(owner, [
        OutboundQueueEntry(
            recipientHex: recipient,
            messageId: 'm',
            payload: Uint8List.fromList([1])),
      ]);
      expect(() => store.load('ef' * 32), throwsStateError);
      expect(() => store.save('ef' * 32, const []), throwsStateError);
    });

    test('refuses a malformed queue', () async {
      final dir = await tempDir();
      final path = '${dir.path}/q.json';
      File(path).writeAsStringSync(jsonEncode({
        'owner': owner,
        'messages': [
          {'recipient': recipient, 'messageId': 'm'},
        ],
        'delivered': [],
      }));
      expect(() => FileOutboundQueueStore(path).load(owner),
          throwsFormatException);
      // A queue without its delivered messages is malformed.
      File(path).writeAsStringSync(jsonEncode({
        'owner': owner,
        'messages': [],
      }));
      expect(() => FileOutboundQueueStore(path).load(owner),
          throwsFormatException);
      // And a kept message without its underlay.
      File(path).writeAsStringSync(jsonEncode({
        'owner': owner,
        'messages': [],
        'delivered': [
          {'sender': recipient, 'messageId': 'd', 'payload': 'AQ=='},
        ],
      }));
      expect(() => FileOutboundQueueStore(path).loadDelivered(owner),
          throwsFormatException);
    });

    test('a layer whose queue cannot be read refuses to start and to send',
        () async {
      if (skip()) return;
      final dir = await tempDir();
      final path = '${dir.path}/q.json';
      File(path).writeAsStringSync('not json');
      final id = await identityFromSeed(0x65);
      final layer = HeadlessGrassrootsNetwork(
        identity: id,
        sodium: sodium!,
        outboundQueueStore: FileOutboundQueueStore(path),
      );
      addTearDown(layer.dispose);
      await expectLater(layer.start(), throwsFormatException);
      await expectLater(
          layer.send(Uint8List(32), Uint8List.fromList([1])),
          throwsFormatException);
    });
  });
}

/// The id a message from [sender] with wire id [wireId] is delivered under.
String _deliveryId(GrassrootsIdentity sender, String wireId) =>
    MessageRouter.deliveryIdOf(sender.publicKey, wireId);

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
