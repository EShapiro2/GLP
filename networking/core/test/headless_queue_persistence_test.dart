import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

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

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    return predicate();
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

    HeadlessGrassrootsNetwork senderLayer() => HeadlessGrassrootsNetwork(
          identity: senderId,
          rendezvousServers: const [],
          sodium: sodium!,
          knownPeersStore: FileKnownPeersStore(knownPeersFile),
          outboundQueueStore: FileOutboundQueueStore(queueFile),
        );

    // The first layer: the recipient's address is known, and nothing
    // listens there.
    final first = senderLayer();
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
      identity: recipientId,
      rendezvousServers: const [],
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
    final second = senderLayer();
    addTearDown(second.dispose);
    expect(await second.start(), isTrue);

    expect(await waitFor(() => received.isNotEmpty, seconds: 45), isTrue,
        reason: 'the restored queue is delivered once the peer is reachable');
    expect(received.first,
        (senderHex, 'queued before termination', messageId));
    expect(
        received.every((r) => r.$2 == 'queued before termination'), isTrue,
        reason: 'a duplicate is allowed; nothing else is delivered');

    // Removed once acknowledged.
    final stopwatch = Stopwatch()..start();
    var left = await FileOutboundQueueStore(queueFile).load(senderHex);
    while (left.isNotEmpty && stopwatch.elapsed < const Duration(seconds: 15)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
      left = await FileOutboundQueueStore(queueFile).load(senderHex);
    }
    expect(left, isEmpty,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('a message delivered while the peer is reachable is saved until its '
      'acknowledgment, and removed then', () async {
    if (skip()) return;
    final store = MemoryOutboundQueueStore();
    final senderId = await identityFromSeed(0x63);
    final recipientId = await identityFromSeed(0x64);
    final sender = HeadlessGrassrootsNetwork(
      identity: senderId,
      rendezvousServers: const [],
      sodium: sodium!,
      outboundQueueStore: store,
    );
    final recipient = HeadlessGrassrootsNetwork(
      identity: recipientId,
      rendezvousServers: const [],
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
    expect(await waitFor(() => received.contains('one')), isTrue);
    expect(
        await _eventually(
            () async => (await store.load(_hex(senderId.publicKey))).isEmpty),
        isTrue,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: const Timeout(Duration(minutes: 1)));

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
      }));
      expect(() => FileOutboundQueueStore(path).load(owner),
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
        rendezvousServers: const [],
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

Future<bool> _eventually(Future<bool> Function() predicate,
    {int seconds = 15}) async {
  final clock = Stopwatch()..start();
  while (clock.elapsed < Duration(seconds: seconds)) {
    if (await predicate()) return true;
    await Future<void>.delayed(const Duration(milliseconds: 100));
  }
  return predicate();
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
