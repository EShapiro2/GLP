import 'dart:convert';
import 'dart:io' show InternetAddress;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking/src/store/outbound_queue_preferences.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// Fair delivery across the termination of the application (GNA §2.10,
/// Networking Assumptions: "Messages to temporarily-unreachable peers are
/// queued and delivered when connectivity is restored; the queue survives the
/// termination of the application"), in the Flutter profile, whose queue is
/// kept in the application's SharedPreferences.
///
/// A message `send` queues is saved before `send` returns and removed once
/// acknowledged; the saved queue is read back when a layer with the same
/// identity and store is instantiated, and replayed once the peer is
/// reachable.  A duplicate after a restart is allowed (at-least-once, GNA
/// §2.9).  Full [GrassrootsNetwork] instances over loopback UDP.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    SharedPreferences.setMockInitialValues({});
    final messenger =
        TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;
    messenger.setMockMethodCallHandler(
      const MethodChannel('dev.fluttercommunity.plus/connectivity'),
      (call) async => ['wifi'],
    );
    messenger.setMockStreamHandler(
      const EventChannel('dev.fluttercommunity.plus/connectivity_status'),
      MockStreamHandler.inline(onListen: (arguments, events) {}),
    );
  });

  String hex(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  /// A layer with the default store of its queue, the application's
  /// SharedPreferences.
  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id) async {
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      rendezvousServers: const [],
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    expect(await network.initialize(), isTrue);
    return network;
  }

  Future<String> listeningAddress(GrassrootsNetwork network) async {
    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (network.debugLocalCandidates().isEmpty &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    return network.debugLocalCandidates().first;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 20}) async {
    final deadline = DateTime.now().add(Duration(seconds: seconds));
    while (!predicate() && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    return predicate();
  }

  Future<List<OutboundQueueEntry>> savedQueue(GrassrootsIdentity id) async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.reload();
    final text = prefs.getString(
        SharedPreferencesOutboundQueueStore.keyFor(hex(id.publicKey)));
    return text == null ? [] : decodeOutboundQueue(text);
  }

  test(
      'a message queued to an unreachable peer survives the layer, and is '
      'delivered by a new layer with the same identity and store once the '
      'peer is reachable', () async {
    final senderId = await identityFromSeed(0x91);
    final recipientId = await identityFromSeed(0x92);

    // The first layer: the recipient is unreachable.
    final first = await startNode(senderId);
    final messageId = await first.send(
      recipientId.publicKey,
      Uint8List.fromList(utf8.encode('queued before termination')),
    );
    expect(messageId, isNotNull);
    expect(first.queuedMessageCount, 1);
    final saved = await savedQueue(senderId);
    expect(saved.map((e) => e.messageId), [messageId],
        reason: 'the message is saved before send returns');
    expect(saved.single.recipientHex, hex(recipientId.publicKey));
    expect(utf8.decode(saved.single.payload), 'queued before termination');
    await first.dispose();

    // The recipient comes up, and a new layer with the same identity and
    // store reads the queue back.
    final recipient = await startNode(recipientId);
    final second = await startNode(senderId);
    addTearDown(() async {
      await second.dispose();
      await recipient.dispose();
    });
    final received = <(String, String, String)>[];
    recipient.onMessageReceived = (sender, payload, id, underlay) =>
        received.add((hex(sender), utf8.decode(payload), id));
    expect(second.queuedMessageCount, 1,
        reason: 'the saved queue is read back when the layer starts');
    expect(
        second.store.state.messages.queuedMessages.map((m) => m.messageId),
        [messageId]);

    // The peer becomes reachable: each knows the other, and the recipient
    // reconnects to its known peers.
    second.putKnownPeer(recipientId.publicKey);
    recipient.putPeerAddress(senderId.publicKey, await listeningAddress(second));
    await recipient.reconnectKnownPeers(reason: 'test');

    expect(await waitFor(() => received.isNotEmpty, seconds: 30), isTrue,
        reason: 'the restored queue is delivered once the peer is reachable');
    expect(received.first,
        (hex(senderId.publicKey), 'queued before termination', messageId));
    expect(
        received.every((r) => r.$2 == 'queued before termination'), isTrue,
        reason: 'a duplicate is allowed; nothing else is delivered');

    // Removed once acknowledged.
    final clock = Stopwatch()..start();
    var left = await savedQueue(senderId);
    while (left.isNotEmpty && clock.elapsed < const Duration(seconds: 15)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
      left = await savedQueue(senderId);
    }
    expect(left, isEmpty,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: const Timeout(Duration(minutes: 2)));

  test("each agent's queue is its own", () async {
    final aId = await identityFromSeed(0x93);
    final bId = await identityFromSeed(0x94);
    final elsewhere = await identityFromSeed(0x95);

    final a = await startNode(aId);
    await a.send(elsewhere.publicKey, Uint8List.fromList([1]));
    await a.dispose();

    final b = await startNode(bId);
    addTearDown(b.dispose);
    expect(b.queuedMessageCount, 0,
        reason: "another agent's queue is not replayed under this identity");
    expect((await savedQueue(aId)).length, 1);
  });

  test('the store of the queue can be another', () async {
    final store = MemoryOutboundQueueStore();
    final id = await identityFromSeed(0x96);
    final elsewhere = await identityFromSeed(0x97);
    GrassrootsNetwork layer() => GrassrootsNetwork(
          identity: id,
          underlays: const {Underlay.lan, Underlay.wan},
          rendezvousServers: const [],
          store: Store<AppState>(appReducer, initialState: const AppState()),
          sodium: sodium,
          localHostAddressReader: _loopbackHostAddresses,
          outboundQueueStore: store,
        );

    final first = layer();
    expect(await first.initialize(), isTrue);
    final messageId =
        await first.send(elsewhere.publicKey, Uint8List.fromList([7]));
    await first.dispose();
    expect((await store.load(hex(id.publicKey))).map((e) => e.messageId),
        [messageId]);
    expect(await savedQueue(id), isEmpty,
        reason: 'the store given takes the place of SharedPreferences');

    final second = layer();
    addTearDown(second.dispose);
    expect(await second.initialize(), isTrue);
    expect(second.queuedMessageCount, 1);
  });
}
