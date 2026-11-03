import 'dart:async';
import 'dart:convert';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking/src/store/outbound_queue_preferences.dart';
import 'package:grassroots_networking_core/src/headless/headless_grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

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

  /// Each node's host-address reader (hermetic: see the glare test), which
  /// tells when its candidates are read.
  final hostReaders = Expando<LoopbackHostReader>();

  /// A layer with the default store of its queue, the application's
  /// SharedPreferences.  [onAction] sees each action its store is
  /// dispatched, before the reducer.
  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id,
      {void Function(dynamic action)? onAction}) async {
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: Store<AppState>(appReducer,
          initialState: const AppState(),
          middleware: [
            (store, action, next) {
              onAction?.call(action);
              next(action);
            },
          ]),
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    hostReaders[network] = hosts;
    expect(await network.initialize(), isTrue);
    return network;
  }

  Future<String> listeningAddress(GrassrootsNetwork network) async {
    await hostReaders[network]!.read;
    return network.debugLocalCandidates().first;
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
    // The recipient's acknowledgement, as the layer dispatches it.
    final acknowledged = Completer<void>();
    final second = await startNode(senderId, onAction: (action) {
      if (action is MessageDeliveredAction &&
          action.messageId == messageId &&
          !acknowledged.isCompleted) {
        acknowledged.complete();
      }
    });
    var secondDisposed = false;
    addTearDown(() async {
      if (!secondDisposed) await second.dispose();
      await recipient.dispose();
    });
    final received = <(String, String, String)>[];
    final firstReceived = Completer<void>();
    recipient.onMessageReceived = (sender, payload, id, underlay) {
      received.add((hex(sender), utf8.decode(payload), id));
      if (!firstReceived.isCompleted) firstReceived.complete();
    };
    expect(second.queuedMessageCount, 1,
        reason: 'the saved queue is read back when the layer starts');
    expect(
        second.store.state.messages.queuedMessages.map((m) => m.messageId),
        [messageId]);

    // The peer becomes reachable: each knows the other, and the recipient
    // reconnects to its known peers.
    second.putKnownPeer(recipientId.publicKey);
    recipient.wan!.putPeerAddress(senderId.publicKey, await listeningAddress(second));
    await recipient.reconnectKnownPeers(reason: 'test');

    await firstReceived.future;
    expect(received, isNotEmpty,
        reason: 'the restored queue is delivered once the peer is reachable');
    expect(received.first,
        (hex(senderId.publicKey), 'queued before termination', messageId));
    expect(
        received.every((r) => r.$2 == 'queued before termination'), isTrue,
        reason: 'a duplicate is allowed; nothing else is delivered');

    // Removed once acknowledged: the acknowledgement arrives, and the layer's
    // disposal waits for every save of the queue begun so far.
    await acknowledged.future;
    await second.dispose();
    secondDisposed = true;
    final left = await savedQueue(senderId);
    expect(left, isEmpty,
        reason: 'an acknowledged message is removed from the saved queue');
  }, timeout: Timeout.none);

  test(
      'a layer started with a restored queue delivers it to a recipient that '
      'does not connect first: the heartbeat sweeps the unreachable known '
      'peers at their recorded addresses (GNA §2.5, Heartbeat)', () async {
    final senderId = await identityFromSeed(0x98);
    final recipientId = await identityFromSeed(0x99);

    // The first layer queues a message to a peer it cannot reach.
    final first = await startNode(senderId);
    final messageId = await first.send(
      recipientId.publicKey,
      Uint8List.fromList(utf8.encode('restored and swept')),
    );
    expect(messageId, isNotNull);
    await first.dispose();

    // The recipient knows the sender's key and no address of it, so it
    // cannot connect first.
    final recipient = await startNode(recipientId);
    recipient.putKnownPeer(senderId.publicKey);
    final received = <(String, String, String)>[];
    final firstReceived = Completer<void>();
    recipient.onMessageReceived = (sender, payload, id, underlay) {
      received.add((hex(sender), utf8.decode(payload), id));
      if (!firstReceived.isCompleted) firstReceived.complete();
    };
    expect(recipient.store.state.knownPeers.dialBook, isEmpty);

    // A new layer with the same identity and store reads the queue back and
    // is given the recipient's address; nothing else asks it to dial.
    final second = await startNode(senderId);
    addTearDown(() async {
      await second.dispose();
      await recipient.dispose();
    });
    expect(second.queuedMessageCount, 1);
    second.wan!
        .putPeerAddress(recipientId.publicKey, await listeningAddress(recipient));

    await firstReceived.future;
    expect(received, isNotEmpty,
        reason: 'the heartbeat dials the unreachable known peer, and its '
            'reachability drains the restored queue');
    expect(received.first,
        (hex(senderId.publicKey), 'restored and swept', messageId));
  }, timeout: Timeout.none);

  test(
      'a message delivered and not acknowledged is delivered again after a '
      'restart of the layer, and one acknowledged is not (GNA §2.9, §2.11)',
      () async {
    final senderId = await identityFromSeed(0x9a);
    final recipientId = await identityFromSeed(0x9b);

    // The sender: a headless layer, whose port is known once it starts.
    final sender = HeadlessGrassrootsNetwork(
      identity: senderId,
      sodium: sodium,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(sender.dispose);
    sender.putKnownPeer(recipientId.publicKey);
    final connected = Completer<void>();
    sender.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(recipientId.publicKey) && !connected.isCompleted) {
        connected.complete();
      }
    };
    expect(await sender.start(), isTrue);

    // The recipient, keeping its queue in SharedPreferences, dials it.
    final first = await startNode(recipientId);
    final both = Completer<Map<String, String>>();
    final received = <String, String>{};
    first.onMessageReceived = (s, payload, id, u) {
      received[utf8.decode(payload)] = id;
      if (received.length == 2 && !both.isCompleted) both.complete(received);
    };
    first.putKnownPeer(senderId.publicKey);
    first.wan!.putPeerAddress(
        senderId.publicKey, '127.0.0.1:${sender.boundPort}');
    await first.reconnectKnownPeers(reason: 'test');
    await connected.future;

    final keptId = await sender.send(
        recipientId.publicKey, Uint8List.fromList(utf8.encode('kept')));
    final ackedId = await sender.send(
        recipientId.publicKey, Uint8List.fromList(utf8.encode('acked')));
    expect(await both.future, {'kept': keptId, 'acked': ackedId});

    // The runtime acknowledges one of the two; then the layer terminates.
    await first.acknowledge(ackedId!);
    await first.dispose();
    const store = SharedPreferencesOutboundQueueStore();
    expect(
        (await store.loadDelivered(hex(recipientId.publicKey)))
            .map((e) => e.messageId),
        [keptId],
        reason: 'an acknowledged message is kept no longer');

    // The restart: a layer with the same identity and store delivers the
    // unacknowledged message again, as it was delivered, and nothing else.
    final second = await startNode(recipientId);
    final again = <(String, String, String)>[];
    final firstAgain = Completer<void>();
    second.onMessageReceived = (s, payload, id, u) {
      again.add((hex(s), utf8.decode(payload), id));
      if (!firstAgain.isCompleted) firstAgain.complete();
    };
    // The kept messages are delivered again in one turn of the event loop,
    // which ends before the wait on the first of them resumes.
    await firstAgain.future;
    expect(again, [(hex(senderId.publicKey), 'kept', keptId)]);

    await second.acknowledge(keptId!);
    await second.dispose();
    expect(await store.loadDelivered(hex(recipientId.publicKey)), isEmpty,
        reason: 'acknowledged, it is not delivered at the next restart');
  }, timeout: Timeout.none);

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
          store: Store<AppState>(appReducer, initialState: const AppState()),
          sodium: sodium,
          localHostAddressReader: LoopbackHostReader().call,
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
