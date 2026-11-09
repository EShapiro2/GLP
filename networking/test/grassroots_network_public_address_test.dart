import 'dart:async';
import 'dart:convert';
import 'dart:io' show InternetAddress, RawDatagramSocket;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/headless/headless_grassroots_network.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/signaling/signaling_codec.dart';

/// The agent cannot observe its own public address, and asks no external
/// service for it (spec §Connectivity and Address: an agent learns it from a
/// rendezvous server, which observes the source address of the agent's
/// packets; §Rendezvous Server: no external STUN infrastructure is required).
///
/// Two properties follow, and they are what the removal of the external
/// address-discovery service turns on. First, until a rendezvous server has
/// reflected an address, there is none: `getPublicAddress()` is null and the
/// layer invents nothing. Second — the trap — a node with no public address
/// must still be able to dial, or the first rendezvous connection, which is
/// what seeds the address, could never be made. Its own interface addresses
/// are what the local end of that dial is drawn from.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
    // The layer keeps its fair-delivery queue in the application's
    // SharedPreferences (GNA §2.10); a fresh store for every test.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
    );
  }

  /// Each node's host-address reader (hermetic: see the glare test), which
  /// tells when its candidates are read.
  final hostReaders = Expando<LoopbackHostReader>();

  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id, {
    void Function(String? oldAddress, String? newAddress)?
        onConnectivityStatusChanged,
    List<RendezvousServer> rendezvousServers = const [],
  }) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    final hosts = LoopbackHostReader();
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: hosts.call,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    hostReaders[network] = hosts;
    network.wan!.onConnectivityStatusChanged = onConnectivityStatusChanged;
    // Declared as GLP declares them (GNA §4.2).
    for (final server in rendezvousServers) {
      network.wan!.putRendezvousServer(server.publicKey, server.address);
    }
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    return network;
  }

  Future<Set<String>> localCandidates(GrassrootsNetwork network) async {
    await hostReaders[network]!.read;
    return network.debugLocalCandidates();
  }

  test(
      'onConnectivityStatusChanged fires on startup, with the public address '
      'as it stands (GNA, 2026-10-11, Task C)', () async {
    final reports = <(String?, String?)>[];
    final node = await startNode(
      await identityFromSeed(0x7b),
      onConnectivityStatusChanged: (oldAddress, newAddress) =>
          reports.add((oldAddress, newAddress)),
    );
    addTearDown(node.dispose);
    // Spec §Connectivity and Address: "startup: old=null"; no rendezvous
    // server has reflected an address, so none stands.
    expect(reports, [(null, null)]);
  });

  test('no public address until a rendezvous server reflects one', () async {
    final node = await startNode(await identityFromSeed(0x71));
    addTearDown(node.dispose);

    // Give any would-be discovery the whole window it used to take.
    await Future<void>.delayed(const Duration(seconds: 2));

    expect(node.wan!.getPublicAddress(), isNull,
        reason: 'the layer must invent no public address of its own');
    expect(node.store.state.transports.publicAddress, isNull);
    expect(node.store.state.transports.publicIp, isNull);
    expect(node.store.state.transports.isWellConnected, isFalse);
  });

  test('the local candidates are the agent\'s own interface addresses',
      () async {
    final node = await startNode(await identityFromSeed(0x72));
    addTearDown(node.dispose);

    final candidates = await localCandidates(node);

    expect(candidates, isNotEmpty,
        reason: 'a dial needs a local end, and it comes from the interfaces, '
            'not from a public address the agent cannot observe');
    expect(
      candidates.every((c) => c.startsWith('127.0.0.1:')),
      isTrue,
      reason: 'the injected reader named loopback and nothing else: $candidates',
    );
    expect(node.wan!.getPublicAddress(), isNull,
        reason: 'host candidates are not a public address');
  }, timeout: Timeout.none);

  test('two peers connect with neither holding a public address', () async {
    final callerId = await identityFromSeed(0x73);
    final calleeId = await identityFromSeed(0x74);

    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    final calleeAddress = (await localCandidates(callee)).first;
    expect(await localCandidates(caller), isNotEmpty);

    final delivered = <String>[];
    final firstDelivered = Completer<void>();
    callee.onMessageReceived = (senderPk, payload, messageId, underlay) {
      delivered.add(utf8.decode(payload));
      if (!firstDelivered.isCompleted) firstDelivered.complete();
    };

    // This is the shape of the first connection to a rendezvous server: the
    // dialer has no public address, and the address it dials is one GLP
    // supplied. If a dial needed a public address, the rendezvous path could
    // never seed one and the layer would never obtain an address at all.
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.wan!.putPeerAddress(calleeId.publicKey, calleeAddress);

    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('first contact without an address')),
    );

    await firstDelivered.future;

    expect(delivered, ['first contact without an address']);
    expect(caller.wan!.getPublicAddress(), isNull,
        reason: 'a peer session is not a rendezvous reflection');
  }, timeout: Timeout.none);

  /// A rendezvous server: the headless layer marked one, on the loopback.
  Future<HeadlessGrassrootsNetwork> rendezvousServer(int fill) async {
    final probe = await RawDatagramSocket.bind(InternetAddress.anyIPv4, 0);
    final port = probe.port;
    probe.close();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(fill),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);
    return server;
  }

  test(
      'a declared rendezvous server reflects the address it observes the '
      'agent at, and that is the agent\'s public address (GNA §4.2, §6 '
      '"Address observation")', () async {
    final server = await rendezvousServer(0x75);
    final reports = <(String?, String?)>[];
    final reflected = Completer<void>();
    final node = await startNode(
      await identityFromSeed(0x76),
      onConnectivityStatusChanged: (oldAddress, newAddress) {
        reports.add((oldAddress, newAddress));
        if (newAddress != null && !reflected.isCompleted) reflected.complete();
      },
      rendezvousServers: [
        RendezvousServer(
            '127.0.0.1:${server.boundPort}', server.identity.publicKey),
      ],
    );
    addTearDown(node.dispose);
    await node.start();

    await reflected.future;
    final observed = server.wan.observedPeerAddress(node.identity.publicKey);
    expect(observed, isNotNull);
    expect(node.wan!.getPublicAddress(), observed,
        reason: 'the address the server observes, reflected');
    expect(reports, [(null, null), (null, observed)]);
  }, timeout: Timeout.none);

  test(
      'signaling from a peer that is no declared rendezvous server is '
      'dropped: a reflected address from it is not the agent\'s', () async {
    final peerId = await identityFromSeed(0x77);
    final nodeId = await identityFromSeed(0x78);
    final node = await startNode(nodeId);
    addTearDown(node.dispose);
    await node.start();
    final peer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: peerId,
      sodium: sodium,
    );
    addTearDown(peer.dispose);
    expect(await peer.start(), isTrue);

    // A known peer, connected over a session.
    final peerLinked = Completer<void>();
    // The node is the one peer the headless peer meets.
    peer.onPeerConnected = (pk, underlay, application) {
      if (!peerLinked.isCompleted) peerLinked.complete();
    };
    node.putKnownPeer(peerId.publicKey);
    peer.wan.putPeerAddress(nodeId.publicKey, (await localCandidates(node)).first);
    await peer.send(nodeId.publicKey, Uint8List.fromList([1]));
    await peerLinked.future;
    expect(peer.isPeerReachable(nodeId.publicKey), isTrue);

    expect(
        await peer.debugSendSignaling(
            nodeId.publicKey,
            const SignalingCodec()
                .encode(AddrReflectMessage(ip: '203.0.113.5', port: 4242))),
        isTrue);
    await Future<void>.delayed(const Duration(seconds: 2));
    expect(node.wan!.getPublicAddress(), isNull,
        reason: 'only a declared rendezvous server reflects an address');
  }, timeout: Timeout.none);
}
