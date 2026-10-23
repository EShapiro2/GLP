import 'dart:async';
import 'dart:io' show InternetAddress, RawDatagramSocket, RawSocketEvent;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/headless/headless_grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/session/application.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// The Flutter profile holds a session with each declared rendezvous
/// server, as the headless profile does, and reports it reachable only then
/// (GNA §2.3: "A peer becomes reachable over an underlay when its session on
/// that underlay is established, authenticated, and attested"; §2.6: "neither
/// peer is reported reachable until it succeeds", and "a rendezvous server
/// ... being unattested by design").
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

  /// A Flutter-profile layer running the WAN, [server] declared to it as GLP
  /// declares a rendezvous server (GNA §4.2).
  ///
  /// [onPeerConnected] and [onState] are attached before the layer starts, so
  /// nothing it reports is missed.
  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id,
    RendezvousServer server, {
    required void Function(Uint8List pk, Underlay underlay,
            Application? application)
        onPeerConnected,
    void Function(AppState state)? onState,
  }) async {
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.wan},
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    addTearDown(network.dispose);
    network.wan!.putRendezvousServer(server.publicKey, server.address);
    network.onPeerConnected = onPeerConnected;
    if (onState != null) {
      final sub = network.store.onChange.listen(onState);
      addTearDown(sub.cancel);
    }
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    await network.start();
    return network;
  }

  String hex(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  /// A UDP port nothing listens on.
  Future<int> deadPort() async {
    final probe = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = probe.port;
    probe.close();
    return port;
  }

  /// A headless rendezvous server, started, and the keys of the agents it
  /// reports connected.
  Future<(HeadlessGrassrootsNetwork, List<String>)> headlessServer(
      int fill) async {
    final port = await deadPort();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(fill),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    final connected = <String>[];
    server.onPeerConnected = (pk, underlay, _) => connected.add(hex(pk));
    expect(await server.start(), isTrue);
    return (server, connected);
  }

  /// A Flutter-profile layer running the WAN, started with no rendezvous
  /// server, and the connections and disconnections it reports.
  Future<(GrassrootsNetwork, List<String>)> startBare(
      GrassrootsIdentity id) async {
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.wan},
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    addTearDown(network.dispose);
    final events = <String>[];
    network.onPeerConnected = (pk, underlay, application) =>
        events.add('connected ${hex(pk)} ${underlay.name} $application');
    network.onPeerDisconnected = (pk, underlay) =>
        events.add('disconnected ${hex(pk)} ${underlay.name}');
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    await network.start();
    return (network, events);
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 30}) async {
    final deadline = DateTime.now().add(Duration(seconds: seconds));
    while (!predicate() && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    return predicate();
  }

  group('the rendezvous servers GLP declares while the layer runs (GNA §4.2)',
      () {
    test('a server declared after start is connected and reported unattested',
        () async {
      final (server, serverSaw) = await headlessServer(0x95);
      final (node, events) = await startBare(await identityFromSeed(0x96));
      final serverPk = server.identity.publicKey;
      expect(node.isPeerReachable(serverPk), isFalse);

      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');

      expect(await waitFor(() => node.isPeerReachable(serverPk)), isTrue,
          reason: 'the layer connects to the server GLP declared');
      expect(events, ['connected ${hex(serverPk)} wan null'],
          reason: 'reported over the WAN, unattested (GNA §6, "Unattested")');
      expect(node.peerUnderlays(serverPk), [Underlay.wan]);
      expect(serverSaw, [hex(node.identity.publicKey)]);
    }, timeout: const Timeout(Duration(minutes: 1)));

    test('a second declaration replaces its address', () async {
      final (server, _) = await headlessServer(0x97);
      final (node, events) = await startBare(await identityFromSeed(0x98));
      final serverPk = server.identity.publicKey;

      // Declared first where nothing answers: no session comes.
      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${await deadPort()}');
      await Future<void>.delayed(const Duration(seconds: 2));
      expect(node.isPeerReachable(serverPk), isFalse);

      // Declared again, at its address.
      final address = '127.0.0.1:${server.boundPort}';
      node.wan!.putRendezvousServer(serverPk, address);

      expect(await waitFor(() => node.isPeerReachable(serverPk)), isTrue,
          reason: 'the layer dials the address declared last');
      expect(node.wan!.observedPeerAddress(serverPk), address);
      expect(events, ['connected ${hex(serverPk)} wan null']);
    }, timeout: const Timeout(Duration(minutes: 1)));

    test(
        "a removed server's session ends and it is not dialed again, and "
        'removing a key not declared does nothing', () async {
      final (server, serverSaw) = await headlessServer(0x99);
      final (node, events) = await startBare(await identityFromSeed(0x9a));
      final serverPk = server.identity.publicKey;
      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');
      expect(await waitFor(() => node.isPeerReachable(serverPk)), isTrue);

      // A key not declared: nothing happens.
      node.wan!.removeRendezvousServer((await identityFromSeed(0x9b)).publicKey);
      expect(node.isPeerReachable(serverPk), isTrue);
      expect(events, ['connected ${hex(serverPk)} wan null']);

      // The server removed: its session ends.
      node.wan!.removeRendezvousServer(serverPk);
      expect(await waitFor(() => !node.isPeerReachable(serverPk)), isTrue);
      expect(await waitFor(() => events.length == 2), isTrue);
      expect(events, [
        'connected ${hex(serverPk)} wan null',
        'disconnected ${hex(serverPk)} wan',
      ]);

      // Several heartbeats: the layer dials it no more, so the server sees
      // no second session.
      await Future<void>.delayed(const Duration(seconds: 8));
      expect(node.isPeerReachable(serverPk), isFalse);
      expect(serverSaw, [hex(node.identity.publicKey)],
          reason: 'the server saw the one session, and no other after it');
      expect(events, [
        'connected ${hex(serverPk)} wan null',
        'disconnected ${hex(serverPk)} wan',
      ]);
    }, timeout: const Timeout(Duration(minutes: 1)));
  });

  test(
      'a declared headless rendezvous server is reported reachable once '
      'the session with it is established and authenticated, and unattested '
      '(GNA §2.3, §2.6)', () async {
    final probe = await RawDatagramSocket.bind(InternetAddress.anyIPv4, 0);
    final port = probe.port;
    probe.close();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(0x91),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);
    final serverPk = server.identity.publicKey;

    final nodeId = await identityFromSeed(0x92);
    final connects = <(Underlay, Application?)>[];
    final serverConnects = Completer<void>();
    final node = await startNode(
      nodeId,
      RendezvousServer('127.0.0.1:${server.boundPort}', serverPk),
      onPeerConnected: (pk, underlay, application) {
        if (hex(pk) != hex(serverPk)) return;
        connects.add((underlay, application));
        if (!serverConnects.isCompleted) serverConnects.complete();
      },
    );

    await serverConnects.future.timeout(const Duration(seconds: 30));

    // Reported once, over the WAN, unattested: null for the application.
    expect(connects, [(Underlay.wan, null)]);
    expect(node.isPeerReachable(serverPk), isTrue);
    expect(node.peerUnderlays(serverPk), [Underlay.wan]);
    expect(
      node.store.state.peers
          .getPeerByPubkey(serverPk)
          ?.attestationFor(Underlay.wan),
      PeerAttestation.unattested,
      reason: 'a rendezvous server has no platform attestation to offer',
    );

    // The session is mutual: the server, for its part, completed the
    // handshake and the exchange with the node, which it could do only
    // over a session the node established with it.
    final deadline = DateTime.now().add(const Duration(seconds: 15));
    while (!server.isPeerReachable(nodeId.publicKey) &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 100));
    }
    expect(server.isPeerReachable(nodeId.publicKey), isTrue);
  }, timeout: const Timeout(Duration(minutes: 1)));

  test(
      'a declared rendezvous server that never completes the handshake is '
      'never reported reachable (GNA §2.3, §2.6)', () async {
    // A server that answers nothing: the IK handshake the node starts with
    // the configured key never completes.
    final silentId = await identityFromSeed(0x93);
    final socket = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(socket.close);
    final received = <PacketType>[];
    final kinds = <int>{};
    socket.listen((event) {
      if (event != RawSocketEvent.read) return;
      for (var d = socket.receive(); d != null; d = socket.receive()) {
        final GrassrootsPacket packet;
        try {
          packet = GrassrootsPacket.deserialize(d.data);
        } catch (_) {
          continue;
        }
        received.add(packet.type);
        if (packet.type == PacketType.noiseHandshake &&
            packet.payload.length > 2) {
          kinds.add(packet.payload[2]);
        }
      }
    });
    final silentPk = silentId.publicKey;

    final connects = <Underlay>[];
    // Every state the node's store passes through: none may record an
    // authenticated UDP session with the server.
    var everLive = false;
    final node = await startNode(
      await identityFromSeed(0x94),
      RendezvousServer('127.0.0.1:${socket.port}', silentPk),
      onPeerConnected: (pk, underlay, application) {
        if (hex(pk) == hex(silentPk)) connects.add(underlay);
      },
      onState: (state) {
        if (state.peers.getPeerByPubkey(silentPk)?.hasLiveUdpConnection ??
            false) {
          everLive = true;
        }
      },
    );

    // Several heartbeats: the node dials, starts the handshake and
    // retransmits it, and the server never answers.
    await Future<void>.delayed(const Duration(seconds: 12));

    expect(received, contains(PacketType.noiseHandshake),
        reason: 'the node runs the Noise handshake with its rendezvous '
            'server, as with any peer');
    expect(kinds, {0x11},
        reason: 'IK, with the key GLP declared the server by (GNA §2.6), '
            'and no ANNOUNCE before it');
    expect(received, isNot(contains(PacketType.announce)));
    expect(connects, isEmpty);
    expect(node.isPeerReachable(silentPk), isFalse);
    expect(node.peerUnderlays(silentPk), isEmpty);
    expect(everLive, isFalse,
        reason: 'a UDP path with no session is no authenticated connection');
  }, timeout: const Timeout(Duration(minutes: 1)));
}
