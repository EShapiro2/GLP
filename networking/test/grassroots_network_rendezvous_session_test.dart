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
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/event_waits.dart';
import 'helpers/sodium_test_bootstrap.dart';

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
      localHostAddressReader: LoopbackHostReader().call,
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

  /// A headless rendezvous server, started; the keys of the agents it
  /// reports connected; and the first of them, once the server reports it.
  /// The server reports an agent once its own session and attestation
  /// exchange with it have completed (GNA §2.3, §2.6), an event of its own
  /// that the agent's report of the server does not imply.
  Future<(HeadlessGrassrootsNetwork, List<String>, Future<String>)>
      headlessServer(int fill) async {
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
    final first = Completer<String>();
    server.onPeerConnected = (pk, underlay, _) {
      connected.add(hex(pk));
      if (!first.isCompleted) first.complete(hex(pk));
    };
    expect(await server.start(), isTrue);
    return (server, connected, first.future);
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
      localHostAddressReader: LoopbackHostReader().call,
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

  /// Completes once [node] holds [pk] reachable, or, with [reachable]
  /// false, once it no longer does.
  Future<void> reachability(GrassrootsNetwork node, Uint8List pk,
          {bool reachable = true}) =>
      stateWhere(node.store, (_) => node.isPeerReachable(pk) == reachable);

  group('the rendezvous servers GLP declares while the layer runs (GNA §4.2)',
      () {
    test('a server declared after start is connected and reported unattested',
        () async {
      final (server, serverSaw, serverReported) = await headlessServer(0x95);
      final (node, events) = await startBare(await identityFromSeed(0x96));
      final serverPk = server.identity.publicKey;
      expect(node.isPeerReachable(serverPk), isFalse);

      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');

      await reachability(node, serverPk);
      expect(node.isPeerReachable(serverPk), isTrue,
          reason: 'the layer connects to the server GLP declared');
      expect(events, ['connected ${hex(serverPk)} wan null'],
          reason: 'reported over the WAN, unattested (GNA §6, "Unattested")');
      expect(node.peerUnderlays(serverPk), [Underlay.wan]);
      expect(await serverReported, hex(node.identity.publicKey),
          reason: 'the server reports the node once its own session and '
              'attestation exchange have completed (GNA §2.6)');
      expect(serverSaw, [hex(node.identity.publicKey)]);
    }, timeout: Timeout.none);

    test('a second declaration replaces its address', () async {
      final (server, _, _) = await headlessServer(0x97);
      final (node, events) = await startBare(await identityFromSeed(0x98));
      final serverPk = server.identity.publicKey;

      // Declared first where nothing answers: no session comes.
      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${await deadPort()}');
      await Future<void>.delayed(const Duration(seconds: 2));
      expect(node.isPeerReachable(serverPk), isFalse);

      // Declared again, at its address.
      final address = '127.0.0.1:${server.boundPort}';
      node.wan!.putRendezvousServer(serverPk, address);

      await reachability(node, serverPk);
      expect(node.isPeerReachable(serverPk), isTrue,
          reason: 'the layer dials the address declared last');
      expect(node.wan!.observedPeerAddress(serverPk), address);
      expect(events, ['connected ${hex(serverPk)} wan null']);
    }, timeout: Timeout.none);

    test(
        "a removed server's session ends and it is not dialed again, and "
        'removing a key not declared does nothing', () async {
      final (server, serverSaw, _) = await headlessServer(0x99);
      final (node, events) = await startBare(await identityFromSeed(0x9a));
      final serverPk = server.identity.publicKey;
      node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');
      await reachability(node, serverPk);
      expect(node.isPeerReachable(serverPk), isTrue);

      // A key not declared: nothing happens.
      node.wan!.removeRendezvousServer((await identityFromSeed(0x9b)).publicKey);
      expect(node.isPeerReachable(serverPk), isTrue);
      expect(events, ['connected ${hex(serverPk)} wan null']);

      // The server removed: its session ends.
      node.wan!.removeRendezvousServer(serverPk);
      await reachability(node, serverPk, reachable: false);
      expect(node.isPeerReachable(serverPk), isFalse);
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
    }, timeout: Timeout.none);
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
    final nodeId = await identityFromSeed(0x92);
    final serverSawNode = Completer<void>();
    server.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) == hex(nodeId.publicKey) && !serverSawNode.isCompleted) {
        serverSawNode.complete();
      }
    };
    expect(await server.start(), isTrue);
    final serverPk = server.identity.publicKey;

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

    await serverConnects.future;

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
    await serverSawNode.future;
    expect(server.isPeerReachable(nodeId.publicKey), isTrue);
  }, timeout: Timeout.none);

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
        final peer = state.peers.getPeerByPubkey(silentPk);
        if (peer != null && (peer.lanAuthenticated || peer.wanAuthenticated)) {
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
  }, timeout: Timeout.none);

  // GNA ip.tex, "Address-change recovery": "its established UDP paths break
  // and their sessions are torn down by liveness ...  The heartbeat sweep
  // therefore re-initiates connections to the known peers and the declared
  // rendezvous servers; the rendezvous connection always succeeds".  The
  // server goes silent past the stale threshold, two announce intervals, and
  // comes back at its address: the node tore its session down with its path,
  // so the sweep dials the server again and holds a session with it.
  test(
      'a rendezvous session gone stale is torn down with its path, and the '
      'sweep dials the server again and holds a session with it', () async {
    final port = await deadPort();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(0x9c),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);
    final serverPk = server.identity.publicKey;

    final connected = <Completer<void>>[Completer<void>(), Completer<void>()];
    var connects = 0;
    final lost = Completer<void>();
    final node = await startNode(
      await identityFromSeed(0x9d),
      RendezvousServer('127.0.0.1:$port', serverPk),
      onPeerConnected: (pk, underlay, application) {
        if (hex(pk) != hex(serverPk) || connects >= connected.length) return;
        connected[connects++].complete();
      },
    );
    node.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) == hex(serverPk) && !lost.isCompleted) lost.complete();
    };
    await connected[0].future;

    // The server falls silent: the node's liveness tears the session down
    // with its path, so the node holds no session to announce into.
    await server.stop();
    await lost.future;
    expect(node.isPeerReachable(serverPk), isFalse);
    expect(node.debugHasIpSession(serverPk), isFalse,
        reason: 'the stale session ended with its path, not left standing');

    // The server is back at its address: the sweep dials it again.
    expect(await server.start(), isTrue);
    await connected[1].future;
    expect(node.isPeerReachable(serverPk), isTrue);
  }, timeout: Timeout.none);

  // GNA ip.tex, "Address-change recovery", and proximity.tex, the LAN
  // presence and browsing being for the network the agent is on: a move to
  // another local network, the connection type unchanged (one Wi-Fi for
  // another), restarts the UDP transport and LAN discovery as a change of
  // type does, and the node holds its session with the rendezvous server
  // again from the new network.
  test(
      'a new local network with the connection type unchanged restarts the '
      'UDP transport and LAN discovery, and the rendezvous session is held '
      'again', () async {
    final (server, serverSaw, _) = await headlessServer(0x9e);
    final serverPk = server.identity.publicKey;
    var network =
        localNetworkFromPrefixes([localPrefixOf(InternetAddress('192.168.7.5'))]);
    final node = GrassrootsNetwork(
      identity: await identityFromSeed(0x9f),
      underlays: const {Underlay.lan, Underlay.wan},
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: LoopbackHostReader().call,
      localNetworkReader: () async => network,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    addTearDown(node.dispose);
    final connected = <Completer<void>>[Completer<void>(), Completer<void>()];
    var connects = 0;
    node.onPeerConnected = (pk, underlay, application) {
      if (hex(pk) != hex(serverPk) || connects >= connected.length) return;
      connected[connects++].complete();
    };
    node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');
    expect(await node.initialize(), isTrue);
    await node.start();
    await connected[0].future;
    await stateWhere(
        node.store, (s) => s.localNetwork.networkId == network.networkId);
    final lanStarts = node.debugLanDiscoveryStarts;

    // The phone joins another Wi-Fi network: the connection type stays.
    network =
        localNetworkFromPrefixes([localPrefixOf(InternetAddress('172.20.10.2'))]);
    await node.debugRefreshLocalNetwork();
    expect(node.store.state.localNetwork.networkId, network.networkId);

    // The transport restarted: the session is held again, and LAN
    // discovery was started for the new network.
    await connected[1].future;
    expect(node.isPeerReachable(serverPk), isTrue);
    expect(node.debugLanDiscoveryStarts, greaterThan(lanStarts),
        reason: 'LAN discovery started again on the new network');
    expect(serverSaw.where((k) => k == hex(node.identity.publicKey)),
        hasLength(2),
        reason: 'the server saw a second session from the node');
  }, timeout: Timeout.none);

  // The same for a known peer, as the app holds the directory: the server
  // restarts mid-run, with no session state, at its address and under its
  // key.  The node's stale session ends with its path, and the sweep makes a
  // new one, handshake and all, which the restarted server reports: a
  // session the layer kept standing would have it announce into nothing and
  // count the peer connected while the server received no datagram from it.
  test(
      'a known peer restarted mid-run is reached again under a new session, '
      'the sweep running the handshake', () async {
    final port = await deadPort();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(0xa0),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    final nodeId = await identityFromSeed(0xa1);
    final sessions = <Completer<void>>[Completer<void>(), Completer<void>()];
    var seen = 0;
    server.onPeerConnected = (pk, underlay, _) {
      if (hex(pk) != hex(nodeId.publicKey) || seen >= sessions.length) return;
      sessions[seen++].complete();
    };
    expect(await server.start(), isTrue);
    final serverPk = server.identity.publicKey;

    final (node, _) = await startBare(nodeId);
    final lost = Completer<void>();
    node.onPeerDisconnected = (pk, underlay) {
      if (hex(pk) == hex(serverPk) && !lost.isCompleted) lost.complete();
    };
    node.putKnownPeer(serverPk);
    node.wan!.putPeerAddress(serverPk, '127.0.0.1:$port');
    await sessions[0].future;

    // The server restarts: the node's session with it goes stale and ends.
    await server.stop();
    expect(await server.start(), isTrue);
    await lost.future;
    expect(node.debugHasIpSession(serverPk), isFalse);

    // The sweep reaches it again under a new session, which it reports.
    await sessions[1].future;
    await stateWhere(node.store, (_) => node.isPeerReachable(serverPk));
    expect(node.isPeerReachable(serverPk), isTrue);
  }, timeout: Timeout.none);

  test(
      'a message whose attempts are exhausted on a live session tears that '
      'session down, feeding onPeerDisconnected, and the message stays queued',
      () async {
    final (server, _, _) = await headlessServer(0xa1);
    final (node, events) = await startBare(await identityFromSeed(0xa2));
    final serverPk = server.identity.publicKey;
    node.wan!.putRendezvousServer(serverPk, '127.0.0.1:${server.boundPort}');
    await reachability(node, serverPk);

    // The server goes silent: no acknowledgment can come for the message.
    await server.stop();
    final sent = node.send(serverPk, Uint8List.fromList([7]));
    expect(node.isPeerReachable(serverPk), isTrue,
        reason: 'the session stands before the exhaustion');
    node.debugExhaustIpAttempts(Underlay.wan, serverPk);
    // Reported at once, where the silence sweep would take two announce
    // intervals.
    expect(node.isPeerReachable(serverPk), isFalse,
        reason: 'exhaustion tears the session down');

    await reachability(node, serverPk, reachable: false);
    expect(events, contains('disconnected ${hex(serverPk)} wan'));
    await sent;
    // send returns once the message is saved and the failed attempt queues it
    // afterwards: the hold is awaited on the store.
    await stateWhere(
        node.store, (_) => node.queuedMessageCountForPeer(serverPk) == 1);
    expect(node.queuedMessageCountForPeer(serverPk), 1,
        reason: 'the exhausted message is kept for the next path');
  }, timeout: Timeout.none);
}
