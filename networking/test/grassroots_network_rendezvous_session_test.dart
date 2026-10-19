import 'dart:async';
import 'dart:io' show InternetAddress, RawDatagramSocket, RawSocketEvent;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/headless/headless_grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/packet.dart';
import 'package:grassroots_networking_core/src/models/rendezvous_server.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/protocol/protocol_handler.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// The Flutter profile holds a session with each configured rendezvous
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  /// A Flutter-profile layer running the WAN, configured with [server].
  ///
  /// [onPeerConnected] and [onState] are attached before the layer starts, so
  /// nothing it reports is missed.
  Future<GrassrootsNetwork> startNode(
    GrassrootsIdentity id,
    RendezvousServer server, {
    required void Function(Uint8List pk, Underlay underlay,
            ApplicationIdentity? identity)
        onPeerConnected,
    void Function(AppState state)? onState,
  }) async {
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.wan},
      rendezvousServers: [server],
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    addTearDown(network.dispose);
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

  test(
      'a configured headless rendezvous server is reported reachable once '
      'the session with it is established and authenticated, and unattested '
      '(GNA §2.3, §2.6)', () async {
    final probe = await RawDatagramSocket.bind(InternetAddress.anyIPv4, 0);
    final port = probe.port;
    probe.close();
    final server = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x91, 'rendezvous'),
      rendezvousServers: const [],
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);
    final serverPk = server.identity.publicKey;

    final nodeId = await identityFromSeed(0x92, 'node');
    final connects = <(Underlay, ApplicationIdentity?)>[];
    final serverConnects = Completer<void>();
    final node = await startNode(
      nodeId,
      RendezvousServer('127.0.0.1:${server.boundPort}', serverPk),
      onPeerConnected: (pk, underlay, identity) {
        if (hex(pk) != hex(serverPk)) return;
        connects.add((underlay, identity));
        if (!serverConnects.isCompleted) serverConnects.complete();
      },
    );

    await serverConnects.future.timeout(const Duration(seconds: 30));

    // Reported once, over the WAN, with no attested application identity.
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
      'a configured rendezvous server that never completes the handshake is '
      'never reported reachable (GNA §2.3, §2.6)', () async {
    // A server that answers the node's ANNOUNCE with its own, signed by the
    // configured key, and answers nothing else: the handshake the node
    // starts with it never completes.
    final silentId = await identityFromSeed(0x93, 'silent');
    final silentAnnounce = GrassrootsPacket(
      type: PacketType.announce,
      payload: ProtocolHandler(identity: silentId, sodium: sodium)
          .createAnnouncePayload(),
    ).serialize();
    final socket = await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(socket.close);
    final received = <PacketType>[];
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
        if (packet.type == PacketType.announce) {
          socket.send(silentAnnounce, d.address, d.port);
        }
      }
    });
    final silentPk = silentId.publicKey;

    final connects = <Underlay>[];
    // Every state the node's store passes through: none may record an
    // authenticated UDP session with the server.
    var everLive = false;
    final node = await startNode(
      await identityFromSeed(0x94, 'node'),
      RendezvousServer('127.0.0.1:${socket.port}', silentPk),
      onPeerConnected: (pk, underlay, identity) {
        if (hex(pk) == hex(silentPk)) connects.add(underlay);
      },
      onState: (state) {
        if (state.peers.getPeerByPubkey(silentPk)?.hasLiveUdpConnection ??
            false) {
          everLive = true;
        }
      },
    );

    // Several heartbeats: the node dials, the server announces, the node
    // starts the handshake and retransmits it, and the server never answers.
    await Future<void>.delayed(const Duration(seconds: 12));

    expect(received, contains(PacketType.announce),
        reason: 'the node dials its configured rendezvous server');
    expect(received, contains(PacketType.noiseHandshake),
        reason: 'the node runs the Noise handshake with its rendezvous '
            'server, as with any peer');
    expect(connects, isEmpty);
    expect(node.isPeerReachable(silentPk), isFalse);
    expect(node.peerUnderlays(silentPk), isEmpty);
    expect(everLive, isFalse,
        reason: 'a UDP path with no session is no authenticated connection');
  }, timeout: const Timeout(Duration(minutes: 1)));
}
