import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The headless profile's instantiation (GNA §Instantiation, with GNA's
/// answers of 2026-10-11): it takes the rendezvous servers and holds a session
/// with each, a rendezvous server passing an empty list (Q2); and a server's
/// listen port and static public address as one optional pair (Q4).
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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
      nickname: name,
    );
  }

  /// A UDP port free a moment ago.
  Future<int> freePort() async {
    final socket = await RawDatagramSocket.bind(InternetAddress.anyIPv4, 0);
    final port = socket.port;
    socket.close();
    return port;
  }

  group('the server address, one optional pair', () {
    test('binds its port and is the public address', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final port = await freePort();
      final layer = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x81, 'server'),
        rendezvousServers: const [],
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      expect(await layer.start(), isTrue);
      expect(layer.boundPort, port);
      expect(layer.getPublicAddress(), '203.0.113.9:$port');
    });

    test('without one, an ephemeral port and no public address', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final layer = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x82, 'client'),
        rendezvousServers: const [],
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      expect(await layer.start(), isTrue);
      expect(layer.boundPort, greaterThan(0));
      expect(layer.getPublicAddress(), isNull);
    });

    test('refuses a port out of range and an unparseable address', () {
      expect(
          () => ServerAddress(listenPort: 70000, publicAddress: '1.2.3.4:5'),
          throwsArgumentError);
      expect(() => ServerAddress(listenPort: 9600, publicAddress: 'nowhere'),
          throwsArgumentError);
      expect(
          ServerAddress(listenPort: 9600, publicAddress: '[2001:db8::1]:9600')
              .publicAddress,
          '[2001:db8::1]:9600');
    });
  });

  test(
      'holds a session with each rendezvous server from start, with nothing '
      'sent, and answers the server though it never supplied its key',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final serverId = await identityFromSeed(0x83, 'rendezvous');
    final agentId = await identityFromSeed(0x84, 'agent');
    // A rendezvous server passes an empty list.
    final server = HeadlessGrassrootsNetwork(
      identity: serverId,
      rendezvousServers: const [],
      sodium: sodium!,
    );
    addTearDown(server.dispose);
    // The headless profile answers only the known-peer set (spec §Trust
    // levels), so this server is told the agent's key.
    server.putKnownPeer(agentId.publicKey);
    final serverSaw = Completer<Underlay>();
    server.onPeerConnected = (pk, underlay, _) {
      if (_hex(pk) == _hex(agentId.publicKey) && !serverSaw.isCompleted) {
        serverSaw.complete(underlay);
      }
    };
    expect(await server.start(), isTrue);

    final agent = HeadlessGrassrootsNetwork(
      identity: agentId,
      rendezvousServers: [
        RendezvousServer('127.0.0.1:${server.boundPort}', serverId.publicKey),
      ],
      sodium: sodium!,
    );
    addTearDown(agent.dispose);
    final agentSaw = Completer<Underlay>();
    agent.onPeerConnected = (pk, underlay, _) {
      if (_hex(pk) == _hex(serverId.publicKey) && !agentSaw.isCompleted) {
        agentSaw.complete(underlay);
      }
    };
    expect(await agent.start(), isTrue);

    expect(await agentSaw.future.timeout(const Duration(seconds: 15)),
        Underlay.wan);
    expect(await serverSaw.future.timeout(const Duration(seconds: 15)),
        Underlay.wan);
    expect(agent.isPeerReachable(serverId.publicKey), isTrue);
    expect(agent.store.state.knownPeers.known.containsKey(_hex(serverId.publicKey)),
        isFalse,
        reason: 'a rendezvous server is answered as configured, not as a '
            'known peer');
  }, timeout: const Timeout(Duration(minutes: 1)));
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
