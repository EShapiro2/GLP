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
/// listen port and static public address as one optional pair (Q4); and
/// whether it is a rendezvous server, one having a server address and
/// answering any key's WAN contact (GNA §2.2, §2.7; GNA, 2026-10-12, Task D).
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async {
    final seed = Uint8List.fromList(List.filled(32, fill));
    return GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPairFromSeed(seed),
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
        identity: await identityFromSeed(0x81),
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
        identity: await identityFromSeed(0x82),
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

  group('the rendezvous-server parameter (GNA §2.2, §2.5, §2.7)', () {
    /// [caller] dials [layer] with a payload; whether [layer] receives it
    /// and both report the other reachable within [seconds].
    Future<bool> sessionCompletes(HeadlessGrassrootsNetwork layer,
        HeadlessGrassrootsNetwork caller,
        {int seconds = 10}) async {
      final received = Completer<void>();
      layer.onMessageReceived = (sender, payload, _, underlay) {
        if (_hex(sender) == _hex(caller.identity.publicKey) &&
            underlay == Underlay.wan &&
            !received.isCompleted) {
          received.complete();
        }
      };
      caller.putPeerAddress(
          layer.identity.publicKey, '127.0.0.1:${layer.boundPort}');
      await caller.send(layer.identity.publicKey, Uint8List.fromList([7]));
      try {
        await received.future.timeout(Duration(seconds: seconds));
      } on TimeoutException {
        return false;
      }
      return layer.isPeerReachable(caller.identity.publicKey) &&
          caller.isPeerReachable(layer.identity.publicKey);
    }

    test(
        'marked, the layer completes ANNOUNCE and the handshake with an '
        'unknown key over the WAN, and the session carries a message',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final port = await freePort();
      final server = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x85),
        rendezvousServers: const [],
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        rendezvousServer: true,
        sodium: sodium!,
      );
      addTearDown(server.dispose);
      final stranger = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x86),
        rendezvousServers: const [],
        sodium: sodium!,
      );
      addTearDown(stranger.dispose);
      expect(server.rendezvousServer, isTrue);
      expect(await server.start(), isTrue);
      expect(await stranger.start(), isTrue);

      expect(await sessionCompletes(server, stranger), isTrue);
      expect(server.peerUnderlays(stranger.identity.publicKey), [Underlay.wan]);
      expect(
          server.store.state.knownPeers.known
              .containsKey(_hex(stranger.identity.publicKey)),
          isFalse,
          reason: 'answered as a rendezvous server answers, not as a known '
              'peer');
    }, timeout: const Timeout(Duration(minutes: 1)));

    test('unmarked, the same layer refuses the unknown key', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final port = await freePort();
      final server = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x87),
        rendezvousServers: const [],
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        sodium: sodium!,
      );
      addTearDown(server.dispose);
      final stranger = HeadlessGrassrootsNetwork(
        identity: await identityFromSeed(0x88),
        rendezvousServers: const [],
        sodium: sodium!,
      );
      addTearDown(stranger.dispose);
      expect(server.rendezvousServer, isFalse, reason: 'unmarked by default');
      expect(await server.start(), isTrue);
      expect(await stranger.start(), isTrue);

      expect(await sessionCompletes(server, stranger, seconds: 6), isFalse);
      expect(server.isPeerReachable(stranger.identity.publicKey), isFalse);
      expect(stranger.isPeerReachable(server.identity.publicKey), isFalse);
    }, timeout: const Timeout(Duration(minutes: 1)));

    test('marked without a server address, the constructor refuses it',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final identity = await identityFromSeed(0x89);
      expect(
          () => HeadlessGrassrootsNetwork(
                identity: identity,
                rendezvousServers: const [],
                rendezvousServer: true,
                sodium: sodium!,
              ),
          throwsArgumentError);
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
    final serverId = await identityFromSeed(0x83);
    final agentId = await identityFromSeed(0x84);
    // A rendezvous server passes an empty list, and is marked one (GNA §2.2),
    // so it answers the agent's key though no one supplied it (GNA §2.7).
    final port = await freePort();
    final server = HeadlessGrassrootsNetwork(
      identity: serverId,
      rendezvousServers: const [],
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
      rendezvousServer: true,
      sodium: sodium!,
    );
    addTearDown(server.dispose);
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

  test(
      'a rendezvous server reflects the address it observes an agent at, and '
      'the agent, which has no server address, takes it as its public '
      'address (GNA §4.2, §6 "Address observation")', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final serverId = await identityFromSeed(0x85);
    final port = await freePort();
    final server = HeadlessGrassrootsNetwork(
      identity: serverId,
      rendezvousServers: const [],
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
      rendezvousServer: true,
      sodium: sodium!,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);

    final agent = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(0x86),
      rendezvousServers: [
        RendezvousServer('127.0.0.1:${server.boundPort}', serverId.publicKey),
      ],
      sodium: sodium!,
    );
    addTearDown(agent.dispose);
    final changes = <String>[];
    final reflected = Completer<String>();
    agent.onConnectivityStatusChanged = (oldAddress, newAddress) {
      changes.add('$oldAddress -> $newAddress');
      if (newAddress != null && !reflected.isCompleted) {
        reflected.complete(newAddress);
      }
    };
    expect(await agent.start(), isTrue);

    final address =
        await reflected.future.timeout(const Duration(seconds: 15));
    expect(address,
        server.observedPeerAddress(agent.identity.publicKey),
        reason: 'the address the server observes the agent at');
    expect(address, endsWith(':${agent.boundPort}'));
    expect(agent.getPublicAddress(), address);
    expect(changes, ['null -> null', 'null -> $address'],
        reason: 'none at startup, then the reflected one');
  }, timeout: const Timeout(Duration(minutes: 1)));
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
