import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The headless profile's instantiation (GNA §2.2, with GNA's answers of
/// 2026-10-11): a server's listen port and static public address as one
/// optional pair (Q4); and whether it is a rendezvous server, one having a
/// server address and answering any key's WAN contact (GNA §2.2, §2.8; GNA,
/// 2026-10-12, Task D).  It is given no rendezvous server: GLP declares them,
/// and the layer holds a session with each (GNA §4.2).
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
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x81),
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      expect(await layer.start(), isTrue);
      expect(layer.boundPort, port);
      expect(layer.wan.getPublicAddress(), '203.0.113.9:$port');
    });

    test('without one, an ephemeral port and no public address', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final layer = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x82),
        sodium: sodium!,
      );
      addTearDown(layer.dispose);
      expect(await layer.start(), isTrue);
      expect(layer.boundPort, greaterThan(0));
      expect(layer.wan.getPublicAddress(), isNull);
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
    /// [caller] dials [layer] with a payload; completes once [layer]
    /// receives it.
    Future<void> dial(HeadlessGrassrootsNetwork layer,
        HeadlessGrassrootsNetwork caller, Completer<void> received) async {
      layer.onMessageReceived = (sender, payload, _, underlay) {
        if (_hex(sender) == _hex(caller.getIdentity()) &&
            underlay == Underlay.wan &&
            !received.isCompleted) {
          received.complete();
        }
      };
      caller.wan.putPeerAddress(
          layer.getIdentity(), '127.0.0.1:${layer.boundPort}');
      await caller.send(layer.getIdentity(), Uint8List.fromList([7]));
    }

    /// [caller] dials [layer] with a payload; whether, once [layer] receives
    /// it, both report the other reachable.
    Future<bool> sessionCompletes(HeadlessGrassrootsNetwork layer,
        HeadlessGrassrootsNetwork caller) async {
      final received = Completer<void>();
      await dial(layer, caller, received);
      await received.future;
      return layer.isPeerReachable(caller.getIdentity()) &&
          caller.isPeerReachable(layer.getIdentity());
    }

    test(
        'marked, the layer answers an unknown key\'s IK handshake over the '
        'WAN, and the session carries a message',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final port = await freePort();
      final server = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x85),
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        rendezvousServer: true,
        sodium: sodium!,
      );
      addTearDown(server.dispose);
      final stranger = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x86),
        sodium: sodium!,
      );
      addTearDown(stranger.dispose);
      expect(server.rendezvousServer, isTrue);
      expect(await server.start(), isTrue);
      expect(await stranger.start(), isTrue);

      expect(await sessionCompletes(server, stranger), isTrue);
      expect(server.peerUnderlays(stranger.getIdentity()), [Underlay.wan]);
      expect(
          server.store.state.knownPeers.known
              .containsKey(_hex(stranger.getIdentity())),
          isFalse,
          reason: 'answered as a rendezvous server answers, not as a known '
              'peer');
    }, timeout: Timeout.none);

    test('unmarked, the same layer refuses the unknown key', () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final port = await freePort();
      final server = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x87),
        serverAddress:
            ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
        sodium: sodium!,
      );
      addTearDown(server.dispose);
      final stranger = HeadlessGrassrootsNetwork(
        outboundQueueStore: MemoryOutboundQueueStore(),
        identity: await identityFromSeed(0x88),
        sodium: sodium!,
      );
      addTearDown(stranger.dispose);
      expect(server.rendezvousServer, isFalse, reason: 'unmarked by default');
      expect(await server.start(), isTrue);
      expect(await stranger.start(), isTrue);

      final received = Completer<void>();
      await dial(server, stranger, received);
      await Future<void>.delayed(const Duration(seconds: 6));
      expect(received.isCompleted, isFalse);
      expect(server.isPeerReachable(stranger.getIdentity()), isFalse);
      expect(stranger.isPeerReachable(server.getIdentity()), isFalse);
    });

    test('marked without a server address, the constructor refuses it',
        () async {
      if (sodiumUnavailable != null) {
        markTestSkipped(sodiumUnavailable!);
        return;
      }
      final identity = await identityFromSeed(0x89);
      expect(
          () => HeadlessGrassrootsNetwork(
                outboundQueueStore: MemoryOutboundQueueStore(),
                identity: identity,
                rendezvousServer: true,
                sodium: sodium!,
              ),
          throwsArgumentError);
    });
  });

  test(
      'holds a session with each declared rendezvous server from start, with '
      'nothing sent, and answers the server though it never supplied its key',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final serverId = await identityFromSeed(0x83);
    final agentId = await identityFromSeed(0x84);
    // A rendezvous server declares none, and is marked one (GNA §2.2), so it
    // answers the agent's key though no one supplied it (GNA §2.8).
    final port = await freePort();
    final server = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: serverId,
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
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: agentId,
      sodium: sodium!,
    );
    addTearDown(agent.dispose);
    agent.wan.putRendezvousServer(
        serverId.publicKey, '127.0.0.1:${server.boundPort}');
    final agentSaw = Completer<Underlay>();
    agent.onPeerConnected = (pk, underlay, _) {
      if (_hex(pk) == _hex(serverId.publicKey) && !agentSaw.isCompleted) {
        agentSaw.complete(underlay);
      }
    };
    expect(await agent.start(), isTrue);

    expect(await agentSaw.future, Underlay.wan);
    expect(await serverSaw.future, Underlay.wan);
    expect(agent.isPeerReachable(serverId.publicKey), isTrue);
    expect(agent.store.state.knownPeers.known.containsKey(_hex(serverId.publicKey)),
        isFalse,
        reason: 'a rendezvous server is answered as declared, not as a '
            'known peer');
  }, timeout: Timeout.none);

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
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: serverId,
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '203.0.113.9:$port'),
      rendezvousServer: true,
      sodium: sodium!,
    );
    addTearDown(server.dispose);
    expect(await server.start(), isTrue);

    final agent = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(0x86),
      sodium: sodium!,
    );
    addTearDown(agent.dispose);
    agent.wan.putRendezvousServer(
        serverId.publicKey, '127.0.0.1:${server.boundPort}');
    final changes = <String>[];
    final reflected = Completer<String>();
    agent.wan.onConnectivityStatusChanged = (oldAddress, newAddress) {
      changes.add('$oldAddress -> $newAddress');
      if (newAddress != null && !reflected.isCompleted) {
        reflected.complete(newAddress);
      }
    };
    expect(await agent.start(), isTrue);

    final address = await reflected.future;
    expect(address,
        server.wan.observedPeerAddress(agent.getIdentity()),
        reason: 'the address the server observes the agent at');
    expect(address, endsWith(':${agent.boundPort}'));
    expect(agent.wan.getPublicAddress(), address);
    expect(changes, ['null -> null', 'null -> $address'],
        reason: 'none at startup, then the reflected one');
  }, timeout: Timeout.none);
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
