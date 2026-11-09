import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'helpers/event_waits.dart';

/// The headless profile brought to the API's list for the WAN underlay (GNA,
/// 2026-10-11, Task C): `sign`, `verify`, `observedPeerAddress`, `punchUdp`,
/// `peerUnderlays`, `networkIdentity` / `onNetworkChanged`, and
/// `onConnectivityStatusChanged` fired on startup; and the WAN contact gate
/// (GNA §2.5, §2.7).
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  Future<HeadlessGrassrootsNetwork> node(
    int fill, {
    List<RendezvousServer> rendezvousServers = const [],
    ServerAddress? serverAddress,
    bool rendezvousServer = false,
    bool start = true,
  }) async {
    final layer = HeadlessGrassrootsNetwork(
      outboundQueueStore: MemoryOutboundQueueStore(),
      identity: await identityFromSeed(fill),
      serverAddress: serverAddress,
      rendezvousServer: rendezvousServer,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    // Declared as GLP declares them (GNA §4.2).
    for (final server in rendezvousServers) {
      layer.wan.putRendezvousServer(server.publicKey, server.address);
    }
    if (start) expect(await layer.start(), isTrue);
    return layer;
  }

  /// [a] dials [b] and both report the other reachable.
  Future<void> connect(
    HeadlessGrassrootsNetwork a,
    HeadlessGrassrootsNetwork b,
  ) async {
    b.putKnownPeer(a.getIdentity());
    a.wan.putPeerAddress(b.getIdentity(), '127.0.0.1:${b.boundPort}');
    await a.send(b.getIdentity(), Uint8List.fromList([1]));
    await untilBothReachable(a, b);
  }

  void skipWithoutSodium() {
    if (sodiumUnavailable != null) markTestSkipped(sodiumUnavailable!);
  }

  test('removeKnownPeer clears the recorded address, not resurrected by a re-supply',
      () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final layer = await node(0x61, start: false);
    final peer = await identityFromSeed(0x62);
    final peerHex =
        peer.publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

    layer.wan.putPeerAddress(peer.publicKey, '203.0.113.5:4001');
    expect(layer.store.state.peers.getPeerByPubkeyHex(peerHex)?.udpAddress,
        equals('203.0.113.5:4001'));

    layer.removeKnownPeer(peer.publicKey);

    expect(layer.store.state.knownPeers.addressOf(peerHex), isNull);
    expect(layer.store.state.peers.getPeerByPubkeyHex(peerHex)?.udpAddress,
        isNull);

    layer.putKnownPeer(peer.publicKey);
    expect(layer.store.state.knownPeers.addressOf(peerHex), isNull);
    expect(layer.store.state.peers.getPeerByPubkeyHex(peerHex)?.udpAddress,
        isNull);
  });

  test('sign and verify are the layer\'s, under its key', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa1, start: false);
    final b = await node(0xa2, start: false);
    final message = Uint8List.fromList(utf8.encode('attest(alice, bob)'));

    final signature = a.sign(message);
    expect(a.verify(a.getIdentity(), message, signature), isTrue);
    expect(b.verify(a.getIdentity(), message, signature), isTrue,
        reason: 'anyone verifies against the signer\'s key');
    expect(b.verify(b.getIdentity(), message, signature), isFalse);
    expect(a.verify(a.getIdentity(), Uint8List.fromList([0]), signature),
        isFalse);
    expect(a.verify(a.getIdentity(), message, Uint8List(3)), isFalse,
        reason: 'a malformed signature is a false, not an error');
  });

  test(
      'a connected peer: reachable over the WAN, observed at the source '
      'address of its session; an unconnected one neither', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa3);
    final b = await node(0xa4);
    final stranger = await identityFromSeed(0xa5);
    await connect(a, b);

    expect(a.peerUnderlays(b.getIdentity()), [Underlay.wan]);
    expect(b.peerUnderlays(a.getIdentity()), [Underlay.wan]);
    expect(a.wan.observedPeerAddress(b.getIdentity()),
        '127.0.0.1:${b.boundPort}');
    expect(b.wan.observedPeerAddress(a.getIdentity()),
        '127.0.0.1:${a.boundPort}',
        reason: 'the address the layer observes, which no one supplied');

    expect(a.peerUnderlays(stranger.publicKey), isEmpty);
    expect(a.wan.observedPeerAddress(stranger.publicKey), isNull);
  }, timeout: Timeout.none);

  String hexOf(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  test(
      'a known peer on an authenticated session has the address it is '
      'observed at recorded as its address', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa7);
    final b = await node(0xa8);
    await connect(a, b);

    // Spec ip.tex §Connectivity and Address: the observed address is recorded.
    expect(
      b.store.state.knownPeers.known[hexOf(a.getIdentity())],
      '127.0.0.1:${a.boundPort}',
    );
  }, timeout: Timeout.none);

  test(
      'authenticated traffic from a known peer re-records the address it is '
      'observed at, replacing a stale one', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa9);
    final b = await node(0xaa);
    await connect(a, b);
    final aHex = hexOf(a.getIdentity());
    final observed = '127.0.0.1:${a.boundPort}';

    b.wan.putPeerAddress(a.getIdentity(), '127.0.0.1:9');
    expect(b.store.state.knownPeers.known[aHex], '127.0.0.1:9');

    final received = Completer<void>();
    b.onMessageReceived = (sender, payload, messageId, underlay) {
      if (!received.isCompleted) received.complete();
    };
    await a.send(b.getIdentity(), Uint8List.fromList([2]));
    await received.future;

    expect(b.store.state.knownPeers.known[aHex], observed);
  }, timeout: Timeout.none);

  test('punchUdp sends punch datagrams toward the address', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa6);
    final target =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(target.close);
    // A punch datagram is the GRPU magic alone: it carries no key.
    final punched = Completer<Uint8List>();
    target.listen((event) {
      if (event != RawSocketEvent.read) return;
      final datagram = target.receive();
      if (datagram == null || punched.isCompleted) return;
      final data = datagram.data;
      if (data.length >= 4 &&
          data[0] == 0x47 &&
          data[1] == 0x52 &&
          data[2] == 0x50 &&
          data[3] == 0x55) {
        punched.complete(data);
      }
    });

    a.wan.punchUdp('127.0.0.1:${target.port}');
    final data = await punched.future;
    expect(data, orderedEquals([0x47, 0x52, 0x50, 0x55]));
    expect(() => a.wan.punchUdp('nowhere'), throwsArgumentError);
  }, timeout: Timeout.none);

  test(
      'networkIdentity is the attached local network\'s fingerprint, '
      'reported by onNetworkChanged at start', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa7, start: false);
    final changes = <(String?, String?)>[];
    a.wan.onNetworkChanged = (oldId, newId) => changes.add((oldId, newId));
    expect(a.wan.networkIdentity(), isNull, reason: 'not read before start');
    expect(await a.start(), isTrue);

    final attached = (await readLocalNetwork()).networkId;
    expect(a.wan.networkIdentity(), attached);
    expect(changes, attached == null ? isEmpty : [(null, attached)]);
  });

  test(
      'onConnectivityStatusChanged fires on startup: the static address, or '
      'none', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final server = await node(0xa8,
        serverAddress:
            ServerAddress(listenPort: 0, publicAddress: '203.0.113.9:9600'),
        start: false);
    final client = await node(0xa9, start: false);
    final serverReports = <(String?, String?)>[];
    final clientReports = <(String?, String?)>[];
    server.wan.onConnectivityStatusChanged =
        (oldAddress, newAddress) => serverReports.add((oldAddress, newAddress));
    client.wan.onConnectivityStatusChanged =
        (oldAddress, newAddress) => clientReports.add((oldAddress, newAddress));
    expect(await server.start(), isTrue);
    expect(await client.start(), isTrue);

    expect(serverReports, [(null, '203.0.113.9:9600')]);
    expect(clientReports, [(null, null)]);
  });

  group('the WAN contact gate (GNA §2.5, §2.7)', () {
    /// [caller] dials [receiver] over the WAN and sends.
    Future<void> dial(
      HeadlessGrassrootsNetwork caller,
      HeadlessGrassrootsNetwork receiver,
    ) async {
      caller.wan.putPeerAddress(
          receiver.getIdentity(), '127.0.0.1:${receiver.boundPort}');
      await caller.send(receiver.getIdentity(), Uint8List.fromList([9]));
    }

    /// [caller] dials [receiver], and [receiver] answers: both sides report
    /// the other reachable.
    Future<void> answered(
      HeadlessGrassrootsNetwork caller,
      HeadlessGrassrootsNetwork receiver,
    ) async {
      await dial(caller, receiver);
      await untilBothReachable(caller, receiver);
    }

    test('an unknown key is refused', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final receiver = await node(0xc1);
      final stranger = await node(0xc2);
      await dial(stranger, receiver);
      await Future<void>.delayed(const Duration(seconds: 6));
      expect(
          receiver.isPeerReachable(stranger.getIdentity()) &&
              stranger.isPeerReachable(receiver.getIdentity()),
          isFalse);
      expect(receiver.isPeerReachable(stranger.getIdentity()), isFalse);
    });

    test('a key given by putKnownPeer is answered', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final receiver = await node(0xc3);
      final caller = await node(0xc4);
      receiver.putKnownPeer(caller.getIdentity());
      await answered(caller, receiver);
      expect(receiver.peerUnderlays(caller.getIdentity()), [Underlay.wan]);
    }, timeout: Timeout.none);

    test('a layer marked as a rendezvous server answers both', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final server = await node(0xc5,
          serverAddress:
              ServerAddress(listenPort: 0, publicAddress: '203.0.113.9:9600'),
          rendezvousServer: true);
      final stranger = await node(0xc6);
      final known = await node(0xc7);
      server.putKnownPeer(known.getIdentity());
      // An unknown key, and a key given by putKnownPeer.
      await answered(stranger, server);
      await answered(known, server);
      expect(
          server.store.state.knownPeers.known
              .containsKey(_hex(stranger.getIdentity())),
          isFalse,
          reason: 'the stranger is answered as a rendezvous server answers, '
              'not by becoming known');
    }, timeout: Timeout.none);
  });
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
