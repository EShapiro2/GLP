import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';
import 'package:grassroots_networking_core/src/transport/hole_punch_service.dart';

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

  Future<GrassrootsIdentity> identityFromSeed(int fill, String name) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
        nickname: name,
      );

  Future<HeadlessGrassrootsNetwork> node(
    int fill, {
    List<RendezvousServer> rendezvousServers = const [],
    ServerAddress? serverAddress,
    bool rendezvousServer = false,
    bool start = true,
  }) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(fill, 'node'),
      rendezvousServers: rendezvousServers,
      serverAddress: serverAddress,
      rendezvousServer: rendezvousServer,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    if (start) expect(await layer.start(), isTrue);
    return layer;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    return predicate();
  }

  /// [a] dials [b] and both report the other reachable.
  Future<void> connect(
    HeadlessGrassrootsNetwork a,
    HeadlessGrassrootsNetwork b,
  ) async {
    b.putKnownPeer(a.identity.publicKey);
    a.putPeerAddress(b.identity.publicKey, '127.0.0.1:${b.boundPort}');
    await a.send(b.identity.publicKey, Uint8List.fromList([1]));
    expect(
        await waitFor(() =>
            a.isPeerReachable(b.identity.publicKey) &&
            b.isPeerReachable(a.identity.publicKey)),
        isTrue);
  }

  void skipWithoutSodium() {
    if (sodiumUnavailable != null) markTestSkipped(sodiumUnavailable!);
  }

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
    final stranger = await identityFromSeed(0xa5, 'stranger');
    await connect(a, b);

    expect(a.peerUnderlays(b.identity.publicKey), [Underlay.wan]);
    expect(b.peerUnderlays(a.identity.publicKey), [Underlay.wan]);
    expect(a.observedPeerAddress(b.identity.publicKey),
        '127.0.0.1:${b.boundPort}');
    expect(b.observedPeerAddress(a.identity.publicKey),
        '127.0.0.1:${a.boundPort}',
        reason: 'the address the layer observes, which no one supplied');

    expect(a.peerUnderlays(stranger.publicKey), isEmpty);
    expect(a.observedPeerAddress(stranger.publicKey), isNull);
  });

  test('punchUdp sends punch datagrams toward the address', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa6);
    final target =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(target.close);
    final punched = Completer<PunchPacket>();
    target.listen((event) {
      if (event != RawSocketEvent.read) return;
      final datagram = target.receive();
      final packet =
          datagram == null ? null : PunchPacket.tryParse(datagram.data);
      if (packet != null && !punched.isCompleted) punched.complete(packet);
    });

    a.punchUdp('127.0.0.1:${target.port}');
    final packet = await punched.future.timeout(const Duration(seconds: 5));
    expect(packet.senderPubkey, a.identity.publicKey);
    expect(() => a.punchUdp('nowhere'), throwsArgumentError);
  });

  test(
      'networkIdentity is the attached local network\'s fingerprint, '
      'reported by onNetworkChanged at start', () async {
    skipWithoutSodium();
    if (sodiumUnavailable != null) return;
    final a = await node(0xa7, start: false);
    final changes = <(String?, String?)>[];
    a.onNetworkChanged = (oldId, newId) => changes.add((oldId, newId));
    expect(a.networkIdentity(), isNull, reason: 'not read before start');
    expect(await a.start(), isTrue);

    final attached = (await readLocalNetwork()).networkId;
    expect(a.networkIdentity(), attached);
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
    server.onConnectivityStatusChanged =
        (oldAddress, newAddress) => serverReports.add((oldAddress, newAddress));
    client.onConnectivityStatusChanged =
        (oldAddress, newAddress) => clientReports.add((oldAddress, newAddress));
    expect(await server.start(), isTrue);
    expect(await client.start(), isTrue);

    expect(serverReports, [(null, '203.0.113.9:9600')]);
    expect(clientReports, [(null, null)]);
  });

  group('the WAN contact gate (GNA §2.5, §2.7)', () {
    /// [caller] dials [receiver] over the WAN and sends; whether [receiver]
    /// answered, both sides reporting the other reachable.
    Future<bool> answered(
      HeadlessGrassrootsNetwork caller,
      HeadlessGrassrootsNetwork receiver, {
      int seconds = 6,
    }) async {
      caller.putPeerAddress(
          receiver.identity.publicKey, '127.0.0.1:${receiver.boundPort}');
      await caller.send(receiver.identity.publicKey, Uint8List.fromList([9]));
      return waitFor(
          () =>
              receiver.isPeerReachable(caller.identity.publicKey) &&
              caller.isPeerReachable(receiver.identity.publicKey),
          seconds: seconds);
    }

    test('an unknown key is refused', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final receiver = await node(0xc1);
      final stranger = await node(0xc2);
      expect(await answered(stranger, receiver), isFalse);
      expect(receiver.isPeerReachable(stranger.identity.publicKey), isFalse);
    }, timeout: const Timeout(Duration(minutes: 1)));

    test('a key given by putKnownPeer is answered', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final receiver = await node(0xc3);
      final caller = await node(0xc4);
      receiver.putKnownPeer(caller.identity.publicKey);
      expect(await answered(caller, receiver, seconds: 15), isTrue);
      expect(receiver.peerUnderlays(caller.identity.publicKey), [Underlay.wan]);
    }, timeout: const Timeout(Duration(minutes: 1)));

    test('a layer marked as a rendezvous server answers both', () async {
      skipWithoutSodium();
      if (sodiumUnavailable != null) return;
      final server = await node(0xc5,
          serverAddress:
              ServerAddress(listenPort: 0, publicAddress: '203.0.113.9:9600'),
          rendezvousServer: true);
      final stranger = await node(0xc6);
      final known = await node(0xc7);
      server.putKnownPeer(known.identity.publicKey);
      expect(await answered(stranger, server, seconds: 15), isTrue,
          reason: 'an unknown key');
      expect(await answered(known, server, seconds: 15), isTrue,
          reason: 'a key given by putKnownPeer');
      expect(
          server.store.state.knownPeers.known
              .containsKey(_hex(stranger.identity.publicKey)),
          isFalse,
          reason: 'the stranger is answered as a rendezvous server answers, '
              'not by becoming known');
    }, timeout: const Timeout(Duration(minutes: 1)));
  });
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
