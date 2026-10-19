import 'dart:async';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The headless profile's liveness and address-change recovery (GNA §2.5,
/// ANNOUNCE and Liveness; §4.2, Address-change recovery): ANNOUNCE on every
/// live session on the heartbeat; a session silent for two intervals torn
/// down; the heartbeat sweep re-initiating connections to known peers at
/// their recorded addresses; and a verified ANNOUNCE for a key from a new
/// address resetting the session the peer no longer holds.
///
/// An address change is simulated on the loopback by stopping the moving
/// layer and starting it again, which binds a new port and loses its
/// sessions, as an address change breaks a host's paths.  Until 2026-10-13
/// the profile had none of the four, and a moved agent could not reach a
/// known peer again.
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

  Future<HeadlessGrassrootsNetwork> node(int fill) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: await GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      ),
      rendezvousServers: const [],
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    expect(await layer.start(), isTrue);
    return layer;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    return predicate();
  }

  /// [a] and [b] know each other, [a] with [b]'s address, and are connected.
  Future<void> connect(
      HeadlessGrassrootsNetwork a, HeadlessGrassrootsNetwork b) async {
    b.putKnownPeer(a.identity.publicKey);
    a.putPeerAddress(b.identity.publicKey, '127.0.0.1:${b.boundPort}');
    await a.send(b.identity.publicKey, Uint8List.fromList([1]));
    expect(
        await waitFor(() =>
            a.isPeerReachable(b.identity.publicKey) &&
            b.isPeerReachable(a.identity.publicKey)),
        isTrue);
  }

  bool skip() {
    if (sodiumUnavailable == null) return false;
    markTestSkipped(sodiumUnavailable!);
    return true;
  }

  test(
      'a moved agent reaches its known peer again: its message is delivered, '
      'the peer resets the session the mover no longer holds, before liveness '
      'would have torn it down, and observes it at its new address', () async {
    if (skip()) return;
    final a = await node(0xc1);
    final b = await node(0xc2);
    final atB = <String>[];
    b.onPeerConnected = (pk, u, _) => atB.add('up');
    b.onPeerDisconnected = (pk, u) => atB.add('down');
    final got = <int>[];
    b.onMessageReceived = (s, p, id, u) => got.add(p.first);
    await connect(a, b);

    final oldPort = a.boundPort;
    await a.stop();
    expect(await a.start(), isTrue);
    expect(a.boundPort, isNot(oldPort), reason: 'the address changed');

    await a.send(b.identity.publicKey, Uint8List.fromList([2]));
    expect(await waitFor(() => got.contains(2), seconds: 15), isTrue,
        reason: 'delivered well inside the two intervals liveness waits');
    expect(atB, ['up', 'down', 'up'],
        reason: 'the stale session reset, then the fresh one attested');
    expect(b.observedPeerAddress(a.identity.publicKey),
        '127.0.0.1:${a.boundPort}');
  }, timeout: const Timeout(Duration(minutes: 1)));

  test(
      'the heartbeat sweep re-initiates the moved agent\'s connection to its '
      'known peer at the recorded address, with nothing sent', () async {
    if (skip()) return;
    final a = await node(0xc3);
    final b = await node(0xc4);
    await connect(a, b);

    await a.stop();
    expect(await a.start(), isTrue);
    expect(a.isPeerReachable(b.identity.publicKey), isFalse);

    expect(
        await waitFor(
            () =>
                a.isPeerReachable(b.identity.publicKey) &&
                b.isPeerReachable(a.identity.publicKey),
            seconds: 30),
        isTrue,
        reason: 'within a heartbeat interval or two');
  }, timeout: const Timeout(Duration(minutes: 1)));

  for (final (first, second) in [(0xc9, 0xca), (0xcb, 0xcc)]) {
    test(
        'two known peers with each other\'s address, nothing sent, connect '
        'by their heartbeat sweeps, both dialing (seeds $first, $second)',
        () async {
      if (skip()) return;
      final a = await node(first);
      final b = await node(second);
      a.putPeerAddress(b.identity.publicKey, '127.0.0.1:${b.boundPort}');
      b.putPeerAddress(a.identity.publicKey, '127.0.0.1:${a.boundPort}');
      expect(
          await waitFor(
              () =>
                  a.isPeerReachable(b.identity.publicKey) &&
                  b.isPeerReachable(a.identity.publicKey),
              seconds: 40),
          isTrue,
          reason: 'simultaneous reconnection settles on the deterministic '
              'initiator');
    }, timeout: const Timeout(Duration(minutes: 1)));
  }

  test(
      'a session silent for two intervals is torn down, feeding '
      'onPeerDisconnected', () async {
    if (skip()) return;
    final a = await node(0xc5);
    final b = await node(0xc6);
    final atA = <String>[];
    a.onPeerDisconnected = (pk, u) => atA.add('down');
    await connect(a, b);

    // b goes silent: its layer stops, sending nothing.
    await b.stop();
    final clock = Stopwatch()..start();
    expect(await waitFor(() => atA.contains('down'), seconds: 45), isTrue);
    expect(a.isPeerReachable(b.identity.publicKey), isFalse);
    expect(clock.elapsed,
        greaterThan(HeadlessGrassrootsNetwork.heartbeatInterval),
        reason: 'not before the silence has lasted');
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('a live idle session is kept by the heartbeat\'s ANNOUNCE', () async {
    if (skip()) return;
    final a = await node(0xc7);
    final b = await node(0xc8);
    final events = <String>[];
    a.onPeerDisconnected = (pk, u) => events.add('a lost b');
    b.onPeerDisconnected = (pk, u) => events.add('b lost a');
    await connect(a, b);
    await Future<void>.delayed(HeadlessGrassrootsNetwork.heartbeatInterval * 4);
    expect(events, isEmpty);
    expect(a.isPeerReachable(b.identity.publicKey), isTrue);
    expect(b.isPeerReachable(a.identity.publicKey), isTrue);
  }, timeout: const Timeout(Duration(minutes: 2)));
}
