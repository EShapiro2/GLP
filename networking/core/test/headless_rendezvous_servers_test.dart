import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

import 'helpers/event_waits.dart';

/// The rendezvous servers GLP declares while the layer runs, in the headless
/// profile (GNA §4.2, Rendezvous servers: "The rendezvous servers the layer
/// connects to are those GLP has declared, each by its public key and its
/// address, and GLP adds and removes them while the layer runs"):
/// `wan.putRendezvousServer(pk, address)` adds a server or replaces its
/// address, and `wan.removeRendezvousServer(pk)` removes it and ends the
/// layer's session with it, a key not declared doing nothing; the heartbeat
/// dials the declared servers (GNA §2.5, §4.2).
///
/// Needs a native libsodium, as `headless_api_test.dart` does; self-skips
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

  bool skipped() {
    if (sodiumUnavailable == null) return false;
    markTestSkipped(sodiumUnavailable!);
    return true;
  }

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  /// A UDP port nothing listens on.
  Future<int> deadPort() async {
    final socket =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = socket.port;
    socket.close();
    return port;
  }

  /// A rendezvous server, started; the agents it reports connected; and the
  /// first of them, once the server reports it.  The server reports an agent
  /// once its own session and attestation exchange with it have completed
  /// (GNA §2.3, §2.6), an event of its own that the agent's report of the
  /// server does not imply.
  Future<(HeadlessGrassrootsNetwork, List<String>, Future<String>)>
      rendezvousServer(int fill) async {
    final port = await deadPort();
    final server = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(fill),
      serverAddress:
          ServerAddress(listenPort: port, publicAddress: '127.0.0.1:$port'),
      rendezvousServer: true,
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(server.dispose);
    final connected = <String>[];
    final first = Completer<String>();
    server.onPeerConnected = (pk, underlay, _) {
      connected.add(_hex(pk));
      if (!first.isCompleted) first.complete(_hex(pk));
    };
    expect(await server.start(), isTrue);
    return (server, connected, first.future);
  }

  /// An agent, started with no rendezvous server, and what it reports.
  Future<(HeadlessGrassrootsNetwork, List<String>)> agent(int fill) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(fill),
      sodium: sodium!,
      outboundQueueStore: MemoryOutboundQueueStore(),
    );
    addTearDown(layer.dispose);
    final events = <String>[];
    layer.onPeerConnected = (pk, underlay, application) =>
        events.add('connected ${_hex(pk)} ${underlay.name} $application');
    layer.onPeerDisconnected = (pk, underlay) =>
        events.add('disconnected ${_hex(pk)} ${underlay.name}');
    expect(await layer.start(), isTrue);
    return (layer, events);
  }

  test('a server declared after start is connected and reported unattested',
      () async {
    if (skipped()) return;
    final (server, serverSaw, serverReported) = await rendezvousServer(0xa1);
    final (layer, events) = await agent(0xa2);
    final serverHex = _hex(server.identity.publicKey);
    expect(layer.isPeerReachable(server.identity.publicKey), isFalse);

    layer.wan.putRendezvousServer(
        server.identity.publicKey, '127.0.0.1:${server.boundPort}');

    // The layer connects to the server GLP declared.
    await until(
        [layer], () => layer.isPeerReachable(server.identity.publicKey));
    expect(events, ['connected $serverHex wan null'],
        reason: 'reported over the WAN, unattested (GNA §6, "Unattested")');
    expect(await serverReported, _hex(layer.identity.publicKey),
        reason: 'the server reports the agent once its own session and '
            'attestation exchange have completed (GNA §2.6)');
    expect(serverSaw, [_hex(layer.identity.publicKey)]);
    expect(layer.store.state.knownPeers.known.containsKey(serverHex), isFalse,
        reason: 'a declared rendezvous server is not a known peer');
  }, timeout: Timeout.none);

  test('a second declaration replaces its address', () async {
    if (skipped()) return;
    final (server, _, _) = await rendezvousServer(0xa3);
    final (layer, events) = await agent(0xa4);

    // Declared first where nothing answers: no session comes.
    layer.wan.putRendezvousServer(
        server.identity.publicKey, '127.0.0.1:${await deadPort()}');
    await Future<void>.delayed(const Duration(seconds: 2));
    expect(layer.isPeerReachable(server.identity.publicKey), isFalse);

    // Declared again, at its address.
    final address = '127.0.0.1:${server.boundPort}';
    layer.wan.putRendezvousServer(server.identity.publicKey, address);

    // The layer dials the address declared last.
    await until(
        [layer], () => layer.isPeerReachable(server.identity.publicKey));
    expect(layer.wan.observedPeerAddress(server.identity.publicKey), address);
    expect(events, ['connected ${_hex(server.identity.publicKey)} wan null']);
  }, timeout: Timeout.none);

  test(
      "a removed server's session ends and it is not dialed again, and "
      'removing a key not declared does nothing', () async {
    if (skipped()) return;
    final (server, serverSaw, _) = await rendezvousServer(0xa5);
    final (layer, events) = await agent(0xa6);
    final serverHex = _hex(server.identity.publicKey);
    layer.wan.putRendezvousServer(
        server.identity.publicKey, '127.0.0.1:${server.boundPort}');
    await until(
        [layer], () => layer.isPeerReachable(server.identity.publicKey));

    // A key not declared: nothing happens.
    layer.wan.removeRendezvousServer((await identityFromSeed(0xa7)).publicKey);
    expect(layer.isPeerReachable(server.identity.publicKey), isTrue);
    expect(events, ['connected $serverHex wan null']);

    // The server removed: its session ends at once.
    layer.wan.removeRendezvousServer(server.identity.publicKey);
    expect(layer.isPeerReachable(server.identity.publicKey), isFalse);
    expect(layer.peerUnderlays(server.identity.publicKey), isEmpty);
    expect(events,
        ['connected $serverHex wan null', 'disconnected $serverHex wan']);

    // Two heartbeats and more: the layer dials it no more, so the server
    // sees no second session.
    await Future<void>.delayed(
        HeadlessGrassrootsNetwork.heartbeatInterval * 2 +
            const Duration(seconds: 3));
    expect(layer.isPeerReachable(server.identity.publicKey), isFalse);
    expect(serverSaw, [_hex(layer.identity.publicKey)],
        reason: 'the server saw the one session, and no other after it');
    expect(events,
        ['connected $serverHex wan null', 'disconnected $serverHex wan']);
  }, timeout: Timeout.none);

  test(
      'ending the last rendezvous session clears the public address it '
      'reflected and reports the loss, once', () async {
    if (skipped()) return;
    final (server, _, _) = await rendezvousServer(0xb1);
    final (layer, _) = await agent(0xb2);
    final changes = <(String?, String?)>[];
    final learned =
        untilAddressChanges(layer, changes, () => changes.isNotEmpty);
    layer.wan.putRendezvousServer(
        server.identity.publicKey, '127.0.0.1:${server.boundPort}');
    await learned;
    final address = changes.single.$2!;
    expect(changes.single, (null, address),
        reason: 'the first reflected address is reported from none');
    expect(layer.wan.getPublicAddress(), address);

    // The server's session is the only rendezvous session: ending it loses
    // the address.
    final lost = untilAddressChanges(layer, changes, () => changes.length == 2);
    layer.wan.removeRendezvousServer(server.identity.publicKey);
    await lost;
    expect(changes, [(null, address), (address, null)],
        reason: 'the loss is reported once');
    expect(layer.wan.getPublicAddress(), isNull);
  }, timeout: Timeout.none);

  test(
      'with two rendezvous sessions, ending one reports nothing and ending '
      'the last reports the loss', () async {
    if (skipped()) return;
    final (first, _, _) = await rendezvousServer(0xb3);
    final (second, _, _) = await rendezvousServer(0xb4);
    final (layer, _) = await agent(0xb5);
    final firstKey = first.identity.publicKey;
    final secondKey = second.identity.publicKey;
    final changes = <(String?, String?)>[];
    final learned =
        untilAddressChanges(layer, changes, () => changes.isNotEmpty);
    layer.wan.putRendezvousServer(firstKey, '127.0.0.1:${first.boundPort}');
    layer.wan.putRendezvousServer(secondKey, '127.0.0.1:${second.boundPort}');
    await until(
        [layer],
        () =>
            layer.isPeerReachable(firstKey) &&
            layer.isPeerReachable(secondKey));
    await learned;
    final address = changes.single.$2!;

    // The second session still stands: nothing is reported.
    layer.wan.removeRendezvousServer(firstKey);
    expect(changes.single, (null, address));
    expect(layer.wan.getPublicAddress(), address);
    expect(layer.isPeerReachable(secondKey), isTrue);

    final lost = untilAddressChanges(layer, changes, () => changes.length == 2);
    layer.wan.removeRendezvousServer(secondKey);
    await lost;
    expect(changes, [(null, address), (address, null)]);
    expect(layer.wan.getPublicAddress(), isNull);
  }, timeout: Timeout.none);

  test(
      'a rendezvous session that reflects an address after a loss reports it '
      'as a first address', () async {
    if (skipped()) return;
    final (server, _, _) = await rendezvousServer(0xb6);
    final (layer, _) = await agent(0xb7);
    final serverKey = server.identity.publicKey;
    final changes = <(String?, String?)>[];
    final learned =
        untilAddressChanges(layer, changes, () => changes.isNotEmpty);
    layer.wan.putRendezvousServer(serverKey, '127.0.0.1:${server.boundPort}');
    await learned;
    final address = changes.single.$2!;
    final lost = untilAddressChanges(layer, changes, () => changes.length == 2);
    layer.wan.removeRendezvousServer(serverKey);
    await lost;
    expect(layer.wan.getPublicAddress(), isNull);

    // Declared again: the server reflects the address once more.
    final relearned =
        untilAddressChanges(layer, changes, () => changes.length == 3);
    layer.wan.putRendezvousServer(serverKey, '127.0.0.1:${server.boundPort}');
    await relearned;
    expect(changes, [(null, address), (address, null), (null, address)]);
    expect(layer.wan.getPublicAddress(), address);
  }, timeout: Timeout.none);

  test('a declaration with an unparseable address is refused', () async {
    if (skipped()) return;
    final (layer, _) = await agent(0xa8);
    final key = (await identityFromSeed(0xa9)).publicKey;
    expect(() => layer.wan.putRendezvousServer(key, 'nowhere'),
        throwsArgumentError);
    expect(() => layer.wan.putRendezvousServer(Uint8List(31), '127.0.0.1:1'),
        throwsArgumentError);
  });
}

/// Records each change of [layer]'s public address into [changes], and
/// completes once [holds] is true, checked after each change (no clock).
Future<void> untilAddressChanges(
  HeadlessGrassrootsNetwork layer,
  List<(String?, String?)> changes,
  bool Function() holds,
) {
  final done = Completer<void>();
  layer.wan.onConnectivityStatusChanged = (oldAddress, newAddress) {
    changes.add((oldAddress, newAddress));
    if (!done.isCompleted && holds()) done.complete();
  };
  return done.future;
}

String _hex(Uint8List bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
