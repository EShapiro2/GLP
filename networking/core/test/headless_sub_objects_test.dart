import 'dart:async';
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:sodium/sodium_sumo.dart';
import 'package:test/test.dart';

import 'package:grassroots_networking_core/grassroots_networking_core.dart';

/// The underlay sub-objects of the headless profile (GNA §2.2): "The layer
/// exposes one sub-object for each underlay it runs, pan, lan and wan, and
/// null for an underlay it does not run"; the headless profile runs the WAN
/// alone (GNA §2), so its `pan` and `lan` are null, and each function of the
/// WAN is reached through `wan`.
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

  Future<GrassrootsIdentity> identityFromSeed(int fill) async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519()
            .newKeyPairFromSeed(Uint8List.fromList(List.filled(32, fill))),
      );

  Future<HeadlessGrassrootsNetwork> node(int fill,
      {ServerAddress? serverAddress}) async {
    final layer = HeadlessGrassrootsNetwork(
      identity: await identityFromSeed(fill),
      rendezvousServers: const [],
      serverAddress: serverAddress,
      sodium: sodium!,
    );
    addTearDown(layer.dispose);
    return layer;
  }

  Future<bool> waitFor(bool Function() predicate, {int seconds = 15}) async {
    final clock = Stopwatch()..start();
    while (!predicate() && clock.elapsed < Duration(seconds: seconds)) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
    return predicate();
  }

  test('the profile has a wan sub-object and no pan or lan sub-object',
      () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final layer = await node(0xd1);
    expect(layer.pan, isNull);
    expect(layer.lan, isNull);
    expect(layer.wan, isA<WanApi>());
    expect(layer.wan, isNot(isA<ProximityApi>()),
        reason: 'a trust level on the WAN cannot be written');
    expect(identical(layer.wan, layer.wan), isTrue,
        reason: 'one sub-object for the layer\'s lifetime');
  });

  test('each function of the WAN is reached through wan', () async {
    if (sodiumUnavailable != null) {
      markTestSkipped(sodiumUnavailable!);
      return;
    }
    final server = await node(0xd2,
        serverAddress:
            ServerAddress(listenPort: 0, publicAddress: '203.0.113.9:9600'));
    final caller = await node(0xd3);

    final addressReports = <(String?, String?)>[];
    final networkReports = <(String?, String?)>[];
    server.wan.onConnectivityStatusChanged =
        (oldAddress, newAddress) => addressReports.add((oldAddress, newAddress));
    server.wan.onNetworkChanged =
        (oldId, newId) => networkReports.add((oldId, newId));
    expect(server.wan.networkIdentity(), isNull, reason: 'not read yet');

    expect(await server.start(), isTrue);
    expect(await caller.start(), isTrue);

    // onConnectivityStatusChanged and getPublicAddress.
    expect(addressReports, [(null, '203.0.113.9:9600')]);
    expect(server.wan.getPublicAddress(), '203.0.113.9:9600');
    expect(caller.wan.getPublicAddress(), isNull);

    // networkIdentity and onNetworkChanged.
    final attached = (await readLocalNetwork()).networkId;
    expect(server.wan.networkIdentity(), attached);
    expect(networkReports, attached == null ? isEmpty : [(null, attached)]);

    // putPeerAddress records the address with the known peer, and refuses an
    // unparseable one.
    final serverKey = server.identity.publicKey;
    final serverHex = serverKey
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    server.putKnownPeer(caller.identity.publicKey);
    caller.wan.putPeerAddress(serverKey, '127.0.0.1:${server.boundPort}');
    expect(caller.store.state.knownPeers.known[serverHex],
        '127.0.0.1:${server.boundPort}');
    expect(() => caller.wan.putPeerAddress(serverKey, 'nowhere'),
        throwsArgumentError);

    // observedPeerAddress: none before the session, the source address of
    // the session once the peer is reachable.
    expect(server.wan.observedPeerAddress(caller.identity.publicKey), isNull);
    await caller.send(serverKey, Uint8List.fromList([1]));
    expect(
        await waitFor(() =>
            server.isPeerReachable(caller.identity.publicKey) &&
            caller.isPeerReachable(serverKey)),
        isTrue);
    expect(server.wan.observedPeerAddress(caller.identity.publicKey),
        '127.0.0.1:${caller.boundPort}');

    // punchUdp refuses an unparseable address and returns at once on a
    // well-formed one.
    expect(() => caller.wan.punchUdp('nowhere'), throwsArgumentError);
    expect(() => caller.wan.punchUdp('127.0.0.1:${server.boundPort}'),
        returnsNormally);
  });
}
