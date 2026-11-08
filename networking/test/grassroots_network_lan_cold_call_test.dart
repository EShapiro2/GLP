import 'dart:async';
import 'dart:io' show InternetAddress, RawDatagramSocket, RawSocketEvent;
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// Under Open trust the layer *contacts* an unmatched LAN instance as well as
/// answering one (spec §Discovery, §Cold-Call Trust Levels). Until 2026-08-02
/// the LAN path was inbound-only, so two mutual strangers on one local network
/// never connected: each accepted, neither initiated. The dial carries no
/// expected key — the key is unset until the peer's ANNOUNCE claims the path,
/// as in BLE first contact — which is why an unmatched token can be dialed at
/// all.
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

  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(),
    );
    final network = GrassrootsNetwork(
      identity: id,
      underlays: const {Underlay.lan, Underlay.wan},
      store: store,
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(minutes: 30),
      ),
    );
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    addTearDown(network.dispose);
    return network;
  }

  /// A token no known key can match: recognition is what Closed trust does and
  /// what Open trust does *not* need, so an unmatched token is the cold call.
  const unmatchedToken = 'grs-00000000000000000000000000000000';

  /// A socket on the loopback standing for the resolved instance: its
  /// address, and a future that completes with the first datagram the layer
  /// sends it, the dial's ANNOUNCE.
  Future<(String, Future<void>)> instance() async {
    final socket =
        await RawDatagramSocket.bind(InternetAddress.loopbackIPv4, 0);
    addTearDown(socket.close);
    final reached = Completer<void>();
    socket.listen((event) {
      if (event == RawSocketEvent.read &&
          socket.receive() != null &&
          !reached.isCompleted) {
        reached.complete();
      }
    });
    return ('127.0.0.1:${socket.port}', reached.future);
  }

  /// Let a dial that should not happen have the time to happen: no event
  /// shows that it did not.
  Future<void> settle() async {
    for (var i = 0; i < 10; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
  }

  test('Open contacts an unmatched LAN instance, keyed by its address',
      timeout: Timeout.none, () async {
    final network = await startNode(await identityFromSeed(0x71));
    await network.lan!.setTrustLevel(ColdCallTrustLevel.open);

    final (address, dialed) = await instance();
    expect(network.debugUdpPeerIdForAddress(address), isNull,
        reason: 'nothing is bound before the instance resolves');

    network.debugResolveLanInstance(unmatchedToken, address);
    await dialed;

    // The path is opened under the address itself, which is the temp peer id
    // the inbound path also mints. Their ANNOUNCE re-keys it to their pubkey.
    expect(
      network.debugUdpPeerIdForAddress(address),
      equals(address),
      reason: 'the dial carries no expected key; the address is the peer id '
          'until ANNOUNCE claims it',
    );
  });

  test('Closed ignores an unmatched LAN instance', () async {
    final network = await startNode(await identityFromSeed(0x72));
    await network.lan!.setTrustLevel(ColdCallTrustLevel.closed);

    const address = '127.0.0.1:45072';
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();

    expect(network.debugUdpPeerIdForAddress(address), isNull,
        reason: 'Closed completes neither ANNOUNCE nor handshake with an '
            'unknown key');
  });

  test('an instance resolved under Closed is contacted once Open is set',
      timeout: Timeout.none, () async {
    final network = await startNode(await identityFromSeed(0x75));
    await network.lan!.setTrustLevel(ColdCallTrustLevel.closed);

    final (address, dialed) = await instance();
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();
    expect(network.debugUdpPeerIdForAddress(address), isNull,
        reason: 'Closed ignores the unmatched instance');

    // GLP opens the place: the instance resolved a moment ago is contacted
    // now, not at its token's next rotation.
    await network.lan!.setTrustLevel(ColdCallTrustLevel.open);
    await dialed;
    expect(network.debugUdpPeerIdForAddress(address), equals(address));
  });

  test('a second resolution of the same instance does not dial twice',
      timeout: Timeout.none, () async {
    final network = await startNode(await identityFromSeed(0x73));
    await network.lan!.setTrustLevel(ColdCallTrustLevel.open);

    final (address, dialed) = await instance();
    network.debugResolveLanInstance(unmatchedToken, address);
    await dialed;
    expect(network.debugUdpPeerIdForAddress(address), equals(address));

    // mDNS re-resolves the same instance on every browse; the standing path
    // must be left alone rather than torn down and reopened.
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();

    expect(network.debugUdpPeerIdForAddress(address), equals(address));
  });

  test('an unusable address is not dialed', () async {
    final network = await startNode(await identityFromSeed(0x74));
    await network.lan!.setTrustLevel(ColdCallTrustLevel.open);

    network.debugResolveLanInstance(unmatchedToken, 'not-an-address');
    await settle();

    expect(network.debugUdpPeerIdForAddress('not-an-address'), isNull);
  });
}
