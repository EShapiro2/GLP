import 'dart:io' show InternetAddress, InternetAddressType;
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking/src/transport/public_address_discovery.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic stand-in for the seeip-backed discovery (see the glare test).
class _LoopbackAddressDiscovery extends PublicAddressDiscovery {
  @override
  Future<InternetAddress?> discoverPublicIp({
    InternetAddressType type = InternetAddressType.IPv6,
  }) async =>
      type == InternetAddressType.IPv4 ? InternetAddress.loopbackIPv4 : null;
}

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

  Future<GrassrootsNetwork> startNode(GrassrootsIdentity id) async {
    final store = Store<AppState>(
      appReducer,
      initialState: const AppState(
        settings: SettingsState(bluetoothEnabled: false),
      ),
    );
    final network = GrassrootsNetwork(
      identity: id,
      store: store,
      sodium: sodium,
      publicAddressDiscovery: _LoopbackAddressDiscovery(),
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

  /// Let the dial and its ANNOUNCE reach the socket.
  Future<void> settle() async {
    for (var i = 0; i < 10; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 50));
    }
  }

  test('Open contacts an unmatched LAN instance, keyed by its address',
      () async {
    final network = await startNode(await identityFromSeed(0x71, 'self'));
    await network.setTrustLevel(ProximityMedium.lan, ColdCallTrustLevel.open);

    const address = '127.0.0.1:45071';
    expect(network.debugUdpPeerIdForAddress(address), isNull,
        reason: 'nothing is bound before the instance resolves');

    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();

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
    final network = await startNode(await identityFromSeed(0x72, 'self'));
    await network.setTrustLevel(ProximityMedium.lan, ColdCallTrustLevel.closed);

    const address = '127.0.0.1:45072';
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();

    expect(network.debugUdpPeerIdForAddress(address), isNull,
        reason: 'Closed completes neither ANNOUNCE nor handshake with an '
            'unknown key');
  });

  test('a second resolution of the same instance does not dial twice',
      () async {
    final network = await startNode(await identityFromSeed(0x73, 'self'));
    await network.setTrustLevel(ProximityMedium.lan, ColdCallTrustLevel.open);

    const address = '127.0.0.1:45073';
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();
    expect(network.debugUdpPeerIdForAddress(address), equals(address));

    // mDNS re-resolves the same instance on every browse; the standing path
    // must be left alone rather than torn down and reopened.
    network.debugResolveLanInstance(unmatchedToken, address);
    await settle();

    expect(network.debugUdpPeerIdForAddress(address), equals(address));
  });

  test('an unusable address is not dialed', () async {
    final network = await startNode(await identityFromSeed(0x74, 'self'));
    await network.setTrustLevel(ProximityMedium.lan, ColdCallTrustLevel.open);

    network.debugResolveLanInstance(unmatchedToken, 'not-an-address');
    await settle();

    expect(network.debugUdpPeerIdForAddress('not-an-address'), isNull);
  });
}
