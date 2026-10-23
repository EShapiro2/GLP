import 'dart:io' show InternetAddress;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/models/underlay_api.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// The underlay sub-objects of the Flutter profile (GNA §2.2): "The layer
/// exposes one sub-object for each underlay it runs, pan, lan and wan, and
/// null for an underlay it does not run."  The functions of one underlay are
/// reached through its sub-object, with no underlay parameter: discovery and
/// the trust level on `pan` and `lan` (GNA §3.1), the public address, peer
/// addresses, hole punching and the local network identity on `wan` (GNA
/// §4.2, §4.3).  Discovery over a live LAN session reaching `lan` and not
/// `pan` is `grassroots_network_lan_trust_test.dart`'s.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    sodium = await initTestSodium();
  });

  setUp(() {
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

  Future<GrassrootsIdentity> identity() async =>
      GrassrootsIdentity.create(keyPair: await Ed25519().newKeyPair());

  Future<GrassrootsNetwork> layerFor(Set<Underlay> underlays) async {
    final layer = GrassrootsNetwork(
      identity: await identity(),
      underlays: underlays,
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      localHostAddressReader: _loopbackHostAddresses,
    );
    addTearDown(layer.dispose);
    return layer;
  }

  test('one sub-object for each underlay the layer runs, null for the others',
      () async {
    const all = [Underlay.pan, Underlay.lan, Underlay.wan];
    for (var mask = 1; mask < 8; mask++) {
      final underlays = {
        for (var i = 0; i < 3; i++)
          if (mask & (1 << i) != 0) all[i],
      };
      final layer = await layerFor(underlays);
      final reason = 'a layer running $underlays';
      expect(layer.pan != null, underlays.contains(Underlay.pan),
          reason: reason);
      expect(layer.lan != null, underlays.contains(Underlay.lan),
          reason: reason);
      expect(layer.wan != null, underlays.contains(Underlay.wan),
          reason: reason);
      expect(layer.pan?.underlay, layer.pan == null ? null : ProximityUnderlay.pan);
      expect(layer.lan?.underlay, layer.lan == null ? null : ProximityUnderlay.lan);
    }
  });

  test('a call that makes no sense on an underlay has no sub-object to go to',
      () async {
    final layer = await layerFor(const {Underlay.pan, Underlay.lan, Underlay.wan});
    expect(layer.pan, isNot(isA<WanApi>()));
    expect(layer.lan, isNot(isA<WanApi>()));
    expect(layer.wan, isNot(isA<ProximityApi>()),
        reason: 'a trust level on the WAN cannot be written');
    expect(identical(layer.wan, layer.wan), isTrue,
        reason: 'one sub-object for the layer\'s lifetime');
  });

  test('getPeers, onPeerDiscovered and setTrustLevel act on their own underlay',
      () async {
    final layer = await layerFor(const {Underlay.pan, Underlay.lan, Underlay.wan});
    final pan = layer.pan!;
    final lan = layer.lan!;

    // The levels the layer holds, in its store; the sub-objects set them and
    // have no call that reads them back (GNA §3.1, Discovery).
    ColdCallTrustLevel levelOf(ProximityUnderlay underlay) =>
        layer.store.state.settings.trustLevelOf(underlay);
    expect(levelOf(ProximityUnderlay.pan), ColdCallTrustLevel.closed);
    expect(levelOf(ProximityUnderlay.lan), ColdCallTrustLevel.closed);

    await pan.setTrustLevel(ColdCallTrustLevel.open);
    expect(levelOf(ProximityUnderlay.pan), ColdCallTrustLevel.open);
    expect(levelOf(ProximityUnderlay.lan), ColdCallTrustLevel.closed,
        reason: 'the PAN level leaves the LAN level as it was');

    await lan.setTrustLevel(ColdCallTrustLevel.open);
    await pan.setTrustLevel(ColdCallTrustLevel.closed);
    expect(levelOf(ProximityUnderlay.pan), ColdCallTrustLevel.closed);
    expect(levelOf(ProximityUnderlay.lan), ColdCallTrustLevel.open);

    // Nothing is nearby on either underlay.
    expect(pan.getPeers(), isEmpty);
    expect(lan.getPeers(), isEmpty);

    // Each sub-object holds its own discovery callback.
    final seen = <String>[];
    pan.onPeerDiscovered = (_) => seen.add('pan');
    lan.onPeerDiscovered = (_) => seen.add('lan');
    lan.onPeerDiscovered!(Uint8List(32));
    expect(seen, ['lan']);
    expect(pan.onPeerDiscovered, isNotNull);
  });

  test('each function of the WAN is reached through wan', () async {
    final layer = await layerFor(const {Underlay.lan, Underlay.wan});
    final wan = layer.wan!;
    final peer = (await identity()).publicKey;
    final peerHex =
        peer.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

    final addressReports = <(String?, String?)>[];
    wan.onConnectivityStatusChanged =
        (oldAddress, newAddress) => addressReports.add((oldAddress, newAddress));
    final networkReports = <(String?, String?)>[];
    wan.onNetworkChanged = (oldId, newId) => networkReports.add((oldId, newId));

    // initialize starts the layer, which reports the public address as it
    // stands: none until a rendezvous server reflects one (GNA §4.2).
    expect(await layer.initialize(), isTrue);
    expect(addressReports, [(null, null)]);
    expect(wan.getPublicAddress(), isNull);

    // networkIdentity and onNetworkChanged: the construction-time read.
    for (var i = 0; i < 50 && wan.networkIdentity() == null; i++) {
      await Future<void>.delayed(const Duration(milliseconds: 20));
    }
    expect(wan.networkIdentity(), layer.store.state.localNetwork.networkId);
    expect(networkReports,
        wan.networkIdentity() == null ? isEmpty : [(null, wan.networkIdentity())]);

    // putPeerAddress records the address with the known peer.
    wan.putPeerAddress(peer, '198.51.100.4:9601');
    expect(layer.store.state.knownPeers.known[peerHex], '198.51.100.4:9601');
    expect(() => wan.putPeerAddress(peer, 'nowhere'), throwsArgumentError);

    // observedPeerAddress: the dial-book address is no observed one.
    expect(wan.observedPeerAddress(peer), isNull);

    // punchUdp.
    expect(() => wan.punchUdp('nowhere'), throwsArgumentError);
    expect(() => wan.punchUdp('203.0.113.5:4242'), returnsNormally);
  });
}
