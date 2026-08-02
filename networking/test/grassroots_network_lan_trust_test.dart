import 'dart:convert';
import 'dart:io' show InternetAddress;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking_core/src/transport/local_network.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// Unsolicited inbound IP contact from outside the agent's own local network
/// is governed by no trust level (spec §Trust levels): it is refused unless
/// the peer is in the known-peer set or presents a valid invite. LAN contact
/// arrives on the same path, a LAN session being an IP session, so the layer
/// tells the two apart by the source address alone — a source within one of
/// the agent's own local address prefixes is LAN contact, governed by the LAN
/// level; any other source is governed by no level.
///
/// Two full [GrassrootsNetwork] instances talk over loopback UDP. Loopback is
/// not a local network, so the receiver's prefixes are injected to place the
/// caller inside or outside them, which is exactly the distinction under test.
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

  Future<(GrassrootsNetwork, Store<AppState>)> startNode(
      GrassrootsIdentity id) async {
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
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        // The heartbeat tick re-reads the attached local network, which would
        // replace the prefixes these tests inject. Long enough that no tick
        // fires inside the test window; each test asserts the injected
        // prefixes were still in force when the decision was taken.
        announceInterval: Duration(minutes: 30),
      ),
    );
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    return (network, store);
  }

  /// Where a node listens, as its own host candidates report it.
  ///
  /// Not `getPublicAddress()`: that stays null until a rendezvous server
  /// reflects an address (spec §Connectivity and Address), and this test has
  /// none. The host candidates are read asynchronously at UDP init, so wait
  /// for them.
  Future<String?> listeningAddress(GrassrootsNetwork network) async {
    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (network.debugLocalCandidates().isEmpty &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    final candidates = network.debugLocalCandidates();
    return candidates.isEmpty ? null : candidates.first;
  }

  /// The receiver's local prefixes, forced so that loopback — the only source
  /// address available in a test — falls inside or outside them.
  final loopbackIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('127.0.0.1'))]);
  final elsewhereIsLocal =
      localNetworkFromPrefixes([localPrefixOf(InternetAddress('192.168.7.5'))]);

  /// An unknown caller dials the receiver and sends. Returns whether the
  /// receiver answered — completed a session with a key it does not know.
  Future<bool> unknownCallerAnswered({
    required ColdCallTrustLevel lanLevel,
    required LocalNetwork receiverLocalNetwork,
  }) async {
    final callerId = await identityFromSeed(0x61, 'caller');
    final receiverId = await identityFromSeed(0x62, 'receiver');

    final (caller, _) = await startNode(callerId);
    final (receiver, receiverStore) = await startNode(receiverId);
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });

    expect(await listeningAddress(caller), isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);

    receiverStore.dispatch(LocalNetworkChangedAction(receiverLocalNetwork));
    await receiver.setTrustLevel(ProximityMedium.lan, lanLevel);

    // The caller learns where to dial; the receiver is told nothing about the
    // caller, so the caller's contact is unsolicited and its key unknown.
    caller.putPeerAddress(
      receiverId.publicKey,
      receiverAddress!,
    );
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('lan cold call')),
    );

    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (!receiver.isPeerReachable(callerId.publicKey) &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    final answered = receiver.isPeerReachable(callerId.publicKey);

    expect(
      receiverStore.state.localNetwork.network,
      receiverLocalNetwork,
      reason: 'the injected prefixes must still be the ones in force, or the '
          'test decided under the machine\'s real local network',
    );
    return answered;
  }

  test('LAN Open answers an unknown caller from within a local prefix',
      () async {
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.open,
        receiverLocalNetwork: loopbackIsLocal,
      ),
      isTrue,
    );
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('LAN Open does not answer a caller from outside every local prefix',
      () async {
    // Same level, same unknown key — only the source address differs, and
    // that alone decides. Contact from the Internet at large is governed by
    // no level, so no level can admit it.
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.open,
        receiverLocalNetwork: elsewhereIsLocal,
      ),
      isFalse,
    );
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('LAN Closed does not answer an unknown caller on the local network',
      () async {
    expect(
      await unknownCallerAnswered(
        lanLevel: ColdCallTrustLevel.closed,
        receiverLocalNetwork: loopbackIsLocal,
      ),
      isFalse,
    );
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('the BLE level does not open the IP path', () async {
    // The two levels are independent, and the medium of discovery is the key:
    // opening BLE says nothing about contact arriving over IP.
    final callerId = await identityFromSeed(0x63, 'ble-caller');
    final receiverId = await identityFromSeed(0x64, 'ble-receiver');

    final (caller, _) = await startNode(callerId);
    final (receiver, receiverStore) = await startNode(receiverId);
    addTearDown(() async {
      await caller.dispose();
      await receiver.dispose();
    });

    expect(await listeningAddress(caller), isNotNull);
    final receiverAddress = await listeningAddress(receiver);
    expect(receiverAddress, isNotNull);

    receiverStore.dispatch(LocalNetworkChangedAction(loopbackIsLocal));
    await receiver.setTrustLevel(
        ProximityMedium.ble, ColdCallTrustLevel.open);

    expect(receiver.trustLevelOf(ProximityMedium.ble),
        ColdCallTrustLevel.open);
    expect(receiver.trustLevelOf(ProximityMedium.lan),
        ColdCallTrustLevel.closed);

    caller.putPeerAddress(
      receiverId.publicKey,
      receiverAddress!,
    );
    await caller.send(
      receiverId.publicKey,
      Uint8List.fromList(utf8.encode('ble-open should not help')),
    );

    await Future<void>.delayed(const Duration(seconds: 6));
    expect(receiver.isPeerReachable(callerId.publicKey), isFalse);
    expect(receiverStore.state.localNetwork.network, loopbackIsLocal,
        reason: 'the caller was inside a local prefix throughout, so only the '
            'LAN level being Closed can have refused it');
  }, timeout: const Timeout(Duration(minutes: 2)));
}
