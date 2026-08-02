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

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic host candidates (see the glare test).
Future<List<InternetAddress>> _loopbackHostAddresses() async =>
    [InternetAddress.loopbackIPv4];

/// The agent cannot observe its own public address, and asks no external
/// service for it (spec §Connectivity and Address: an agent learns it from a
/// rendezvous server, which observes the source address of the agent's
/// packets; §Rendezvous Server: no external STUN infrastructure is required).
///
/// Two properties follow, and they are what the removal of the external
/// address-discovery service turns on. First, until a rendezvous server has
/// reflected an address, there is none: `getPublicAddress()` is null and the
/// layer invents nothing. Second — the trap — a node with no public address
/// must still be able to dial, or the first rendezvous connection, which is
/// what seeds the address, could never be made. Its own interface addresses
/// are what the local end of that dial is drawn from.
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
      localHostAddressReader: _loopbackHostAddresses,
      config: const GrassrootsNetworkConfig(
        announceInterval: Duration(seconds: 2),
      ),
    );
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    return network;
  }

  Future<Set<String>> localCandidates(GrassrootsNetwork network) async {
    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (network.debugLocalCandidates().isEmpty &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    return network.debugLocalCandidates();
  }

  test('no public address until a rendezvous server reflects one', () async {
    final node = await startNode(await identityFromSeed(0x71, 'node'));
    addTearDown(node.dispose);

    // Give any would-be discovery the whole window it used to take.
    await Future<void>.delayed(const Duration(seconds: 2));

    expect(node.getPublicAddress(), isNull,
        reason: 'the layer must invent no public address of its own');
    expect(node.udpAddress, isNull);
    expect(node.store.state.transports.publicAddress, isNull);
    expect(node.store.state.transports.publicIp, isNull);
    expect(node.store.state.transports.isWellConnected, isFalse);
  });

  test('the local candidates are the agent\'s own interface addresses',
      () async {
    final node = await startNode(await identityFromSeed(0x72, 'node'));
    addTearDown(node.dispose);

    final candidates = await localCandidates(node);

    expect(candidates, isNotEmpty,
        reason: 'a dial needs a local end, and it comes from the interfaces, '
            'not from a public address the agent cannot observe');
    expect(
      candidates.every((c) => c.startsWith('127.0.0.1:')),
      isTrue,
      reason: 'the injected reader named loopback and nothing else: $candidates',
    );
    expect(node.getPublicAddress(), isNull,
        reason: 'host candidates are not a public address');
  });

  test('two peers connect with neither holding a public address', () async {
    final callerId = await identityFromSeed(0x73, 'caller');
    final calleeId = await identityFromSeed(0x74, 'callee');

    final caller = await startNode(callerId);
    final callee = await startNode(calleeId);
    addTearDown(() async {
      await caller.dispose();
      await callee.dispose();
    });

    final calleeAddress = (await localCandidates(callee)).first;
    expect(await localCandidates(caller), isNotEmpty);

    final delivered = <String>[];
    callee.onMessageReceived = (messageId, senderPk, payload, transport) {
      delivered.add(utf8.decode(payload));
    };

    // This is the shape of the first connection to a rendezvous server: the
    // dialer has no public address, and the address it dials is one GLP
    // supplied. If a dial needed a public address, the rendezvous path could
    // never seed one and the layer would never obtain an address at all.
    caller.putKnownPeer(calleeId.publicKey);
    callee.putKnownPeer(callerId.publicKey);
    caller.putPeerAddress(calleeId.publicKey, calleeAddress);

    await caller.send(
      calleeId.publicKey,
      Uint8List.fromList(utf8.encode('first contact without an address')),
    );

    final deadline = DateTime.now().add(const Duration(seconds: 20));
    while (delivered.isEmpty && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }

    expect(delivered, ['first contact without an address']);
    expect(caller.getPublicAddress(), isNull,
        reason: 'a peer session is not a rendezvous reflection');
  });
}
