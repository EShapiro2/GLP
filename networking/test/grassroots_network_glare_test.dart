import 'dart:convert';
import 'dart:io' show InternetAddress, InternetAddressType;

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';
import 'package:grassroots_networking/src/transport/public_address_discovery.dart';

import 'helpers/sodium_test_bootstrap.dart';

/// Hermetic stand-in for the seeip-backed discovery: the node's "public"
/// address is its loopback ip:port, so peers dial exactly where it listens.
/// (flutter_test blocks real HTTP, so the default discovery cannot run.)
class _LoopbackAddressDiscovery extends PublicAddressDiscovery {
  @override
  Future<InternetAddress?> discoverPublicIp({
    InternetAddressType type = InternetAddressType.IPv6,
  }) async =>
      type == InternetAddressType.IPv4
          ? InternetAddress.loopbackIPv4
          : null;
}

/// Handshake glare at the coordinator level, per the paper's Wire mechanics
/// note: over IP the phone coordinator may initiate Noise from both ends —
/// the announce-triggered initiation on the acceptor and the send path's
/// initiation on the sender — and the crossed handshakes must resolve by
/// the session manager's deterministic rule (smaller key keeps the
/// initiator role), serialized per pair (spec §Session Establishment: one
/// handshake per medium per pair). Two full [GrassrootsNetwork] instances
/// talk over loopback UDP, hermetically ([_LoopbackAddressDiscovery]);
/// both key orderings of the app-level sender.
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

  String hexOf(Uint8List pk) =>
      pk.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

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
        // Short heartbeat so the non-dialing side's reconnect sweep fires
        // within the test window.
        announceInterval: Duration(seconds: 2),
      ),
    );
    expect(await network.initialize(), isTrue,
        reason: 'UDP must initialize on loopback');
    return network;
  }

  Future<bool> discoveryCompleted(GrassrootsNetwork network) async {
    final deadline = DateTime.now().add(const Duration(seconds: 12));
    while (network.getPublicAddress() == null &&
        DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    return network.getPublicAddress() != null;
  }

  Future<void> runGlareCase({
    required int senderSeed,
    required int recipientSeed,
    required bool senderKeySmaller,
  }) async {
    final senderId = await identityFromSeed(senderSeed, 'sender');
    final recipientId = await identityFromSeed(recipientSeed, 'recipient');
    expect(
      hexOf(senderId.publicKey).compareTo(hexOf(recipientId.publicKey)) < 0,
      senderKeySmaller,
      reason: 'seed choice must give the intended key ordering',
    );

    final sender = await startNode(senderId);
    final recipient = await startNode(recipientId);
    addTearDown(() async {
      await sender.dispose();
      await recipient.dispose();
    });

    expect(await discoveryCompleted(sender), isTrue);
    expect(await discoveryCompleted(recipient), isTrue);

    // The UDP service binds a separate ephemeral port per address family;
    // the loopback dial must target the family (and thus the port) of the
    // node's discovered public address.
    String loopbackOf(String publicAddress) {
      final port = publicAddress.split(':').last;
      return publicAddress.startsWith('[')
          ? '[::1]:$port'
          : '127.0.0.1:$port';
    }

    final senderLoopback = loopbackOf(sender.getPublicAddress()!);
    final recipientLoopback = loopbackOf(recipient.getPublicAddress()!);

    final delivered = <String>[];
    recipient.onMessageReceived = (messageId, senderPk, payload, transport) {
      expect(hexOf(senderPk), hexOf(senderId.publicKey));
      delivered.add(utf8.decode(payload));
    };

    // Closed trust both ways (the default); each side supplies the other's
    // key and loopback address — first contact then rides the coordinator's
    // own dial + ANNOUNCE + Noise machinery, with initiation possible from
    // both ends (the glare under test).
    sender.putPeerAddress(recipientId.publicKey, recipientLoopback);
    recipient.putPeerAddress(senderId.publicKey, senderLoopback);

    final messageId = await sender.send(
      recipientId.publicKey,
      Uint8List.fromList(utf8.encode('coordinator glare payload')),
    );
    expect(messageId, isNotNull);

    final deadline = DateTime.now().add(const Duration(seconds: 30));
    while (delivered.isEmpty && DateTime.now().isBefore(deadline)) {
      await Future<void>.delayed(const Duration(milliseconds: 200));
    }
    expect(delivered, ['coordinator glare payload'],
        reason: 'glare must converge to an established session and deliver');
    expect(sender.isPeerReachable(recipientId.publicKey), isTrue);
    expect(recipient.isPeerReachable(senderId.publicKey), isTrue);
  }

  test('glare converges when the sender key is larger', () async {
    // 0x11 → d04a…, 0x22 → a09a…: sender larger — the recipient's side
    // dials at the UDX level (smaller key), the sender's Noise initiation
    // crosses the acceptor's announce-triggered one.
    await runGlareCase(
        senderSeed: 0x11, recipientSeed: 0x22, senderKeySmaller: false);
  }, timeout: const Timeout(Duration(minutes: 2)));

  test('glare converges when the sender key is smaller', () async {
    // 0x33 → 17cb…, 0x44 → d759…: sender smaller — the sender dials,
    // ANNOUNCEs, and initiates; the recipient's announce-triggered
    // initiation crosses it.
    await runGlareCase(
        senderSeed: 0x33, recipientSeed: 0x44, senderKeySmaller: true);
  }, timeout: const Timeout(Duration(minutes: 2)));
}
