import 'dart:async';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';

import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/places/place_registry.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'package:grassroots_networking_core/src/headless/libsodium_loader.dart';

/// A platform geofencing binding that registers in memory and reports what the
/// test tells it to.
class _FakeGeofenceBackend implements PlaceGeofenceBackend {
  final StreamController<PlaceCrossingReport> _crossings =
      StreamController<PlaceCrossingReport>.broadcast();
  final StreamController<bool> _observability =
      StreamController<bool>.broadcast();
  String? lastRegistrationId;

  @override
  Stream<PlaceCrossingReport> get crossings => _crossings.stream;

  @override
  Stream<bool> get observability => _observability.stream;

  @override
  Future<bool> register(String registrationId, double radiusMetres) async {
    lastRegistrationId = registrationId;
    return true;
  }

  @override
  Future<void> unregister(String registrationId) async {}

  @override
  Future<void> dispose() async {
    await _crossings.close();
    await _observability.close();
  }

  void report(String registrationId, PlaceCrossing crossing) =>
      _crossings.add(PlaceCrossingReport(registrationId, crossing));
}

/// A store of the queue whose every save fails.
class _FailingQueueStore implements OutboundQueueStore {
  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async => [];

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async =>
      throw StateError('the store refuses the save');
}

String _hex(List<int> bytes) =>
    bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

/// The adapter over the layer: the runtime's `GlpNetwork` implemented by
/// [GrassrootsGlpNetwork] over [GrassrootsNetwork] (GNA's task of 2026-10-04
/// 16:07 UTC, item 1; IGLP, Implementation Notes, "Networking Seam", One
/// interface).  Each function the layer has reaches it, each callback the
/// layer fires reaches the runtime with its arguments as the layer gave them,
/// and the two functions the layer does not have as GNA specifies them throw
/// [UnsupportedError].
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late SodiumSumo sodium;
  setUpAll(() async {
    // The core's native libsodium, as the headless glue's test loads it.
    sodium = await initHeadlessSodium();
  });

  // GrassrootsNetwork's constructor subscribes to connectivity changes;
  // stub the plugin channels so construction works in a test isolate.
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

  Future<GrassrootsIdentity> identity() async =>
      GrassrootsIdentity.create(
        keyPair: await Ed25519().newKeyPair(),
      );

  Future<GrassrootsNetwork> layerFor(GrassrootsIdentity self,
          {PlaceGeofenceBackend? backend,
          OutboundQueueStore? queueStore}) async =>
      GrassrootsNetwork(
        identity: self,
        underlays: const {Underlay.pan, Underlay.lan, Underlay.wan},
        rendezvousServers: const [],
        store: Store<AppState>(appReducer, initialState: const AppState()),
        sodium: sodium,
        placeGeofenceBackend: backend,
        outboundQueueStore: queueStore ?? MemoryOutboundQueueStore(),
      );

  test('identity, known peers, addresses, trust, reachability', () async {
    final self = await identity();
    final layer = await layerFor(self);
    final network = GrassrootsGlpNetwork(layer);
    final peer = glp.PubKey.fromList((await identity()).publicKey);

    expect(network.getIdentity(), glp.PubKey.fromList(self.publicKey));

    network.putKnownPeer(peer);
    expect(layer.store.state.knownPeers.known.containsKey(peer.hex), isTrue);
    network.putPeerAddress(peer, '198.51.100.4:9601');
    expect(layer.store.state.knownPeers.known[peer.hex], '198.51.100.4:9601');
    expect(() => network.putPeerAddress(peer, 'nowhere'), throwsArgumentError,
        reason: "the layer's refusal of an unparseable address, unchanged");
    network.removeKnownPeer(peer);
    expect(layer.store.state.knownPeers.known.containsKey(peer.hex), isFalse);

    network.setTrustLevel(glp.ProximityUnderlay.lan, glp.TrustLevel.open);
    // trust_declare(lan, open) reaches the lan sub-object's setTrustLevel
    // (GNA §2.2).
    expect(layer.store.state.settings.trustLevelOf(ProximityUnderlay.lan),
        ColdCallTrustLevel.open);
    expect(layer.store.state.settings.trustLevelOf(ProximityUnderlay.pan),
        ColdCallTrustLevel.closed);

    expect(network.isPeerReachable(peer), isFalse);
    expect(network.peerUnderlays(peer), isEmpty);
    expect(network.getPublicAddress(), layer.wan!.getPublicAddress());
    expect(network.networkIdentity(), layer.wan!.networkIdentity());
    expect(() => network.punchUdp('nowhere'), throwsArgumentError,
        reason: "the layer's refusal of an unparseable address, unchanged");

    await layer.dispose();
  });

  test('sign and verify are the layer\'s', () async {
    final self = await identity();
    final layer = await layerFor(self);
    final network = GrassrootsGlpNetwork(layer);
    final message = Uint8List.fromList('attest(alice, bob)'.codeUnits);

    final signature = network.sign(message);
    expect(signature, layer.sign(message));
    expect(network.verify(network.getIdentity(), message, signature), isTrue);
    expect(
        network.verify(glp.PubKey.fromList((await identity()).publicKey),
            message, signature),
        isFalse);

    await layer.dispose();
  });

  test('callbacks carry what the layer reports, in GNA\'s order', () async {
    final layer = await layerFor(await identity());
    final network = GrassrootsGlpNetwork(layer);
    final peer = glp.PubKey.fromList((await identity()).publicKey);
    final events = <String>[];
    const attested = IosApplicationIdentity('TEAM.app.grassroots');
    Object? identitySeen;

    network.onPeerConnected = (pk, underlay, identity) {
      identitySeen = identity;
      events.add('connected ${pk.hex} $underlay');
    };
    network.onPeerDisconnected =
        (pk, underlay) => events.add('disconnected ${pk.hex} $underlay');
    network.onMessageReceived = (sender, payload, messageId, underlay) =>
        events.add('received ${sender.hex} $payload $messageId $underlay');
    network.onPeerDiscovered =
        (pk, underlay) => events.add('discovered ${pk.hex} $underlay');
    network.onConnectivityStatusChanged =
        (oldAddress, newAddress) => events.add('address $oldAddress $newAddress');
    network.onNetworkChanged =
        (oldId, newId) => events.add('network $oldId $newId');

    layer.lan!.onPeerDiscovered!(peer.bytes);
    layer.onPeerConnected!(peer.bytes, Underlay.pan, attested);
    layer.onMessageReceived!(
        peer.bytes, Uint8List.fromList([1, 2, 3]), 'm-1', Underlay.lan);
    layer.onPeerDisconnected!(peer.bytes, Underlay.wan);
    layer.wan!.onConnectivityStatusChanged!(null, '203.0.113.7:9600');
    layer.wan!.onNetworkChanged!('n-1', null);

    expect(events, [
      'discovered ${peer.hex} ${glp.ProximityUnderlay.lan}',
      'connected ${peer.hex} ${glp.Underlay.pan}',
      'received ${peer.hex} [1, 2, 3] m-1 ${glp.Underlay.lan}',
      'disconnected ${peer.hex} ${glp.Underlay.wan}',
      'address null 203.0.113.7:9600',
      'network n-1 null',
    ]);
    expect(identical(identitySeen, attested), isTrue,
        reason: 'the attested application identity, as the layer reports it');

    network.onPeerConnected = null;
    network.onMessageReceived = null;
    expect(layer.onPeerConnected, isNull);
    expect(layer.onMessageReceived, isNull);

    await layer.dispose();
  });

  test(
      "send awaits the layer's save of the queue, and a save that fails "
      "fails the glue's send (GNA §2.11)", () async {
    final peer = glp.PubKey.fromList((await identity()).publicKey);
    final store = MemoryOutboundQueueStore();
    final self = await identity();
    final saving = await layerFor(self, queueStore: store);
    await GrassrootsGlpNetwork(saving).send(peer, Uint8List.fromList([1]));
    expect(
        (await store.load(_hex(self.publicKey))).map((e) => e.recipientHex),
        [peer.hex],
        reason: "the message is saved once the glue's send completes");
    await saving.dispose();

    final failing =
        await layerFor(await identity(), queueStore: _FailingQueueStore());
    await expectLater(
        GrassrootsGlpNetwork(failing).send(peer, Uint8List.fromList([2])),
        throwsA(isA<StateError>()),
        reason: "a save that fails reaches the caller of the glue's send");
    await failing.dispose();
  });

  test('places: declared, crossed, reported, removed', () async {
    final backend = _FakeGeofenceBackend();
    final layer = await layerFor(await identity(), backend: backend);
    final network = GrassrootsGlpNetwork(layer);
    final events = <(String, glp.PlaceEvent)>[];
    network.onPlaceEvent = (place, event) => events.add((place, event));

    expect(await network.declarePlace('market', 150), isTrue);
    backend.report(backend.lastRegistrationId!, PlaceCrossing.entered);
    await Future<void>.delayed(Duration.zero);
    expect(events, [('market', glp.PlaceEvent.entered)]);
    await network.removePlace('market');
    expect(layer.declaredPlaces, isEmpty);

    await layer.dispose();
  });

  test(
      'getPeers and observedPeerAddress pass straight through (GNA, '
      '2026-10-11, Task C; until then they threw UnsupportedError)', () async {
    final layer = await layerFor(await identity());
    final network = GrassrootsGlpNetwork(layer);
    final peer = glp.PubKey.fromList((await identity()).publicKey);

    // Nothing is nearby and nothing is connected: the layer's answers.
    expect(network.getPeers(), isEmpty);
    expect(layer.pan!.getPeers(), isEmpty);
    expect(layer.lan!.getPeers(), isEmpty);
    network.putPeerAddress(peer, '198.51.100.4:9601');
    expect(network.observedPeerAddress(peer), isNull,
        reason: 'the dial-book address is no observed one');
    expect(layer.wan!.observedPeerAddress(peer.bytes), isNull);

    await layer.dispose();
  });
}
