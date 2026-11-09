import 'dart:async';
import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:redux/redux.dart';
import 'package:sodium/sodium_sumo.dart';

import 'package:grassroots_networking_glue/grassroots_glp_network.dart';
import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/places/place_registry.dart';
import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';
import 'package:grassroots_networking_core/src/session/application.dart';
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

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) async => [];

  @override
  Future<void> saveDelivered(
          String ownerHex, List<DeliveredQueueEntry> entries) async =>
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
          OutboundQueueStore? queueStore,
          Set<Underlay> underlays = const {
            Underlay.pan,
            Underlay.lan,
            Underlay.wan
          }}) async =>
      GrassrootsNetwork(
        identity: self,
        underlays: underlays,
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
    // declare(trust(lan, open)) reaches the lan sub-object's setTrustLevel
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
    // declare(rendezvous(P, A)) and remove(rendezvous(P)) reach the WAN
    // sub-object's putRendezvousServer and removeRendezvousServer (GNA
    // 776016c, Section 4.2): the layer's refusal of an unparseable address
    // comes back unchanged, and a key not declared does nothing.
    expect(() => network.putRendezvousServer(peer, 'nowhere'),
        throwsArgumentError,
        reason: "the layer's refusal of an unparseable address, unchanged");
    expect(() => network.removeRendezvousServer(peer), returnsNormally);
    // acknowledge(messageId) (GNA, Section 2.9) reaches the layer's: a
    // message not kept does nothing.
    expect(() => network.acknowledge('m-1'), returnsNormally);

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
    const attested =
        Application(developer: 'TEAM', name: 'app.grassroots', version: 1);
    glp.Application? applicationSeen;

    network.onPeerConnected = (pk, underlay, application) {
      applicationSeen = application;
      events.add('connected ${pk.hex} $underlay');
    };
    network.onPeerDisconnected =
        (pk, underlay) => events.add('disconnected ${pk.hex} $underlay');
    network.onMessageReceived =
        (sender, payload, messageId, senderMessageId, underlay) => events.add(
            'received ${sender.hex} $payload $messageId $senderMessageId '
            '$underlay');
    network.onPeerDiscovered =
        (pk, underlay) => events.add('discovered ${pk.hex} $underlay');
    network.onPeerLost =
        (pk, underlay) => events.add('lost ${pk.hex} $underlay');
    network.onConnectivityStatusChanged =
        (oldAddress, newAddress) => events.add('address $oldAddress $newAddress');
    network.onNetworkChanged =
        (oldId, newId) => events.add('network $oldId $newId');

    layer.lan!.onPeerDiscovered!(peer.bytes);
    layer.onPeerConnected!(peer.bytes, Underlay.pan, attested);
    layer.pan!.onPeerLost!(peer.bytes);
    layer.onMessageReceived!(
        peer.bytes, Uint8List.fromList([1, 2, 3]), 'm-1', 'w-1', Underlay.lan);
    layer.onPeerDisconnected!(peer.bytes, Underlay.wan);
    layer.wan!.onConnectivityStatusChanged!(null, '203.0.113.7:9600');
    layer.wan!.onNetworkChanged!('n-1', null);

    expect(events, [
      'discovered ${peer.hex} ${glp.ProximityUnderlay.lan}',
      'connected ${peer.hex} ${glp.Underlay.pan}',
      'lost ${peer.hex} ${glp.ProximityUnderlay.pan}',
      'received ${peer.hex} [1, 2, 3] m-1 w-1 ${glp.Underlay.lan}',
      'disconnected ${peer.hex} ${glp.Underlay.wan}',
      'address null 203.0.113.7:9600',
      'network n-1 null',
    ]);
    expect(applicationSeen,
        const glp.Application(
            developer: 'TEAM', name: 'app.grassroots', version: 1),
        reason: 'the attested application, its developer, its name and its '
            'version number, as the runtime holds it (GNA §2.3)');

    network.onPeerConnected = null;
    network.onMessageReceived = null;
    expect(layer.onPeerConnected, isNull);
    expect(layer.onMessageReceived, isNull);

    await layer.dispose();
  });

  test('an unattested identity reaches the runtime as null, and an attested '
      "one as its application, session by session (GNA §2.3; peer_app/2)",
      () async {
    final layer = await layerFor(await identity());
    final network = GrassrootsGlpNetwork(layer);
    final peer = glp.PubKey.fromList((await identity()).publicKey);
    final seen = <String>[];
    network.onPeerConnected =
        (pk, underlay, application) => seen.add('$underlay $application');

    layer.onPeerConnected!(peer.bytes, Underlay.wan, null);
    layer.onPeerConnected!(peer.bytes, Underlay.pan,
        const Application(developer: 'TEAM', name: 'app.grassroots', version: 7));

    expect(seen, [
      '${glp.Underlay.wan} null',
      '${glp.Underlay.pan} Application(TEAM, app.grassroots, 7)',
    ]);

    await layer.dispose();
  });

  test(
      "send awaits the layer's save of the queue and returns the layer's id "
      "for the message, and a save that fails fails the glue's send (GNA "
      "§2.11)", () async {
    final peer = glp.PubKey.fromList((await identity()).publicKey);
    final store = MemoryOutboundQueueStore();
    final self = await identity();
    final saving = await layerFor(self, queueStore: store);
    final id =
        await GrassrootsGlpNetwork(saving).send(peer, Uint8List.fromList([1]));
    expect(
        (await store.load(_hex(self.publicKey)))
            .map((e) => (e.recipientHex, e.messageId)),
        [(peer.hex, id)],
        reason: "the message is saved once the glue's send completes, under "
            "the id the glue's send returns");
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

  test(
      'declare(trust(lan, open)) on a layer that runs no LAN returns, and the '
      'program runs on', () async {
    // GNA §2.2: "a call on an underlay the layer does not run finds no
    // sub-object"; a trust declaration for it does nothing (GNA task O (f)).
    final layer = await layerFor(await identity(),
        underlays: const {Underlay.pan, Underlay.wan});
    expect(layer.lan, isNull);
    final network = GrassrootsGlpNetwork(layer);
    expect(
        () => network.setTrustLevel(
            glp.ProximityUnderlay.lan, glp.TrustLevel.open),
        returnsNormally);
    expect(layer.store.state.settings.trustLevelOf(ProximityUnderlay.pan),
        ColdCallTrustLevel.closed,
        reason: 'the PAN level stands as it was');

    final out = <String>[];
    final engine = GlpEngine(
        rootSelfGlpPath: File('../../programs/self.glp').absolute.path);
    engine.enableMadGLP(agentId: 'alice');
    engine.runtime.outputCallback = out.add;
    engine.madContext!.network = network;
    engine.loadSource('''
procedure go.
go :- declare(trust(lan, open)), declared.
procedure declared.
declared :- send_to_user([running]).
''');
    final result = await engine.runGoal('go');
    expect(result.succeeded, isTrue,
        reason: 'the declaration returns, and aborts nothing');
    expect(out, ['running'], reason: 'the program runs on');

    await layer.dispose();
  });

  test(
      "a message the layer kept is delivered again through the glue at "
      "restart, and the glue's acknowledge(messageId) is the layer's (GNA "
      "§2.9, §2.11)", () async {
    final self = await identity();
    final senderKey = glp.PubKey.fromList((await identity()).publicKey);
    // What a layer that delivered the message and was not told it is applied
    // left behind.
    final store = MemoryOutboundQueueStore();
    await store.saveDelivered(_hex(self.publicKey), [
      DeliveredQueueEntry(
        senderHex: senderKey.hex,
        messageId: 'm-kept',
        senderMessageId: 'w-kept',
        payload: Uint8List.fromList([4, 2]),
        underlay: Underlay.lan,
      ),
    ]);
    final layer = await layerFor(self, queueStore: store);
    final network = GrassrootsGlpNetwork(layer);
    final delivered =
        Completer<(glp.PubKey, List<int>, String, String, glp.Underlay)>();
    network.onMessageReceived = (s, payload, id, senderMessageId, u) =>
        delivered.complete((s, payload, id, senderMessageId, u));
    final (s, payload, id, senderMessageId, u) = await delivered.future;
    expect((s, id, senderMessageId, u),
        (senderKey, 'm-kept', 'w-kept', glp.Underlay.lan));
    expect(payload, [4, 2]);

    network.acknowledge(id);
    await layer.dispose();
    expect(await store.loadDelivered(_hex(self.publicKey)), isEmpty,
        reason: 'acknowledged through the glue, it is kept no longer');
  }, timeout: Timeout.none);
}
