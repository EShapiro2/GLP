import 'dart:async';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:redux/redux.dart';
import 'package:sodium_libs/sodium_libs_sumo.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';
import 'package:grassroots_networking_core/src/models/identity.dart';
import 'package:grassroots_networking_core/src/places/place_registry.dart';
import 'package:grassroots_networking_core/src/store/store.dart';

import 'helpers/sodium_test_bootstrap.dart';

class _FakeGeofenceBackend implements PlaceGeofenceBackend {
  final Set<String> live = {};
  final List<String> unregistered = [];
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
    live.add(registrationId);
    lastRegistrationId = registrationId;
    return true;
  }

  @override
  Future<void> unregister(String registrationId) async {
    unregistered.add(registrationId);
    live.remove(registrationId);
  }

  @override
  Future<void> dispose() async {
    await _crossings.close();
    await _observability.close();
  }

  void report(String registrationId, PlaceCrossing crossing) =>
      _crossings.add(PlaceCrossingReport(registrationId, crossing));

  /// The platform stops (false) or resumes (true) reporting crossings.
  void reportObservability(bool observable) => _observability.add(observable);
}

/// The place functions on the layer's API surface (spec §System Predicates):
/// `declarePlace(place, radius)`, `removePlace(place)`, `onPlaceEvent(cb)`.
/// GLP supplies a name and a radius and receives no coordinates — there is no
/// call here that returns a position.
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

  Future<GrassrootsNetwork> buildNetwork({
    PlaceGeofenceBackend? backend,
  }) async {
    final identity = GrassrootsIdentity.create(
      keyPair: await Ed25519().newKeyPair(),
      nickname: 'places',
    );
    return GrassrootsNetwork(
      identity: await identity,
      store: Store<AppState>(appReducer, initialState: const AppState()),
      sodium: sodium,
      placeGeofenceBackend: backend,
    );
  }

  test('declaring, crossing, and removing a place', () async {
    final backend = _FakeGeofenceBackend();
    final network = await buildNetwork(backend: backend);

    final events = <(String, PlaceEvent)>[];
    network.onPlaceEvent = (place, event) => events.add((place, event));

    expect(await network.declarePlace('market', 150), isTrue);
    expect(network.declaredPlaces, ['market']);

    final id = backend.lastRegistrationId!;
    backend.report(id, PlaceCrossing.entered);
    await Future<void>.delayed(Duration.zero);
    expect(events, [('market', PlaceEvent.entered)]);

    await network.removePlace('market');
    expect(network.declaredPlaces, isEmpty);
    expect(backend.live, isEmpty);

    // A crossing racing the unregistration is dropped, not queued.
    backend.report(id, PlaceCrossing.exited);
    await Future<void>.delayed(Duration.zero);
    expect(events, [('market', PlaceEvent.entered)]);

    await network.dispose();
  });

  test('the layer reports unobservable and observable through onPlaceEvent',
      () async {
    final backend = _FakeGeofenceBackend();
    final network = await buildNetwork(backend: backend);

    final events = <(String, PlaceEvent)>[];
    network.onPlaceEvent = (place, event) => events.add((place, event));

    await network.declarePlace('market', 150);
    final id = backend.lastRegistrationId!;

    // The platform stops reporting — the application lost the foreground, or
    // a permission was withdrawn. Spec §System Predicates: the stream says so,
    // and between the two no crossing is seen and none is delivered late.
    backend.reportObservability(false);
    await Future<void>.delayed(Duration.zero);
    expect(events, [('market', PlaceEvent.unobservable)]);

    backend.report(id, PlaceCrossing.exited);
    await Future<void>.delayed(Duration.zero);
    expect(events, [('market', PlaceEvent.unobservable)],
        reason: 'crossings are dropped while unobservable');

    backend.reportObservability(true);
    await Future<void>.delayed(Duration.zero);
    expect(events, [
      ('market', PlaceEvent.unobservable),
      ('market', PlaceEvent.observable),
    ], reason: 'nothing is replayed on resumption');

    await network.dispose();
  });

  test('dispose leaves nothing registered with the platform', () async {
    final backend = _FakeGeofenceBackend();
    final network = await buildNetwork(backend: backend);

    await network.declarePlace('home', 100);
    await network.declarePlace('office', 250);
    expect(backend.live, hasLength(2));

    await network.dispose();

    expect(backend.live, isEmpty,
        reason: 'platforms bound how many geofences may be registered');
  });

  test('the place functions need a platform geofencing binding', () async {
    final network = await buildNetwork();

    expect(() => network.declarePlace('home', 100), throwsStateError);
    expect(() => network.removePlace('home'), throwsStateError);
    expect(() => network.onPlaceEvent = (_, __) {}, throwsStateError);
    expect(network.declaredPlaces, isEmpty);

    await network.dispose();
  });
}
