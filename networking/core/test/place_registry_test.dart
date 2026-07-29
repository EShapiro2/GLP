import 'dart:async';

import 'package:grassroots_networking_core/src/places/place_registry.dart';
import 'package:test/test.dart';

/// A stand-in for the platform's geofencing: it records what was registered
/// and unregistered, and lets a test report a crossing against any
/// registration — including one the layer has already ended, which is the race
/// the spec requires be dropped.
class FakeGeofenceBackend implements PlaceGeofenceBackend {
  final List<({String registrationId, double radiusMetres})> registered = [];
  final List<String> unregistered = [];
  final Set<String> live = {};

  /// When false, the platform refuses the next registration.
  bool accepts = true;
  bool disposed = false;

  final StreamController<PlaceCrossingReport> _crossings =
      StreamController<PlaceCrossingReport>.broadcast();

  @override
  Stream<PlaceCrossingReport> get crossings => _crossings.stream;

  @override
  Future<bool> register(String registrationId, double radiusMetres) async {
    if (!accepts) return false;
    registered.add((registrationId: registrationId, radiusMetres: radiusMetres));
    live.add(registrationId);
    return true;
  }

  @override
  Future<void> unregister(String registrationId) async {
    unregistered.add(registrationId);
    live.remove(registrationId);
  }

  @override
  Future<void> dispose() async {
    disposed = true;
    await _crossings.close();
  }

  void report(String registrationId, PlaceCrossing crossing) {
    _crossings.add(PlaceCrossingReport(registrationId, crossing));
  }

  String get lastRegistrationId => registered.last.registrationId;
}

void main() {
  late FakeGeofenceBackend backend;
  late PlaceRegistry registry;
  late List<(String, PlaceCrossing)> events;

  setUp(() {
    backend = FakeGeofenceBackend();
    registry = PlaceRegistry(backend: backend);
    events = [];
    registry.onPlaceEvent = (place, crossing) => events.add((place, crossing));
  });

  /// The crossing stream is a broadcast stream; let a reported crossing reach
  /// the registry before asserting.
  Future<void> settle() => Future<void>.delayed(Duration.zero);

  group('declarePlace', () {
    test('registers a geofence of the given radius', () async {
      expect(await registry.declarePlace('home', 100), isTrue);
      expect(backend.registered, hasLength(1));
      expect(backend.registered.single.radiusMetres, 100);
      expect(registry.isDeclared('home'), isTrue);
      expect(registry.declaredPlaces, ['home']);
    });

    test('a refused registration leaves the place undeclared', () async {
      backend.accepts = false;
      expect(await registry.declarePlace('home', 100), isFalse);
      expect(registry.isDeclared('home'), isFalse);
      expect(registry.declaredPlaces, isEmpty);
    });

    test('rejects a radius that is not a positive number of metres', () async {
      expect(() => registry.declarePlace('home', 0), throwsArgumentError);
      expect(() => registry.declarePlace('home', -5), throwsArgumentError);
      expect(() => registry.declarePlace('home', double.nan),
          throwsArgumentError);
      expect(() => registry.declarePlace('home', double.infinity),
          throwsArgumentError);
      expect(backend.registered, isEmpty);
    });

    test('distinct places are registered independently', () async {
      await registry.declarePlace('home', 100);
      await registry.declarePlace('office', 250);
      expect(registry.declaredPlaces, containsAll(['home', 'office']));
      expect(backend.live, hasLength(2));
    });
  });

  group('crossings', () {
    test('entry and exit of a declared place are reported', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;

      backend.report(id, PlaceCrossing.entered);
      backend.report(id, PlaceCrossing.exited);
      await settle();

      expect(events, [
        ('home', PlaceCrossing.entered),
        ('home', PlaceCrossing.exited),
      ]);
    });

    test('a crossing of an undeclared place is dropped', () async {
      backend.report('place-never-registered', PlaceCrossing.entered);
      await settle();
      expect(events, isEmpty);
    });
  });

  group('removePlace', () {
    test('unregisters the geofence and ends the declaration', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;

      await registry.removePlace('home');

      expect(backend.unregistered, [id]);
      expect(backend.live, isEmpty);
      expect(registry.isDeclared('home'), isFalse);
    });

    test('removing a place that is not declared does nothing', () async {
      await registry.removePlace('never-declared');
      expect(backend.unregistered, isEmpty);
      expect(backend.registered, isEmpty);
    });

    test('a crossing reported for a removed place is dropped', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;
      await registry.removePlace('home');

      // The platform's report can race the unregistration.
      backend.report(id, PlaceCrossing.exited);
      await settle();

      expect(events, isEmpty);
    });

    test('a removed place can be declared again, as a new registration',
        () async {
      await registry.declarePlace('home', 100);
      final first = backend.lastRegistrationId;
      await registry.removePlace('home');
      await registry.declarePlace('home', 100);
      final second = backend.lastRegistrationId;

      expect(second, isNot(first));

      backend.report(first, PlaceCrossing.entered);
      backend.report(second, PlaceCrossing.entered);
      await settle();

      expect(events, [('home', PlaceCrossing.entered)],
          reason: 'only the standing registration is reported');
    });
  });

  group('supersession', () {
    test('a further declaration replaces the first', () async {
      await registry.declarePlace('home', 100);
      final first = backend.lastRegistrationId;
      await registry.declarePlace('home', 250);
      final second = backend.lastRegistrationId;

      expect(second, isNot(first));
      expect(backend.unregistered, [first],
          reason: 'the earlier geofence is released, not left registered');
      expect(backend.live, {second});
      expect(registry.declaredPlaces, ['home']);
      expect(backend.registered.last.radiusMetres, 250);
    });

    test('a crossing of the superseded registration is dropped', () async {
      await registry.declarePlace('home', 100);
      final first = backend.lastRegistrationId;
      await registry.declarePlace('home', 250);

      backend.report(first, PlaceCrossing.exited);
      await settle();

      expect(events, isEmpty);
    });

    test('the replacement reports its own crossings', () async {
      await registry.declarePlace('home', 100);
      await registry.declarePlace('home', 250);
      final second = backend.lastRegistrationId;

      backend.report(second, PlaceCrossing.entered);
      await settle();

      expect(events, [('home', PlaceCrossing.entered)]);
    });
  });

  group('release', () {
    test('dispose unregisters every standing declaration', () async {
      await registry.declarePlace('home', 100);
      await registry.declarePlace('office', 250);

      await registry.dispose();

      expect(backend.live, isEmpty,
          reason: 'nothing may be left registered with the platform');
      expect(backend.unregistered, hasLength(2));
      expect(backend.disposed, isTrue);
    });

    test('dispose is idempotent', () async {
      await registry.declarePlace('home', 100);
      await registry.dispose();
      await registry.dispose();
      expect(backend.unregistered, hasLength(1));
    });

    test('declaring after dispose is an error', () async {
      await registry.dispose();
      expect(() => registry.declarePlace('home', 100), throwsStateError);
    });

    test('a crossing after dispose is dropped', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;
      await registry.dispose();

      // The backend's stream is closed by dispose; nothing can arrive, and
      // nothing is reported.
      expect(() => backend.report(id, PlaceCrossing.entered), throwsStateError);
      expect(events, isEmpty);
    });
  });
}
