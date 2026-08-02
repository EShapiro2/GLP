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

  final StreamController<bool> _observability =
      StreamController<bool>.broadcast();

  @override
  Stream<PlaceCrossingReport> get crossings => _crossings.stream;

  @override
  Stream<bool> get observability => _observability.stream;

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
    await _observability.close();
  }

  void report(String registrationId, PlaceCrossing crossing) {
    _crossings.add(PlaceCrossingReport(registrationId, crossing));
  }

  /// The platform stops (false) or resumes (true) reporting crossings.
  void reportObservability(bool observable) {
    _observability.add(observable);
  }

  String get lastRegistrationId => registered.last.registrationId;
}

void main() {
  late FakeGeofenceBackend backend;
  late PlaceRegistry registry;
  late List<(String, PlaceEvent)> events;

  setUp(() {
    backend = FakeGeofenceBackend();
    registry = PlaceRegistry(backend: backend);
    events = [];
    registry.onPlaceEvent = (place, event) => events.add((place, event));
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
        ('home', PlaceEvent.entered),
        ('home', PlaceEvent.exited),
      ]);
    });

    test('a crossing of an undeclared place is dropped', () async {
      backend.report('place-never-registered', PlaceCrossing.entered);
      await settle();
      expect(events, isEmpty);
    });
  });

  /// Spec §System Predicates: a declaration's stream carries `unobservable`
  /// when the platform stops reporting crossings and `observable` when it
  /// resumes, and between the two no crossing is seen and none is delivered
  /// late. Observability is not a property of a registration — whatever stops
  /// the platform stops it for every declaration at once.
  group('observability', () {
    test('a change reaches every standing declaration', () async {
      await registry.declarePlace('home', 100);
      await registry.declarePlace('office', 250);
      await registry.declarePlace('gym', 50);

      backend.reportObservability(false);
      await settle();

      expect(events, hasLength(3));
      expect(events.map((e) => e.$1), containsAll(['home', 'office', 'gym']));
      expect(
        events.every((e) => e.$2 == PlaceEvent.unobservable),
        isTrue,
        reason: 'the platform stopped reporting for all of them at once',
      );

      events.clear();
      backend.reportObservability(true);
      await settle();

      expect(events, hasLength(3));
      expect(
        events.every((e) => e.$2 == PlaceEvent.observable),
        isTrue,
      );
    });

    test('a repeated report of the same state emits nothing', () async {
      await registry.declarePlace('home', 100);

      backend.reportObservability(true);
      await settle();
      expect(events, isEmpty,
          reason: 'a registry is observable at construction');

      backend.reportObservability(false);
      backend.reportObservability(false);
      await settle();

      expect(events, [('home', PlaceEvent.unobservable)]);
    });

    test('crossings are dropped while unobservable', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;

      backend.reportObservability(false);
      await settle();
      events.clear();

      backend.report(id, PlaceCrossing.entered);
      backend.report(id, PlaceCrossing.exited);
      await settle();

      expect(events, isEmpty);
    });

    test('nothing is replayed when reporting resumes', () async {
      await registry.declarePlace('home', 100);
      final id = backend.lastRegistrationId;

      backend.reportObservability(false);
      await settle();
      backend.report(id, PlaceCrossing.entered);
      await settle();
      events.clear();

      backend.reportObservability(true);
      await settle();

      expect(events, [('home', PlaceEvent.observable)],
          reason: 'the crossing that arrived while unobservable is not '
              'delivered late');

      // And the place is live again: the next crossing is reported normally.
      backend.report(id, PlaceCrossing.exited);
      await settle();

      expect(events, [
        ('home', PlaceEvent.observable),
        ('home', PlaceEvent.exited),
      ]);
    });

    test('a place declared while unobservable is told so at once', () async {
      backend.reportObservability(false);
      await settle();
      events.clear();

      await registry.declarePlace('home', 100);

      expect(events, [('home', PlaceEvent.unobservable)],
          reason: 'a program must never take silence on a fresh stream for '
              '"nothing has happened yet"');

      // It joins the fan-out from then on.
      events.clear();
      backend.reportObservability(true);
      await settle();
      expect(events, [('home', PlaceEvent.observable)]);
    });

    test('a removed place receives neither kind', () async {
      await registry.declarePlace('home', 100);
      await registry.declarePlace('office', 250);
      await registry.removePlace('home');
      events.clear();

      backend.reportObservability(false);
      await settle();
      backend.reportObservability(true);
      await settle();

      expect(events, [
        ('office', PlaceEvent.unobservable),
        ('office', PlaceEvent.observable),
      ]);
      expect(
        events.any((e) => e.$1 == 'home'),
        isFalse,
        reason: 'the declaration is over; nothing reopens it',
      );
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

      expect(events, [('home', PlaceEvent.entered)],
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

      expect(events, [('home', PlaceEvent.entered)]);
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
