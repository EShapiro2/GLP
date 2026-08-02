/// Place declarations: the layer half of the place predicates.
///
/// Spec `docs/GLP_Networking_API/sections/system-predicates.tex` §System
/// Predicates. GLP cannot reach the platform's location service, so the layer
/// registers a circular geofence around the device's current location under a
/// name GLP supplies, and reports the crossings the platform observes. GLP
/// supplies a name and a radius and receives no coordinates: it learns of the
/// device's location only that it entered or left a place it declared itself.
///
/// Registering `place_declare/3` and `place_remove/1` as GLP system predicates
/// is not the layer's — that is the GLP runtime's, above this seam. What the
/// layer owns is what is here: the declaration, its release, and which
/// crossings are reported.
library;

import 'dart:async';

import '../platform/compat.dart';

/// A crossing of a declared place's boundary.
///
/// This is what the platform reports against one registration, and it is
/// deliberately narrower than [PlaceEvent]: observability is not a property of
/// a registration. The application loses the foreground, or a permission is
/// withdrawn, and that is true of every declaration at once — so the platform
/// reports crossings per registration and observability across all of them.
/// Two types is the honest model of that, not a compatibility shim.
enum PlaceCrossing {
  /// The device entered the place.
  entered,

  /// The device left the place.
  exited,
}

/// What a declaration's stream carries.
///
/// Spec `docs/GLP_Networking_API/sections/system-predicates.tex`
/// §System Predicates: `place_declare(Place, Radius, Events)` binds `Events` to
/// a stream carrying `entered` and `exited` as the platform reports crossings,
/// `unobservable` when the platform stops reporting them, and `observable` when
/// reporting resumes. Between the two, crossings are not seen and none is
/// delivered late.
enum PlaceEvent {
  /// The device entered the place.
  entered,

  /// The device left the place.
  exited,

  /// The platform has stopped reporting crossings. Until [observable], this
  /// place is not being watched — which is what a program must be told, since
  /// silence is otherwise indistinguishable from having crossed nothing.
  unobservable,

  /// The platform has resumed reporting crossings. Nothing that happened while
  /// unobservable is replayed: the guarantee is that between the two no
  /// crossing is seen and none is delivered late.
  observable,
}

/// The event a crossing becomes on a declaration's stream.
PlaceEvent _eventOf(PlaceCrossing crossing) => switch (crossing) {
      PlaceCrossing.entered => PlaceEvent.entered,
      PlaceCrossing.exited => PlaceEvent.exited,
    };

/// A crossing the platform reports, against the registration it was
/// registered under.
///
/// The registration, not the place name, is what the platform reports against:
/// a place that was removed and declared again is a different registration, so
/// a crossing of the earlier one can be told apart and dropped.
@immutable
class PlaceCrossingReport {
  final String registrationId;
  final PlaceCrossing crossing;

  const PlaceCrossingReport(this.registrationId, this.crossing);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PlaceCrossingReport &&
          runtimeType == other.runtimeType &&
          registrationId == other.registrationId &&
          crossing == other.crossing;

  @override
  int get hashCode => Object.hash(registrationId, crossing);

  @override
  String toString() => 'PlaceCrossingReport($registrationId, ${crossing.name})';
}

/// The platform's geofencing, as the layer needs it.
///
/// Registering a geofence needs the device's current location, which only the
/// platform has; this is the whole of what the layer asks of it. Kept as an
/// interface because the platform binding is a plugin dependency and a
/// user-facing location permission, and because the layer's own behaviour —
/// supersession, release, and which crossings are reported — is decided here,
/// not there.
abstract class PlaceGeofenceBackend {
  /// Register a circular geofence of [radiusMetres] around the device's
  /// current location, under [registrationId]. Returns false when the platform
  /// refuses — no location fix, permission withheld, or the platform's cap on
  /// registered geofences reached.
  Future<bool> register(String registrationId, double radiusMetres);

  /// Unregister [registrationId]. Unregistering one that is not registered
  /// does nothing.
  Future<void> unregister(String registrationId);

  /// Crossings the platform observes, against the registration they belong to.
  Stream<PlaceCrossingReport> get crossings;

  /// Whether the platform is reporting crossings at all: false when it stops —
  /// the application losing the foreground, a permission withdrawn — and true
  /// when it resumes.
  ///
  /// This is not per registration. Whatever stops the platform reporting stops
  /// it for every declaration at once, so it is reported once here and the
  /// registry fans it out to the places that are standing.
  Stream<bool> get observability;

  Future<void> dispose();
}

/// The declared places and their platform registrations.
class PlaceRegistry {
  PlaceRegistry({required PlaceGeofenceBackend backend}) : _backend = backend {
    _crossings = _backend.crossings.listen(_onCrossingReported);
    _observability = _backend.observability.listen(_onObservabilityChanged);
  }

  final PlaceGeofenceBackend _backend;
  late final StreamSubscription<PlaceCrossingReport> _crossings;
  late final StreamSubscription<bool> _observability;

  /// Whether the platform is currently reporting crossings. Observable at
  /// construction: a backend that never says otherwise is one that reports.
  bool _observable = true;

  /// Whether the platform is currently reporting crossings.
  bool get isObservable => _observable;

  /// The registration currently standing for each declared place.
  final Map<String, String> _registrationOf = {};

  /// The place each live registration belongs to. A registration that has been
  /// removed or superseded is absent, which is how its crossings are dropped.
  final Map<String, String> _placeOf = {};

  int _nextRegistrationId = 0;
  bool _disposed = false;

  /// Called for each event of a place that is still declared. Receives the
  /// place GLP named and the event; the runtime routes it onto the stream that
  /// place's declaration bound.
  void Function(String place, PlaceEvent event)? onPlaceEvent;

  /// The places currently declared.
  Iterable<String> get declaredPlaces => _registrationOf.keys;

  /// Whether [place] is currently declared.
  bool isDeclared(String place) => _registrationOf.containsKey(place);

  /// Register a geofence of [radiusMetres] around the device's current
  /// location under [place]. Returns false when the platform refuses, in which
  /// case [place] is left undeclared.
  ///
  /// A further declaration under the same name replaces the first: the earlier
  /// registration is unregistered before the new one is made, so the platform's
  /// cap on registered geofences is never pushed past by a replacement, and
  /// crossings of the earlier one are dropped from that moment.
  Future<bool> declarePlace(String place, double radiusMetres) async {
    if (_disposed) {
      throw StateError('declarePlace on a disposed PlaceRegistry');
    }
    if (!radiusMetres.isFinite || radiusMetres <= 0) {
      throw ArgumentError.value(
        radiusMetres,
        'radiusMetres',
        'a place radius must be a positive number of metres',
      );
    }

    await _releaseDeclaration(place);

    final registrationId = 'place-${_nextRegistrationId++}';
    final registered = await _backend.register(registrationId, radiusMetres);
    if (!registered) {
      debugPrint('[place] Platform refused the geofence for "$place"');
      return false;
    }

    _registrationOf[place] = registrationId;
    _placeOf[registrationId] = place;

    // A place declared while the platform is not reporting is told so at once,
    // so a program never takes silence on a fresh stream for "nothing has
    // happened yet" when the truth is that nothing is being watched.
    if (!_observable) {
      onPlaceEvent?.call(place, PlaceEvent.unobservable);
    }
    return true;
  }

  /// End the declaration of [place]: the geofence is unregistered with the
  /// platform. Removing a place that is not declared does nothing.
  Future<void> removePlace(String place) async {
    if (_disposed) return;
    await _releaseDeclaration(place);
  }

  /// Release every declaration. Platforms bound how many geofences an
  /// application may register, so nothing is left registered once the layer is
  /// done with it.
  Future<void> dispose() async {
    if (_disposed) return;
    _disposed = true;
    await _crossings.cancel();
    await _observability.cancel();
    for (final registrationId in _registrationOf.values) {
      await _backend.unregister(registrationId);
    }
    _registrationOf.clear();
    _placeOf.clear();
    await _backend.dispose();
  }

  Future<void> _releaseDeclaration(String place) async {
    final registrationId = _registrationOf.remove(place);
    if (registrationId == null) return;
    _placeOf.remove(registrationId);
    await _backend.unregister(registrationId);
  }

  /// The platform started or stopped reporting crossings. Every standing
  /// declaration is told, because whatever stopped the platform stopped it for
  /// all of them at once.
  void _onObservabilityChanged(bool observable) {
    if (_disposed || observable == _observable) return;
    _observable = observable;
    final event = observable ? PlaceEvent.observable : PlaceEvent.unobservable;
    // Iterate a copy: a callback may declare or remove a place, and that would
    // otherwise mutate the map underneath us.
    for (final place in _registrationOf.keys.toList()) {
      onPlaceEvent?.call(place, event);
    }
  }

  void _onCrossingReported(PlaceCrossingReport report) {
    if (!_observable) {
      // The platform is not reporting, so anything arriving is from before it
      // stopped. It is dropped rather than delivered late: between unobservable
      // and observable, no crossing is seen and none is delivered late.
      debugPrint(
        '[place] Dropping ${report.crossing.name} for '
        '${report.registrationId} while unobservable',
      );
      return;
    }
    final place = _placeOf[report.registrationId];
    if (place == null) {
      // The platform's report of a crossing can race the unregistration, so a
      // crossing of a place already removed or superseded arrives after the
      // declaration ended. It is dropped: the declaration it belonged to is
      // over, and nothing reopens it.
      debugPrint(
        '[place] Dropping ${report.crossing.name} for the ended '
        'registration ${report.registrationId}',
      );
      return;
    }
    onPlaceEvent?.call(place, _eventOf(report.crossing));
  }
}
