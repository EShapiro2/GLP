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
enum PlaceCrossing {
  /// The device entered the place.
  entered,

  /// The device left the place.
  exited,
}

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

  Future<void> dispose();
}

/// The declared places and their platform registrations.
class PlaceRegistry {
  PlaceRegistry({required PlaceGeofenceBackend backend}) : _backend = backend {
    _crossings = _backend.crossings.listen(_onCrossingReported);
  }

  final PlaceGeofenceBackend _backend;
  late final StreamSubscription<PlaceCrossingReport> _crossings;

  /// The registration currently standing for each declared place.
  final Map<String, String> _registrationOf = {};

  /// The place each live registration belongs to. A registration that has been
  /// removed or superseded is absent, which is how its crossings are dropped.
  final Map<String, String> _placeOf = {};

  int _nextRegistrationId = 0;
  bool _disposed = false;

  /// Called for each crossing of a place that is still declared. Receives the
  /// place GLP named and which way the boundary was crossed; the runtime routes
  /// it onto the stream that place's declaration bound.
  void Function(String place, PlaceCrossing crossing)? onPlaceEvent;

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

  void _onCrossingReported(PlaceCrossingReport report) {
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
    onPlaceEvent?.call(place, report.crossing);
  }
}
