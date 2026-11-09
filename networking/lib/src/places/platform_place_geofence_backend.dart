/// The platform half of the place predicates: the geofencing the layer's
/// [PlaceGeofenceBackend] asks of iOS and Android.
///
/// Spec `docs/GLP_Networking_API/sections/system-predicates.tex` §System
/// Predicates, and §Geofencing. GLP cannot reach the platform's location
/// service, so the layer registers a circular region about a centre under a
/// name GLP supplies, and reports the crossings the platform observes.  The
/// centre is the one the runtime gives, kept from the place's first
/// declaration, or the device's current location where it gives none, and the
/// native side answers with the centre it registered, which the runtime keeps
/// (GNA a1cb804).  GLP supplies a name and a radius and receives no position:
/// the centre crosses the seam to the runtime and reaches no program.
///
/// The layer's own behaviour — supersession, release, which crossings are
/// reported — is [PlaceRegistry]'s, above this. What is here is the platform
/// binding alone.
library;

import 'dart:async';

import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

import 'package:grassroots_networking_core/src/places/place_registry.dart';

/// The method channel the native side answers on.
const MethodChannel placeGeofenceMethodChannel =
    MethodChannel('grassroots/places');

/// Crossings the platform observes, against the registration they belong to.
const EventChannel placeGeofenceCrossingChannel =
    EventChannel('grassroots/places/crossings');

/// Whether the platform is reporting crossings at all.
const EventChannel placeGeofenceObservabilityChannel =
    EventChannel('grassroots/places/observability');

/// [PlaceGeofenceBackend] over the platform's location service — region
/// monitoring on iOS, `GeofencingClient` on Android.
///
/// Both platforms are driven under foreground location authorization only.
/// That is a standing decision of this project, not a platform limit, and it
/// is what the observability stream is for: reporting stops when the
/// application leaves the foreground or authorization is withdrawn, and
/// resumes when it returns. Between the two no crossing is seen and none is
/// delivered late, which the registry above enforces.
class PlatformPlaceGeofenceBackend implements PlaceGeofenceBackend {
  PlatformPlaceGeofenceBackend({
    MethodChannel methods = placeGeofenceMethodChannel,
    EventChannel crossingEvents = placeGeofenceCrossingChannel,
    EventChannel observabilityEvents = placeGeofenceObservabilityChannel,
  })  : _methods = methods,
        _crossingEvents = crossingEvents,
        _observabilityEvents = observabilityEvents;

  final MethodChannel _methods;
  final EventChannel _crossingEvents;
  final EventChannel _observabilityEvents;

  final StreamController<PlaceCrossingReport> _crossings =
      StreamController<PlaceCrossingReport>.broadcast();
  final StreamController<bool> _observability =
      StreamController<bool>.broadcast();

  StreamSubscription<dynamic>? _crossingSubscription;
  StreamSubscription<dynamic>? _observabilitySubscription;
  bool _listening = false;
  bool _disposed = false;

  @override
  Stream<PlaceCrossingReport> get crossings {
    _listen();
    return _crossings.stream;
  }

  @override
  Stream<bool> get observability {
    _listen();
    return _observability.stream;
  }

  /// Attach to the native event channels, once.
  ///
  /// Both streams are attached together: the registry takes them in the same
  /// breath, and a platform that answers one answers both.
  void _listen() {
    if (_listening || _disposed) return;
    _listening = true;

    _crossingSubscription = _crossingEvents.receiveBroadcastStream().listen(
      (event) {
        final report = _crossingFrom(event);
        if (report != null && !_crossings.isClosed) _crossings.add(report);
      },
      onError: (Object error) =>
          debugPrint('[place] Crossing channel error: $error'),
    );

    _observabilitySubscription =
        _observabilityEvents.receiveBroadcastStream().listen(
      (event) {
        if (event is bool && !_observability.isClosed) {
          _observability.add(event);
        }
      },
      onError: (Object error) =>
          debugPrint('[place] Observability channel error: $error'),
    );
  }

  /// Decode one native crossing. A malformed event is dropped with a log
  /// rather than thrown: the alternative is a platform message killing the
  /// stream every declaration depends on.
  PlaceCrossingReport? _crossingFrom(dynamic event) {
    if (event is! Map) {
      debugPrint('[place] Ignoring malformed crossing event: $event');
      return null;
    }
    final registrationId = event['registrationId'];
    final crossing = event['crossing'];
    if (registrationId is! String || crossing is! String) {
      debugPrint('[place] Ignoring malformed crossing event: $event');
      return null;
    }
    return switch (crossing) {
      'entered' =>
        PlaceCrossingReport(registrationId, PlaceCrossing.entered),
      'exited' => PlaceCrossingReport(registrationId, PlaceCrossing.exited),
      _ => () {
          debugPrint('[place] Ignoring unknown crossing "$crossing"');
          return null;
        }(),
    };
  }

  @override
  Future<PlaceCentre?> register(
      String registrationId, PlaceCentre? centre, double radiusMetres) async {
    if (_disposed) return null;
    _listen();
    try {
      // The native side answers with the centre it registered, as a map of
      // `latitude` and `longitude` in degrees, and with null where the
      // platform refuses.
      final registered =
          await _methods.invokeMethod<Map<Object?, Object?>>('register', {
        'registrationId': registrationId,
        'radiusMetres': radiusMetres,
        if (centre != null) 'latitude': centre.latitude,
        if (centre != null) 'longitude': centre.longitude,
      });
      return _centreFrom(registered);
    } on MissingPluginException {
      // No native binding under this embedding — the platform refuses, which
      // is what a declaration with nothing behind it must report.
      debugPrint('[place] No platform geofencing on this embedding');
      return null;
    } on PlatformException catch (e) {
      debugPrint('[place] Registration of $registrationId refused: ${e.code} '
          '${e.message}');
      return null;
    }
  }

  /// The centre a native answer names, or null where it names none or is
  /// malformed, which is a refusal: a registration about a centre the layer
  /// cannot name is one the runtime could not declare again.
  PlaceCentre? _centreFrom(Map<Object?, Object?>? answer) {
    if (answer == null) return null;
    final latitude = answer['latitude'];
    final longitude = answer['longitude'];
    if (latitude is! num || longitude is! num) {
      debugPrint('[place] Ignoring malformed registration answer: $answer');
      return null;
    }
    return PlaceCentre(
        latitude: latitude.toDouble(), longitude: longitude.toDouble());
  }

  @override
  Future<void> unregister(String registrationId) async {
    if (_disposed) return;
    try {
      await _methods.invokeMethod<void>('unregister', {
        'registrationId': registrationId,
      });
    } on MissingPluginException {
      // Nothing was registered, so nothing is left registered.
    } on PlatformException catch (e) {
      debugPrint('[place] Unregistering $registrationId failed: ${e.code} '
          '${e.message}');
    }
  }

  @override
  Future<void> dispose() async {
    if (_disposed) return;
    _disposed = true;
    await _crossingSubscription?.cancel();
    await _observabilitySubscription?.cancel();
    // A backstop for the registry's own release: platforms bound how many
    // regions an application may register, so nothing is left registered once
    // the layer is done with it.
    try {
      await _methods.invokeMethod<void>('dispose');
    } on MissingPluginException {
      // Nothing native to release.
    } on PlatformException catch (e) {
      debugPrint('[place] Platform release failed: ${e.code} ${e.message}');
    }
    await _crossings.close();
    await _observability.close();
  }
}
