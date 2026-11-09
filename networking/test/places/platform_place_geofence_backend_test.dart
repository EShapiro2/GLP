import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';

import 'package:grassroots_networking/src/places/platform_place_geofence_backend.dart';
import 'package:grassroots_networking_core/src/places/place_registry.dart';

/// The platform binding behind the place predicates (spec §System Predicates),
/// over mocked channels.
///
/// What is testable off a device is the seam itself: that a refusal is
/// reported as a refusal rather than thrown, that a crossing arrives as the
/// registration it belongs to and the direction it went, that observability
/// arrives as a bool, and that a malformed platform message is dropped rather
/// than killing the stream every declaration depends on. Whether a geofence
/// actually fires is a device matter and is not claimed here.
void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  late List<MethodCall> calls;
  late MockStreamHandlerEventSink crossingSink;
  late MockStreamHandlerEventSink observabilitySink;
  /// The native side's answer to `register`: the centre registered, as a map
  /// of latitude and longitude, or null for a refusal.
  Map<String, Object?>? registerAnswer;
  Object? registerThrows;

  /// The device's location, which the native side answers for a registration
  /// about no centre.
  const here = {'latitude': 32.11, 'longitude': 34.80};
  const hereCentre = PlaceCentre(latitude: 32.11, longitude: 34.80);

  final messenger =
      TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger;

  setUp(() {
    calls = [];
    registerAnswer = here;
    registerThrows = null;

    messenger.setMockMethodCallHandler(placeGeofenceMethodChannel,
        (call) async {
      calls.add(call);
      if (call.method == 'register') {
        if (registerThrows != null) throw registerThrows!;
        return registerAnswer;
      }
      return null;
    });

    messenger.setMockStreamHandler(
      placeGeofenceCrossingChannel,
      MockStreamHandler.inline(
        onListen: (arguments, events) => crossingSink = events,
      ),
    );
    messenger.setMockStreamHandler(
      placeGeofenceObservabilityChannel,
      MockStreamHandler.inline(
        onListen: (arguments, events) => observabilitySink = events,
      ),
    );
  });

  tearDown(() {
    messenger.setMockMethodCallHandler(placeGeofenceMethodChannel, null);
    messenger.setMockStreamHandler(placeGeofenceCrossingChannel, null);
    messenger.setMockStreamHandler(placeGeofenceObservabilityChannel, null);
  });

  test('register passes the registration and the radius, and the centre '
      'where one is given, and returns the centre the platform registered',
      () async {
    // Spec §Geofencing: declarePlace "returns the centre it registered, the
    // device's location where centre is null".
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    expect(await backend.register('place-0', null, 150), hereCentre);
    expect(calls.single.method, 'register');
    expect(calls.single.arguments, {
      'registrationId': 'place-0',
      'radiusMetres': 150.0,
    }, reason: 'no centre given, none is passed: the device\'s location');

    const kept = PlaceCentre(latitude: 51.5, longitude: -0.12);
    registerAnswer = {'latitude': 51.5, 'longitude': -0.12};
    expect(await backend.register('place-1', kept, 50), kept);
    expect(calls.last.arguments, {
      'registrationId': 'place-1',
      'radiusMetres': 50.0,
      'latitude': 51.5,
      'longitude': -0.12,
    });

    registerAnswer = null;
    expect(await backend.register('place-2', null, 50), isNull,
        reason: 'a platform refusal is a refusal, not an exception');
  });

  test('a malformed answer to register is a refusal', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    registerAnswer = {'latitude': 'north'};
    expect(await backend.register('place-0', null, 150), isNull);
  });

  test('a platform exception on register is a refusal', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    registerThrows = PlatformException(code: 'bad-arguments');
    expect(await backend.register('place-0', null, 150), isNull);
  });

  test('an embedding with no native binding refuses every registration',
      () async {
    messenger.setMockMethodCallHandler(placeGeofenceMethodChannel, null);
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    // This is the headless and unit-test case: the three place functions are
    // present, nothing is behind them, and a declaration is refused.
    expect(await backend.register('place-0', null, 150), isNull);
  });

  test('unregister names the registration', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    await backend.register('place-0', null, 150);
    await backend.unregister('place-0');

    expect(calls.last.method, 'unregister');
    expect(calls.last.arguments, {'registrationId': 'place-0'});
  });

  test('crossings arrive as the registration and the direction', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    final seen = <PlaceCrossingReport>[];
    backend.crossings.listen(seen.add);
    await Future<void>.delayed(Duration.zero);

    crossingSink.success({
      'registrationId': 'place-0',
      'crossing': 'entered',
    });
    crossingSink.success({
      'registrationId': 'place-0',
      'crossing': 'exited',
    });
    await Future<void>.delayed(Duration.zero);

    expect(seen, [
      const PlaceCrossingReport('place-0', PlaceCrossing.entered),
      const PlaceCrossingReport('place-0', PlaceCrossing.exited),
    ]);
  });

  test('a malformed crossing is dropped and the stream survives it', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    final seen = <PlaceCrossingReport>[];
    backend.crossings.listen(seen.add);
    await Future<void>.delayed(Duration.zero);

    crossingSink.success('not a crossing');
    crossingSink.success({'registrationId': 'place-0'});
    crossingSink.success({'registrationId': 'place-0', 'crossing': 'dwelt'});
    crossingSink.success({
      'registrationId': 'place-0',
      'crossing': 'entered',
    });
    await Future<void>.delayed(Duration.zero);

    expect(seen, [
      const PlaceCrossingReport('place-0', PlaceCrossing.entered),
    ]);
  });

  test('observability arrives as it is reported', () async {
    final backend = PlatformPlaceGeofenceBackend();
    addTearDown(backend.dispose);

    final seen = <bool>[];
    backend.observability.listen(seen.add);
    await Future<void>.delayed(Duration.zero);

    observabilitySink.success(false);
    observabilitySink.success(true);
    await Future<void>.delayed(Duration.zero);

    expect(seen, [false, true]);
  });

  test('dispose releases the platform and closes the streams', () async {
    final backend = PlatformPlaceGeofenceBackend();

    backend.crossings.listen((_) {});
    await backend.register('place-0', null, 150);
    await backend.dispose();

    expect(calls.last.method, 'dispose',
        reason: 'nothing is left registered once the layer is done with it');
    // Disposed twice is harmless, and registration after disposal is refused.
    await backend.dispose();
    expect(await backend.register('place-1', null, 150), isNull);
  });
}
