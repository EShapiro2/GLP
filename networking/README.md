# The Grassroots networking layer

This directory is the networking layer of Grassroots: the transport by which madGLP agents reach one another.  It is specified by GNA, the GLP Networking API paper (repository `GLP-Networking-API`, on disc at `/Users/udi/Grassroots/GNA`).  The paper is the specification and this directory its implementation; where the two differ, the paper governs.  The record of the implementation against the paper --- conformance, known deviations, what is not yet implemented --- is GNA's `docs/implementation-notes.md`, and the rules for working here are `CLAUDE.md`.

The interface the layer offers the runtime is GNA Section 2; the proximity underlays, PAN and LAN, are Section 3; the WAN is Section 4; the system predicates over the layer are Section 5; the rendezvous server is Section 6.

The code was subtree-imported from Dan Bachar's `grassroots-networking`.

## Packages

| Directory | Package | What it holds |
|---|---|---|
| `core/` | `grassroots_networking_core` | Pure Dart, no Flutter.  Identity, the wire format, Noise sessions, the message protocol, routing, the Redux store, the UDP transport, signaling and peer links, attestation verification, the place registry; and the headless profile, `HeadlessGrassrootsNetwork`, which runs the WAN alone (GNA Sections 2 and 2.2), with two command-line entry points, `bin/glp_net_service.dart` and `bin/glp_net_push.dart`. |
| `.` | `grassroots_networking` | The Flutter package: `GrassrootsNetwork`, the layer over all three underlays, with BLE (`lib/src/transport/ble_transport_service.dart`) and LAN discovery (`lib/src/transport/lan_discovery_service.dart`); the platform attestation producers and the geofencing backends (`android/`, `ios/`); and a demo app (`lib/main.dart`).  It depends on the core and re-exports it. |
| `glue/` | `grassroots_networking_glue` | The backend glue: the runtime's network interface, `GlpNetwork`, implemented over the layer --- `HeadlessGlpNetwork` over the headless profile, `GrassrootsGlpNetwork` over `GrassrootsNetwork`.  It depends on the core, the Flutter package and `glp_runtime`, so that neither the core nor its users carry the GLP runtime. |
| `bootstrap_anchor/` | `bootstrap_anchor` | The Dart rendezvous server.  It is retired: the GLP rendezvous agent, `programs/examples/rendezvous/`, replaces it within GNA's reconnection task, and the directory is deleted when that is done. |

## Building and testing

The core and the headless profile need the native libsodium: `brew install libsodium` on macOS, `apt-get install libsodium23` on Debian, or `LIBSODIUM_PATH` naming the shared library.  A test that needs it skips itself when it is absent.

Each package is tested in its own directory, by the runner its pubspec calls for.  The repository suite, `test/run_all_tests.sh`, does not run these tests.

| Package | Build | Test |
|---|---|---|
| core | `cd networking/core && dart pub get`; a server entry point compiles with `dart compile exe bin/glp_net_service.dart -o glp_net_service` | `cd networking/core && dart test` |
| Flutter package | `cd networking && flutter pub get`; the demo app runs with `flutter run` | `cd networking && flutter test` |
| glue | `cd networking/glue && flutter pub get` | `cd networking/glue && flutter test` (a Flutter test, since the package depends on the Flutter package) |
| bootstrap_anchor | `cd networking/bootstrap_anchor && dart pub get` | `cd networking/bootstrap_anchor && dart test` |

`integration_test/` holds on-device tests, run with `flutter test integration_test/<file>` against a simulator or a device; `e2e_agent_test.dart` coordinates its agents through the host blackboard, `dart run tool/e2e_blackboard.dart`, and its header names the roles.

## Platform setup

What an application must declare to run the layer, as the demo app declares it.

### Android

From `android/app/src/main/AndroidManifest.xml`:

- `INTERNET`, for the LAN and the WAN.
- `BLUETOOTH_SCAN` with `usesPermissionFlags="neverForLocation"`, `BLUETOOTH_CONNECT` and `BLUETOOTH_ADVERTISE`, for the PAN on Android 12 and later; `BLUETOOTH` and `BLUETOOTH_ADMIN` with `maxSdkVersion="30"` before it.
- `ACCESS_FINE_LOCATION` and `ACCESS_COARSE_LOCATION`, for the place functions on every version and for BLE scanning on Android 6 to 11.  `ACCESS_BACKGROUND_LOCATION` is deliberately absent: location is foreground-only.
- `FOREGROUND_SERVICE` and `FOREGROUND_SERVICE_CONNECTED_DEVICE`, with the service `.TransportForegroundService` declared with `foregroundServiceType="connectedDevice"`, which keeps the layer running when the application is in the background; and `POST_NOTIFICATIONS`, on Android 13 and later, for that service's notification.
- The receiver `.GeofenceBroadcastReceiver`, not exported, which receives the platform's geofence crossings.
- The feature `android.hardware.bluetooth_le`, required.

The demo app also declares `android.hardware.camera` as not required, for its own photo messages; the layer does not need it.

### iOS

From `ios/Runner/Info.plist`:

- `NSBluetoothAlwaysUsageDescription` and `NSBluetoothPeripheralUsageDescription`, for the PAN, and `UIBackgroundModes` with `bluetooth-central` and `bluetooth-peripheral`.
- `NSLocalNetworkUsageDescription` and `NSBonjourServices` with `_grassroots._udp`, for LAN discovery.
- `NSLocationWhenInUseUsageDescription`, for the place functions, while in use only.
- `GrassrootsAppIdentifierPrefix`, set to `$(AppIdentifierPrefix)`, from which the attestation producer forms the application identity.

The demo app's `NSCameraUsageDescription` and `NSPhotoLibraryUsageDescription` are for its own photo messages; the layer does not need them.  The iOS deployment target is 13.0 (`ios/Podfile`).
