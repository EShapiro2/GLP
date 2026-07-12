import 'dart:ffi';
import 'dart:io';

import 'package:sodium/sodium_sumo.dart';

/// Opens the system libsodium for FFI.
///
/// The layer needs the Ed25519↔X25519 birational map
/// (`crypto_sign_ed25519_{sk,pk}_to_curve25519`) to derive and verify Noise
/// static keys. `package:cryptography` cannot perform that conversion, so a
/// headless embedding links the native libsodium the same way the Flutter
/// client does (via `sodium_libs`), keeping the derived static byte-identical
/// across ends.
///
/// Override the search with the `LIBSODIUM_PATH` environment variable.
DynamicLibrary loadLibsodium() {
  final override = Platform.environment['LIBSODIUM_PATH'];
  final candidates = <String>[
    if (override != null && override.isNotEmpty) override,
    if (Platform.isLinux) ...[
      'libsodium.so.23',
      'libsodium.so',
      '/usr/lib/x86_64-linux-gnu/libsodium.so.23',
      '/usr/lib/aarch64-linux-gnu/libsodium.so.23',
    ],
    if (Platform.isMacOS) ...[
      'libsodium.dylib',
      '/usr/local/lib/libsodium.dylib',
      '/opt/homebrew/lib/libsodium.dylib',
    ],
    if (Platform.isWindows) ...['libsodium.dll', 'sodium.dll'],
  ];

  Object? lastError;
  for (final name in candidates) {
    try {
      return DynamicLibrary.open(name);
    } catch (e) {
      lastError = e;
    }
  }
  throw StateError(
    'Could not load libsodium. Install it (Debian: `apt-get install '
    'libsodium23`, macOS: `brew install libsodium`) or point '
    'LIBSODIUM_PATH at the shared library. Last error: $lastError',
  );
}

/// Loads libsodium and returns the initialized [SodiumSumo] instance a
/// headless embedding passes to the layer. (The Flutter package gets its
/// instance from `sodium_libs`' `SodiumSumoInit.init()` instead; both wrap
/// the same `package:sodium` types.)
Future<SodiumSumo> initHeadlessSodium() => SodiumSumoInit.init(loadLibsodium);
