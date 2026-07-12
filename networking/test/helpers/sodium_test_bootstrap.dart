import 'dart:convert';
import 'dart:ffi';
import 'dart:io' show File, Platform;

import 'package:sodium/sodium.dart' as sodium;
import 'package:sodium/sodium_sumo.dart' as sodium_sumo;
import 'package:sodium_libs/sodium_libs.dart' as sodium_libs
    show SodiumPlatform;
import 'package:sodium_libs/sodium_libs_sumo.dart' as sodium_libs_sumo
    show SodiumSumoInit;

bool _registeredSodiumPlatform = false;

/// Registers the sodium platform plugin in plain Flutter test isolates and
/// returns a SUMO handle (needed for the Ed25519↔X25519 conversion the Noise
/// session manager performs). A [sodium_sumo.SodiumSumo] is also a
/// [sodium.Sodium], so callers needing only the base API can hold it as one.
///
/// App launches normally get this through Flutter's generated plugin
/// registrant. These unit tests call [SodiumSumoInit.init] directly, so they
/// need to install the host platform implementation first.
Future<sodium_sumo.SodiumSumo> initTestSodium() async {
  if (!_registeredSodiumPlatform) {
    sodium_libs.SodiumPlatform.instance = _TestSodiumPlatform();
    _registeredSodiumPlatform = true;
  }

  return sodium_libs_sumo.SodiumSumoInit.init();
}

class _TestSodiumPlatform extends sodium_libs.SodiumPlatform {
  @override
  Future<sodium.Sodium> loadSodium() =>
      sodium.SodiumInit.init(_openBundledLibSodium);

  @override
  Future<sodium_sumo.SodiumSumo> loadSodiumSumo() =>
      sodium_sumo.SodiumSumoInit.init(_openBundledLibSodium);
}

Future<DynamicLibrary> _openBundledLibSodium() async =>
    DynamicLibrary.open(_bundledLibSodiumPath());

String _bundledLibSodiumPath() {
  if (Platform.isMacOS) {
    return _packagePath(
      'sodium_libs',
      'darwin/Libraries/libsodium.xcframework/'
          'macos-arm64_arm64e_x86_64/libsodium.framework/Versions/A/libsodium',
    );
  }

  if (Platform.isLinux) {
    return 'libsodium.so';
  }

  if (Platform.isWindows) {
    return 'libsodium.dll';
  }

  throw UnsupportedError(
    'No sodium_libs test bootstrap for ${Platform.operatingSystem}',
  );
}

String _packagePath(String packageName, String relativePath) {
  final packageConfig = File('.dart_tool/package_config.json');
  final packages =
      jsonDecode(packageConfig.readAsStringSync()) as Map<String, Object?>;
  final package = (packages['packages'] as List<Object?>)
      .cast<Map<String, Object?>>()
      .singleWhere((entry) => entry['name'] == packageName);

  final rootUri = Uri.parse(package['rootUri'] as String);
  final resolvedRootUri = rootUri.hasScheme
      ? rootUri
      : packageConfig.parent.uri.resolveUri(rootUri);
  final baseUri = resolvedRootUri.path.endsWith('/')
      ? resolvedRootUri
      : resolvedRootUri.replace(path: '${resolvedRootUri.path}/');

  return baseUri.resolve(relativePath).toFilePath();
}
