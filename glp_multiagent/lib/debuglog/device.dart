/// What a run reports first about the build and the device
/// (GLP/phones/docs/debugging.md, "The phone side", item 2).
///
/// The build is named by GLP main's commit, which tool/sync_glp_assets.sh
/// writes into the app's assets, `assets/glp/build.txt`, when it builds the
/// bundle before every build.  The device is named by its model identifier,
/// as `iPhone12,3`, read from the kernel (`sysctlbyname` of `hw.machine` on
/// iOS and of `hw.model` on macOS), and by its operating system and version.
library;

import 'dart:ffi';
import 'dart:io';

import 'package:ffi/ffi.dart';
import 'package:flutter/services.dart' show rootBundle;

/// The asset that names the build.
const String buildAsset = 'assets/glp/build.txt';

/// GLP main's commit the app was built from, as the assets hold it; `unknown`
/// where they hold none.
Future<String> buildCommit() async {
  try {
    final s = (await rootBundle.loadString(buildAsset)).trim();
    return s.isEmpty ? 'unknown' : s;
  } on Object catch (_) {
    return 'unknown';
  }
}

typedef _SysctlC = Int32 Function(
    Pointer<Utf8> name, Pointer<Void> old, Pointer<Size> oldLen,
    Pointer<Void> newValue, Size newLen);
typedef _SysctlDart = int Function(
    Pointer<Utf8> name, Pointer<Void> old, Pointer<Size> oldLen,
    Pointer<Void> newValue, int newLen);

/// The device's model identifier; `unknown` where the platform gives none.
String deviceModel() {
  final String key;
  if (Platform.isIOS) {
    key = 'hw.machine';
  } else if (Platform.isMacOS) {
    key = 'hw.model';
  } else {
    return 'unknown';
  }
  try {
    final sysctl = DynamicLibrary.process()
        .lookupFunction<_SysctlC, _SysctlDart>('sysctlbyname');
    return using((arena) {
      final name = key.toNativeUtf8(allocator: arena);
      final len = arena<Size>();
      if (sysctl(name, nullptr, len, nullptr, 0) != 0) return 'unknown';
      final buf = arena<Uint8>(len.value);
      if (sysctl(name, buf.cast(), len, nullptr, 0) != 0) return 'unknown';
      return buf.cast<Utf8>().toDartString();
    });
  } on Object catch (_) {
    return 'unknown';
  }
}

/// The device's facts: its model, its operating system and that system's
/// version.
Map<String, Object?> deviceFacts() => {
      'model': deviceModel(),
      'os': Platform.operatingSystem,
      'osVersion': Platform.operatingSystemVersion,
    };
