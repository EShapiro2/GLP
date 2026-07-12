/// Pure-Dart stand-ins for the Flutter `foundation.dart` helpers the core
/// uses, so the same source files serve the Flutter package and headless
/// (server) embeddings without a Flutter dependency.
library;

export 'package:meta/meta.dart' show immutable, visibleForTesting;

/// Signature of [debugPrint].
typedef DebugPrintCallback = void Function(String? message, {int? wrapWidth});

/// Prints a message to the console. Assignable, like Flutter's
/// `debugPrint`, so embedders can redirect or silence layer logging;
/// defaults to [print]. When the core runs inside the Flutter package the
/// default still reaches the device console via stdout.
DebugPrintCallback debugPrint = _defaultDebugPrint;

void _defaultDebugPrint(String? message, {int? wrapWidth}) {
  // ignore: avoid_print
  print(message ?? '');
}

/// Element-wise equality for lists (semantics of Flutter's `listEquals`).
bool listEquals<T>(List<T>? a, List<T>? b) {
  if (a == null) return b == null;
  if (b == null || a.length != b.length) return false;
  if (identical(a, b)) return true;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

/// Entry-wise equality for maps (semantics of Flutter's `mapEquals`).
bool mapEquals<K, V>(Map<K, V>? a, Map<K, V>? b) {
  if (a == null) return b == null;
  if (b == null || a.length != b.length) return false;
  if (identical(a, b)) return true;
  for (final key in a.keys) {
    if (!b.containsKey(key) || b[key] != a[key]) return false;
  }
  return true;
}

/// Element-wise equality for sets (semantics of Flutter's `setEquals`).
bool setEquals<T>(Set<T>? a, Set<T>? b) {
  if (a == null) return b == null;
  if (b == null || a.length != b.length) return false;
  if (identical(a, b)) return true;
  return a.containsAll(b);
}
