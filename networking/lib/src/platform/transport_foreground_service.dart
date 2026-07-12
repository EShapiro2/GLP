import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

/// Controls the Android foreground service that keeps the app process
/// unfrozen while the transport stack runs.
///
/// Modern Android freezes cached (backgrounded / screen-off) processes
/// within minutes. A frozen Dart VM stops sending ANNOUNCEs (peers mark us
/// stale), stops processing scan results (the reverse BLE leg of a
/// dual-role pair never dials), and stalls UDP keepalives — while the radio
/// links themselves stay up, so peers' Redux view drifts away from the
/// actual link state. The foreground service (Android-side:
/// `TransportForegroundService.kt`, `connectedDevice` type) exempts the
/// process from freezing for the lifetime of the transport stack.
///
/// No-op on every platform but Android. Safe to call without the native
/// side present (unit tests): a missing channel handler is swallowed.
class TransportForegroundService {
  static const MethodChannel _channel =
      MethodChannel('grassroots/foreground_service');

  /// Start (or restart) the foreground service. Call when the transport
  /// stack starts; idempotent on the native side.
  static Future<void> start() => _invoke('start');

  /// Stop the foreground service and remove its notification. Call when the
  /// transport stack stops.
  static Future<void> stop() => _invoke('stop');

  static Future<void> _invoke(String method) async {
    if (kIsWeb || defaultTargetPlatform != TargetPlatform.android) return;
    try {
      await _channel.invokeMethod<void>(method);
    } on MissingPluginException {
      // No native handler (unit tests, non-app embeddings) — nothing to do.
    } on PlatformException catch (e) {
      debugPrint('[foreground-service] $method failed: ${e.message}');
    }
  }
}
