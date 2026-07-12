@TestOn('vm')
library;

import 'package:flutter/foundation.dart'
    show TargetPlatform, debugDefaultTargetPlatformOverride;
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/platform/transport_foreground_service.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();
  const channel = MethodChannel('grassroots/foreground_service');

  tearDown(() {
    debugDefaultTargetPlatformOverride = null;
    TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger
        .setMockMethodCallHandler(channel, null);
  });

  test('start/stop invoke the platform channel on Android', () async {
    debugDefaultTargetPlatformOverride = TargetPlatform.android;
    final invoked = <String>[];
    TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger
        .setMockMethodCallHandler(channel, (call) async {
      invoked.add(call.method);
      return null;
    });

    await TransportForegroundService.start();
    await TransportForegroundService.stop();

    expect(invoked, ['start', 'stop']);
  });

  test('no-op on non-Android platforms', () async {
    debugDefaultTargetPlatformOverride = TargetPlatform.iOS;
    final invoked = <String>[];
    TestDefaultBinaryMessengerBinding.instance.defaultBinaryMessenger
        .setMockMethodCallHandler(channel, (call) async {
      invoked.add(call.method);
      return null;
    });

    await TransportForegroundService.start();
    await TransportForegroundService.stop();

    expect(invoked, isEmpty,
        reason: 'The foreground service is an Android cached-app-freezing '
            'countermeasure; other platforms must not be touched.');
  });

  test('missing native handler is swallowed (unit-test environments)',
      () async {
    debugDefaultTargetPlatformOverride = TargetPlatform.android;
    // No mock handler registered → MissingPluginException internally.
    await expectLater(TransportForegroundService.start(), completes);
    await expectLater(TransportForegroundService.stop(), completes);
  });
}
