import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart'
    show shouldColdStartAfterSettingsChange;

/// Regression tests for the bootstrap wedge: when the first initialize() finds
/// no usable transport (e.g. BLE permission denied + UDP off) it returns
/// without calling start(), so _started stays false. A later settings change
/// that brings a transport up must trigger a full cold start — otherwise the
/// transport is live but inert (no advertising/scanning/timers) until restart.
void main() {
  group('shouldColdStartAfterSettingsChange', () {
    test('cold-starts when never started and a transport just came up', () {
      // BLE only.
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: false,
          startedNow: false,
          bleAvailable: true,
          udpAvailable: false,
        ),
        isTrue,
      );
      // UDP only.
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: false,
          startedNow: false,
          bleAvailable: false,
          udpAvailable: true,
        ),
        isTrue,
      );
      // Both.
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: false,
          startedNow: false,
          bleAvailable: true,
          udpAvailable: true,
        ),
        isTrue,
      );
    });

    test('does not cold-start on the warm path (already started)', () {
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: true,
          startedNow: true,
          bleAvailable: true,
          udpAvailable: true,
        ),
        isFalse,
      );
    });

    test('does not cold-start when no transport is available', () {
      // e.g. a disable-only settings change, or a transport that failed to
      // initialize — nothing usable, so nothing to start.
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: false,
          startedNow: false,
          bleAvailable: false,
          udpAvailable: false,
        ),
        isFalse,
      );
    });

    test('does not re-start if start already ran during this change', () {
      // Defensive: if an earlier branch in the same settings change already
      // flipped _started, the live startedNow guard prevents a second start().
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: true,
          wasStarted: false,
          startedNow: true,
          bleAvailable: true,
          udpAvailable: true,
        ),
        isFalse,
      );
    });

    test('does not cold-start when autoStart is off (manual-start contract)',
        () {
      // With autoStart off the caller drives start() itself; an unrelated
      // settings change must not start the stack on its behalf.
      expect(
        shouldColdStartAfterSettingsChange(
          autoStart: false,
          wasStarted: false,
          startedNow: false,
          bleAvailable: true,
          udpAvailable: true,
        ),
        isFalse,
      );
    });
  });
}
