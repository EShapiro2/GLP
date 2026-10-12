import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/settings_state.dart';
import 'package:grassroots_networking_core/src/store/settings_actions.dart';
import 'package:grassroots_networking_core/src/store/settings_reducer.dart';

/// A non-settings action used to verify the reducer ignores unknown actions.
class _UnknownAction extends SettingsAction {}

void main() {
  group('settingsReducer', () {
    group('default state', () {
      test('initial state runs both BLE roles and is closed on every underlay',
          () {
        const state = SettingsState.initial;

        expect(state.bleRoleMode, BleRoleMode.auto);
        expect(state.coldCallTrustLevels, SettingsState.closedOnEveryUnderlay);
      });

      test('unknown action returns the same state unchanged', () {
        const state = SettingsState.initial;
        final result = settingsReducer(state, _UnknownAction());

        expect(result, same(state));
      });
    });

    group('SetBleRoleModeAction', () {
      test('sets the role mode and leaves the trust levels untouched', () {
        final open = SettingsState.initial
            .withTrustLevel(ProximityUnderlay.lan, ColdCallTrustLevel.open);
        final result =
            settingsReducer(open, SetBleRoleModeAction(BleRoleMode.centralOnly));

        expect(result.bleRoleMode, BleRoleMode.centralOnly);
        expect(result.coldCallTrustLevels, open.coldCallTrustLevels);
      });
    });

    group('the persisted blob, read with no tolerance', () {
      test('round-trips the role mode and the levels', () {
        final state = SettingsState.initial
            .copyWith(bleRoleMode: BleRoleMode.peripheralOnly)
            .withTrustLevel(ProximityUnderlay.pan, ColdCallTrustLevel.open);
        expect(SettingsState.fromJson(state.toJson()), state);
        expect(state.toJson().keys.toSet(), {'bleRoleMode', 'coldCallTrustLevels'});
      });

      test('a blob carrying the transport toggles is malformed', () {
        // The underlays and the rendezvous servers are constructor arguments
        // since 2026-10-12 (GNA §Instantiation); a blob written before then
        // carries the toggles and the servers, and is discarded whole.
        final old = SettingsState.initial.toJson()
          ..['bluetoothEnabled'] = true
          ..['udpEnabled'] = true;
        expect(() => SettingsState.fromJson(old), throwsFormatException);
      });

      test('a blob carrying rendezvous servers is malformed', () {
        final old = SettingsState.initial.toJson()
          ..['rendezvousServers'] = <Object>[];
        expect(() => SettingsState.fromJson(old), throwsFormatException);
      });

      test('a blob lacking the role mode is malformed', () {
        final blob = SettingsState.initial.toJson()..remove('bleRoleMode');
        expect(() => SettingsState.fromJson(blob), throwsFormatException);
      });

      test('a blob naming no role mode is malformed', () {
        final blob = SettingsState.initial.toJson()..['bleRoleMode'] = 'both';
        expect(() => SettingsState.fromJson(blob), throwsFormatException);
      });
    });

    group('SetColdCallTrustLevelAction', () {
      test('every medium defaults to closed until set', () {
        for (final underlay in ProximityUnderlay.values) {
          expect(
            SettingsState.initial.trustLevelOf(underlay),
            ColdCallTrustLevel.closed,
            reason: '${underlay.name} must start closed',
          );
        }
      });

      test('updates the named medium\'s trust level', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.pan,
            ColdCallTrustLevel.open,
          ),
        );

        expect(result.trustLevelOf(ProximityUnderlay.pan),
            ColdCallTrustLevel.open);
        expect(result.bleRoleMode, state.bleRoleMode);
      });

      test('leaves the other medium\'s level untouched', () {
        // The two levels are independent: neither declaration implies the
        // other (spec §Trust levels).
        final bleOpen = settingsReducer(
          SettingsState.initial,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.pan,
            ColdCallTrustLevel.open,
          ),
        );
        expect(bleOpen.trustLevelOf(ProximityUnderlay.lan),
            ColdCallTrustLevel.closed);

        final bothOpen = settingsReducer(
          bleOpen,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.lan,
            ColdCallTrustLevel.open,
          ),
        );
        expect(bothOpen.trustLevelOf(ProximityUnderlay.pan),
            ColdCallTrustLevel.open);
        expect(bothOpen.trustLevelOf(ProximityUnderlay.lan),
            ColdCallTrustLevel.open);

        final lanOnly = settingsReducer(
          bothOpen,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.pan,
            ColdCallTrustLevel.closed,
          ),
        );
        expect(lanOnly.trustLevelOf(ProximityUnderlay.pan),
            ColdCallTrustLevel.closed);
        expect(lanOnly.trustLevelOf(ProximityUnderlay.lan),
            ColdCallTrustLevel.open);
      });

      test('round-trips through JSON, one level per medium', () {
        final bleOpen = settingsReducer(
          SettingsState.initial,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.pan,
            ColdCallTrustLevel.open,
          ),
        );
        final restored = SettingsState.fromJson(bleOpen.toJson());
        expect(restored.trustLevelOf(ProximityUnderlay.pan),
            ColdCallTrustLevel.open);
        expect(restored.trustLevelOf(ProximityUnderlay.lan),
            ColdCallTrustLevel.closed);
      });

      test('a blob carrying the old single level is malformed', () {
        // No tolerant decoder: the per-underlay form is required, and a blob
        // written before the levels were split does not carry it.
        final old = SettingsState.initial.toJson()
          ..remove('coldCallTrustLevels')
          ..['coldCallTrustLevel'] = 'open';
        expect(() => SettingsState.fromJson(old), throwsFormatException);
      });

      test('the persisted levels are keyed by the proximity underlays', () {
        final json = settingsReducer(
          SettingsState.initial,
          SetColdCallTrustLevelAction(
            ProximityUnderlay.pan,
            ColdCallTrustLevel.open,
          ),
        ).toJson();
        expect(json['coldCallTrustLevels'], {'pan': 'open', 'lan': 'closed'});
      });

      test('a blob keyed by anything but the proximity underlays is malformed',
          () {
        // No tolerant decoder (networking/CLAUDE.md): a key that names no
        // ProximityUnderlay is not read past, a `ble` key included.
        final blob = SettingsState.initial.toJson()
          ..['coldCallTrustLevels'] = {'ble': 'open', 'lan': 'closed'};
        expect(() => SettingsState.fromJson(blob), throwsFormatException);
      });

      test('a blob lacking a proximity underlay\'s level is malformed', () {
        final blob = SettingsState.initial.toJson()
          ..['coldCallTrustLevels'] = {'pan': 'open'};
        expect(() => SettingsState.fromJson(blob), throwsFormatException);
      });

      test('a blob naming no trust level is malformed', () {
        final blob = SettingsState.initial.toJson()
          ..['coldCallTrustLevels'] = {'pan': 'ajar', 'lan': 'closed'};
        expect(() => SettingsState.fromJson(blob), throwsFormatException);
      });
    });

    group('HydrateSettingsAction', () {
      test('replaces entire settings state', () {
        const state = SettingsState.initial;
        final hydratedState = SettingsState.initial
            .copyWith(bleRoleMode: BleRoleMode.centralOnly)
            .withTrustLevel(ProximityUnderlay.lan, ColdCallTrustLevel.open);
        final result = settingsReducer(
          state,
          HydrateSettingsAction(hydratedState),
        );

        expect(result, equals(hydratedState));
        expect(result.bleRoleMode, BleRoleMode.centralOnly);
        expect(result.trustLevelOf(ProximityUnderlay.lan),
            ColdCallTrustLevel.open);
      });

      test('completely discards previous state', () {
        final previousState = SettingsState.initial
            .copyWith(bleRoleMode: BleRoleMode.peripheralOnly)
            .withTrustLevel(ProximityUnderlay.pan, ColdCallTrustLevel.open);
        const hydratedState = SettingsState.initial;
        final result = settingsReducer(
          previousState,
          HydrateSettingsAction(hydratedState),
        );

        expect(result, equals(hydratedState));
        expect(result.bleRoleMode, BleRoleMode.auto);
        expect(result.trustLevelOf(ProximityUnderlay.pan),
            ColdCallTrustLevel.closed);
      });
    });
  });
}
