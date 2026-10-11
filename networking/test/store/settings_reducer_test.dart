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
      test('initial state has bluetoothEnabled=true and udpEnabled=true', () {
        const state = SettingsState.initial;

        expect(state.bluetoothEnabled, isTrue);
        expect(state.udpEnabled, isTrue);
        expect(state.transportPriority, [
          TransportProtocol.bluetooth,
          TransportProtocol.udp,
        ]);
      });

      test('unknown action returns the same state unchanged', () {
        const state = SettingsState.initial;
        final result = settingsReducer(state, _UnknownAction());

        expect(result, same(state));
      });
    });

    group('SetBluetoothEnabledAction', () {
      test('sets bluetoothEnabled to true', () {
        const state = SettingsState(bluetoothEnabled: false);
        final result = settingsReducer(
          state,
          SetBluetoothEnabledAction(true),
        );

        expect(result.bluetoothEnabled, isTrue);
      });

      test('sets bluetoothEnabled to false', () {
        const state = SettingsState(bluetoothEnabled: true);
        final result = settingsReducer(
          state,
          SetBluetoothEnabledAction(false),
        );

        expect(result.bluetoothEnabled, isFalse);
      });

      test('preserves udpEnabled and transportPriority', () {
        const state = SettingsState(
          bluetoothEnabled: true,
          udpEnabled: false,
          transportPriority: [TransportProtocol.udp],
        );
        final result = settingsReducer(
          state,
          SetBluetoothEnabledAction(false),
        );

        expect(result.bluetoothEnabled, isFalse);
        expect(result.udpEnabled, isFalse);
        expect(result.transportPriority, [TransportProtocol.udp]);
      });
    });

    group('SetUdpEnabledAction', () {
      test('sets udpEnabled to true', () {
        const state = SettingsState(udpEnabled: false);
        final result = settingsReducer(
          state,
          SetUdpEnabledAction(true),
        );

        expect(result.udpEnabled, isTrue);
      });

      test('sets udpEnabled to false', () {
        const state = SettingsState(udpEnabled: true);
        final result = settingsReducer(
          state,
          SetUdpEnabledAction(false),
        );

        expect(result.udpEnabled, isFalse);
      });

      test('preserves bluetoothEnabled and transportPriority', () {
        const state = SettingsState(
          bluetoothEnabled: false,
          udpEnabled: true,
          transportPriority: [
            TransportProtocol.udp,
            TransportProtocol.bluetooth,
          ],
        );
        final result = settingsReducer(
          state,
          SetUdpEnabledAction(false),
        );

        expect(result.udpEnabled, isFalse);
        expect(result.bluetoothEnabled, isFalse);
        expect(result.transportPriority, [
          TransportProtocol.udp,
          TransportProtocol.bluetooth,
        ]);
      });
    });

    group('UpdateTransportSettingsAction', () {
      test('updates multiple settings at once', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          UpdateTransportSettingsAction(
            bluetoothEnabled: false,
            udpEnabled: false,
          ),
        );

        expect(result.bluetoothEnabled, isFalse);
        expect(result.udpEnabled, isFalse);
        // transportPriority not provided, so it should remain at default
        expect(result.transportPriority, [
          TransportProtocol.bluetooth,
          TransportProtocol.udp,
        ]);
      });

      test('can change transport priority order', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          UpdateTransportSettingsAction(
            transportPriority: [
              TransportProtocol.udp,
              TransportProtocol.bluetooth,
            ],
          ),
        );

        expect(result.transportPriority, [
          TransportProtocol.udp,
          TransportProtocol.bluetooth,
        ]);
        // Fields not provided in the action should remain unchanged
        expect(result.bluetoothEnabled, isTrue);
        expect(result.udpEnabled, isTrue);
      });

      test('updates all fields simultaneously', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          UpdateTransportSettingsAction(
            bluetoothEnabled: false,
            udpEnabled: true,
            transportPriority: [TransportProtocol.udp],
          ),
        );

        expect(result.bluetoothEnabled, isFalse);
        expect(result.udpEnabled, isTrue);
        expect(result.transportPriority, [TransportProtocol.udp]);
      });
    });

    group('rendezvous server actions', () {
      const serverA = RendezvousServerSettings(
        address: '[2001:db8::10]:9516',
        pubkeyHex:
            'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      );
      const serverB = RendezvousServerSettings(
        address: '198.51.100.20:9514',
        pubkeyHex:
            'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
      );

      test('SetRendezvousServersAction stores multiple configured servers', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          SetRendezvousServersAction(const [serverA, serverB]),
        );

        expect(result.configuredRendezvousServers,
            equals(const [serverA, serverB]));
        expect(result.anchorAddress, equals(serverA.address));
        expect(result.anchorPubkeyHex, equals(serverA.pubkeyHex));
      });

      test('AddRendezvousServerAction deduplicates matching server', () {
        final state = SettingsState(
          rendezvousServers: const [serverA],
          anchorAddress: serverA.address,
          anchorPubkeyHex: serverA.pubkeyHex,
        );

        final result = settingsReducer(
          state,
          AddRendezvousServerAction(serverA),
        );

        expect(result.configuredRendezvousServers, equals(const [serverA]));
      });

      test('RemoveRendezvousServerAction removes only matching server', () {
        final state = SettingsState(
          rendezvousServers: const [serverA, serverB],
          anchorAddress: serverA.address,
          anchorPubkeyHex: serverA.pubkeyHex,
        );

        final result = settingsReducer(
          state,
          RemoveRendezvousServerAction(serverA),
        );

        expect(result.configuredRendezvousServers, equals(const [serverB]));
        expect(result.anchorAddress, equals(serverB.address));
        expect(result.anchorPubkeyHex, equals(serverB.pubkeyHex));
      });

      test('SetAnchorServerAction still hydrates legacy single server', () {
        const state = SettingsState.initial;
        final result = settingsReducer(
          state,
          SetAnchorServerAction(
            anchorAddress: serverA.address,
            anchorPubkeyHex: serverA.pubkeyHex,
          ),
        );

        expect(result.configuredRendezvousServers, equals(const [serverA]));
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
        expect(result.bluetoothEnabled, state.bluetoothEnabled);
        expect(result.udpEnabled, state.udpEnabled);
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
        const hydratedState = SettingsState(
          bluetoothEnabled: false,
          udpEnabled: false,
        );
        final result = settingsReducer(
          state,
          HydrateSettingsAction(hydratedState),
        );

        expect(result, equals(hydratedState));
        expect(result.bluetoothEnabled, isFalse);
        expect(result.udpEnabled, isFalse);
      });

      test('handles custom transport priority', () {
        const state = SettingsState.initial;
        const hydratedState = SettingsState(
          bluetoothEnabled: true,
          udpEnabled: false,
          transportPriority: [
            TransportProtocol.udp,
            TransportProtocol.bluetooth,
          ],
        );
        final result = settingsReducer(
          state,
          HydrateSettingsAction(hydratedState),
        );

        expect(result.transportPriority, [
          TransportProtocol.udp,
          TransportProtocol.bluetooth,
        ]);
        expect(result.bluetoothEnabled, isTrue);
        expect(result.udpEnabled, isFalse);
      });

      test('completely discards previous state', () {
        const previousState = SettingsState(
          bluetoothEnabled: false,
          udpEnabled: false,
          transportPriority: [TransportProtocol.udp],
        );
        const hydratedState = SettingsState(
          bluetoothEnabled: true,
          udpEnabled: true,
          transportPriority: [
            TransportProtocol.bluetooth,
            TransportProtocol.udp,
          ],
        );
        final result = settingsReducer(
          previousState,
          HydrateSettingsAction(hydratedState),
        );

        expect(result, equals(hydratedState));
        expect(result.bluetoothEnabled, isTrue);
        expect(result.udpEnabled, isTrue);
        expect(result.transportPriority, [
          TransportProtocol.bluetooth,
          TransportProtocol.udp,
        ]);
      });
    });
  });
}
