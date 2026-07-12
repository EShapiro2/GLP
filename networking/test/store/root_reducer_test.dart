import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/store/app_state.dart';
import 'package:grassroots_networking/src/store/reducers.dart';
import 'package:grassroots_networking/src/store/peers_actions.dart';
import 'package:grassroots_networking/src/store/messages_actions.dart';
import 'package:grassroots_networking/src/store/friendships_actions.dart';
import 'package:grassroots_networking/src/store/settings_actions.dart';
import 'package:grassroots_networking/src/store/transports_actions.dart';
import 'package:grassroots_networking/src/store/transports_state.dart';
import 'package:grassroots_networking/src/store/peers_state.dart';
import 'package:grassroots_networking/src/transport/transport_service.dart';

void main() {
  group('appReducer', () {
    // =========================================================
    // 1. Routing to sub-reducers
    // =========================================================
    group('routes actions to sub-reducers', () {
      test('PeerAction routes to peersReducer', () {
        const initial = AppState.initial;
        final action = BleDeviceDiscoveredAction(
          deviceId: 'dev1',
          rssi: -50,
        );

        final result = appReducer(initial, action);

        // The peers state should have changed (device added to discoveredBlePeers)
        expect(result.peers, isNot(equals(initial.peers)));
        expect(result.peers.discoveredBlePeers.containsKey('dev1'), isTrue);
        final discovered = result.peers.discoveredBlePeers['dev1']!;
        expect(discovered.transportId, 'dev1');
        expect(discovered.rssi, -50);

        // Other state sections remain unchanged
        expect(result.messages, equals(initial.messages));
        expect(result.friendships, equals(initial.friendships));
        expect(result.settings, equals(initial.settings));
        expect(result.transports, equals(initial.transports));
      });

      test('MessageAction routes to messagesReducer', () {
        const initial = AppState.initial;
        final action = MessageSentAction(
          messageId: 'msg1',
          transport: MessageTransport.ble,
          recipientPubkey: Uint8List(32),
          payloadSize: 100,
        );

        final result = appReducer(initial, action);

        // The messages state should have changed (outgoing message recorded)
        expect(result.messages, isNot(equals(initial.messages)));
        expect(result.messages.outgoingMessages.containsKey('msg1'), isTrue);
        final msg = result.messages.outgoingMessages['msg1']!;
        expect(msg.transport, MessageTransport.ble);
        expect(msg.payloadSize, 100);

        // Other state sections remain unchanged
        expect(result.peers, equals(initial.peers));
        expect(result.friendships, equals(initial.friendships));
        expect(result.settings, equals(initial.settings));
        expect(result.transports, equals(initial.transports));
      });

      test('FriendshipAction routes to friendshipsReducer', () {
        const initial = AppState.initial;
        final action = CreateFriendRequestAction(
          peerPubkeyHex: 'abc123',
          message: 'hi',
        );

        final result = appReducer(initial, action);

        // The friendships state should have changed (pending request added)
        expect(result.friendships, isNot(equals(initial.friendships)));
        expect(result.friendships.friendships.containsKey('abc123'), isTrue);

        // Other state sections remain unchanged
        expect(result.peers, equals(initial.peers));
        expect(result.messages, equals(initial.messages));
        expect(result.settings, equals(initial.settings));
        expect(result.transports, equals(initial.transports));
      });

      test('SettingsAction routes to settingsReducer', () {
        const initial = AppState.initial;
        // Default bluetoothEnabled is true, so setting it to false should change state
        final action = SetBluetoothEnabledAction(false);

        final result = appReducer(initial, action);

        // The settings state should have changed
        expect(result.settings, isNot(equals(initial.settings)));
        expect(result.settings.bluetoothEnabled, isFalse);

        // Other state sections remain unchanged
        expect(result.peers, equals(initial.peers));
        expect(result.messages, equals(initial.messages));
        expect(result.friendships, equals(initial.friendships));
        expect(result.transports, equals(initial.transports));
      });

      test('TransportAction routes to transportsReducer', () {
        const initial = AppState.initial;
        final action = BleTransportStateChangedAction(
          TransportState.initializing,
        );

        final result = appReducer(initial, action);

        expect(result.transports, isNot(equals(initial.transports)));
        expect(result.transports.bleState, TransportState.initializing);

        // Other state sections remain unchanged
        expect(result.peers, equals(initial.peers));
        expect(result.messages, equals(initial.messages));
        expect(result.friendships, equals(initial.friendships));
        expect(result.settings, equals(initial.settings));
      });
    });

    // =========================================================
    // 2. Transport state actions
    // =========================================================
    group('transport state actions', () {
      test('BleTransportStateChangedAction updates BLE state', () {
        const initial = AppState.initial;
        expect(initial.transports.bleState, TransportState.uninitialized);

        final result = appReducer(
          initial,
          BleTransportStateChangedAction(TransportState.ready),
        );

        expect(result.transports.bleState, TransportState.ready);
        expect(result.transports.udpState, TransportState.uninitialized);
      });

      test('UdpTransportStateChangedAction updates UDP state', () {
        const initial = AppState.initial;

        final result = appReducer(
          initial,
          UdpTransportStateChangedAction(TransportState.active),
        );

        expect(result.transports.udpState, TransportState.active);
        expect(result.transports.bleState, TransportState.uninitialized);
      });

      test(
        'BleTransportStateChangedAction with error stores error message',
        () {
          const initial = AppState.initial;

          final result = appReducer(
            initial,
            BleTransportStateChangedAction(
              TransportState.error,
              error: 'BLE unavailable',
            ),
          );

          expect(result.transports.bleState, TransportState.error);
          expect(result.transports.bleError, 'BLE unavailable');
        },
      );

      test('BleScanningChangedAction updates scanning flag', () {
        final state = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.active),
        );

        final result = appReducer(state, BleScanningChangedAction(true));

        expect(result.transports.bleScanning, isTrue);
        expect(result.transports.bleState, TransportState.active);

        final result2 = appReducer(result, BleScanningChangedAction(false));
        expect(result2.transports.bleScanning, isFalse);
      });

      test('NetworkConnectionTypeUpdatedAction updates connection type', () {
        const initial = AppState.initial;

        final result = appReducer(
          initial,
          NetworkConnectionTypeUpdatedAction(NetworkConnectionType.wifi),
        );

        expect(
          result.transports.networkConnectionType,
          NetworkConnectionType.wifi,
        );
        expect(
          result.transports.publicAddress,
          initial.transports.publicAddress,
        );
        expect(result.transports.publicIp, initial.transports.publicIp);
      });

      test(
        'ClearPublicConnectivityAction clears stale public network info',
        () {
          final state = AppState.initial.copyWith(
            transports: const TransportsState(
              udpState: TransportState.active,
              publicAddress: '203.0.113.10:4242',
              publicIp: '203.0.113.10',
              networkConnectionType: NetworkConnectionType.cellular,
            ),
          );

          final result = appReducer(state, ClearPublicConnectivityAction());

          expect(result.transports.publicAddress, isNull);
          expect(result.transports.publicIp, isNull);
          expect(
            result.transports.networkConnectionType,
            NetworkConnectionType.cellular,
          );
          expect(result.transports.udpState, TransportState.active);
        },
      );

      test(
        'well-connected only requires a globally routable public address',
        () {
          const publicState = TransportsState(
            publicAddress: '[2606:4700::1]:4242',
          );
          const privateState = TransportsState(publicAddress: '10.0.0.4:4242');

          expect(publicState.lastUnsolicitedInboundAt, isNull);
          expect(publicState.isWellConnected, isTrue);
          expect(privateState.isWellConnected, isFalse);
        },
      );
    });

    // =========================================================
    // 3. Derived state (isHealthy, statusDisplayString)
    // =========================================================
    group('derived transport state', () {
      test('isHealthy is true when any transport is active', () {
        final bleActive = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.active),
        );
        expect(bleActive.isHealthy, isTrue);

        final udpActive = AppState.initial.copyWith(
          transports: const TransportsState(udpState: TransportState.active),
        );
        expect(udpActive.isHealthy, isTrue);

        const noneActive = AppState.initial;
        expect(noneActive.isHealthy, isFalse);
      });

      test('statusDisplayString reflects transport state', () {
        expect(AppState.initial.statusDisplayString, 'Initializing...');

        final ready = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.ready),
        );
        expect(ready.statusDisplayString, 'Ready');

        final active = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.active),
        );
        expect(active.statusDisplayString, 'Online');

        final scanning = AppState.initial.copyWith(
          transports: const TransportsState(
            bleState: TransportState.active,
            bleScanning: true,
          ),
        );
        expect(scanning.statusDisplayString, 'Scanning for peers...');
      });
    });

    // =========================================================
    // 4. Unknown action returns same state
    // =========================================================
    group('unknown actions', () {
      test('unknown action returns the same state unchanged', () {
        final state = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.active),
        );

        final result = appReducer(state, 'some_unknown_action');

        expect(result, equals(state));
        expect(identical(result, state), isTrue);
      });

      test('null action returns the same state unchanged', () {
        const state = AppState.initial;

        final result = appReducer(state, null);

        expect(result, equals(state));
        expect(identical(result, state), isTrue);
      });
    });

    // =========================================================
    // 5. Actions preserve unrelated state sections
    // =========================================================
    group('actions preserve unrelated state sections', () {
      test(
        'BleTransportStateChangedAction preserves peers, messages, friendships, settings',
        () {
          final now = DateTime.now();
          final stateWithData = AppState(
            peers: PeersState(
              discoveredBlePeers: {
                'dev1': DiscoveredPeerState(
                  transportId: 'dev1',
                  rssi: -60,
                  discoveredAt: now,
                  lastSeen: now,
                ),
              },
            ),
          );

          final result = appReducer(
            stateWithData,
            BleTransportStateChangedAction(TransportState.active),
          );

          // Transports changed
          expect(result.transports.bleState, TransportState.active);

          // Peers preserved exactly
          expect(result.peers, equals(stateWithData.peers));
          expect(result.peers.discoveredBlePeers.containsKey('dev1'), isTrue);

          // Messages, friendships, settings preserved
          expect(result.messages, equals(stateWithData.messages));
          expect(result.friendships, equals(stateWithData.friendships));
          expect(result.settings, equals(stateWithData.settings));
        },
      );

      test('PeerAction preserves transports and other sub-states', () {
        final state = AppState.initial.copyWith(
          transports: const TransportsState(bleState: TransportState.active),
        );

        final action = BleDeviceDiscoveredAction(deviceId: 'dev2', rssi: -70);
        final result = appReducer(state, action);

        // Peers changed
        expect(result.peers.discoveredBlePeers.containsKey('dev2'), isTrue);

        // Everything else preserved
        expect(result.transports, equals(state.transports));
        expect(result.messages, equals(state.messages));
        expect(result.friendships, equals(state.friendships));
        expect(result.settings, equals(state.settings));
      });
    });
  });
}
