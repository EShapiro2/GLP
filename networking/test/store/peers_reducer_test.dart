import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking_core/src/store/messages_actions.dart';
import 'package:grassroots_networking_core/src/store/peers_state.dart';
import 'package:grassroots_networking_core/src/store/peers_actions.dart';
import 'package:grassroots_networking_core/src/store/peers_reducer.dart';
import 'package:grassroots_networking_core/src/models/peer.dart';

/// Generate a deterministic 32-byte public key from a seed value.
Uint8List _testPubkey(int seed) {
  return Uint8List.fromList(List.generate(32, (i) => (seed + i) % 256));
}

/// Convert a pubkey to hex string (mirrors _pubkeyToHex in the reducer).
String _pubkeyHex(Uint8List pubkey) {
  return pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
}

void main() {
  // =========================================================================
  // BLE Discovery Actions
  // =========================================================================

  group('BleDeviceDiscoveredAction', () {
    test('adds a new discovered peer', () {
      const state = PeersState.initial;
      final action = BleDeviceDiscoveredAction(
        deviceId: 'device-1',
        displayName: 'Pixel 7',
        rssi: -55,
      );

      final result = peersReducer(state, action);

      expect(result.discoveredBlePeers.length, 1);
      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.transportId, 'device-1');
      expect(peer.displayName, 'Pixel 7');
      expect(peer.rssi, -55);
      expect(peer.isConnecting, false);
      expect(peer.isConnected, false);
    });

    test('updates existing peer RSSI and lastSeen', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            displayName: 'Pixel 7',
            rssi: -80,
            discoveredAt: now.subtract(const Duration(seconds: 30)),
            lastSeen: now.subtract(const Duration(seconds: 10)),
          ),
        },
      );
      final action = BleDeviceDiscoveredAction(deviceId: 'device-1', rssi: -50);

      final result = peersReducer(initial, action);

      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.rssi, -50);
      // lastSeen should be updated (newer than the original)
      expect(
        peer.lastSeen.isAfter(now.subtract(const Duration(seconds: 10))),
        true,
      );
    });

    test('preserves existing displayName when action has no displayName', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            displayName: 'Pixel 7',
            rssi: -80,
            discoveredAt: now,
            lastSeen: now,
          ),
        },
      );
      final action = BleDeviceDiscoveredAction(
        deviceId: 'device-1',
        rssi: -60,
        // displayName intentionally omitted (null)
      );

      final result = peersReducer(initial, action);

      expect(result.discoveredBlePeers['device-1']!.displayName, 'Pixel 7');
    });
  });

  // =========================================================================
  // BleDeviceRssiUpdatedAction
  // =========================================================================

  group('BleDeviceRssiUpdatedAction', () {
    test('updates RSSI of existing discovered peer', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -80,
            discoveredAt: now,
            lastSeen: now,
          ),
        },
      );
      final action = BleDeviceRssiUpdatedAction(
        deviceId: 'device-1',
        rssi: -45,
      );

      final result = peersReducer(initial, action);

      expect(result.discoveredBlePeers['device-1']!.rssi, -45);
    });

    test('is a no-op for unknown device', () {
      const state = PeersState.initial;
      final action = BleDeviceRssiUpdatedAction(
        deviceId: 'nonexistent',
        rssi: -45,
      );

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // BleDeviceConnectingAction
  // =========================================================================

  group('BleDeviceConnectingAction', () {
    test('sets isConnecting=true', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
          ),
        },
      );
      final action = BleDeviceConnectingAction('device-1');

      final result = peersReducer(initial, action);

      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.isConnecting, true);
    });

    test('is a no-op for unknown device', () {
      const state = PeersState.initial;
      final action = BleDeviceConnectingAction('nonexistent');

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // BleDeviceConnectedAction
  // =========================================================================

  group('BleDeviceConnectedAction', () {
    test('sets isConnected=true and isConnecting=false', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
            isConnecting: true,
          ),
        },
      );
      final action = BleDeviceConnectedAction('device-1');

      final result = peersReducer(initial, action);

      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.isConnected, true);
      expect(peer.isConnecting, false);
    });
  });

  // =========================================================================
  // BleDeviceConnectionFailedAction
  // =========================================================================

  group('BleDeviceConnectionFailedAction', () {
    test('sets isConnecting=false and isConnected=false', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
            isConnecting: true,
          ),
        },
      );
      final action = BleDeviceConnectionFailedAction('device-1');

      final result = peersReducer(initial, action);

      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.isConnecting, false);
      expect(peer.isConnected, false);
    });
  });

  // =========================================================================
  // BleDeviceDisconnectedAction
  // =========================================================================

  group('BleDeviceDisconnectedAction', () {
    test('sets isConnecting=false and isConnected=false', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
            isConnecting: true,
            isConnected: true,
          ),
        },
      );
      final action = BleDeviceDisconnectedAction('device-1');

      final result = peersReducer(initial, action);

      final peer = result.discoveredBlePeers['device-1']!;
      expect(peer.isConnecting, false);
      expect(peer.isConnected, false);
    });
  });

  // =========================================================================
  // BleDeviceRemovedAction
  // =========================================================================

  group('BleDeviceRemovedAction', () {
    test('removes device from discovered peers map', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
          ),
          'device-2': DiscoveredPeerState(
            transportId: 'device-2',
            rssi: -70,
            discoveredAt: now,
            lastSeen: now,
          ),
        },
      );
      final action = BleDeviceRemovedAction('device-1');

      final result = peersReducer(initial, action);

      expect(result.discoveredBlePeers.length, 1);
      expect(result.discoveredBlePeers.containsKey('device-1'), false);
      expect(result.discoveredBlePeers.containsKey('device-2'), true);
    });
  });

  // =========================================================================
  // StaleDiscoveredBlePeersRemovedAction
  // =========================================================================

  group('StaleDiscoveredBlePeersRemovedAction', () {
    test('removes peers older than threshold and keeps fresh peers', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'stale-device': DiscoveredPeerState(
            transportId: 'stale-device',
            rssi: -60,
            discoveredAt: now.subtract(const Duration(minutes: 10)),
            lastSeen: now.subtract(const Duration(minutes: 5)),
          ),
          'fresh-device': DiscoveredPeerState(
            transportId: 'fresh-device',
            rssi: -50,
            discoveredAt: now.subtract(const Duration(seconds: 30)),
            lastSeen: now.subtract(const Duration(seconds: 5)),
          ),
        },
      );
      final action = StaleDiscoveredBlePeersRemovedAction(
        const Duration(minutes: 2),
      );

      final result = peersReducer(initial, action);

      expect(result.discoveredBlePeers.length, 1);
      expect(result.discoveredBlePeers.containsKey('stale-device'), false);
      expect(result.discoveredBlePeers.containsKey('fresh-device'), true);
    });
  });

  // =========================================================================
  // ClearDiscoveredBlePeersAction
  // =========================================================================

  group('ClearDiscoveredBlePeersAction', () {
    test('empties the discovered peers map', () {
      final now = DateTime.now();
      final initial = PeersState(
        discoveredBlePeers: {
          'device-1': DiscoveredPeerState(
            transportId: 'device-1',
            rssi: -60,
            discoveredAt: now,
            lastSeen: now,
          ),
          'device-2': DiscoveredPeerState(
            transportId: 'device-2',
            rssi: -70,
            discoveredAt: now,
            lastSeen: now,
          ),
        },
      );
      final action = ClearDiscoveredBlePeersAction();

      final result = peersReducer(initial, action);

      expect(result.discoveredBlePeers, isEmpty);
    });
  });

  // =========================================================================
  // PeerAnnounceReceivedAction — split BLE device IDs
  // =========================================================================

  group('PeerAnnounceReceivedAction', () {
    test('creates new peer with central BLE device ID', () {
      final pubkey = _testPubkey(1);
      const state = PeersState.initial;
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 2,
        rssi: -55,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-central-1',
      );

      final result = peersReducer(state, action);

      final hex = _pubkeyHex(pubkey);
      expect(result.peers.length, 1);
      final peer = result.peers[hex]!;
      expect(peer.publicKey, pubkey);
      expect(peer.protocolVersion, 2);
      expect(peer.rssi, -55);
      expect(peer.transport, PeerTransport.bleDirect);
      expect(peer.bleCentralDeviceId, 'ble-central-1');
      expect(peer.blePeripheralDeviceId, isNull);
      expect(peer.bleDeviceId, 'ble-central-1'); // convenience getter
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.lastSeen, isNotNull);
      expect(peer.lastBleSeen, isNotNull);
    });

    test('creates new peer with peripheral BLE device ID', () {
      final pubkey = _testPubkey(1);
      const state = PeersState.initial;
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 1,
        rssi: -60,
        transport: PeerTransport.bleDirect,
        blePeripheralDeviceId: 'ble-peripheral-1',
      );

      final result = peersReducer(state, action);

      final hex = _pubkeyHex(pubkey);
      final peer = result.peers[hex]!;
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.blePeripheralDeviceId, 'ble-peripheral-1');
      expect(peer.bleDeviceId, 'ble-peripheral-1'); // fallback to peripheral
    });

    test(
      'preserves both BLE IDs on sequential ANNOUNCEs from different roles',
      () {
        final pubkey = _testPubkey(1);
        final hex = _pubkeyHex(pubkey);

        // First: central ANNOUNCE
        var state = peersReducer(
          PeersState.initial,
          PeerAnnounceReceivedAction(
            publicKey: pubkey,
            protocolVersion: 1,
            rssi: -55,
            transport: PeerTransport.bleDirect,
            bleCentralDeviceId: 'central-id',
          ),
        );

        expect(state.peers[hex]!.bleCentralDeviceId, 'central-id');
        expect(state.peers[hex]!.blePeripheralDeviceId, isNull);

        // Second: peripheral ANNOUNCE — should NOT overwrite central
        state = peersReducer(
          state,
          PeerAnnounceReceivedAction(
            publicKey: pubkey,
            protocolVersion: 1,
            rssi: -50,
            transport: PeerTransport.bleDirect,
            blePeripheralDeviceId: 'peripheral-id',
          ),
        );

        final peer = state.peers[hex]!;
        expect(peer.bleCentralDeviceId, 'central-id');
        expect(peer.blePeripheralDeviceId, 'peripheral-id');
        expect(
          peer.bleDeviceId,
          'central-id',
        ); // convenience getter prefers central
      },
    );

    test('updates existing peer', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.disconnected,
            rssi: -90,
            protocolVersion: 1,
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 3,
        rssi: -40,
        transport: PeerTransport.udp,
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.protocolVersion, 3);
      expect(peer.rssi, -40);
      expect(peer.transport, PeerTransport.udp);
      expect(peer.connectionState, PeerConnectionState.connected);
    });

    test('merges ANNOUNCE fields without dropping non-ANNOUNCE peer state', () {
      final pubkey = _testPubkey(6);
      final hex = _pubkeyHex(pubkey);
      final directReachAt = DateTime.now().subtract(const Duration(minutes: 2));
      const udpAddress = '[2001:db8::6]:4006';
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.disconnected,
            transport: PeerTransport.udp,
            rssi: -90,
            protocolVersion: 1,
            udpAddress: udpAddress,
            lastDirectReachAt: directReachAt,
            hasLiveUdpConnection: true,
          ),
        },
      );

      final result = peersReducer(
        initial,
        PeerAnnounceReceivedAction(
          publicKey: pubkey,
          protocolVersion: 3,
          rssi: -40,
          transport: PeerTransport.udp,
        ),
      );

      final peer = result.peers[hex]!;
      expect(peer.protocolVersion, 3);
      expect(peer.rssi, -40);
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.udpAddress, udpAddress);
      expect(peer.hasLiveUdpConnection, isTrue);
      expect(peer.lastDirectReachAt, directReachAt);
    });

    test('sets connectionState to connected', () {
      final pubkey = _testPubkey(2);
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 1,
        rssi: -60,
      );

      final result = peersReducer(PeersState.initial, action);

      final hex = _pubkeyHex(pubkey);
      expect(result.peers[hex]!.connectionState, PeerConnectionState.connected);
    });

    test('UDP ANNOUNCE does not clear existing BLE IDs', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-id',
            blePeripheralDeviceId: 'peripheral-id',
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 1,
        rssi: -40,
        transport: PeerTransport.udp,
        // No BLE device IDs — should preserve existing
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.bleCentralDeviceId, 'central-id');
      expect(peer.blePeripheralDeviceId, 'peripheral-id');
    });

    test('UDP ANNOUNCE records lastUdpSeen', () {
      final pubkey = _testPubkey(3);
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 1,
        rssi: -42,
        transport: PeerTransport.udp,
      );

      final result = peersReducer(PeersState.initial, action);

      final peer = result.peers[_pubkeyHex(pubkey)]!;
      expect(peer.lastUdpSeen, isNotNull);
    });

    test('BLE ANNOUNCE preserves existing lastUdpSeen', () {
      final pubkey = _testPubkey(4);
      final hex = _pubkeyHex(pubkey);
      final udpSeenAt = DateTime.now().subtract(const Duration(minutes: 1));
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            lastSeen: udpSeenAt,
            lastUdpSeen: udpSeenAt,
            udpAddress: '[2001:db8::4]:4004',
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        protocolVersion: 1,
        rssi: -55,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'central-4',
      );

      final result = peersReducer(initial, action);

      expect(result.peers[hex]!.lastUdpSeen, equals(udpSeenAt));
    });

    // The "preserve RV servers across ANNOUNCE updates" invariant moved with
    // knownRvServers to the friendship record — see friendships_reducer_test.
  });

  // =========================================================================
  // PeerRssiUpdatedAction
  // =========================================================================

  group('PeerRssiUpdatedAction', () {
    test('updates peer RSSI', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(publicKey: pubkey, rssi: -80),
        },
      );
      final action = PeerRssiUpdatedAction(publicKey: pubkey, rssi: -45);

      final result = peersReducer(initial, action);

      expect(result.peers[hex]!.rssi, -45);
    });

    test('is a no-op for unknown peer', () {
      const state = PeersState.initial;
      final action = PeerRssiUpdatedAction(
        publicKey: _testPubkey(99),
        rssi: -45,
      );

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // PeerBleDisconnectedAction — role-specific clearing
  // =========================================================================

  group('PeerBleDisconnectedAction', () {
    test('clears both BLE IDs when no role specified', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
            blePeripheralDeviceId: 'peripheral-1',
          ),
        },
      );
      final action = PeerBleDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.connectionState, PeerConnectionState.disconnected);
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.blePeripheralDeviceId, isNull);
      expect(peer.bleDeviceId, isNull);
    });

    test('clears only central ID when role=central', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
            blePeripheralDeviceId: 'peripheral-1',
          ),
        },
      );
      final action = PeerBleDisconnectedAction(pubkey, role: BleRole.central);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.blePeripheralDeviceId, 'peripheral-1');
      // Still has peripheral, so stays connected
      expect(peer.connectionState, PeerConnectionState.connected);
    });

    test('clears only peripheral ID when role=peripheral', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
            blePeripheralDeviceId: 'peripheral-1',
          ),
        },
      );
      final action = PeerBleDisconnectedAction(
        pubkey,
        role: BleRole.peripheral,
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.bleCentralDeviceId, 'central-1');
      expect(peer.blePeripheralDeviceId, isNull);
      expect(peer.connectionState, PeerConnectionState.connected);
    });

    test('marks disconnected if no BLE IDs remain and no UDP address', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
          ),
        },
      );
      final action = PeerBleDisconnectedAction(pubkey, role: BleRole.central);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.connectionState, PeerConnectionState.disconnected);
      expect(peer.bleCentralDeviceId, isNull);
    });

    test(
        'does NOT consult UDP state — connectionState flips to disconnected '
        'once both BLE roles are gone, regardless of UDP liveness. UDP '
        'fields are preserved untouched (a different action drives those).',
        () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
            udpAddress: '[2001:db8::1]:4001',
            hasLiveUdpConnection: true,
          ),
        },
      );
      final action = PeerBleDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      // BLE is the only thing this action governs. Once both roles clear,
      // connectionState reflects "no live BLE", and UDP-derived state is
      // left for `PeerUdpDisconnectedAction` / `PeerUdpConnectionChangedAction`
      // to manage.
      expect(peer.connectionState, PeerConnectionState.disconnected);
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.blePeripheralDeviceId, isNull);
      // UDP fields untouched.
      expect(peer.udpAddress, '[2001:db8::1]:4001');
      expect(peer.hasLiveUdpConnection, isTrue);
    });
  });

  // =========================================================================
  // PeerUdpDisconnectedAction
  // =========================================================================

  group('PeerUdpDisconnectedAction', () {
    test('marks disconnected if no BLE device', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = PeerUdpDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.connectionState, PeerConnectionState.disconnected);
      // udpAddress is preserved — it's the last known location for reconnection
      expect(peer.udpAddress, '[2001:db8::1]:4001');
    });

    test('keeps connected if has BLE device (central)', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-1',
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = PeerUdpDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.bleCentralDeviceId, 'central-1');
      // udpAddress is preserved — it's the last known location for reconnection
      expect(peer.udpAddress, '[2001:db8::1]:4001');
    });

    test('preserves udpAddress for reconnection', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = PeerUdpDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      // UDP address must never be cleared on disconnect — it's
      // the last known location and the only way to attempt reconnection.
      expect(result.peers[hex]!.udpAddress, '[2001:db8::1]:4001');
    });
  });

  // =========================================================================
  // PeerUdpSeenAction
  // =========================================================================

  group('PeerUdpSeenAction', () {
    test('updates lastSeen and lastUdpSeen for existing peer', () {
      final pubkey = _testPubkey(5);
      final hex = _pubkeyHex(pubkey);
      final initialSeenAt = DateTime.now().subtract(const Duration(minutes: 5));
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            lastSeen: initialSeenAt,
            lastUdpSeen: initialSeenAt,
          ),
        },
      );

      final result = peersReducer(initial, PeerUdpSeenAction(pubkey));

      final peer = result.peers[hex]!;
      expect(peer.lastSeen, isNotNull);
      expect(peer.lastUdpSeen, isNotNull);
      expect(peer.lastSeen!.isAfter(initialSeenAt), isTrue);
      expect(peer.lastUdpSeen!.isAfter(initialSeenAt), isTrue);
    });

    test('is a no-op for unknown peer', () {
      const state = PeersState.initial;

      final result = peersReducer(state, PeerUdpSeenAction(_testPubkey(42)));

      expect(result, same(state));
    });
  });

  // =========================================================================
  // PeerDisconnectedAction
  // =========================================================================

  group('PeerDisconnectedAction', () {
    test('sets connectionState to disconnected', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
          ),
        },
      );
      final action = PeerDisconnectedAction(pubkey);

      final result = peersReducer(initial, action);

      expect(
        result.peers[hex]!.connectionState,
        PeerConnectionState.disconnected,
      );
    });
  });

  // =========================================================================
  // AssociateBleDeviceAction — role-based
  // =========================================================================

  group('AssociateBleDeviceAction', () {
    test('sets bleCentralDeviceId when role=central', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {hex: PeerState(publicKey: pubkey)},
      );
      final action = AssociateBleDeviceAction(
        publicKey: pubkey,
        deviceId: 'ble-central-99',
        role: BleRole.central,
      );

      final result = peersReducer(initial, action);

      expect(result.peers[hex]!.bleCentralDeviceId, 'ble-central-99');
      expect(result.peers[hex]!.blePeripheralDeviceId, isNull);
    });

    test('sets blePeripheralDeviceId when role=peripheral', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {hex: PeerState(publicKey: pubkey)},
      );
      final action = AssociateBleDeviceAction(
        publicKey: pubkey,
        deviceId: 'ble-peripheral-99',
        role: BleRole.peripheral,
      );

      final result = peersReducer(initial, action);

      expect(result.peers[hex]!.bleCentralDeviceId, isNull);
      expect(result.peers[hex]!.blePeripheralDeviceId, 'ble-peripheral-99');
    });

    test('is a no-op for unknown peer', () {
      const state = PeersState.initial;
      final action = AssociateBleDeviceAction(
        publicKey: _testPubkey(99),
        deviceId: 'ble-device-99',
        role: BleRole.central,
      );

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // AssociateUdpAddressAction
  // =========================================================================

  group('AssociateUdpAddressAction', () {
    test('sets udpAddress', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {hex: PeerState(publicKey: pubkey)},
      );
      final action = AssociateUdpAddressAction(
        publicKey: pubkey,
        address: '[2001:db8::1]:4001',
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.udpAddress, '[2001:db8::1]:4001');
    });

    test('clears udpAddress when address is empty', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = AssociateUdpAddressAction(publicKey: pubkey, address: '');

      final result = peersReducer(initial, action);

      // The reducer constructs PeerState directly (not via copyWith) so it
      // can actually set nullable fields to null. An empty address clears
      // the stored udpAddress.
      final peer = result.peers[hex]!;
      expect(peer.udpAddress, isNull);
    });

    test('creates a minimal dial-book entry for an unknown peer', () {
      // putPeerAddress seam: GLP can feed an address for a peer never seen,
      // and the layer materializes a dial-book entry for send/reconnection.
      final pubkey = _testPubkey(99);
      final hex = _pubkeyHex(pubkey);
      final action = AssociateUdpAddressAction(
        publicKey: pubkey,
        address: '[2001:db8::1]:4001',
      );

      final result = peersReducer(PeersState.initial, action);

      final created = result.peers[hex];
      expect(created, isNotNull);
      expect(created!.udpAddress, '[2001:db8::1]:4001');
      expect(created.hasKnownAddress, isTrue);
      // A GLP-fed address is a dial-book hint only, never a live session.
      expect(created.isReachable, isFalse);
      expect(created.hasLiveUdpConnection, isFalse);
      expect(created.connectionState, PeerConnectionState.disconnected);
    });

    test('is a no-op for an unknown peer with an empty address', () {
      const state = PeersState.initial;
      final action = AssociateUdpAddressAction(
        publicKey: _testPubkey(98),
        address: '',
      );

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // StalePeersRemovedAction
  // =========================================================================

  group('StalePeersRemovedAction', () {
    test('removes stale unprotected peers', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), false);
    });

    test('does NOT mutate connectionState or BLE IDs of protected peers', () {
      // Strict-projection contract: timer-driven reducers may not flip
      // connectionState or clear BLE device IDs. Those are exclusively
      // driven by plugin events (BLE) and UDX events (UDP). The stale
      // reducer's only job is bounding memory by removing unprotected
      // peers we haven't heard from; known peers (GLP-supplied keys) are
      // passed as the protected set.
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
            bleCentralDeviceId: 'central-1',
            blePeripheralDeviceId: 'peripheral-1',
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = StalePeersRemovedAction(
        const Duration(minutes: 2),
        protectedPubkeyHexes: {hex},
      );

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), true);
      final peer = result.peers[hex]!;
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.bleCentralDeviceId, 'central-1');
      expect(peer.blePeripheralDeviceId, 'peripheral-1');
      expect(peer.udpAddress, '[2001:db8::1]:4001');
    });

    test('keeps fresh peers unchanged', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(seconds: 10)),
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), true);
      expect(result.peers[hex]!.connectionState, PeerConnectionState.connected);
    });

    test('removes stale unprotected peers regardless of connectionState', () {
      // The reducer doesn't gate on connectionState — that field is
      // plugin-/UDX-driven, not timer-driven. Unprotected stale peers are
      // removed for memory pressure whether they read as connected or
      // disconnected at the moment the timer fires.
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            connectionState: PeerConnectionState.disconnected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), false);
    });
  });

  // =========================================================================
  // PeerState convenience getters
  // =========================================================================

  group('PeerState getters', () {
    test('hasBleConnection is true with central ID', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        bleCentralDeviceId: 'central-1',
      );
      expect(peer.hasBleConnection, true);
      expect(peer.bleDeviceId, 'central-1');
    });

    test('hasBleConnection is true with peripheral ID', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        blePeripheralDeviceId: 'peripheral-1',
      );
      expect(peer.hasBleConnection, true);
      expect(peer.bleDeviceId, 'peripheral-1');
    });

    test('hasBleConnection is false with no IDs', () {
      final peer = PeerState(publicKey: _testPubkey(1));
      expect(peer.hasBleConnection, false);
      expect(peer.bleDeviceId, isNull);
    });

    test('bleDeviceId prefers central over peripheral', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        bleCentralDeviceId: 'central-1',
        blePeripheralDeviceId: 'peripheral-1',
      );
      expect(peer.bleDeviceId, 'central-1');
    });

    test('isReachable requires an authenticated AND attested BLE session, '
        'not just a link', () {
      final linkOnly = PeerState(
        publicKey: _testPubkey(1),
        blePeripheralDeviceId: 'peripheral-1',
      );
      // A raw BLE link without a completed Noise session is not reachable —
      // onPeerConnected fires only after authentication (#2b).
      expect(linkOnly.isReachable, false);

      // Nor is an authenticated session on its own: the handshake is followed
      // by the attestation exchange, and "a peer becomes reachable when its
      // session is established, authenticated, and attested" (spec §Session
      // Establishment).
      final authenticated = linkOnly.copyWith(bleAuthenticated: true);
      expect(authenticated.isReachable, false);

      final attested = authenticated.withAttestation(
        MessageTransport.ble,
        PeerAttestation.unattested,
      );
      expect(attested.isReachable, true,
          reason: 'a peer whose platform provides no attestation is reported '
              'unattested, not refused');
    });

    test('activeTransport prefers BLE', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        bleCentralDeviceId: 'central-1',
        udpAddress: '[2001:db8::1]:4001',
      );
      expect(peer.activeTransport, PeerTransport.bleDirect);
    });

    test('hasPublicUdpAddress requires a globally routable UDP address', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        udpAddress: '[2606:4700::1]:4001',
      );

      expect(peer.lastDirectReachAt, isNull);
      expect(peer.hasPublicUdpAddress, isTrue);
    });

    test('hasPublicUdpAddress is false for non-routable UDP addresses', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        udpAddress: '10.0.0.4:4001',
      );

      expect(peer.hasPublicUdpAddress, isFalse);
    });

    test(
        'nearbyBlePeers includes any peer with a live BLE path regardless '
        'of connectionState', () {
      final stranger = _testPubkey(1);
      final friend = _testPubkey(2);
      final discoveredNoBle = _testPubkey(3);
      // A peer whose `connectionState` was somehow not flipped to connected
      // (stale Redux row, race during ANNOUNCE) but who still has a BLE
      // attachment should be counted as "nearby". The BLE attachment is the
      // ground truth, not the global state flag.
      final state = PeersState(
        peers: {
          _pubkeyHex(stranger): PeerState(
            publicKey: stranger,
            connectionState: PeerConnectionState.discovered,
            bleCentralDeviceId: 'central:stranger',
          ),
          _pubkeyHex(friend): PeerState(
            publicKey: friend,
            connectionState: PeerConnectionState.connected,
            blePeripheralDeviceId: 'peripheral:friend',
          ),
          _pubkeyHex(discoveredNoBle): PeerState(
            publicKey: discoveredNoBle,
            connectionState: PeerConnectionState.connected,
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final hexes = state.nearbyBlePeers.map((p) => p.pubkeyHex).toSet();
      expect(hexes, equals({_pubkeyHex(stranger), _pubkeyHex(friend)}),
          reason: 'Filter is `hasBleConnection`, full stop.');
    });

  });

  // =========================================================================
  // DiscoveredPeerState backoff getters
  // =========================================================================

  // =========================================================================
  // Unknown action
  // =========================================================================

  group('unknown action', () {
    test('returns state unchanged for unknown action type', () {
      const state = PeersState.initial;
      final result = peersReducer(state, 'unknown-action');

      expect(result, same(state));
    });
  });
}
