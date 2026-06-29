import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/store/peers_state.dart';
import 'package:grassroots_networking/src/store/peers_actions.dart';
import 'package:grassroots_networking/src/store/peers_reducer.dart';
import 'package:grassroots_networking/src/models/peer.dart';

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
        nickname: 'Alice',
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
      expect(peer.nickname, 'Alice');
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
        nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'OldNick',
            connectionState: PeerConnectionState.disconnected,
            rssi: -90,
            protocolVersion: 1,
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        nickname: 'NewNick',
        protocolVersion: 3,
        rssi: -40,
        transport: PeerTransport.udp,
        udpAddress: '[2001:db8::1]:4001',
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.nickname, 'NewNick');
      expect(peer.protocolVersion, 3);
      expect(peer.rssi, -40);
      expect(peer.transport, PeerTransport.udp);
      expect(peer.udpAddress, '[2001:db8::1]:4001');
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
            nickname: 'OldNick',
            connectionState: PeerConnectionState.disconnected,
            transport: PeerTransport.udp,
            rssi: -90,
            protocolVersion: 1,
            udpAddress: udpAddress,
            udpAddressCandidates: const {udpAddress},
            isFriend: true,
            lastDirectReachAt: directReachAt,
            hasLiveUdpConnection: true,
          ),
        },
      );

      final result = peersReducer(
        initial,
        PeerAnnounceReceivedAction(
          publicKey: pubkey,
          nickname: 'NewNick',
          protocolVersion: 3,
          rssi: -40,
          transport: PeerTransport.udp,
          udpAddress: udpAddress,
        ),
      );

      final peer = result.peers[hex]!;
      expect(peer.nickname, 'NewNick');
      expect(peer.protocolVersion, 3);
      expect(peer.rssi, -40);
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.isFriend, isTrue);
      expect(peer.hasLiveUdpConnection, isTrue);
      expect(peer.lastDirectReachAt, directReachAt);
    });

    test('sets connectionState to connected', () {
      final pubkey = _testPubkey(2);
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        nickname: 'Bob',
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
            nickname: 'Alice',
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central-id',
            blePeripheralDeviceId: 'peripheral-id',
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        nickname: 'Alice',
        protocolVersion: 1,
        rssi: -40,
        transport: PeerTransport.udp,
        udpAddress: '[2001:db8::1]:4001',
        // No BLE device IDs — should preserve existing
      );

      final result = peersReducer(initial, action);

      final peer = result.peers[hex]!;
      expect(peer.bleCentralDeviceId, 'central-id');
      expect(peer.blePeripheralDeviceId, 'peripheral-id');
      expect(peer.udpAddress, '[2001:db8::1]:4001');
    });

    test('UDP ANNOUNCE records lastUdpSeen', () {
      final pubkey = _testPubkey(3);
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        nickname: 'Carol',
        protocolVersion: 1,
        rssi: -42,
        transport: PeerTransport.udp,
        udpAddress: '[2001:db8::3]:4003',
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
            nickname: 'Dana',
            connectionState: PeerConnectionState.connected,
            lastSeen: udpSeenAt,
            lastUdpSeen: udpSeenAt,
            udpAddress: '[2001:db8::4]:4004',
          ),
        },
      );
      final action = PeerAnnounceReceivedAction(
        publicKey: pubkey,
        nickname: 'Dana',
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
          hex: PeerState(publicKey: pubkey, nickname: 'Alice', rssi: -80),
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Alice',
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
            nickname: 'Eve',
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
            nickname: 'Alice',
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
        peers: {hex: PeerState(publicKey: pubkey, nickname: 'Alice')},
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
        peers: {hex: PeerState(publicKey: pubkey, nickname: 'Alice')},
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
        peers: {hex: PeerState(publicKey: pubkey, nickname: 'Alice')},
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
            nickname: 'Alice',
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
      expect(created.udpAddressCandidates, contains('[2001:db8::1]:4001'));
      expect(created.hasKnownAddress, isTrue);
      // A GLP-fed address is a dial-book hint only, never a live session.
      expect(created.isReachable, isFalse);
      expect(created.hasLiveUdpConnection, isFalse);
      expect(created.isFriend, isFalse);
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
  // FriendEstablishedAction
  // =========================================================================

  group('FriendEstablishedAction', () {
    test('sets isFriend=true on existing peer', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(publicKey: pubkey, nickname: 'Alice', isFriend: false),
        },
      );
      final action = FriendEstablishedAction(
        publicKey: pubkey,
        nickname: 'Alice',
      );

      final result = peersReducer(initial, action);

      expect(result.peers[hex]!.isFriend, true);
    });

    test('creates new peer with isFriend=true if not exists', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final action = FriendEstablishedAction(
        publicKey: pubkey,
        nickname: 'NewFriend',
      );

      final result = peersReducer(PeersState.initial, action);

      expect(result.peers.length, 1);
      final peer = result.peers[hex]!;
      expect(peer.isFriend, true);
      expect(peer.nickname, 'NewFriend');
      expect(peer.connectionState, PeerConnectionState.discovered);
    });

    test('creates new peer with empty nickname when nickname is null', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final action = FriendEstablishedAction(publicKey: pubkey);

      final result = peersReducer(PeersState.initial, action);

      expect(result.peers[hex]!.nickname, '');
      expect(result.peers[hex]!.isFriend, true);
    });
  });

  // =========================================================================
  // FriendRemovedAction
  // =========================================================================

  group('FriendRemovedAction', () {
    test('removes peer entirely if no BLE connection', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            nickname: 'Alice',
            isFriend: true,
            udpAddress: '[2001:db8::1]:4001',
            // no BLE device IDs
          ),
        },
      );
      final action = FriendRemovedAction(pubkey);

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), false);
      expect(result.peers, isEmpty);
    });

    test(
      'clears isFriend and UDP fields but keeps peer if has BLE central',
      () {
        final pubkey = _testPubkey(1);
        final hex = _pubkeyHex(pubkey);
        final initial = PeersState(
          peers: {
            hex: PeerState(
              publicKey: pubkey,
              nickname: 'Alice',
              connectionState: PeerConnectionState.connected,
              isFriend: true,
              bleCentralDeviceId: 'central-1',
              udpAddress: '[2001:db8::1]:4001',
            ),
          },
        );
        final action = FriendRemovedAction(pubkey);

        final result = peersReducer(initial, action);

        expect(result.peers.containsKey(hex), true);
        final peer = result.peers[hex]!;
        expect(peer.isFriend, false);
        expect(peer.udpAddress, isNull);
        expect(peer.bleCentralDeviceId, 'central-1');
        expect(peer.connectionState, PeerConnectionState.connected);
        expect(peer.transport, PeerTransport.bleDirect);
      },
    );

    test('keeps peer if has BLE peripheral', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            nickname: 'Alice',
            connectionState: PeerConnectionState.connected,
            isFriend: true,
            blePeripheralDeviceId: 'peripheral-1',
          ),
        },
      );
      final action = FriendRemovedAction(pubkey);

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), true);
      final peer = result.peers[hex]!;
      expect(peer.isFriend, false);
      expect(peer.blePeripheralDeviceId, 'peripheral-1');
    });

    test('is a no-op for unknown peer', () {
      const state = PeersState.initial;
      final action = FriendRemovedAction(_testPubkey(99));

      final result = peersReducer(state, action);

      expect(result, same(state));
    });
  });

  // =========================================================================
  // StalePeersRemovedAction
  // =========================================================================

  group('StalePeersRemovedAction', () {
    test('removes stale non-friend peers', () {
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            nickname: 'Alice',
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
            isFriend: false,
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), false);
    });

    test('does NOT mutate connectionState or BLE IDs of stale friends', () {
      // Strict-projection contract: timer-driven reducers may not flip
      // connectionState or clear BLE device IDs. Those are exclusively
      // driven by plugin events (BLE) and UDX events (UDP). The stale
      // reducer's only job is bounding memory by removing non-friend
      // peers we haven't heard from.
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            nickname: 'Alice',
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
            isFriend: true,
            bleCentralDeviceId: 'central-1',
            blePeripheralDeviceId: 'peripheral-1',
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

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
            nickname: 'Alice',
            connectionState: PeerConnectionState.connected,
            lastSeen: now.subtract(const Duration(seconds: 10)),
            isFriend: false,
          ),
        },
      );
      final action = StalePeersRemovedAction(const Duration(minutes: 2));

      final result = peersReducer(initial, action);

      expect(result.peers.containsKey(hex), true);
      expect(result.peers[hex]!.connectionState, PeerConnectionState.connected);
    });

    test('removes stale non-friend peers regardless of connectionState', () {
      // The reducer doesn't gate on connectionState — that field is
      // plugin-/UDX-driven, not timer-driven. Non-friend stale peers are
      // removed for memory pressure whether they read as connected or
      // disconnected at the moment the timer fires.
      final pubkey = _testPubkey(1);
      final hex = _pubkeyHex(pubkey);
      final now = DateTime.now();
      final initial = PeersState(
        peers: {
          hex: PeerState(
            publicKey: pubkey,
            nickname: 'Alice',
            connectionState: PeerConnectionState.disconnected,
            lastSeen: now.subtract(const Duration(minutes: 10)),
            isFriend: false,
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
        nickname: 'Alice',
        bleCentralDeviceId: 'central-1',
      );
      expect(peer.hasBleConnection, true);
      expect(peer.bleDeviceId, 'central-1');
    });

    test('hasBleConnection is true with peripheral ID', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        blePeripheralDeviceId: 'peripheral-1',
      );
      expect(peer.hasBleConnection, true);
      expect(peer.bleDeviceId, 'peripheral-1');
    });

    test('hasBleConnection is false with no IDs', () {
      final peer = PeerState(publicKey: _testPubkey(1), nickname: 'Alice');
      expect(peer.hasBleConnection, false);
      expect(peer.bleDeviceId, isNull);
    });

    test('bleDeviceId prefers central over peripheral', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        bleCentralDeviceId: 'central-1',
        blePeripheralDeviceId: 'peripheral-1',
      );
      expect(peer.bleDeviceId, 'central-1');
    });

    test('isReachable requires an authenticated BLE session, not just a link',
        () {
      final linkOnly = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        blePeripheralDeviceId: 'peripheral-1',
      );
      // A raw BLE link without a completed Noise session is not reachable —
      // onPeerConnected fires only after authentication (#2b).
      expect(linkOnly.isReachable, false);

      final authenticated = linkOnly.copyWith(bleAuthenticated: true);
      expect(authenticated.isReachable, true);
    });

    test('activeTransport prefers BLE', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        bleCentralDeviceId: 'central-1',
        udpAddress: '[2001:db8::1]:4001',
      );
      expect(peer.activeTransport, PeerTransport.bleDirect);
    });

    test('isWellConnected only requires a globally routable UDP address', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        udpAddress: '[2606:4700::1]:4001',
      );

      expect(peer.lastDirectReachAt, isNull);
      expect(peer.hasPublicUdpAddress, isTrue);
      expect(peer.isWellConnected, isTrue);
    });

    test('isWellConnected is false for non-routable UDP addresses', () {
      final peer = PeerState(
        publicKey: _testPubkey(1),
        nickname: 'Alice',
        udpAddress: '10.0.0.4:4001',
      );

      expect(peer.hasPublicUdpAddress, isFalse);
      expect(peer.isWellConnected, isFalse);
    });

    test(
        'nearbyBlePeers includes any peer with a live BLE path regardless '
        'of connectionState or friendship', () {
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
            nickname: 'Stranger',
            connectionState: PeerConnectionState.discovered,
            bleCentralDeviceId: 'central:stranger',
          ),
          _pubkeyHex(friend): PeerState(
            publicKey: friend,
            nickname: 'Friend',
            connectionState: PeerConnectionState.connected,
            blePeripheralDeviceId: 'peripheral:friend',
            isFriend: true,
          ),
          _pubkeyHex(discoveredNoBle): PeerState(
            publicKey: discoveredNoBle,
            nickname: 'UdpOnly',
            connectionState: PeerConnectionState.connected,
            udpAddress: '[2001:db8::1]:4001',
          ),
        },
      );
      final hexes = state.nearbyBlePeers.map((p) => p.pubkeyHex).toSet();
      expect(hexes, equals({_pubkeyHex(stranger), _pubkeyHex(friend)}),
          reason: 'Filter is `hasBleConnection`, full stop.');
    });

    test('onlineFriends filters purely by friend × live UDP, ignores BLE', () {
      final udpFriend = _testPubkey(1);
      final bleFriend = _testPubkey(2);
      final udpStranger = _testPubkey(3);
      final state = PeersState(
        peers: {
          _pubkeyHex(udpFriend): PeerState(
            publicKey: udpFriend,
            nickname: 'UdpFriend',
            connectionState: PeerConnectionState.disconnected,
            hasLiveUdpConnection: true,
            isFriend: true,
          ),
          _pubkeyHex(bleFriend): PeerState(
            publicKey: bleFriend,
            nickname: 'BleFriend',
            connectionState: PeerConnectionState.connected,
            bleCentralDeviceId: 'central:friend',
            isFriend: true,
          ),
          _pubkeyHex(udpStranger): PeerState(
            publicKey: udpStranger,
            nickname: 'UdpStranger',
            hasLiveUdpConnection: true,
          ),
        },
      );
      final hexes = state.onlineFriends.map((p) => p.pubkeyHex).toSet();
      expect(hexes, equals({_pubkeyHex(udpFriend)}),
          reason: 'A friend with a live UDP stream is "online" even if '
              '`connectionState` says disconnected. BLE-only friends and '
              'UDP-connected strangers do not count.');
    });
  });

  group('PeersState rendezvous server getters', () {
    test(
      'wellConnectedFriends accepts public-address friends without proof',
      () {
        final friendPubkey = _testPubkey(12);
        final privateFriendPubkey = _testPubkey(13);
        final state = PeersState(
          peers: {
            _pubkeyHex(friendPubkey): PeerState(
              publicKey: friendPubkey,
              nickname: 'Friend',
              isFriend: true,
              udpAddress: '[2606:4700::12]:4012',
            ),
            _pubkeyHex(privateFriendPubkey): PeerState(
              publicKey: privateFriendPubkey,
              nickname: 'Private',
              isFriend: true,
              udpAddress: '10.0.0.13:4013',
            ),
          },
        );

        expect(
          state.wellConnectedFriends.map((p) => p.publicKey).toList(),
          equals([friendPubkey]),
        );
      },
    );

    // friendRvServers aggregation moved to FriendshipsState (knownRvServers
    // is now friendship-scoped). Covered by friendships_state_test.

    test('stores FRIEND_LIST updates from direct friends only', () {
      final friendPubkey = _testPubkey(20);
      final strangerPubkey = _testPubkey(21);
      final commonPubkey = _testPubkey(22);
      final friendHex = _pubkeyHex(friendPubkey);
      final strangerHex = _pubkeyHex(strangerPubkey);
      final commonHex = _pubkeyHex(commonPubkey);
      final state = PeersState(
        peers: {
          friendHex: PeerState(
            publicKey: friendPubkey,
            nickname: 'Friend',
            isFriend: true,
          ),
          strangerHex: PeerState(
            publicKey: strangerPubkey,
            nickname: 'Stranger',
            isFriend: false,
          ),
        },
      );

      final fromStranger = peersReducer(
        state,
        PeerFriendListUpdatedAction(
          publicKey: strangerPubkey,
          friendPubkeyHexes: {commonHex},
        ),
      );
      expect(fromStranger, same(state));

      final fromFriend = peersReducer(
        state,
        PeerFriendListUpdatedAction(
          publicKey: friendPubkey,
          friendPubkeyHexes: {commonHex, friendHex},
        ),
      );

      expect(fromFriend.friendsOfFriends[friendHex], equals({commonHex}));
    });

    test(
      'commonFriendHexesWith intersects direct friends with FoF entries',
      () {
        final friendPubkey = _testPubkey(23);
        final commonPubkey = _testPubkey(24);
        final remoteOnlyPubkey = _testPubkey(25);
        final friendHex = _pubkeyHex(friendPubkey);
        final commonHex = _pubkeyHex(commonPubkey);
        final remoteOnlyHex = _pubkeyHex(remoteOnlyPubkey);
        final state = PeersState(
          peers: {
            friendHex: PeerState(
              publicKey: friendPubkey,
              nickname: 'Friend',
              isFriend: true,
            ),
            commonHex: PeerState(
              publicKey: commonPubkey,
              nickname: 'Common',
              isFriend: true,
            ),
            remoteOnlyHex: PeerState(
              publicKey: remoteOnlyPubkey,
              nickname: 'RemoteOnly',
              isFriend: false,
            ),
          },
          friendsOfFriends: {
            friendHex: {commonHex, remoteOnlyHex},
          },
        );

        expect(state.commonFriendHexesWith(friendHex), equals({commonHex}));
      },
    );

    test(
      'mediatorsForFriend returns live direct friends that advertise target',
      () {
        final mediatorPubkey = _testPubkey(26);
        final offlineMediatorPubkey = _testPubkey(27);
        final targetPubkey = _testPubkey(28);
        final mediatorHex = _pubkeyHex(mediatorPubkey);
        final offlineMediatorHex = _pubkeyHex(offlineMediatorPubkey);
        final targetHex = _pubkeyHex(targetPubkey);
        final state = PeersState(
          peers: {
            mediatorHex: PeerState(
              publicKey: mediatorPubkey,
              nickname: 'Mediator',
              isFriend: true,
              hasLiveUdpConnection: true,
            ),
            offlineMediatorHex: PeerState(
              publicKey: offlineMediatorPubkey,
              nickname: 'Offline',
              isFriend: true,
            ),
            targetHex: PeerState(
              publicKey: targetPubkey,
              nickname: 'Target',
              isFriend: true,
            ),
          },
          friendsOfFriends: {
            mediatorHex: {targetHex},
            offlineMediatorHex: {targetHex},
          },
        );

        expect(
          state.mediatorsForFriend(targetHex).map((p) => p.pubkeyHex).toList(),
          equals([mediatorHex]),
        );
      },
    );

    test('FriendRemovedAction removes FoF entry and references', () {
      final removedPubkey = _testPubkey(29);
      final remainingPubkey = _testPubkey(30);
      final removedHex = _pubkeyHex(removedPubkey);
      final remainingHex = _pubkeyHex(remainingPubkey);
      final state = PeersState(
        peers: {
          removedHex: PeerState(
            publicKey: removedPubkey,
            nickname: 'Removed',
            isFriend: true,
          ),
          remainingHex: PeerState(
            publicKey: remainingPubkey,
            nickname: 'Remaining',
            isFriend: true,
          ),
        },
        friendsOfFriends: {
          removedHex: {remainingHex},
          remainingHex: {removedHex},
        },
      );

      final result = peersReducer(state, FriendRemovedAction(removedPubkey));

      expect(result.friendsOfFriends.containsKey(removedHex), isFalse);
      expect(result.friendsOfFriends[remainingHex], isEmpty);
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
