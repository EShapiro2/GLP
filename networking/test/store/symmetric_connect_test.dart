import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/store/peers_actions.dart';
import 'package:grassroots_networking/src/store/peers_reducer.dart';
import 'package:grassroots_networking/src/store/peers_state.dart';

/// These tests pin down the user's "symmetric connect" requirement:
/// the Redux projection of the BLE transport must reach the same final state
/// regardless of the order in which path events and ANNOUNCEs arrive, and
/// must NOT depend on time-based inference.
void main() {
  Uint8List pubkey(int seed) =>
      Uint8List.fromList(List.generate(32, (i) => (seed + i) % 256));

  String pubkeyHex(Uint8List p) =>
      p.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  group('symmetric connect — both sides agree on connection state', () {
    test(
        'sequence A: discovered → connecting → connected → ANNOUNCE → '
        'peer is connected with correct central pathId', () {
      const pathId = 'central:AA:BB:CC:DD:EE:FF';
      final pk = pubkey(1);

      var state = PeersState.initial;
      state = peersReducer(state, BleDeviceDiscoveredAction(
        deviceId: pathId,
        rssi: -55,
      ));
      state = peersReducer(state, BleDeviceConnectingAction(pathId));
      state = peersReducer(state, BleDeviceConnectedAction(pathId));
      state = peersReducer(state, PeerAnnounceReceivedAction(
        publicKey: pk,
        nickname: 'Alice',
        protocolVersion: 1,
        rssi: -55,
        bleCentralDeviceId: pathId,
      ));

      final peer = state.peers[pubkeyHex(pk)]!;
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.bleCentralDeviceId, pathId);
      expect(peer.blePeripheralDeviceId, isNull);
      expect(state.discoveredBlePeers[pathId]!.isConnected, true);
    });

    test(
        'sequence B (peripheral side): peripheral path connected → '
        'ANNOUNCE arrives first → peer marked connected', () {
      const pathId = 'peripheral:AA:BB:CC:DD:EE:FF';
      final pk = pubkey(2);

      var state = PeersState.initial;
      state = peersReducer(state, PeerAnnounceReceivedAction(
        publicKey: pk,
        nickname: 'Bob',
        protocolVersion: 1,
        rssi: -50,
        blePeripheralDeviceId: pathId,
      ));

      final peer = state.peers[pubkeyHex(pk)]!;
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.blePeripheralDeviceId, pathId);
      expect(peer.bleCentralDeviceId, isNull);
    });

    test(
        'both peers run sequence A simultaneously — final states are '
        'symmetric (each sees the other as connected)', () {
      // Simulate device A's view (talking to peer B).
      var aView = PeersState.initial;
      const aPath = 'central:B-MAC';
      aView = peersReducer(aView,
          BleDeviceDiscoveredAction(deviceId: aPath, rssi: -55));
      aView = peersReducer(aView, BleDeviceConnectingAction(aPath));
      aView = peersReducer(aView, BleDeviceConnectedAction(aPath));
      aView = peersReducer(aView, PeerAnnounceReceivedAction(
        publicKey: pubkey(20),
        nickname: 'B',
        protocolVersion: 1,
        rssi: -55,
        bleCentralDeviceId: aPath,
      ));

      // Simulate device B's view (talking to peer A).
      var bView = PeersState.initial;
      const bPath = 'peripheral:A-MAC';
      bView = peersReducer(bView, PeerAnnounceReceivedAction(
        publicKey: pubkey(10),
        nickname: 'A',
        protocolVersion: 1,
        rssi: -55,
        blePeripheralDeviceId: bPath,
      ));

      // Symmetry: A sees B as connected, B sees A as connected.
      expect(aView.peers[pubkeyHex(pubkey(20))]!.isConnected, true);
      expect(bView.peers[pubkeyHex(pubkey(10))]!.isConnected, true);
    });
  });

  group('disconnect symmetry — plugin-driven only, no time inference', () {
    test(
        'PeerBleDisconnectedAction clears the matching role only, '
        'preserves the other role', () {
      final pk = pubkey(3);
      var state = PeersState.initial;
      // Establish a peer with both a central and peripheral path.
      state = peersReducer(state, PeerAnnounceReceivedAction(
        publicKey: pk,
        nickname: 'Carol',
        protocolVersion: 1,
        rssi: -50,
        bleCentralDeviceId: 'central:cmac',
        blePeripheralDeviceId: 'peripheral:cmac',
      ));
      expect(state.peers[pubkeyHex(pk)]!.bleCentralDeviceId, 'central:cmac');
      expect(state.peers[pubkeyHex(pk)]!.blePeripheralDeviceId,
          'peripheral:cmac');

      // Plugin emits disconnect for the central path only.
      state = peersReducer(state,
          PeerBleDisconnectedAction(pk, role: BleRole.central));

      final peer = state.peers[pubkeyHex(pk)]!;
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.blePeripheralDeviceId, 'peripheral:cmac');
      // Still connected because the peripheral role is alive.
      expect(peer.connectionState, PeerConnectionState.connected);
    });

    test('PeerBleDisconnectedAction clears connectionState when no transport remains',
        () {
      final pk = pubkey(4);
      var state = PeersState.initial;
      state = peersReducer(state, PeerAnnounceReceivedAction(
        publicKey: pk,
        nickname: 'Dave',
        protocolVersion: 1,
        rssi: -50,
        bleCentralDeviceId: 'central:dmac',
      ));
      expect(state.peers[pubkeyHex(pk)]!.isConnected, true);

      state = peersReducer(state,
          PeerBleDisconnectedAction(pk, role: BleRole.central));

      final peer = state.peers[pubkeyHex(pk)]!;
      expect(peer.bleCentralDeviceId, isNull);
      expect(peer.connectionState, PeerConnectionState.disconnected);
    });

    test('StalePeersRemovedAction does NOT mutate connectionState of friends',
        () {
      final pk = pubkey(5);
      final ancient = DateTime.now().subtract(const Duration(hours: 1));
      var state = PeersState(peers: {
        pubkeyHex(pk): PeerState(
          publicKey: pk,
          nickname: 'Eve',
          connectionState: PeerConnectionState.connected,
          isFriend: true,
          lastSeen: ancient,
          bleCentralDeviceId: 'central:emac',
        ),
      });

      state = peersReducer(state,
          StalePeersRemovedAction(const Duration(seconds: 30)));

      final peer = state.peers[pubkeyHex(pk)]!;
      // connectionState is unchanged — only plugin events / UDP events
      // can flip this.
      expect(peer.connectionState, PeerConnectionState.connected);
      expect(peer.bleCentralDeviceId, 'central:emac');
    });

    test(
        'StalePeersRemovedAction removes non-friend peers when stale, but '
        'never mutates their connectionState (avoids confusing UI)', () {
      final pk = pubkey(6);
      final ancient = DateTime.now().subtract(const Duration(hours: 1));
      var state = PeersState(peers: {
        pubkeyHex(pk): PeerState(
          publicKey: pk,
          nickname: 'Frank',
          connectionState: PeerConnectionState.connected,
          isFriend: false,
          lastSeen: ancient,
        ),
      });

      state = peersReducer(state,
          StalePeersRemovedAction(const Duration(seconds: 30)));

      // Stranger peer is removed; no need to flip connectionState.
      expect(state.peers, isEmpty);
    });
  });

  group('no-opinion guarantees — backoff & error fields are gone', () {
    test(
        'BleDeviceConnectionFailedAction: pure transition, no error/backoff '
        'fields stored', () {
      const pathId = 'central:gmac';
      var state = PeersState(discoveredBlePeers: {
        pathId: DiscoveredPeerState(
          transportId: pathId,
          rssi: -60,
          discoveredAt: DateTime.now(),
          lastSeen: DateTime.now(),
          isConnecting: true,
        ),
      });

      state = peersReducer(state, BleDeviceConnectionFailedAction(pathId));

      final disc = state.discoveredBlePeers[pathId]!;
      expect(disc.isConnecting, false);
      expect(disc.isConnected, false);
    });

    test(
        'BleDeviceDiscoveredAction creates a fresh entry that does NOT carry '
        'inferred state from a previous incarnation', () {
      const pathId = 'central:hmac';
      var state = PeersState.initial;
      state = peersReducer(state, BleDeviceDiscoveredAction(
        deviceId: pathId,
        rssi: -50,
      ));

      final disc = state.discoveredBlePeers[pathId]!;
      expect(disc.isConnecting, false);
      expect(disc.isConnected, false);
    });
  });
}
