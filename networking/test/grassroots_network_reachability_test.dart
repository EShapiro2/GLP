import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart'
    show processReachabilityTransitions;
import 'package:grassroots_networking/src/models/peer.dart';
import 'package:grassroots_networking/src/store/messages_actions.dart';
import 'package:grassroots_networking/src/store/peers_state.dart';

/// Tests for the reachability-transition diff that drives the per-transport
/// `onPeerConnected` / `onPeerDisconnected` callbacks on `GrassrootsNetwork`.
///
/// The contract: fire connect for each transport that becomes reachable and
/// disconnect for each transport that stops being reachable, independently per
/// medium. A peer reachable over both BLE and IP fires two connects; losing one
/// of two live transports fires a disconnect for that medium while the peer
/// stays reachable over the other; a previously-reachable peer removed from the
/// store fires a disconnect for every medium it still held.
void main() {
  Uint8List pubkey(int seed) =>
      Uint8List.fromList(List.generate(32, (i) => (seed + i) % 256));

  String pubkeyHex(Uint8List p) =>
      p.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  PeersState stateWith(List<PeerState> peers) {
    final map = <String, PeerState>{
      for (final p in peers) p.pubkeyHex: p,
    };
    return PeersState.initial.copyWith(peers: map);
  }

  PeerState peerOnBle(int seed) => PeerState(
        publicKey: pubkey(seed),
        nickname: 'P$seed',
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
        // Reachability now requires an authenticated (Noise) BLE session.
        bleAuthenticated: true,
      );

  PeerState peerOnUdp(int seed) => PeerState(
        publicKey: pubkey(seed),
        nickname: 'P$seed',
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.udp,
        udpAddress: '10.0.0.$seed:9514',
        hasLiveUdpConnection: true,
      );

  PeerState peerOnBoth(int seed) => PeerState(
        publicKey: pubkey(seed),
        nickname: 'P$seed',
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
        bleAuthenticated: true,
        udpAddress: '10.0.0.$seed:9514',
        hasLiveUdpConnection: true,
      );

  PeerState peerOffline(int seed) => PeerState(
        publicKey: pubkey(seed),
        nickname: 'P$seed',
        connectionState: PeerConnectionState.disconnected,
        transport: PeerTransport.bleDirect,
      );

  /// One fired callback, recorded as a (pubkey-hex, transport) tuple. The
  /// pubkey is stored as hex because `Uint8List` compares by identity, not
  /// contents, so record equality on the raw bytes would never match.
  late List<(String, MessageTransport)> connects;
  late List<(String, MessageTransport)> disconnects;
  late Map<String, Set<MessageTransport>> tracker;

  void onConnected(Uint8List pk, MessageTransport t) =>
      connects.add((pubkeyHex(pk), t));
  void onDisconnected(Uint8List pk, MessageTransport t) =>
      disconnects.add((pubkeyHex(pk), t));

  void tick(PeersState s) => processReachabilityTransitions(
        peersState: s,
        lastKnownTransports: tracker,
        onConnected: onConnected,
        onDisconnected: onDisconnected,
      );

  setUp(() {
    connects = [];
    disconnects = [];
    tracker = {};
  });

  group('reachability transitions', () {
    test('new BLE-reachable peer fires onPeerConnected for BLE', () {
      tick(stateWith([peerOnBle(1)]));
      expect(connects, [(pubkeyHex(pubkey(1)), MessageTransport.ble)]);
      expect(disconnects, isEmpty);
    });

    test('idempotent: same reachable state across two ticks fires once', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOnBle(1)]));
      expect(connects, [(pubkeyHex(pubkey(1)), MessageTransport.ble)]);
      expect(disconnects, isEmpty);
    });

    test('peer on both transports fires a connect per medium, BLE before IP',
        () {
      tick(stateWith([peerOnBoth(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(1)), MessageTransport.udp),
      ]);
      expect(disconnects, isEmpty);
    });

    test('peer with BLE then also UDP fires a second connect for UDP', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOnBoth(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(1)), MessageTransport.udp),
      ]);
      expect(disconnects, isEmpty);
    });

    test('peer with both loses BLE while UDP remains: disconnect for BLE only',
        () {
      tick(stateWith([peerOnBoth(1)]));
      tick(stateWith([peerOnUdp(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), MessageTransport.ble)]);
      // No spurious UDP event; UDP stayed live throughout.
      expect(connects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(1)), MessageTransport.udp),
      ]);
    });

    test('peer with both loses UDP while BLE remains: disconnect for UDP only',
        () {
      tick(stateWith([peerOnBoth(1)]));
      tick(stateWith([peerOnBle(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), MessageTransport.udp)]);
    });

    test('peer loses its last transport fires disconnect for that medium', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOffline(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), MessageTransport.ble)]);
      // Unreachable peer holds no tracker entry.
      expect(tracker.containsKey(pubkeyHex(pubkey(1))), false);
    });

    test('peer goes offline then comes back on a different medium', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOffline(1)]));
      tick(stateWith([peerOnUdp(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(1)), MessageTransport.udp),
      ]);
      expect(disconnects, [(pubkeyHex(pubkey(1)), MessageTransport.ble)]);
    });

    test(
        'peer removed from store while reachable on both surfaces a disconnect '
        'per held medium', () {
      tick(stateWith([peerOnBoth(1)]));
      expect(tracker[pubkeyHex(pubkey(1))],
          {MessageTransport.ble, MessageTransport.udp});

      // Peer entry vanishes (PeerRemovedAction / StalePeersRemovedAction).
      tick(stateWith([]));
      expect(disconnects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(1)), MessageTransport.udp),
      ]);
      expect(tracker.containsKey(pubkeyHex(pubkey(1))), false);
    });

    test('multiple peers tracked independently', () {
      tick(stateWith([peerOnBle(1), peerOnUdp(2)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
        (pubkeyHex(pubkey(2)), MessageTransport.udp),
      ]);

      tick(stateWith([peerOnBle(1), peerOffline(2)]));
      expect(disconnects, [(pubkeyHex(pubkey(2)), MessageTransport.udp)]);

      tick(stateWith([peerOffline(1), peerOffline(2)]));
      expect(disconnects, [
        (pubkeyHex(pubkey(2)), MessageTransport.udp),
        (pubkeyHex(pubkey(1)), MessageTransport.ble),
      ]);
    });

    test('null callbacks are safe', () {
      processReachabilityTransitions(
        peersState: stateWith([peerOnBle(1)]),
        lastKnownTransports: tracker,
        onConnected: null,
        onDisconnected: null,
      );
      // No throw; tracker still updated.
      expect(tracker[pubkeyHex(pubkey(1))], {MessageTransport.ble});
    });
  });

  // onPeerDiscovered is no longer driven by reachability transitions — it now
  // fires at ANNOUNCE receipt (identity learned, ahead of the Noise session),
  // decoupled from onPeerConnected. See GrassrootsNetwork._setupRouterCallbacks
  // (onPeerAnnounced) and the api.tex onPeerDiscovered note.
}
