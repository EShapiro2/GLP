import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart'
    show processReachabilityTransitions;
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/session/application_identity.dart';
import 'package:grassroots_networking_core/src/store/peers_state.dart';

/// Tests for the reachability-transition diff that drives the per-underlay
/// `onPeerConnected` / `onPeerDisconnected` callbacks on `GrassrootsNetwork`.
///
/// The contract: fire connect for each underlay that becomes reachable and
/// disconnect for each underlay that stops being reachable, independently per
/// underlay. A peer reachable over the PAN and over IP fires two connects;
/// losing one of two live underlays fires a disconnect for that underlay while
/// the peer stays reachable over the other; a previously-reachable peer removed
/// from the store fires a disconnect for every underlay it still held. A UDP
/// session reports the underlay — LAN or WAN — it was attested on.
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
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
        // Reachability requires an authenticated (Noise) BLE session AND a
        // completed attestation exchange (spec §Session Establishment).
        bleAuthenticated: true,
        bleAttestation: PeerAttestation.unattested,
      );

  PeerState peerOnUdp(int seed, [Underlay underlay = Underlay.wan]) =>
      PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.udp,
        udpAddress: '10.0.0.$seed:9514',
        hasLiveUdpConnection: true,
        udpAttestation: PeerAttestation.unattested,
        udpUnderlay: underlay,
      );

  PeerState peerOnBoth(int seed) => PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
        bleAuthenticated: true,
        bleAttestation: PeerAttestation.unattested,
        udpAddress: '10.0.0.$seed:9514',
        hasLiveUdpConnection: true,
        udpAttestation: PeerAttestation.unattested,
        udpUnderlay: Underlay.wan,
      );

  PeerState peerOffline(int seed) => PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.disconnected,
        transport: PeerTransport.bleDirect,
      );

  /// One fired callback, recorded as a (pubkey-hex, underlay) tuple. The
  /// pubkey is stored as hex because `Uint8List` compares by identity, not
  /// contents, so record equality on the raw bytes would never match.
  late List<(String, Underlay)> connects;
  late List<(String, Underlay)> disconnects;
  late Map<String, Set<Underlay>> tracker;

  void onConnected(
    Uint8List pk,
    Underlay u,
    ApplicationIdentity? identity,
  ) =>
      connects.add((pubkeyHex(pk), u));
  void onDisconnected(Uint8List pk, Underlay u) =>
      disconnects.add((pubkeyHex(pk), u));

  void tick(PeersState s) => processReachabilityTransitions(
        peersState: s,
        lastKnownUnderlays: tracker,
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
      expect(connects, [(pubkeyHex(pubkey(1)), Underlay.pan)]);
      expect(disconnects, isEmpty);
    });

    test('idempotent: same reachable state across two ticks fires once', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOnBle(1)]));
      expect(connects, [(pubkeyHex(pubkey(1)), Underlay.pan)]);
      expect(disconnects, isEmpty);
    });

    test('peer on both transports fires a connect per medium, BLE before IP',
        () {
      tick(stateWith([peerOnBoth(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(disconnects, isEmpty);
    });

    test('peer with BLE then also UDP fires a second connect for UDP', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOnBoth(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(disconnects, isEmpty);
    });

    test('peer with both loses BLE while UDP remains: disconnect for BLE only',
        () {
      tick(stateWith([peerOnBoth(1)]));
      tick(stateWith([peerOnUdp(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.pan)]);
      // No spurious UDP event; UDP stayed live throughout.
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
    });

    test('peer with both loses UDP while BLE remains: disconnect for UDP only',
        () {
      tick(stateWith([peerOnBoth(1)]));
      tick(stateWith([peerOnBle(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.wan)]);
    });

    test('peer loses its last transport fires disconnect for that medium', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOffline(1)]));
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.pan)]);
      // Unreachable peer holds no tracker entry.
      expect(tracker.containsKey(pubkeyHex(pubkey(1))), false);
    });

    test('peer goes offline then comes back on a different medium', () {
      tick(stateWith([peerOnBle(1)]));
      tick(stateWith([peerOffline(1)]));
      tick(stateWith([peerOnUdp(1)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.pan)]);
    });

    test(
        'peer removed from store while reachable on both surfaces a disconnect '
        'per held medium', () {
      tick(stateWith([peerOnBoth(1)]));
      expect(tracker[pubkeyHex(pubkey(1))],
          {Underlay.pan, Underlay.wan});

      // Peer entry vanishes (PeerRemovedAction / StalePeersRemovedAction).
      tick(stateWith([]));
      expect(disconnects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(tracker.containsKey(pubkeyHex(pubkey(1))), false);
    });

    test('multiple peers tracked independently', () {
      tick(stateWith([peerOnBle(1), peerOnUdp(2)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.pan),
        (pubkeyHex(pubkey(2)), Underlay.wan),
      ]);

      tick(stateWith([peerOnBle(1), peerOffline(2)]));
      expect(disconnects, [(pubkeyHex(pubkey(2)), Underlay.wan)]);

      tick(stateWith([peerOffline(1), peerOffline(2)]));
      expect(disconnects, [
        (pubkeyHex(pubkey(2)), Underlay.wan),
        (pubkeyHex(pubkey(1)), Underlay.pan),
      ]);
    });

    test('a UDP session reports the underlay it was attested on', () {
      tick(stateWith([peerOnUdp(1, Underlay.lan)]));
      tick(stateWith([peerOffline(1)]));
      tick(stateWith([peerOnUdp(1, Underlay.wan)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.lan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.lan)]);
    });

    test('a UDP session re-attested on the other IP underlay moves the peer',
        () {
      tick(stateWith([peerOnUdp(1, Underlay.lan)]));
      tick(stateWith([peerOnUdp(1, Underlay.wan)]));
      expect(connects, [
        (pubkeyHex(pubkey(1)), Underlay.lan),
        (pubkeyHex(pubkey(1)), Underlay.wan),
      ]);
      expect(disconnects, [(pubkeyHex(pubkey(1)), Underlay.lan)]);
    });

    test('null callbacks are safe', () {
      processReachabilityTransitions(
        peersState: stateWith([peerOnBle(1)]),
        lastKnownUnderlays: tracker,
        onConnected: null,
        onDisconnected: null,
      );
      // No throw; tracker still updated.
      expect(tracker[pubkeyHex(pubkey(1))], {Underlay.pan});
    });
  });

  // onPeerDiscovered is no longer driven by reachability transitions — it now
  // fires at ANNOUNCE receipt (identity learned, ahead of the Noise session),
  // decoupled from onPeerConnected. See GrassrootsNetwork._setupRouterCallbacks
  // (onPeerAnnounced) and the api.tex onPeerDiscovered note.
}
