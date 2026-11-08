import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/grassroots_network.dart'
    show processProximityLoss;
import 'package:grassroots_networking_core/src/models/peer.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/peers_state.dart';

/// Tests for the loss of an agent discovered on a proximity underlay, which
/// drives `onPeerLost(pk)` on the `pan` and `lan` sub-objects: "Callback when
/// an agent reported discovered on this underlay is no longer discovered on
/// it: its presence is no longer observed and it holds no session on the
/// underlay" (GNA cfbfeac §3.1).  An agent is lost where a tick observed its
/// presence or session on the underlay and a later one observes neither, or
/// where its entry leaves the store; a lost agent is discovered again on its
/// return.
void main() {
  Uint8List pubkey(int seed) =>
      Uint8List.fromList(List.generate(32, (i) => (seed + i) % 256));

  String hexOf(Uint8List p) =>
      p.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  PeersState stateWith(List<PeerState> peers) =>
      PeersState.initial.copyWith(peers: {for (final p in peers) p.pubkeyHex: p});

  /// A BLE path to the peer, its advertisement answered, no session yet.
  PeerState onBlePath(int seed) => PeerState(
        publicKey: pubkey(seed),
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
      );

  /// A session on the PAN.
  PeerState onPanSession(int seed) => PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.bleDirect,
        bleCentralDeviceId: 'ble-$seed',
        bleAuthenticated: true,
        bleAttestation: PeerAttestation.unattested,
      );

  /// A session on the LAN.
  PeerState onLanSession(int seed) => PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.connected,
        transport: PeerTransport.udp,
        udpAddress: '10.0.0.$seed:9514',
        hasLiveUdpConnection: true,
        udpAttestation: PeerAttestation.unattested,
        udpUnderlay: Underlay.lan,
      );

  /// Neither a path nor a session on any underlay.
  PeerState away(int seed) => PeerState(
        publicKey: pubkey(seed),
        connectionState: PeerConnectionState.disconnected,
        transport: PeerTransport.bleDirect,
      );

  late Map<ProximityUnderlay, Set<String>> discovered;
  late Map<ProximityUnderlay, Set<String>> present;
  late List<(String, ProximityUnderlay)> lost;

  void tick(PeersState s) => processProximityLoss(
        peersState: s,
        discoveredOn: discovered,
        presentOn: present,
        onLost: (pk, u) => lost.add((hexOf(pk), u)),
      );

  setUp(() {
    discovered = {for (final u in ProximityUnderlay.values) u: <String>{}};
    present = {for (final u in ProximityUnderlay.values) u: <String>{}};
    lost = [];
  });

  test('an agent on a BLE path and then on neither path nor session is lost '
      'on the PAN, and leaves the discovered set', () {
    final hex = hexOf(pubkey(1));
    discovered[ProximityUnderlay.pan]!.add(hex);
    tick(stateWith([onBlePath(1)]));
    tick(stateWith([onPanSession(1)]));
    expect(lost, isEmpty, reason: 'its presence and its session observed');
    tick(stateWith([away(1)]));
    expect(lost, [(hex, ProximityUnderlay.pan)]);
    expect(discovered[ProximityUnderlay.pan], isEmpty,
        reason: 'discovered again on its return');
    tick(stateWith([away(1)]));
    expect(lost, hasLength(1), reason: 'lost once');
  });

  test('an agent whose entry leaves the store is lost on every underlay it '
      'was discovered on', () {
    final hex = hexOf(pubkey(2));
    discovered[ProximityUnderlay.pan]!.add(hex);
    discovered[ProximityUnderlay.lan]!.add(hex);
    tick(stateWith([]));
    expect(lost.toSet(),
        {(hex, ProximityUnderlay.pan), (hex, ProximityUnderlay.lan)});
  });

  test('an agent discovered by its ANNOUNCE alone, its presence never '
      'observed after, is not lost while its entry stands', () {
    final hex = hexOf(pubkey(3));
    discovered[ProximityUnderlay.lan]!.add(hex);
    tick(stateWith([away(3)]));
    expect(lost, isEmpty);
  });

  test('the underlays are each their own: a session lost on the LAN while '
      'the BLE path stands is lost on the LAN alone', () {
    final hex = hexOf(pubkey(4));
    discovered[ProximityUnderlay.pan]!.add(hex);
    discovered[ProximityUnderlay.lan]!.add(hex);
    final both = onLanSession(4).copyWith(bleCentralDeviceId: 'ble-4');
    tick(stateWith([both]));
    tick(stateWith([onBlePath(4)]));
    expect(lost, [(hex, ProximityUnderlay.lan)]);
  });

  test('an agent never discovered is never lost', () {
    tick(stateWith([onPanSession(5)]));
    tick(stateWith([away(5)]));
    expect(lost, isEmpty);
  });
}
