/// A simulated layer for the phones of a test in one process
/// (test/superapp_nearby_test.dart, test/scripted_build_test.dart): the
/// network carries payloads between keys, and reports discovery as GNA's
/// layer does: an agent on a proximity underlay contacts another it does not
/// know only while the trust of that underlay is open at both (GNA, "Trust
/// levels": "Open: The agent completes the ANNOUNCE exchange and the Noise XX
/// handshake with any unknown contact"; "Closed: The agent answers only an IK
/// handshake from a known key"), and then each is told the other,
/// `onPeerDiscovered(pk)` on that underlay (GNA, Section "Proximity").
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';

typedef AirKeyPair = ({PubKey pub, Uint8List priv});

/// Carries payloads between the [AirNetwork]s joined to it, by key, each
/// delivery in a later turn of the event loop, as a layer delivers; and
/// tells two phones of each other once both have opened the trust of a
/// proximity underlay.
class Air {
  final Map<String, AirNetwork> _at = {};
  int _next = 0;

  /// The pairs told of each other, per underlay: `<underlay> <hex> <hex>`.
  final Set<String> _met = {};

  void carry(PubKey from, PubKey to, Uint8List payload) {
    final id = 'm${_next++}';
    Timer.run(() =>
        _at[to.hex]?.onMessageReceived?.call(from, payload, id, Underlay.pan));
  }

  /// [n]'s trust on [u] changed: every pair now open at both is told of
  /// each other, once.
  void trustChanged(AirNetwork n, ProximityUnderlay u) {
    for (final o in _at.values) {
      if (identical(o, n)) continue;
      if (n.trust[u] != TrustLevel.open || o.trust[u] != TrustLevel.open) {
        continue;
      }
      final pair = ([n.keyPair.pub.hex, o.keyPair.pub.hex]..sort()).join(' ');
      if (!_met.add('${u.name} $pair')) continue;
      Timer.run(() {
        n.onPeerDiscovered?.call(o.keyPair.pub, u);
        o.onPeerDiscovered?.call(n.keyPair.pub, u);
      });
    }
  }
}

/// A [GlpNetwork] in the [Air], instantiated with its key pair, both
/// proximity underlays Closed until the program declares them open.
class AirNetwork extends GlpNetwork {
  final AirKeyPair keyPair;
  final Air air;
  final Map<ProximityUnderlay, TrustLevel> trust = {
    for (final u in ProximityUnderlay.values) u: TrustLevel.closed,
  };

  /// Each trust declaration, in order: `<underlay> <level>`.
  final List<String> declared = [];

  /// Told each trust declaration as it is made, after [declared] holds it.
  void Function()? onDeclared;

  /// The known peers the program declared, `declare(peer(K))`, by key hex.
  final Set<String> known = {};

  AirNetwork(this.keyPair, this.air) {
    air._at[keyPair.pub.hex] = this;
  }

  @override
  PubKey getIdentity() => keyPair.pub;

  @override
  Future<void> send(PubKey pk, Uint8List payload) {
    air.carry(keyPair.pub, pk, payload);
    return Future.value();
  }

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {
    declared.add('${underlay.name} ${level.name}');
    trust[underlay] = level;
    air.trustChanged(this, underlay);
    onDeclared?.call();
  }

  @override
  void acknowledge(String messageId) {}
  @override
  void putKnownPeer(PubKey pk) => known.add(pk.hex);
  @override
  void removeKnownPeer(PubKey pk) => known.remove(pk.hex);
  @override
  bool isPeerReachable(PubKey pk) => air._at.containsKey(pk.hex);
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [Underlay.pan];
  @override
  Uint8List sign(Uint8List message) => throw UnimplementedError();
  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      throw UnimplementedError();
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  String? getPublicAddress() => null;
  @override
  String? networkIdentity() => null;
  @override
  void putPeerAddress(PubKey pk, String address) {}
  @override
  String? observedPeerAddress(PubKey pk) => null;
  @override
  void punchUdp(String address) {}
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      Future.value(false);
  @override
  Future<void> removePlace(String place) => Future.value();
}

