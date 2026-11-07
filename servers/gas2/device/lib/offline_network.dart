// A networking layer with no network of its own: the person's identity, their
// signature, and no one near.  GAS on the web runs each person's agent over
// it, wrapped by the relay (RelayGlpNetwork, relayOnly), which carries every
// message: an agent on a server has no Bluetooth and no local network.
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:sodium/sodium_sumo.dart';

class OfflineGlpNetwork implements GlpNetwork {
  final SodiumSumo sodium;
  final PubKey me;
  final SecureKey _key;

  /// [privateKey]: libsodium's Ed25519 secret key, seed then public key.
  OfflineGlpNetwork({required this.sodium, required this.me, required Uint8List privateKey})
      : _key = SecureKey.fromList(sodium, privateKey);

  @override
  PubKey getIdentity() => me;

  @override
  Uint8List sign(Uint8List message) => sodium.crypto.sign.detached(message: message, secretKey: _key);

  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) {
    try {
      return sodium.crypto.sign.verifyDetached(message: message, signature: signature, publicKey: signer.bytes);
    } on Object {
      return false;
    }
  }

  // No one is near, and nothing is sent but by the relay.
  @override
  bool isPeerReachable(PubKey pk) => false;
  @override
  List<Underlay> peerUnderlays(PubKey pk) => const [];
  @override
  Future<void> send(PubKey pk, Uint8List payload) async =>
      throw StateError('no network here: the relay sends');
  @override
  void acknowledge(String messageId) {}
  @override
  List<DiscoveredPeer> getPeers() => const [];
  @override
  void putKnownPeer(PubKey pk) {}
  @override
  void removeKnownPeer(PubKey pk) {}
  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {}
  @override
  String? getPublicAddress() => null;
  @override
  String? networkIdentity() => null;
  @override
  void putPeerAddress(PubKey pk, String address) {}
  @override
  void putRendezvousServer(PubKey pk, String address) {}
  @override
  void removeRendezvousServer(PubKey pk) {}
  @override
  String? observedPeerAddress(PubKey pk) => null;
  @override
  void punchUdp(String address) {}
  @override
  Future<bool> declarePlace(String place, double radiusMetres) async => false;
  @override
  Future<void> removePlace(String place) async {}

  @override
  void Function(PubKey pk, Underlay underlay, Object? identity)? onPeerConnected;
  @override
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;
  @override
  void Function(PubKey sender, Uint8List payload, String messageId, Underlay underlay)? onMessageReceived;
  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;
  @override
  void Function(String? oldAddress, String? newAddress)? onConnectivityStatusChanged;
  @override
  void Function(String? oldId, String? newId)? onNetworkChanged;
  @override
  void Function(String place, PlaceEvent event)? onPlaceEvent;
}
