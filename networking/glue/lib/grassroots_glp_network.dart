/// The runtime's network interface over the networking layer: GNA's backend
/// glue for an agent on a phone.  [GrassrootsGlpNetwork] implements
/// glp_runtime's `GlpNetwork` over [GrassrootsNetwork].
///
/// A library of the glue package, beside the layer's core and depending on
/// the layer and the runtime, so that neither the core nor the Flutter package
/// carries the GLP runtime (GNA, 2026-10-11); and a library of its own, so
/// that the layer's library does not carry the runtime's names, several of
/// which (`Underlay`, `ProximityUnderlay`, `PlaceEvent`) the two share.
///
/// IGLP, Implementation Notes, "Networking Seam", One interface: "The
/// multiagent runtime reaches the network through one interface, the one the
/// GLP Networking API specifies"; that interface is the runtime's
/// `GlpNetwork`.  This class implements it over [GrassrootsNetwork], the layer
/// over the three underlays (GNA, Section "Networking API").
///
/// Every function the layer has passes straight through, and nothing is
/// interpreted: a key travels as its 32 bytes, an underlay, a trust level and
/// a place event by their names, which the two sides share; the attested
/// application identity of `onPeerConnected` travels as the layer reports it,
/// since "the runtime relies on none of it" (IGLP, The contract).
///
/// Every function of the API passes straight through (GNA, 2026-10-11, Task
/// C): `getPeers` answers the agents on the proximity underlays, and
/// `observedPeerAddress` the address at which the layer observes a connected
/// peer.  Until 2026-10-12 the two threw [UnsupportedError], the layer's
/// answering every reachable peer and the dial-book address.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/settings_state.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';

/// [glp.GlpNetwork] over a [GrassrootsNetwork].
class GrassrootsGlpNetwork implements glp.GlpNetwork {
  /// The layer this glue passes through to.
  final GrassrootsNetwork layer;

  GrassrootsGlpNetwork(this.layer);

  static glp.PubKey _key(Uint8List bytes) => glp.PubKey.fromList(bytes);

  static glp.Underlay _underlay(Underlay u) => glp.Underlay.values.byName(u.name);

  // --- Identity ---

  @override
  glp.PubKey getIdentity() => _key(layer.getIdentity());

  // --- Known peers ---

  @override
  void putKnownPeer(glp.PubKey pk) => layer.putKnownPeer(pk.bytes);

  @override
  void removeKnownPeer(glp.PubKey pk) => layer.removeKnownPeer(pk.bytes);

  // --- Connection and reachability ---

  void Function(glp.PubKey pk, glp.Underlay underlay, Object? identity)?
      _onPeerConnected;

  @override
  void Function(glp.PubKey pk, glp.Underlay underlay, Object? identity)?
      get onPeerConnected => _onPeerConnected;

  @override
  set onPeerConnected(
      void Function(glp.PubKey pk, glp.Underlay underlay, Object? identity)?
          cb) {
    _onPeerConnected = cb;
    layer.onPeerConnected = cb == null
        ? null
        : (pk, underlay, identity) => cb(_key(pk), _underlay(underlay), identity);
  }

  void Function(glp.PubKey pk, glp.Underlay underlay)? _onPeerDisconnected;

  @override
  void Function(glp.PubKey pk, glp.Underlay underlay)? get onPeerDisconnected =>
      _onPeerDisconnected;

  @override
  set onPeerDisconnected(
      void Function(glp.PubKey pk, glp.Underlay underlay)? cb) {
    _onPeerDisconnected = cb;
    layer.onPeerDisconnected = cb == null
        ? null
        : (pk, underlay) => cb(_key(pk), _underlay(underlay));
  }

  @override
  bool isPeerReachable(glp.PubKey pk) => layer.isPeerReachable(pk.bytes);

  @override
  List<glp.Underlay> peerUnderlays(glp.PubKey pk) =>
      [for (final u in layer.peerUnderlays(pk.bytes)) _underlay(u)];

  // --- Point-to-point communication ---

  @override
  void send(glp.PubKey pk, Uint8List payload) =>
      unawaited(layer.send(pk.bytes, payload));

  void Function(glp.PubKey sender, Uint8List payload, String messageId,
      glp.Underlay underlay)? _onMessageReceived;

  @override
  void Function(glp.PubKey sender, Uint8List payload, String messageId,
      glp.Underlay underlay)? get onMessageReceived => _onMessageReceived;

  @override
  set onMessageReceived(
      void Function(glp.PubKey sender, Uint8List payload, String messageId,
              glp.Underlay underlay)?
          cb) {
    _onMessageReceived = cb;
    layer.onMessageReceived = cb == null
        ? null
        : (sender, payload, messageId, underlay) =>
            cb(_key(sender), payload, messageId, _underlay(underlay));
  }

  // --- Cryptography ---

  @override
  Uint8List sign(Uint8List message) => layer.sign(message);

  @override
  bool verify(glp.PubKey signer, Uint8List message, Uint8List signature) =>
      layer.verify(signer.bytes, message, signature);

  // --- Proximity underlays ---

  @override
  List<glp.DiscoveredPeer> getPeers() => [
        for (final peer in layer.getPeers())
          glp.DiscoveredPeer(_key(peer.publicKey),
              glp.ProximityUnderlay.values.byName(peer.underlay.name)),
      ];

  void Function(glp.PubKey pk, glp.ProximityUnderlay underlay)?
      _onPeerDiscovered;

  @override
  void Function(glp.PubKey pk, glp.ProximityUnderlay underlay)?
      get onPeerDiscovered => _onPeerDiscovered;

  @override
  set onPeerDiscovered(
      void Function(glp.PubKey pk, glp.ProximityUnderlay underlay)? cb) {
    _onPeerDiscovered = cb;
    layer.onPeerDiscovered = cb == null
        ? null
        : (pk, underlay) =>
            cb(_key(pk), glp.ProximityUnderlay.values.byName(underlay.name));
  }

  @override
  void setTrustLevel(glp.ProximityUnderlay underlay, glp.TrustLevel level) =>
      unawaited(layer.setTrustLevel(
        ProximityUnderlay.values.byName(underlay.name),
        ColdCallTrustLevel.values.byName(level.name),
      ));

  // --- WAN ---

  void Function(String? oldAddress, String? newAddress)?
      _onConnectivityStatusChanged;

  @override
  void Function(String? oldAddress, String? newAddress)?
      get onConnectivityStatusChanged => _onConnectivityStatusChanged;

  @override
  set onConnectivityStatusChanged(
      void Function(String? oldAddress, String? newAddress)? cb) {
    _onConnectivityStatusChanged = cb;
    layer.onConnectivityStatusChanged = cb;
  }

  @override
  String? getPublicAddress() => layer.getPublicAddress();

  @override
  String? networkIdentity() => layer.networkIdentity();

  void Function(String? oldId, String? newId)? _onNetworkChanged;

  @override
  void Function(String? oldId, String? newId)? get onNetworkChanged =>
      _onNetworkChanged;

  @override
  set onNetworkChanged(void Function(String? oldId, String? newId)? cb) {
    _onNetworkChanged = cb;
    layer.onNetworkChanged = cb;
  }

  @override
  Future<String> generatePeerLink() => layer.generatePeerLink();

  @override
  Future<glp.PubKey> consumePeerLink(String uri) =>
      layer.consumePeerLink(uri).then(_key);

  @override
  void putPeerAddress(glp.PubKey pk, String address) =>
      layer.putPeerAddress(pk.bytes, address);

  @override
  String? observedPeerAddress(glp.PubKey pk) =>
      layer.observedPeerAddress(pk.bytes);

  @override
  void punchUdp(String address) => layer.punchUdp(address);

  // --- Places ---

  @override
  Future<bool> declarePlace(String place, double radiusMetres) =>
      layer.declarePlace(place, radiusMetres);

  @override
  Future<void> removePlace(String place) => layer.removePlace(place);

  void Function(String place, glp.PlaceEvent event)? _onPlaceEvent;

  @override
  void Function(String place, glp.PlaceEvent event)? get onPlaceEvent =>
      _onPlaceEvent;

  @override
  set onPlaceEvent(void Function(String place, glp.PlaceEvent event)? cb) {
    _onPlaceEvent = cb;
    layer.onPlaceEvent = cb == null
        ? null
        : (place, event) => cb(place, glp.PlaceEvent.values.byName(event.name));
  }
}
