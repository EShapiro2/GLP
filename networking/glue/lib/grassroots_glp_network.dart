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
/// peer.
///
/// The layer exposes one sub-object for each underlay it runs, `pan`, `lan`
/// and `wan`, null for one it does not run (GNA §2.2), and a function of one
/// underlay is called on its sub-object, with no underlay parameter.  The
/// runtime's interface keeps the underlay parameter of `setTrustLevel`, which
/// `trust_declare/2` takes, so this glue maps `trust_declare(pan|lan, Level)`
/// to that sub-object's `setTrustLevel(Level)`; `getPeers` gathers both
/// proximity sub-objects' peers, and `onPeerDiscovered` the discoveries of
/// both.  A function of an underlay the layer does not run finds no
/// sub-object and throws [UnsupportedError], as the runtime's interface has
/// a realisation do for a function it does not provide; a callback of such an
/// underlay is held and never fires.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart' as glp;
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/models/underlay_api.dart';
import 'package:grassroots_networking_core/src/store/settings_state.dart';

import 'package:grassroots_networking/src/grassroots_network.dart';

/// [glp.GlpNetwork] over a [GrassrootsNetwork].
class GrassrootsGlpNetwork implements glp.GlpNetwork {
  /// The layer this glue passes through to.
  final GrassrootsNetwork layer;

  GrassrootsGlpNetwork(this.layer);

  static glp.PubKey _key(Uint8List bytes) => glp.PubKey.fromList(bytes);

  static glp.Underlay _underlay(Underlay u) => glp.Underlay.values.byName(u.name);

  static Never _absent(String function, String underlay) =>
      throw UnsupportedError('$function: the layer (GrassrootsNetwork) runs '
          'no $underlay underlay, so it has no $underlay sub-object (GNA '
          '§2.2)');

  /// The layer's sub-object of proximity underlay [underlay], or null.
  ProximityApi? _proximity(glp.ProximityUnderlay underlay) => switch (underlay) {
        glp.ProximityUnderlay.pan => layer.pan,
        glp.ProximityUnderlay.lan => layer.lan,
      };

  /// The layer's `wan` sub-object, or [UnsupportedError] for [function].
  WanApi _wan(String function) => layer.wan ?? _absent(function, 'WAN');

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

  /// `send(pubKey, payload)`: the layer's `send`, awaited, so that a save of
  /// the queue that fails reaches the caller: the layer saves the message
  /// before its `send` returns, so that the queue survives the termination
  /// of the application (GNA §2.11).
  @override
  Future<void> send(glp.PubKey pk, Uint8List payload) async {
    await layer.send(pk.bytes, payload);
  }

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
        for (final underlay in glp.ProximityUnderlay.values)
          for (final pk in _proximity(underlay)?.getPeers() ?? const [])
            glp.DiscoveredPeer(_key(pk), underlay),
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
    for (final underlay in glp.ProximityUnderlay.values) {
      _proximity(underlay)?.onPeerDiscovered =
          cb == null ? null : (pk) => cb(_key(pk), underlay);
    }
  }

  /// `trust_declare(pan|lan, Level)`: that sub-object's `setTrustLevel`.
  @override
  void setTrustLevel(glp.ProximityUnderlay underlay, glp.TrustLevel level) {
    final proximity = _proximity(underlay) ??
        _absent('setTrustLevel', underlay.name.toUpperCase());
    unawaited(proximity
        .setTrustLevel(ColdCallTrustLevel.values.byName(level.name)));
  }

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
    layer.wan?.onConnectivityStatusChanged = cb;
  }

  @override
  String? getPublicAddress() => _wan('getPublicAddress').getPublicAddress();

  @override
  String? networkIdentity() => _wan('networkIdentity').networkIdentity();

  void Function(String? oldId, String? newId)? _onNetworkChanged;

  @override
  void Function(String? oldId, String? newId)? get onNetworkChanged =>
      _onNetworkChanged;

  @override
  set onNetworkChanged(void Function(String? oldId, String? newId)? cb) {
    _onNetworkChanged = cb;
    layer.wan?.onNetworkChanged = cb;
  }

  @override
  void putPeerAddress(glp.PubKey pk, String address) =>
      _wan('putPeerAddress').putPeerAddress(pk.bytes, address);

  @override
  String? observedPeerAddress(glp.PubKey pk) =>
      _wan('observedPeerAddress').observedPeerAddress(pk.bytes);

  @override
  void punchUdp(String address) => _wan('punchUdp').punchUdp(address);

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
