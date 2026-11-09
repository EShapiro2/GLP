/// The runtime's network interface over the headless layer: GNA's backend
/// glue for a server-side agent.  [HeadlessGlpNetwork] implements
/// glp_runtime's `GlpNetwork` over [HeadlessGrassrootsNetwork].
///
/// A library of the glue package, beside the layer's core and depending on
/// both the core and the runtime, so that the core and its users carry no GLP
/// runtime (GNA, 2026-10-11); and a library of its own, so that the layer's
/// library does not carry the runtime's names, several of which (`Underlay`,
/// `ProximityUnderlay`, `PlaceEvent`) the two share.
///
/// IGLP, Implementation Notes, "Networking Seam", One interface: "The
/// multiagent runtime reaches the network through one interface, the one the
/// GLP Networking API specifies"; that interface is the runtime's
/// `GlpNetwork`.  This class implements it over [HeadlessGrassrootsNetwork],
/// the headless profile of GNA, Section "Networking API": "a server-side
/// agent ... realizes the WAN underlay alone, headless, with proximity
/// discovery absent".
///
/// Every function the layer has passes straight through, and nothing is
/// interpreted: a key travels as its 32 bytes, an underlay, a trust level and
/// a place event by their names, which the two sides share; the attested
/// application of `onPeerConnected` travels as its developer, its name and
/// its version number, which the two sides share, or null for an unattested
/// identity (GNA §2.3); the runtime records it for `peer_app/2` (GLP-Spec
/// appendix-guards, "Networking seam", 5fab8ac).
///
/// The profile has no proximity underlay and no location service (GNA,
/// Section "Instantiation": "Location service ... none on a headless server"),
/// so `getPeers` is empty, there being no proximity underlay to find a nearby
/// agent on; `setTrustLevel` does nothing, since "a call on an underlay the
/// layer does not run finds no sub-object" (GNA §2.2), the layer's `pan` and
/// `lan` sub-objects being null; and the place functions are refused as
/// unsupported, throwing [UnsupportedError], since a headless layer "refuses
/// declarePlace and removePlace" (GNA §2.7).  Every
/// other function of the API the layer has (GNA, 2026-10-11, Task C), and
/// passes straight through, the WAN functions to the layer's `wan`
/// sub-object.  A callback the layer has no event for --- onPeerDiscovered,
/// onPeerLost, onPlaceEvent --- is held and never fires.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart' as glp;

import 'package:grassroots_networking_core/src/headless/headless_grassroots_network.dart';
import 'package:grassroots_networking_core/src/session/application.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';

/// [glp.GlpNetwork] over a [HeadlessGrassrootsNetwork].
class HeadlessGlpNetwork implements glp.GlpNetwork {
  /// The layer this glue passes through to.
  final HeadlessGrassrootsNetwork layer;

  HeadlessGlpNetwork(this.layer);

  static glp.PubKey _key(Uint8List bytes) => glp.PubKey.fromList(bytes);

  static glp.Underlay _underlay(Underlay u) => glp.Underlay.values.byName(u.name);

  /// The attested application as the runtime holds it: its developer, its
  /// name and its version number; null, an unattested identity, as null.
  static glp.Application? _application(Application? a) => a == null
      ? null
      : glp.Application(
          developer: a.developer, name: a.name, version: a.version);

  static Never _absent(String function, String why) => throw UnsupportedError(
      '$function: the headless layer (HeadlessGrassrootsNetwork) does not '
      'provide it: $why');

  // --- Identity ---

  @override
  glp.PubKey getIdentity() => _key(layer.getIdentity());

  // --- Known peers ---

  @override
  void putKnownPeer(glp.PubKey pk) => layer.putKnownPeer(pk.bytes);

  @override
  void removeKnownPeer(glp.PubKey pk) => layer.removeKnownPeer(pk.bytes);

  // --- Connection and reachability ---

  void Function(
          glp.PubKey pk, glp.Underlay underlay, glp.Application? application)?
      _onPeerConnected;

  @override
  void Function(
          glp.PubKey pk, glp.Underlay underlay, glp.Application? application)?
      get onPeerConnected => _onPeerConnected;

  @override
  set onPeerConnected(
      void Function(
          glp.PubKey pk, glp.Underlay underlay, glp.Application? application)?
          cb) {
    _onPeerConnected = cb;
    layer.onPeerConnected = cb == null
        ? null
        : (pk, underlay, application) =>
            cb(_key(pk), _underlay(underlay), _application(application));
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

  /// `send(pubKey, payload) → messageId`: the layer's `send`, awaited, so
  /// that a save of the queue that fails reaches the caller: the layer saves
  /// the message before its `send` returns, so that the queue survives the
  /// termination of the application (GNA §2.11).  Its value is the id the
  /// layer gave the message, which the recipient receives as
  /// `senderMessageId`.  The layer refuses a key that is not 32 bytes, with
  /// no id, and that refusal is thrown as an [ArgumentError].
  @override
  Future<String> send(glp.PubKey pk, Uint8List payload) async {
    final messageId = await layer.send(pk.bytes, payload);
    if (messageId == null) {
      throw ArgumentError.value(pk, 'pk', 'not a 32-byte public key');
    }
    return messageId;
  }

  void Function(glp.PubKey sender, Uint8List payload, String messageId,
      String senderMessageId, glp.Underlay underlay)? _onMessageReceived;

  @override
  void Function(glp.PubKey sender, Uint8List payload, String messageId,
          String senderMessageId, glp.Underlay underlay)?
      get onMessageReceived => _onMessageReceived;

  @override
  set onMessageReceived(
      void Function(glp.PubKey sender, Uint8List payload, String messageId,
              String senderMessageId, glp.Underlay underlay)?
          cb) {
    _onMessageReceived = cb;
    layer.onMessageReceived = cb == null
        ? null
        : (sender, payload, messageId, senderMessageId, underlay) => cb(
            _key(sender),
            payload,
            messageId,
            senderMessageId,
            _underlay(underlay));
  }

  /// `acknowledge(messageId)` (GNA, Section 2.9): the layer's, which keeps a
  /// delivered message until it is acknowledged, and delivers it again at
  /// restart otherwise (Section 2.11).  The runtime's interface returns
  /// nothing, so the layer's save is not awaited; a save that fails is the
  /// layer's to log, and the message is delivered again at restart.
  @override
  void acknowledge(String messageId) =>
      unawaited(layer.acknowledge(messageId));

  // --- Cryptography ---

  @override
  Uint8List sign(Uint8List message) => layer.sign(message);

  @override
  bool verify(glp.PubKey signer, Uint8List message, Uint8List signature) =>
      layer.verify(signer.bytes, message, signature);

  // --- Proximity underlays (absent from this profile) ---

  /// Empty: the profile has no proximity underlay to scan or browse.
  @override
  List<glp.DiscoveredPeer> getPeers() => const [];

  @override
  void Function(glp.PubKey pk, glp.ProximityUnderlay underlay)?
      onPeerDiscovered;

  @override
  void Function(glp.PubKey pk, glp.ProximityUnderlay underlay)? onPeerLost;

  /// `declare(trust(U, L))`: the profile runs no proximity underlay, so the
  /// declaration finds no sub-object and does nothing (GNA §2.2).
  @override
  void setTrustLevel(glp.ProximityUnderlay underlay, glp.TrustLevel level) {}

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
    layer.wan.onConnectivityStatusChanged = cb;
  }

  @override
  String? getPublicAddress() => layer.wan.getPublicAddress();

  @override
  String? networkIdentity() => layer.wan.networkIdentity();

  void Function(String? oldId, String? newId)? _onNetworkChanged;

  @override
  void Function(String? oldId, String? newId)? get onNetworkChanged =>
      _onNetworkChanged;

  @override
  set onNetworkChanged(void Function(String? oldId, String? newId)? cb) {
    _onNetworkChanged = cb;
    layer.wan.onNetworkChanged = cb;
  }

  @override
  void putPeerAddress(glp.PubKey pk, String address) =>
      layer.wan.putPeerAddress(pk.bytes, address);

  @override
  String? observedPeerAddress(glp.PubKey pk) =>
      layer.wan.observedPeerAddress(pk.bytes);

  /// `putRendezvousServer(pk, address)`: the WAN sub-object's (GNA 776016c, Section 4.2), behind `declare(rendezvous(P, A))`.
  @override
  void putRendezvousServer(glp.PubKey pk, String address) =>
      layer.wan.putRendezvousServer(pk.bytes, address);

  /// `removeRendezvousServer(pk)`: the WAN sub-object's (GNA 776016c, Section 4.2), behind `remove(rendezvous(P))`.
  @override
  void removeRendezvousServer(glp.PubKey pk) =>
      layer.wan.removeRendezvousServer(pk.bytes);

  @override
  void punchUdp(String address) => layer.wan.punchUdp(address);

  // --- Places (no location service on a headless server) ---

  @override
  Future<glp.PlaceCentre?> declarePlace(
          String place, glp.PlaceCentre? centre, double radiusMetres) =>
      _absent('declarePlace', 'a headless server has no location service');

  @override
  Future<void> removePlace(String place) =>
      _absent('removePlace', 'a headless server has no location service');

  @override
  void Function(String place, glp.PlaceEvent event)? onPlaceEvent;
}
