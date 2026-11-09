/// The layer's events, as the glue gives them to the agent: a [GlpNetwork]
/// that passes every call through to the one it wraps, GNA's glue on a phone,
/// and reports each callback of the glue as the agent receives it, and each
/// trust level as the agent sets it (GLP/phones/docs/debugging.md, "The phone
/// side", item 3, `layer`).
///
/// It is the agent's network only in a debuggable build (main_superapp.dart):
/// the agent and the layer are the same with it as without it, and it reads
/// what the callbacks already give, adding nothing (item 5).  A key is
/// reported as its 64 hex characters.
library;

import 'dart:typed_data';

import 'package:glp_runtime/multiagent/glp_network.dart';

import 'reporter.dart';

/// [network], its callbacks reported to [reporter].
class ReportingGlpNetwork implements GlpNetwork {
  final GlpNetwork network;
  final DebugReporter reporter;

  ReportingGlpNetwork(this.network, this.reporter);

  void _report(String kind, Map<String, Object?> fields) =>
      reporter.event(Source.layer, kind, fields);

  static Map<String, Object?>? _application(Application? a) => a == null
      ? null
      : {'developer': a.developer, 'name': a.name, 'version': a.version};

  // --- Identity, known peers, reachability, cryptography: passed through ---

  @override
  PubKey getIdentity() => network.getIdentity();

  @override
  void putKnownPeer(PubKey pk) => network.putKnownPeer(pk);

  @override
  void removeKnownPeer(PubKey pk) => network.removeKnownPeer(pk);

  @override
  bool isPeerReachable(PubKey pk) => network.isPeerReachable(pk);

  @override
  List<Underlay> peerUnderlays(PubKey pk) => network.peerUnderlays(pk);

  @override
  Future<void> send(PubKey pk, Uint8List payload) => network.send(pk, payload);

  @override
  void acknowledge(String messageId) => network.acknowledge(messageId);

  @override
  Uint8List sign(Uint8List message) => network.sign(message);

  @override
  bool verify(PubKey signer, Uint8List message, Uint8List signature) =>
      network.verify(signer, message, signature);

  @override
  List<DiscoveredPeer> getPeers() => network.getPeers();

  // --- The callbacks, each reported as the agent receives it ---

  void Function(PubKey pk, Underlay underlay, Application? application)?
      _onPeerConnected;

  @override
  void Function(PubKey pk, Underlay underlay, Application? application)?
      get onPeerConnected => _onPeerConnected;

  @override
  set onPeerConnected(
      void Function(PubKey pk, Underlay underlay, Application? application)?
          cb) {
    _onPeerConnected = cb;
    network.onPeerConnected = cb == null
        ? null
        : (pk, underlay, application) {
            _report('onPeerConnected', {
              'peer': pk.hex,
              'underlay': underlay.name,
              'application': _application(application),
            });
            cb(pk, underlay, application);
          };
  }

  void Function(PubKey pk, Underlay underlay)? _onPeerDisconnected;

  @override
  void Function(PubKey pk, Underlay underlay)? get onPeerDisconnected =>
      _onPeerDisconnected;

  @override
  set onPeerDisconnected(void Function(PubKey pk, Underlay underlay)? cb) {
    _onPeerDisconnected = cb;
    network.onPeerDisconnected = cb == null
        ? null
        : (pk, underlay) {
            _report('onPeerDisconnected',
                {'peer': pk.hex, 'underlay': underlay.name});
            cb(pk, underlay);
          };
  }

  void Function(PubKey sender, Uint8List payload, String messageId,
      Underlay underlay)? _onMessageReceived;

  @override
  void Function(PubKey sender, Uint8List payload, String messageId,
      Underlay underlay)? get onMessageReceived => _onMessageReceived;

  @override
  set onMessageReceived(
      void Function(PubKey sender, Uint8List payload, String messageId,
              Underlay underlay)?
          cb) {
    _onMessageReceived = cb;
    network.onMessageReceived = cb == null
        ? null
        : (sender, payload, messageId, underlay) {
            _report('onMessageReceived', {
              'peer': sender.hex,
              'size': payload.length,
              'messageId': messageId,
              'underlay': underlay.name,
            });
            cb(sender, payload, messageId, underlay);
          };
  }

  void Function(PubKey pk, ProximityUnderlay underlay)? _onPeerDiscovered;

  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? get onPeerDiscovered =>
      _onPeerDiscovered;

  @override
  set onPeerDiscovered(void Function(PubKey pk, ProximityUnderlay underlay)? cb) {
    _onPeerDiscovered = cb;
    network.onPeerDiscovered = cb == null
        ? null
        : (pk, underlay) {
            _report('onPeerDiscovered',
                {'peer': pk.hex, 'underlay': underlay.name});
            cb(pk, underlay);
          };
  }

  void Function(PubKey pk, ProximityUnderlay underlay)? _onPeerLost;

  @override
  void Function(PubKey pk, ProximityUnderlay underlay)? get onPeerLost =>
      _onPeerLost;

  @override
  set onPeerLost(void Function(PubKey pk, ProximityUnderlay underlay)? cb) {
    _onPeerLost = cb;
    network.onPeerLost = cb == null
        ? null
        : (pk, underlay) {
            _report('onPeerLost', {'peer': pk.hex, 'underlay': underlay.name});
            cb(pk, underlay);
          };
  }

  void Function(String? oldAddress, String? newAddress)?
      _onConnectivityStatusChanged;

  @override
  void Function(String? oldAddress, String? newAddress)?
      get onConnectivityStatusChanged => _onConnectivityStatusChanged;

  @override
  set onConnectivityStatusChanged(
      void Function(String? oldAddress, String? newAddress)? cb) {
    _onConnectivityStatusChanged = cb;
    network.onConnectivityStatusChanged = cb == null
        ? null
        : (oldAddress, newAddress) {
            _report('onConnectivityStatusChanged',
                {'old': oldAddress, 'new': newAddress});
            cb(oldAddress, newAddress);
          };
  }

  void Function(String? oldId, String? newId)? _onNetworkChanged;

  @override
  void Function(String? oldId, String? newId)? get onNetworkChanged =>
      _onNetworkChanged;

  @override
  set onNetworkChanged(void Function(String? oldId, String? newId)? cb) {
    _onNetworkChanged = cb;
    network.onNetworkChanged = cb == null
        ? null
        : (oldId, newId) {
            _report('onNetworkChanged', {'old': oldId, 'new': newId});
            cb(oldId, newId);
          };
  }

  void Function(String place, PlaceEvent event)? _onPlaceEvent;

  @override
  void Function(String place, PlaceEvent event)? get onPlaceEvent =>
      _onPlaceEvent;

  @override
  set onPlaceEvent(void Function(String place, PlaceEvent event)? cb) {
    _onPlaceEvent = cb;
    network.onPlaceEvent = cb == null
        ? null
        : (place, event) {
            _report('onPlaceEvent', {'place': place, 'event': event.name});
            cb(place, event);
          };
  }

  // --- The trust levels, as the agent sets them ---

  @override
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level) {
    _report('setTrustLevel', {'underlay': underlay.name, 'level': level.name});
    network.setTrustLevel(underlay, level);
  }

  // --- WAN and places: passed through ---

  @override
  String? getPublicAddress() => network.getPublicAddress();

  @override
  String? networkIdentity() => network.networkIdentity();

  @override
  void putPeerAddress(PubKey pk, String address) =>
      network.putPeerAddress(pk, address);

  @override
  void putRendezvousServer(PubKey pk, String address) =>
      network.putRendezvousServer(pk, address);

  @override
  void removeRendezvousServer(PubKey pk) => network.removeRendezvousServer(pk);

  @override
  String? observedPeerAddress(PubKey pk) => network.observedPeerAddress(pk);

  @override
  void punchUdp(String address) => network.punchUdp(address);

  @override
  Future<PlaceCentre?> declarePlace(
          String place, PlaceCentre? centre, double radiusMetres) =>
      network.declarePlace(place, centre, radiusMetres);

  @override
  Future<void> removePlace(String place) => network.removePlace(place);
}
