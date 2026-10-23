/// The layer's underlay sub-objects (GNA §2.2, Instantiation): "The layer
/// exposes one sub-object for each underlay it runs, `pan`, `lan` and `wan`,
/// and null for an underlay it does not run."  The functions that span all
/// underlays are called on the layer itself; "the functions that belong to one
/// underlay are called on its sub-object, and take no underlay parameter:
/// discovery and the trust level on `pan` and `lan` (Section 3.1), and the
/// public address, peer addresses, hole punching, the rendezvous servers and
/// the local network identity on `wan` (Section 4)."  So a call that makes no sense on an
/// underlay cannot be written, and a call on an underlay the layer does not
/// run finds no sub-object.
///
/// Both profiles implement them: the Flutter profile, `GrassrootsNetwork`,
/// each sub-object of the underlays it is instantiated with; the headless
/// profile, `HeadlessGrassrootsNetwork`, `wan` alone (GNA §2: "a server-side
/// agent ... realizes the WAN underlay alone, headless, with proximity
/// discovery absent").
library;

import 'dart:typed_data';

import '../store/settings_state.dart';
import 'underlay.dart';

/// The functions of one proximity underlay, PAN or LAN, each acting on that
/// underlay alone (GNA §3.1, Discovery): the `pan` and `lan` sub-objects.
abstract interface class ProximityApi {
  /// The proximity underlay this sub-object belongs to.
  ProximityUnderlay get underlay;

  /// `getPeers() → List<Peer>`: the nearby Grassroots agents found on this
  /// underlay, by public key: "on `pan` a BLE scan for the Grassroots prefix,
  /// on `lan` an mDNS browse for the service type" (GNA §3.1).
  List<Uint8List> getPeers();

  /// `onPeerDiscovered(pk)`: callback for each nearby Grassroots agent found
  /// on this underlay (GNA §3.1).
  abstract void Function(Uint8List pk)? onPeerDiscovered;

  /// `setTrustLevel(level)`: set the cold-call trust level of this underlay
  /// to Open or Closed (GNA §2.7, §3.1); it is Closed until set.
  Future<void> setTrustLevel(ColdCallTrustLevel level);
}

/// The functions of the WAN underlay (GNA §4.2, §4.3): the `wan` sub-object.
abstract interface class WanApi {
  /// `onConnectivityStatusChanged(oldAddress?, newAddress?)`: "Callback when
  /// the networking layer detects a change in the agent's public IP address
  /// ... Fired on startup and on every address change" (GNA §4.2).
  abstract void Function(String? oldAddress, String? newAddress)?
      onConnectivityStatusChanged;

  /// `getPublicAddress() → Address?`: "The agent's current public `ip:port`,
  /// or `null` if not yet known" (GNA §4.2).
  String? getPublicAddress();

  /// `networkIdentity() → NetworkId?`: "An opaque fingerprint of the local
  /// network the agent is currently attached to, or `null` when it is
  /// attached to none or no constituent is readable" (GNA §4.3).
  String? networkIdentity();

  /// `onNetworkChanged(oldId?, newId?)`: "Callback when the fingerprint
  /// changes" (GNA §4.3).
  abstract void Function(String? oldId, String? newId)? onNetworkChanged;

  /// `putPeerAddress(pk, address)`: "Inform the networking layer of a peer's
  /// current public `ip:port`, learned at the GLP level; the layer records it
  /// with the known peer" (GNA §4.2).  Throws [ArgumentError] on an
  /// unparseable address.
  void putPeerAddress(Uint8List pk, String address);

  /// `observedPeerAddress(pk) → Address?`: "The public `ip:port` at which the
  /// layer observes the connected peer, or `null` if none is observed"
  /// (GNA §4.2); behind `peer_address/2`.
  String? observedPeerAddress(Uint8List pk);

  /// `punchUdp(address)`: "Send toward the address a short burst of UDP
  /// datagrams to open the NAT mapping" (GNA §4.2); behind `punch_udp/1`.
  /// Throws [ArgumentError] on an unparseable address.
  void punchUdp(String address);

  /// `putRendezvousServer(pk, address)`: "Add the rendezvous server with
  /// public key pk at address to the servers the layer connects to; a
  /// further declaration of the same pk replaces its address" (GNA §4.2);
  /// behind `rendezvous_declare/2`.  Throws [ArgumentError] on an unparseable
  /// address or a key that is not 32 bytes.
  void putRendezvousServer(Uint8List pk, String address);

  /// `removeRendezvousServer(pk)`: "Remove the server from those the layer
  /// connects to, and end the layer's session with it; a key not declared
  /// does nothing" (GNA §4.2); behind `rendezvous_remove/1`.
  void removeRendezvousServer(Uint8List pk);
}
