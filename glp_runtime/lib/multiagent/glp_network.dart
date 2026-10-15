/// GlpNetwork — the one Dart interface madGLP uses to talk to networking.
///
/// IGLP, Implementation Notes, "Networking Seam", One interface: "The
/// multiagent runtime reaches the network through one interface, the one the
/// GLP Networking API specifies ... A simulation realisation of the same
/// interface backs the tests and plays, so transport integration is a backend
/// swap."  The interface is GNA's API (Section "Networking API" and the
/// Proximity, WAN and Seam sections; Section "Complete API Summary" lists it
/// whole), in the summary's order: identity, known peers, connection and
/// reachability, communication, cryptography, the proximity underlays, the
/// WAN, places.  Two realisations exist behind it: `SimulationNetworkClient`
/// (this package) and the glue over the networking layer (GNA's, in
/// `networking/`).
///
/// The layer is instantiated once per agent with its key pair (GNA,
/// "Identity": "The key pair ... is supplied to the layer's constructor when
/// it is instantiated; it is immutable for the layer's lifetime"), so the
/// interface has no call that installs an identity.  Until 2026-10-11 it had
/// one, `putIdentity`, which the API does not.
///
/// Callbacks are settable fields, matching `MadContext.onMessageReady` house
/// style, each with the API's parameters in the API's order.  `sign`/`verify`
/// are synchronous: their callers are the GLP body kernels, and Ed25519
/// signing is pure CPU work.
///
/// See: IGLP app:in-networking, One interface and The contract.
library;

import 'dart:typed_data';

/// The underlay a peer is reachable over: GNA, "Networking API": "An Underlay
/// is one of PAN, LAN or WAN; a peer may be reachable over more than one
/// underlay at a time."  Until 2026-10-11 the interface had a transport, ble
/// or ip, in its place.
enum Underlay { pan, lan, wan }

/// Trust level governing first contact (GNA, "Cold Calls", Trust levels).
///
/// Under [closed], the agent completes neither the ANNOUNCE exchange nor the
/// session handshake with an unknown key on that underlay; [open] completes
/// both with any unknown contact.
enum TrustLevel { open, closed }

/// A proximity underlay, PAN or LAN: "A level is held per ProximityUnderlay,
/// and is set with setTrustLevel(underlay, level); both levels default to
/// Closed" (GNA, Trust levels).  Each underlay has its own cold-call
/// [TrustLevel], which `trust_declare/2` declares of `pan` or `lan`
/// (GLP-Spec appendix-guards, "Proximity trust", e0b32d7).  It is also the
/// underlay of discovery ([GlpNetwork.onPeerDiscovered]).
enum ProximityUnderlay { pan, lan }

/// A 32-byte Ed25519 public key, with value equality and a hex string form.
///
/// The network-level identity of an agent.  Over a real network it is the
/// agent's name; symbolic agent names are the simulation realisation's choice,
/// resolved to a [PubKey] at the seam (IGLP, Remark "Agent Names").
class PubKey {
  /// The 32 raw bytes of the Ed25519 public key.
  final Uint8List bytes;

  PubKey(this.bytes) {
    if (bytes.length != 32) {
      throw ArgumentError(
          'PubKey must be 32 bytes (Ed25519 public key), got ${bytes.length}');
    }
  }

  /// Build a [PubKey] from a list of byte values (e.g. `PublicKey.bytes`).
  PubKey.fromList(List<int> bytes) : this(Uint8List.fromList(bytes));

  /// Parse a 64-character hex string into a [PubKey].
  factory PubKey.fromHex(String hex) {
    if (hex.length != 64) {
      throw ArgumentError('PubKey hex must be 64 chars, got ${hex.length}');
    }
    final out = Uint8List(32);
    for (var i = 0; i < 32; i++) {
      out[i] = int.parse(hex.substring(i * 2, i * 2 + 2), radix: 16);
    }
    return PubKey(out);
  }

  /// Lowercase hex string form (64 chars).
  String get hex {
    final sb = StringBuffer();
    for (final b in bytes) {
      sb.write(b.toRadixString(16).padLeft(2, '0'));
    }
    return sb.toString();
  }

  @override
  bool operator ==(Object other) {
    if (other is! PubKey) return false;
    if (bytes.length != other.bytes.length) return false;
    for (var i = 0; i < bytes.length; i++) {
      if (bytes[i] != other.bytes[i]) return false;
    }
    return true;
  }

  @override
  int get hashCode => Object.hashAll(bytes);

  @override
  String toString() => 'PubKey(${hex.substring(0, 8)}…)';
}

/// An event on a declared place's stream (GNA, "The Seam", place functions).
///
/// [entered] and [exited] are the declaring device's own crossings of the
/// place. [unobservable] says the layer has stopped reporting crossings — the
/// application losing the foreground, a permission withdrawn — and [observable]
/// that reporting has resumed; between the two no crossing is seen and none is
/// delivered late. The two exist because the layer takes no background-location
/// permission: without them a place the device has left cannot be told from a
/// place nothing is watching.
///
/// Observability is a property of the platform's reporting, not of one
/// registration, so the layer reports it once and delivers it on the stream of
/// every standing declaration.
enum PlaceEvent { entered, exited, unobservable, observable }

/// A nearby agent found by proximity discovery: its key, known once ANNOUNCE
/// has run, and the proximity underlay it was found on — what
/// `onPeerDiscovered(pk, underlay)` reports of it (GNA, "Concepts and
/// Principles", Discovery).  The element of [GlpNetwork.getPeers].
class DiscoveredPeer {
  /// The peer's public key.
  final PubKey pk;

  /// The proximity underlay the peer was discovered over.
  final ProximityUnderlay underlay;

  DiscoveredPeer(this.pk, this.underlay);

  @override
  String toString() => 'DiscoveredPeer($pk, $underlay)';
}

/// The networking interface madGLP talks to.
///
/// One realisation wraps the simulation isolate transport; another wraps the
/// networking layer.  The runtime drives outgoing traffic via [send] and
/// receives via [onMessageReceived].  A realisation that does not provide a
/// function throws [UnsupportedError] from it.
abstract class GlpNetwork {
  // --- Identity (GNA, "Identity") ---

  /// This layer's own public key: `getIdentity() → PubKey`.  The key pair was
  /// supplied when the layer was instantiated; the layer holds the private key
  /// for [sign].
  PubKey getIdentity();

  // --- Known peers (GNA, "Known Peers") ---

  /// Add [pk] to the known-peer set: `putKnownPeer(pk)`.  The set is what
  /// Closed trust recognises.  Backs `peer_declare/1`.
  void putKnownPeer(PubKey pk);

  /// Remove [pk] and any recorded address from the known-peer set:
  /// `removeKnownPeer(pk)`.
  void removeKnownPeer(PubKey pk);

  // --- Connection and reachability (GNA, "Connection and Reachability") ---

  /// `onPeerConnected(pk, underlay, identity?)`: fires when a peer becomes
  /// reachable, with its key, the [Underlay] over which it connected, and its
  /// attested application identity, null where the peer's platform provides
  /// no attestation.  The identity is carried as the layer reports it: "What
  /// the layer attests of a peer and its session is the API's to specify; the
  /// runtime relies on none of it here" (IGLP, The contract).
  void Function(PubKey pk, Underlay underlay, Object? identity)?
      onPeerConnected;

  /// `onPeerDisconnected(pk, underlay)`: fires when a peer is no longer
  /// reachable over [Underlay] `underlay`.
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;

  /// `isPeerReachable(pk) → bool`: whether [pk] is currently reachable via any
  /// underlay.
  bool isPeerReachable(PubKey pk);

  /// `peerUnderlays(pk) → List<Underlay>`: the underlays by which [pk] is
  /// currently reachable (empty if unreachable).
  List<Underlay> peerUnderlays(PubKey pk);

  // --- Point-to-point communication (GNA, "Point-to-Point Communication") ---

  /// `send(pubKey, payload)`: send opaque [payload] bytes to peer [pk].  Order
  /// is arbitrary; delivery is fair (GNA, "Networking Assumptions").
  void send(PubKey pk, Uint8List payload);

  /// `onMessageReceived(sender, payload, messageId, underlay)`: fires on
  /// delivery of an inbound message, with the authenticated sender's key, the
  /// opaque payload, the layer-assigned messageId, and the [Underlay] over
  /// which it was delivered.
  void Function(
          PubKey sender, Uint8List payload, String messageId, Underlay underlay)?
      onMessageReceived;

  // --- Cryptography (GNA, "The Seam", Signatures) ---
  // The layer holds the private key.  Synchronous.

  /// `sign(bytes)`: Ed25519 signature over [message] under this layer's key.
  Uint8List sign(Uint8List message);

  /// `verify(pk, bytes, signature)`: whether [signature] over [message] is
  /// [signer]'s Ed25519 signature.
  bool verify(PubKey signer, Uint8List message, Uint8List signature);

  // --- Proximity underlays, PAN and LAN (GNA, "Concepts and Principles",
  // Discovery) ---

  /// `getPeers() → List<Peer>`: the nearby Grassroots agents found by scanning
  /// and browsing the proximity underlays.
  List<DiscoveredPeer> getPeers();

  /// `onPeerDiscovered(pk, underlay)`: fires for each nearby Grassroots agent
  /// found, with the proximity underlay of discovery.
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerDiscovered;

  /// `setTrustLevel(underlay, level)`: set the cold-call trust level of
  /// proximity underlay [underlay] to [level]; "both levels default to
  /// Closed" (GNA, Trust levels).  Backs `trust_declare/2`.
  void setTrustLevel(ProximityUnderlay underlay, TrustLevel level);

  // --- WAN (GNA, "WAN Underlay") ---

  /// `onConnectivityStatusChanged(oldAddress?, newAddress?)`: fires when the
  /// layer detects a change in the agent's public `ip:port`, "either of which
  /// may be null when no public address is available"; fired on startup and on
  /// every address change.
  void Function(String? oldAddress, String? newAddress)?
      onConnectivityStatusChanged;

  /// `getPublicAddress() → Address?`: the agent's current public `ip:port`, or
  /// null if not yet known.
  String? getPublicAddress();

  /// `networkIdentity() → NetworkId?`: an opaque fingerprint of the local
  /// network the agent is attached to, or null when it is attached to none or
  /// no constituent is readable (GNA, "Local Network Identity").
  String? networkIdentity();

  /// `onNetworkChanged(oldId?, newId?)`: fires when the fingerprint changes;
  /// either may be null.
  void Function(String? oldId, String? newId)? onNetworkChanged;

  /// `putPeerAddress(pk, address)`: inform the layer of peer [pk]'s current
  /// public `ip:port`, learned at the GLP level; the layer records it with the
  /// known peer.  Backs `address_declare/2`.
  void putPeerAddress(PubKey pk, String address);

  /// `observedPeerAddress(pk) → Address?`: the public `ip:port` at which the
  /// layer observes the connected peer [pk], or null if none is observed.
  /// Backs `peer_address/2`, which reports the two as `address(S)` and `none`.
  String? observedPeerAddress(PubKey pk);

  /// `punchUdp(address)`: send a short burst of UDP datagrams toward
  /// [address], opening the NAT mapping; returns nothing.  Backs
  /// `punch_udp/1`.
  void punchUdp(String address);

  // --- Places (GNA, "The Seam", place functions) ---

  /// `declarePlace(place, radius)`: declare [place] — the region of
  /// [radiusMetres] about this device's location at the time of the call —
  /// and report its crossings through [onPlaceEvent].  Backs `place_declare/3`.
  ///
  /// Returns false when the platform refuses, leaving the place undeclared. A
  /// further declaration under the same name supersedes the first. GLP supplies
  /// a name and a radius and receives no coordinates.
  Future<bool> declarePlace(String place, double radiusMetres);

  /// `removePlace(place)`: end the declaration of [place].  Backs
  /// `place_remove/1`.  Removing a place that is not declared does nothing.
  Future<void> removePlace(String place);

  /// `onPlaceEvent(place, event)`: fires on each event of a declared place,
  /// naming the place and the event; feeds the stream of `place_declare/3`.
  ///
  /// A crossing reported for a place already removed or superseded is dropped
  /// by the layer and never arrives here.
  void Function(String place, PlaceEvent event)? onPlaceEvent;

  // --- The seam predicates ---
  //
  // Eight of the functions above are the layer functions behind the seven
  // networking seam predicates (IGLP, Definition "Seam Predicates"; GNA,
  // "The Seam"; IGLP, The contract: "these eight back the seam predicates"):
  // [observedPeerAddress] behind `peer_address/2`, [punchUdp] behind
  // `punch_udp/1`, [declarePlace] and [onPlaceEvent] behind `place_declare/3`,
  // [removePlace] behind `place_remove/1`, [setTrustLevel] behind
  // `trust_declare/2`, [putKnownPeer] behind `peer_declare/1`, and
  // [putPeerAddress] behind `address_declare/2`.  GLP code decides who acts,
  // when, and whether; each function performs only the mechanism.  Until
  // 2026-10-11 this comment counted six functions behind five predicates.
}
