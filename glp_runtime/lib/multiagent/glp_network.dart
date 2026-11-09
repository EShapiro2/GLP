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

import 'package:glp_runtime/wire/codec.dart' show bytesEqual;

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
/// [TrustLevel], which `declare(trust(U, L))` declares of `pan` or `lan`
/// (GLP-Spec appendix-guards, "Proximity trust" and "Networking seam", 1ce0263).  It is also the
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
  bool operator ==(Object other) =>
      other is PubKey && bytesEqual(bytes, other.bytes);

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

/// The centre of a declared place: the location the layer registered its
/// geofence about, as latitude and longitude in degrees, which is how the
/// platforms' geofencing takes a centre (GNA a1cb804, Section "Geofencing":
/// "a circle of a given radius in metres about a centre ... the centre is the
/// device's location when the agent first declares the place, which the
/// runtime keeps and gives again when it declares the place again at a
/// restart").  The runtime keeps it with the place's standing declaration
/// ([SeamDeclarations.placeCentre]) and does not read it otherwise.  The
/// layer's own class of the same name and fields is GNA's (networking/core,
/// places/place_registry.dart); the glue maps one to the other, as it maps an
/// application.
class PlaceCentre {
  const PlaceCentre({required this.latitude, required this.longitude});

  /// Degrees north of the equator, negative south of it.
  final double latitude;

  /// Degrees east of the prime meridian, negative west of it.
  final double longitude;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PlaceCentre &&
          other.latitude == latitude &&
          other.longitude == longitude;

  @override
  int get hashCode => Object.hash(latitude, longitude);

  @override
  String toString() => 'PlaceCentre($latitude, $longitude)';
}

/// The attested application: what `onPeerConnected` carries for a peer whose
/// identity is attested (GNA api.tex, Section 2.3, `onPeerConnected(pk,
/// underlay, application?)`: "for an attested identity, the attested
/// application: its developer, its name and its version number; null for an
/// unattested identity").  The runtime records it for `peer_app/2`, which
/// assigns it as `app(D, N, V)` (GLP-Spec appendix-guards, "Networking seam",
/// 5fab8ac; IGLP, Definition "Seam Predicates", 4a334c2).  The layer's own
/// class of the same name and fields is GNA's (networking/core,
/// session/application.dart); the glue maps one to the other, as it maps an
/// underlay by its name.
class Application {
  const Application({
    required this.developer,
    required this.name,
    required this.version,
  });

  /// The developer: on Android the SHA-256 digest of the developer's signing
  /// certificate, in lowercase hex; on iOS the developer's team identifier
  /// (GNA, Section 2.6).
  final String developer;

  /// The application's name: on Android the package; on iOS the bundle
  /// identifier.
  final String name;

  /// The version number the application signed in the session's digest.
  final int version;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Application &&
          other.developer == developer &&
          other.name == name &&
          other.version == version;

  @override
  int get hashCode => Object.hash(developer, name, version);

  @override
  String toString() => 'Application($developer, $name, $version)';
}

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
  /// Closed trust recognises.  Backs `declare(peer(P))`.
  void putKnownPeer(PubKey pk);

  /// Remove [pk] and any recorded address from the known-peer set:
  /// `removeKnownPeer(pk)`.  Backs `remove(peer(P))`.
  void removeKnownPeer(PubKey pk);

  // --- Connection and reachability (GNA, "Connection and Reachability") ---

  /// `onPeerConnected(pk, underlay, application?)`: fires when a peer becomes
  /// reachable, with its key, the [Underlay] over which it connected, and,
  /// for an attested identity, the attested [Application], its developer, its
  /// name and its version number; null for an unattested identity (GNA
  /// api.tex, Section 2.3).  The runtime records the application of each
  /// peer's session for `peer_app/2` ([MadContext.peerConnected]); until
  /// 2026-11-07 it carried an `Object?` and relied on none of it.
  void Function(PubKey pk, Underlay underlay, Application? application)?
      onPeerConnected;

  /// `onPeerDisconnected(pk, underlay)`: fires when a peer is no longer
  /// reachable over [Underlay] `underlay`.  A peer reported disconnected over
  /// every underlay it was reported connected over has no session, and
  /// `peer_app/2` assigns `none` for it ([MadContext.peerDisconnected]).
  void Function(PubKey pk, Underlay underlay)? onPeerDisconnected;

  /// `isPeerReachable(pk) → bool`: whether [pk] is currently reachable via any
  /// underlay.
  bool isPeerReachable(PubKey pk);

  /// `peerUnderlays(pk) → List<Underlay>`: the underlays by which [pk] is
  /// currently reachable (empty if unreachable).
  List<Underlay> peerUnderlays(PubKey pk);

  // --- Point-to-point communication (GNA, "Point-to-Point Communication") ---

  /// `send(pubKey, payload)`: send opaque [payload] bytes to peer [pk].  Order
  /// is arbitrary; delivery is fair (GNA, "Networking Assumptions").  The
  /// future is the layer's answer: it "completes once the message is saved for
  /// delivery ..., and fails if the message cannot be saved" (GNA, Section
  /// 2.9).  The runtime awaits it, and keeps a refused message and sends it
  /// again later (IGLP b014a9e, Implementation Notes, "Networking Seam", The
  /// contract: "the layer refuses to send a message it cannot save, and the
  /// runtime waits for its answer and, on a refusal, keeps the message and
  /// sends it again later").
  Future<void> send(PubKey pk, Uint8List payload);

  /// `onMessageReceived(sender, payload, messageId, underlay)`: fires on
  /// delivery of an inbound message, with the authenticated sender's key, the
  /// opaque payload, the layer-assigned messageId, and the [Underlay] over
  /// which it was delivered.
  void Function(
          PubKey sender, Uint8List payload, String messageId, Underlay underlay)?
      onMessageReceived;

  /// `acknowledge(messageId)`: "Confirm that the message delivered under messageId has been applied and saved by the runtime; the layer keeps a delivered message until it is acknowledged, and delivers it again at restart otherwise" (GNA 776016c, Section 2.9; Section 2.11, Fair message delivery).  The runtime calls it once the cycle that applied the message is saved, or, where the agent keeps no state, once the cycle is complete (IGLP, Implementation Notes, "Persistence": "a delivered message acknowledged to the layer, only from a state the store holds").
  void acknowledge(String messageId);

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

  /// `onPeerLost(pk, underlay)`: fires when an agent reported discovered on
  /// the proximity underlay [underlay] is no longer discovered on it: "its
  /// presence is no longer observed and it holds no session on the underlay"
  /// (GNA cfbfeac, Section 3.1, Discovery).  With [onPeerDiscovered] it is
  /// behind the stream of `declare(nearby(Underlay, Events))`, `gone(K)`
  /// (GNA, "The layer functions").  Until 2026-11-08 the interface had none,
  /// and `gone(K)` was fed from [onPeerDisconnected].
  void Function(PubKey pk, ProximityUnderlay underlay)? onPeerLost;

  /// `setTrustLevel(underlay, level)`: set the cold-call trust level of
  /// proximity underlay [underlay] to [level]; "both levels default to
  /// Closed" (GNA, Trust levels).  Backs `declare(trust(U, L))`.
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
  /// known peer.  Backs `declare(address(P, A))`.
  void putPeerAddress(PubKey pk, String address);

  /// `putRendezvousServer(pk, address)`: "Add the rendezvous server with public key pk at address to the servers the layer connects to; a further declaration of the same pk replaces its address" (GNA 776016c, Section 4.2).  Backs `declare(rendezvous(P, A))`.
  void putRendezvousServer(PubKey pk, String address);

  /// `removeRendezvousServer(pk)`: "Remove the server from those the layer connects to, and end the layer's session with it; a key not declared does nothing" (GNA 776016c, Section 4.2).  Backs `remove(rendezvous(P))`.
  void removeRendezvousServer(PubKey pk);

  /// `observedPeerAddress(pk) → Address?`: the public `ip:port` at which the
  /// layer observes the connected peer [pk], or null if none is observed.
  /// Backs `peer_address/2`, which reports the two as `address(S)` and `none`.
  String? observedPeerAddress(PubKey pk);

  /// `punchUdp(address)`: send a short burst of UDP datagrams toward
  /// [address], opening the NAT mapping; returns nothing.  Backs
  /// `punch_udp/1`.
  void punchUdp(String address);

  // --- Places (GNA, "The Seam", place functions) ---

  /// `declarePlace(place, centre, radius)`: declare [place] — the region of
  /// [radiusMetres] metres about [centre], or about this device's location at
  /// the time of the call where [centre] is null — and report its crossings
  /// through [onPlaceEvent].  Returns the centre the layer registered, the
  /// device's location where [centre] was null, which the runtime keeps and
  /// gives again when it declares the place again at a restart; null where
  /// the platform refuses, leaving the place undeclared (GNA a1cb804, Section
  /// "Geofencing", and "System Predicates": "the runtime giving the device's
  /// location at the agent's first declaration of the place and the same
  /// centre at a restart"; IGLP 96cd44a, Implementation Notes, "Persistence":
  /// the store keeps "the places with the centre each was declared about",
  /// and a restart makes "each place about the centre the store holds for
  /// it").  Backs `declare(place(P, R, E))`, [place] being the printed form
  /// of the constant P, `7` for 7 (IGLP, Implementation Notes, "The
  /// contract").  A further declaration under the same name supersedes the
  /// first.  GLP supplies a name and a radius and receives no coordinates:
  /// the centre crosses the seam between the runtime and the layer and
  /// reaches no program.  Until 2026-11-09 the layer took a name and a radius
  /// and returned whether it registered, so a place made again at a restart
  /// was centred where the device then was.
  Future<PlaceCentre?> declarePlace(
      String place, PlaceCentre? centre, double radiusMetres);

  /// `removePlace(place)`: end the declaration of [place].  Backs
  /// `remove(place(P))`.  Removing a place that is not declared does nothing.
  Future<void> removePlace(String place);

  /// `onPlaceEvent(place, event)`: fires on each event of a declared place,
  /// naming the place and the event; feeds the stream of
  /// `declare(place(P, R, E))`.
  ///
  /// A crossing reported for a place already removed or superseded is dropped
  /// by the layer and never arrives here.
  void Function(String place, PlaceEvent event)? onPlaceEvent;

  // --- The seam predicates ---
  //
  // These functions are the layer functions behind the five networking seam predicates (IGLP, Definition "Seam Predicates", 3b736bb, peer_app/2 since 4a334c2; GLP-Spec appendix-guards, "Networking seam", 1ce0263, peer_app/2 since 5fab8ac; GNA 776016c, Section 5): [observedPeerAddress] behind `peer_address/2`; [onPeerConnected] and [onPeerDisconnected], recorded by the runtime, behind `peer_app/2`; [punchUdp] behind `punch_udp/1`; behind `declare/1`, [putKnownPeer] for `peer(P)`, [putPeerAddress] for `address(P, A)`, [putRendezvousServer] for `rendezvous(P, A)`, [setTrustLevel] for `trust(U, L)`, and [declarePlace] and [onPlaceEvent] for `place(P, R, E)`; and behind `remove/1`, [removeKnownPeer] for `peer(P)`, [removeRendezvousServer] for `rendezvous(P)` and [removePlace] for `place(P)`.  GLP code decides who acts, when, and whether; each function performs only the mechanism.  Until 2026-11-03 seven predicates stood behind them, `place_declare/3`, `place_remove/1`, `trust_declare/2`, `peer_declare/1` and `address_declare/2` in place of `declare/1` and `remove/1`.
}
