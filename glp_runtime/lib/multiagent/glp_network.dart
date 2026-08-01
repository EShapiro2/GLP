/// GlpNetwork — the one Dart interface madGLP uses to talk to networking.
///
/// Transcribes the GLP Networking API paper (Sections 2–3) per
/// `docs/ma/networking-seam-spec.md` v0.2. Two realizations exist behind this
/// interface: `SimulationNetwork` (this repo, wraps the isolate transport) and
/// the real BLE/IP layer (Dan's). Integration is a backend swap.
///
/// Callbacks are settable fields, matching `MadContext.onMessageReady` house
/// style. `sign`/`verify` are synchronous (spec §2): their future callers are
/// the GLP body kernels, and Ed25519 signing is pure CPU work.
///
/// See: docs/ma/networking-seam-spec.md Section 2.
library;

import 'dart:typed_data';

/// Transport medium a peer is reachable over.
enum Transport { ble, ip }

/// Trust level governing first contact (paper Section 3).
///
/// Under [closed], an agent does not connect to, and receives no first contact
/// from, agents it has never contacted. [open] accepts cold-calls from anyone.
enum TrustLevel { open, closed }

/// A 32-byte Ed25519 public key, with value equality and a hex string form.
///
/// This is the network-level identity of an agent. Symbolic agent identifiers
/// (e.g. "alice") are resolved to a [PubKey] only at the seam (spec §4); GLP
/// programs and global names keep the symbolic identifiers.
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

/// An event on a declared place's stream (paper Section 5).
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

/// A device found by scanning, before its identity is fully established.
///
/// Carries transport-specific identification; in the simulation realization the
/// public key is known directly (spec §2, §3).
class DiscoveredPeer {
  /// The peer's public key.
  final PubKey pk;

  /// The transport the peer was discovered over.
  final Transport transport;

  DiscoveredPeer(this.pk, this.transport);

  @override
  String toString() => 'DiscoveredPeer($pk, $transport)';
}

/// The networking interface madGLP talks to.
///
/// One realization wraps the simulation isolate transport; another wraps the
/// real BLE/IP layer. The runtime adapter (spec §4) drives outgoing traffic via
/// [send] and receives via [onMessageReceived].
abstract class GlpNetwork {
  // --- Identity (paper Section 2.1) ---

  /// Install this layer's identity (its key pair). The layer holds the private
  /// key for [sign].
  void putIdentity(PubKey pub, Uint8List priv);

  /// This layer's own public key.
  PubKey getIdentity();

  // --- Connection and reachability (Section 2.2) ---

  /// Fires when a peer becomes reachable over transport [t].
  void Function(PubKey pk, Transport t)? onPeerConnected;

  /// Fires when a peer is no longer reachable over transport [t].
  void Function(PubKey pk, Transport t)? onPeerDisconnected;

  /// Whether [pk] is currently reachable over any transport.
  bool isPeerReachable(PubKey pk);

  /// The transports over which [pk] is currently reachable (`[]` if none).
  List<Transport> peerTransports(PubKey pk);

  // --- Point-to-point communication (Section 2.5) ---

  /// Send opaque [payload] bytes to peer [pk]. Order is arbitrary; delivery is
  /// fair (every message sent is eventually delivered while reachable).
  void send(PubKey pk, Uint8List payload);

  /// Fires on delivery of an inbound message, with the authenticated [sender]
  /// key, the opaque [payload], the layer-assigned [messageId], and [t].
  void Function(PubKey sender, Uint8List payload, String messageId, Transport t)?
      onMessageReceived;

  // --- Signing primitives backing the system predicates (Section 2) ---
  // The layer holds the private key. Synchronous (spec §2).

  /// Sign [message] with this layer's private key (Ed25519).
  Uint8List sign(Uint8List message);

  /// Verify that [signature] over [message] is valid for [signer] (Ed25519).
  bool verify(PubKey signer, Uint8List message, Uint8List signature);

  // --- BLE (Section 3) ---

  /// Currently discovered/adjacent peers.
  List<DiscoveredPeer> getPeers();

  /// Fires when a new peer is discovered.
  void Function(DiscoveredPeer p)? onPeerDiscovered;

  /// Set the trust level governing first contact.
  void setTrustLevel(TrustLevel level);

  // --- IP (Section 4) — UnsupportedError in SimulationNetwork ---

  /// Fires when this layer's public IP address changes.
  void Function(String? oldAddr, String newAddr)? onConnectivityStatusChanged;

  /// This layer's public IP address.
  String getPublicAddress();

  /// Generate a peer-link URI others can consume to reach this layer.
  String generatePeerLink();

  /// Consume a peer-link URI to reach another layer.
  void consumePeerLink(String uri);

  // --- Seam predicates (paper Section 5) ---
  //
  // The five functions behind the four networking seam predicates
  // `peer_address/2`, `punch_udp/1`, `place_declare/3` and `place_remove/1`
  // (IGLP, Definition Seam Predicates). GLP code decides who acts, when, and
  // whether; each function performs only the mechanism.

  /// The address at which this layer observes peer [pk], or null if none is
  /// observed. Backs `peer_address/2`.
  String? observedPeerAddress(PubKey pk);

  /// Open a path to [address] and return nothing. Backs `punch_udp/1`.
  void punchUdp(String address);

  /// Declare [place] — the region of [radiusMetres] about this device's
  /// location at the time of the call — and report its crossings through
  /// [onPlaceEvent]. Backs `place_declare/3`.
  ///
  /// Returns false when the platform refuses, leaving the place undeclared. A
  /// further declaration under the same name supersedes the first. GLP supplies
  /// a name and a radius and receives no coordinates.
  Future<bool> declarePlace(String place, double radiusMetres);

  /// End the declaration of [place]. Backs `place_remove/1`. Removing a place
  /// that is not declared does nothing.
  Future<void> removePlace(String place);

  /// Fires on each event of a declared place, naming the place and the event.
  ///
  /// A crossing reported for a place already removed or superseded is dropped
  /// by the layer and never arrives here.
  void Function(String place, PlaceEvent event)? onPlaceEvent;
}
