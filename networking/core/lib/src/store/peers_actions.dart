import 'dart:typed_data';
import '../models/peer.dart';
import '../models/underlay.dart';
import 'peers_state.dart';

/// Which BLE role our device played in the connection.
/// - central: we scanned and connected to the remote peer
/// - peripheral: the remote peer connected to us
enum BleRole { central, peripheral }

/// Base class for peer-related actions
abstract class PeerAction {}

// ===== BLE Discovery Actions =====

/// A BLE device was discovered during scan
class BleDeviceDiscoveredAction extends PeerAction {
  final String deviceId;
  final String? displayName;
  final int rssi;
  final String? serviceUuid;

  BleDeviceDiscoveredAction({
    required this.deviceId,
    this.displayName,
    required this.rssi,
    this.serviceUuid,
  });
}

/// RSSI updated for a discovered BLE device
class BleDeviceRssiUpdatedAction extends PeerAction {
  final String deviceId;
  final int rssi;

  BleDeviceRssiUpdatedAction({required this.deviceId, required this.rssi});
}

/// Mark a discovered BLE device as connecting
class BleDeviceConnectingAction extends PeerAction {
  final String deviceId;

  BleDeviceConnectingAction(this.deviceId);
}

/// Mark a discovered BLE device as connected (transport level)
class BleDeviceConnectedAction extends PeerAction {
  final String deviceId;

  BleDeviceConnectedAction(this.deviceId);
}

/// Mark a discovered BLE device connection as failed.
/// Errors themselves are events, not state — they're logged via the plugin's
/// path-changed stream and not retained in Redux.
class BleDeviceConnectionFailedAction extends PeerAction {
  final String deviceId;

  BleDeviceConnectionFailedAction(this.deviceId);
}

/// Mark a discovered BLE device as disconnected
class BleDeviceDisconnectedAction extends PeerAction {
  final String deviceId;

  BleDeviceDisconnectedAction(this.deviceId);
}

/// Remove a discovered BLE device
class BleDeviceRemovedAction extends PeerAction {
  final String deviceId;

  BleDeviceRemovedAction(this.deviceId);
}

/// Remove stale discovered BLE devices
class StaleDiscoveredBlePeersRemovedAction extends PeerAction {
  final Duration staleThreshold;

  StaleDiscoveredBlePeersRemovedAction(this.staleThreshold);
}

/// Clear all discovered BLE peers
class ClearDiscoveredBlePeersAction extends PeerAction {}

// ===== Peer Identity Actions (after ANNOUNCE) =====

/// An ANNOUNCE packet was received - add or update peer identity.
///
/// Only one of [bleCentralDeviceId] or [blePeripheralDeviceId] should be set
/// per action, based on which BLE role our device played.
class PeerAnnounceReceivedAction extends PeerAction {
  final Uint8List publicKey;
  final int protocolVersion;

  /// BLE signal strength in dBm. Non-null only when the ANNOUNCE arrived over
  /// BLE (carries `payload.rssi` from the plugin). Null for UDP ANNOUNCEs —
  /// the reducer keeps any existing RSSI in that case rather than clobbering.
  final int? rssi;

  final PeerTransport transport;

  /// BLE device ID from our central role (we connected to them)
  final String? bleCentralDeviceId;

  /// BLE device ID from our peripheral role (they connected to us)
  final String? blePeripheralDeviceId;

  PeerAnnounceReceivedAction({
    required this.publicKey,
    required this.protocolVersion,
    this.rssi,
    this.transport = PeerTransport.bleDirect,
    this.bleCentralDeviceId,
    this.blePeripheralDeviceId,
  });
}

/// Update peer RSSI
class PeerRssiUpdatedAction extends PeerAction {
  final Uint8List publicKey;
  final int rssi;

  PeerRssiUpdatedAction({required this.publicKey, required this.rssi});
}

/// Mark peer as disconnected from BLE.
/// If [role] is provided, only clears the device ID for that role.
/// If [role] is null, clears both BLE device IDs.
class PeerBleDisconnectedAction extends PeerAction {
  final Uint8List publicKey;
  final BleRole? role;

  PeerBleDisconnectedAction(this.publicKey, {this.role});
}

/// A peer's Noise session on the IP [underlay] — [Underlay.lan] or
/// [Underlay.wan] — was established or torn down. Updates
/// [PeerState.lanAuthenticated] or [PeerState.wanAuthenticated], and clears
/// that session's attestation; the other sessions are untouched.
class PeerIpSessionChangedAction extends PeerAction {
  final String pubkeyHex;
  final Underlay underlay;
  final bool connected;

  PeerIpSessionChangedAction({
    required this.pubkeyHex,
    required this.underlay,
    required this.connected,
  }) : assert(underlay != Underlay.pan);
}

/// A BLE Noise session with a peer completed authentication.
/// Sets [PeerState.bleAuthenticated] — the BLE half of consolidated
/// reachability. Cleared by [PeerBleDisconnectedAction] when the last BLE
/// path drops.
class PeerBleAuthenticatedAction extends PeerAction {
  final Uint8List publicKey;

  PeerBleAuthenticatedAction(this.publicKey);
}

/// The attestation exchange completed on one session (spec §Session
/// Establishment). Until it has, the peer is not reachable over that
/// session's underlay: "A peer becomes reachable over an underlay when its
/// session on that underlay is established, authenticated, and attested."
///
/// [underlay] is the session's: [Underlay.pan] for the GATT session,
/// [Underlay.lan] for the LAN session and [Underlay.wan] for the WAN session.
///
/// A failed verification produces no action of this kind — it tears the
/// session down instead.
class PeerAttestedAction extends PeerAction {
  final Uint8List publicKey;
  final Underlay underlay;
  final PeerAttestation attestation;

  PeerAttestedAction({
    required this.publicKey,
    required this.underlay,
    required this.attestation,
  });
}

/// A verified packet was received from a peer on its session on the IP
/// [underlay] — [Underlay.lan] or [Underlay.wan].
///
/// Refreshes that session's staleness clock alone, so a silent session is aged
/// out even while the peer is live on another underlay (spec §ANNOUNCE and
/// Liveness: "independently per underlay").
class PeerIpSeenAction extends PeerAction {
  final Uint8List publicKey;
  final Underlay underlay;

  PeerIpSeenAction(this.publicKey, this.underlay)
      : assert(underlay != Underlay.pan);
}

/// A session-authenticated packet was received over BLE from a peer.
///
/// Refreshes the BLE liveness clock the stale sweep checks: any authenticated
/// traffic counts, not only ANNOUNCE (spec §ANNOUNCE and Liveness).
class PeerBleSeenAction extends PeerAction {
  final Uint8List publicKey;

  PeerBleSeenAction(this.publicKey);
}

/// Mark peer as disconnected on its session on the IP [underlay] —
/// [Underlay.lan] or [Underlay.wan].
class PeerIpDisconnectedAction extends PeerAction {
  final Uint8List publicKey;
  final Underlay underlay;

  PeerIpDisconnectedAction(this.publicKey, this.underlay)
      : assert(underlay != Underlay.pan);
}

/// Mark peer as fully disconnected
class PeerDisconnectedAction extends PeerAction {
  final Uint8List publicKey;

  PeerDisconnectedAction(this.publicKey);
}

/// Remove peer completely
class PeerRemovedAction extends PeerAction {
  final Uint8List publicKey;

  PeerRemovedAction(this.publicKey);
}

/// Remove stale peers that haven't been seen.
///
/// [protectedPubkeyHexes] are never removed — the coordinator passes the
/// known-peer set (GLP-supplied keys) so dial-book entries survive memory
/// pressure and reconnection stays possible.
class StalePeersRemovedAction extends PeerAction {
  final Duration staleThreshold;
  final Set<String> protectedPubkeyHexes;

  StalePeersRemovedAction(
    this.staleThreshold, {
    this.protectedPubkeyHexes = const {},
  });
}

/// Clear all peers
class ClearAllPeersAction extends PeerAction {}

// ===== Association Actions =====

/// Associate a BLE device ID with a pubkey for a specific role
class AssociateBleDeviceAction extends PeerAction {
  final Uint8List publicKey;
  final String deviceId;
  final BleRole role;

  AssociateBleDeviceAction({
    required this.publicKey,
    required this.deviceId,
    required this.role,
  });
}

/// Associate a UDP address with a pubkey
class AssociateUdpAddressAction extends PeerAction {
  final Uint8List publicKey;
  final String address;

  AssociateUdpAddressAction({required this.publicKey, required this.address});
}

/// Register a peer identity supplied by GLP, without any address or live
/// connection (`putKnownPeer`, spec §Known Peers). Creates a minimal
/// disconnected entry for an unknown peer; a no-op for a known one.
class PeerIdentityRegisteredAction extends PeerAction {
  final Uint8List publicKey;

  PeerIdentityRegisteredAction({required this.publicKey});
}

// ===== Reachability Verification Actions =====

/// We successfully reached a peer at their UDP address without coordinating
/// a hole-punch — empirical evidence that they accept unsolicited inbound.
class PeerDirectReachObservedAction extends PeerAction {
  final Uint8List publicKey;
  final DateTime observedAt;

  PeerDirectReachObservedAction(this.publicKey, {DateTime? observedAt})
      : observedAt = observedAt ?? DateTime.now();
}
