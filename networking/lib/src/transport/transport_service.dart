import 'dart:async';
import 'dart:typed_data';
import 'package:flutter/widgets.dart';

/// Transport type identifier
enum TransportType {
  /// Bluetooth Low Energy direct P2P transport
  ble,

  /// WebRTC-based P2P transport (STUN/TURN/TURNS)
  webrtc,

  /// UDP-based transport (grassroots_dart_udx)
  udp,
}

/// Display metadata for a transport service.
///
/// Used by UI components to show appropriate icons and labels
/// for each transport type.
class TransportDisplayInfo {
  /// Icon to display for this transport (e.g., Bluetooth, WiFi, Globe)
  final IconData icon;

  /// Human-readable name of the transport
  final String name;

  /// Short description of the transport
  final String description;

  /// Color associated with this transport (optional)
  final Color? color;

  const TransportDisplayInfo({
    required this.icon,
    required this.name,
    required this.description,
    this.color,
  });
}

/// Transport connection state
enum TransportState {
  /// Transport is not initialized
  uninitialized,

  /// Transport is initializing
  initializing,

  /// Transport is ready but not active
  ready,

  /// Transport is actively running (scanning/listening)
  active,

  /// Transport encountered an error
  error,

  /// Transport is disposed
  disposed,
}

/// Event emitted when data is received from a peer
class TransportDataEvent {
  /// The peer that sent the data
  final String peerId;

  /// The transport this data came from
  final TransportType transport;

  /// The raw data received
  final Uint8List data;

  /// Timestamp when received
  final DateTime timestamp;

  TransportDataEvent({
    required this.peerId,
    required this.transport,
    required this.data,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// Event emitted when a peer connects or disconnects
class TransportConnectionEvent {
  /// The peer ID
  final String peerId;

  /// The transport type
  final TransportType transport;

  /// Whether this is a connection (true) or disconnection (false)
  final bool connected;

  /// Additional info about the connection/disconnection
  final String? reason;

  /// True if this connection was initiated by the remote peer (we accepted),
  /// false if we initiated. Always false for disconnect events.
  ///
  /// Used to record unsolicited inbound traffic at our advertised address.
  final bool isIncoming;

  TransportConnectionEvent({
    required this.peerId,
    required this.transport,
    required this.connected,
    this.reason,
    this.isIncoming = false,
  });
}

/// Abstract interface for transport services.
///
/// This interface defines the contract that all transport implementations
/// must fulfill, allowing the application layer to switch between
/// different transport protocols (BLE, WebRTC, etc.) seamlessly.
///
/// ## Implementation Requirements
///
/// Implementations must:
/// 1. Handle peer discovery appropriate to the transport
/// 2. Manage connections to discovered peers
/// 3. Provide reliable data transmission (or indicate unreliability)
/// 4. Map transport-specific peer IDs to the common format
///
/// ## Lifecycle
///
/// ```
/// create → initialize() → start() → [active use] → stop() → dispose()
/// ```
abstract class TransportService {
  /// The type of transport this service provides
  TransportType get type;

  /// Display information for this transport (icon, name, description)
  /// Used by UI to show transport indicators next to peers
  TransportDisplayInfo get displayInfo;

  /// Current state of the transport
  TransportState get state;

  /// Stream of received data events
  Stream<TransportDataEvent> get dataStream;

  /// Stream of connection events
  Stream<TransportConnectionEvent> get connectionStream;

  /// Number of connected peers
  int get connectedCount;

  /// Whether the transport is currently active
  bool get isActive;

  /// Initialize the transport service.
  ///
  /// This should:
  /// - Set up any required resources
  /// - Check permissions
  /// - Prepare for [start] to be called
  ///
  /// Returns true if initialization succeeded.
  Future<bool> initialize();

  /// Start the transport (begin discovery/listening).
  ///
  /// After this call, the transport should:
  /// - Begin discovering peers (if applicable)
  /// - Accept incoming connections
  /// - Emit events through the streams
  Future<void> start();

  /// Stop the transport (stop discovery/listening).
  ///
  /// This should:
  /// - Stop discovering new peers
  /// - Optionally maintain existing connections
  /// - Can be restarted with [start]
  Future<void> stop();

  /// Send data to a specific peer.
  ///
  /// Returns true if the data was sent (or queued) successfully.
  /// Note: This doesn't guarantee delivery for unreliable transports.
  Future<bool> sendToPeer(String peerId, Uint8List data);

  /// Broadcast data to all connected peers.
  ///
  /// [excludePeerIds] can be used to exclude specific peers
  /// (e.g., to skip friends who get a separate addressed ANNOUNCE).
  Future<void> broadcast(Uint8List data, {Set<String>? excludePeerIds});

  /// Associate a peer with a public key.
  ///
  /// Called after identity exchange (e.g., ANNOUNCE packet).
  /// This allows higher layers to address peers by pubkey.
  void associatePeerWithPubkey(String peerId, Uint8List pubkey);

  /// Get peer ID for a public key (if known).
  String? getPeerIdForPubkey(Uint8List pubkey);

  /// Get public key for a peer ID (if known).
  Uint8List? getPubkeyForPeerId(String peerId);

  /// Clean up resources.
  ///
  /// After this call, the transport cannot be used again.
  Future<void> dispose();
}

/// Convenience extension on TransportState
extension TransportStateX on TransportState {
  /// Whether this state represents a usable (initialized) transport
  bool get isUsable =>
      this == TransportState.ready || this == TransportState.active;
}
