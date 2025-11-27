/// Packet structure for the Grassroots BLE protocol.
///
/// All protocol messages are encapsulated in packets with a fixed header
/// and variable-length fields.
library;

import 'dart:typed_data';

import '../constants.dart';
import 'message_type.dart';
import 'packet_flags.dart';
import 'peer_id.dart';

/// A protocol packet containing header and payload.
///
/// Packet format:
/// - Header (14 bytes): version, type, ttl, flags, timestamp, payload length
/// - Sender ID (8 bytes)
/// - Recipient ID (8 bytes, optional)
/// - Payload (variable)
/// - Signature (64 bytes, optional)
class Packet {
  /// Protocol version (always 1).
  final int version;

  /// Message type.
  final MessageType type;

  /// Time-to-live for routing (decremented at each hop).
  final int ttl;

  /// Packet flags indicating optional fields.
  final PacketFlags flags;

  /// Timestamp in milliseconds since epoch.
  final int timestamp;

  /// Sender's peer ID.
  final PeerId senderId;

  /// Recipient's peer ID (null for broadcast).
  final PeerId? recipientId;

  /// Payload bytes.
  final Uint8List payload;

  /// Ed25519 signature (optional).
  final Uint8List? signature;

  /// Creates a new packet.
  Packet({
    this.version = protocolVersion,
    required this.type,
    this.ttl = defaultTtl,
    PacketFlags? flags,
    int? timestamp,
    required this.senderId,
    this.recipientId,
    required this.payload,
    this.signature,
  })  : flags = flags ??
            PacketFlags.from(
              hasRecipient: recipientId != null,
              hasSignature: signature != null,
            ),
        timestamp = timestamp ?? DateTime.now().millisecondsSinceEpoch;

  /// Creates a broadcast packet.
  factory Packet.broadcast({
    required MessageType type,
    required PeerId senderId,
    required Uint8List payload,
    int ttl = defaultTtl,
    Uint8List? signature,
  }) {
    return Packet(
      type: type,
      senderId: senderId,
      recipientId: null,
      payload: payload,
      ttl: ttl,
      signature: signature,
    );
  }

  /// Creates a directed packet.
  factory Packet.directed({
    required MessageType type,
    required PeerId senderId,
    required PeerId recipientId,
    required Uint8List payload,
    int ttl = defaultTtl,
    Uint8List? signature,
  }) {
    return Packet(
      type: type,
      senderId: senderId,
      recipientId: recipientId,
      payload: payload,
      ttl: ttl,
      signature: signature,
    );
  }

  /// Whether this is a broadcast packet.
  bool get isBroadcast => recipientId == null || recipientId!.isBroadcast;

  /// Whether this packet has a signature.
  bool get isSigned => signature != null;

  /// Returns a copy of this packet with decremented TTL.
  ///
  /// Returns null if TTL would become negative.
  Packet? decrementTtl() {
    if (ttl <= 0) return null;
    return Packet(
      version: version,
      type: type,
      ttl: ttl - 1,
      flags: flags,
      timestamp: timestamp,
      senderId: senderId,
      recipientId: recipientId,
      payload: payload,
      signature: signature,
    );
  }

  /// Returns a copy of this packet with a new signature.
  Packet withSignature(Uint8List sig) {
    if (sig.length != signatureSize) {
      throw ArgumentError('Signature must be $signatureSize bytes');
    }
    return Packet(
      version: version,
      type: type,
      ttl: ttl,
      flags: flags.withSignature(),
      timestamp: timestamp,
      senderId: senderId,
      recipientId: recipientId,
      payload: payload,
      signature: Uint8List.fromList(sig),
    );
  }

  /// Calculates the total size of this packet when encoded.
  int get encodedSize {
    var size = headerSize + peerIdSize; // header + sender
    if (flags.hasRecipient) size += peerIdSize;
    size += payload.length;
    if (flags.hasSignature) size += signatureSize;
    return size;
  }

  @override
  String toString() {
    return 'Packet('
        'type: $type, '
        'ttl: $ttl, '
        'sender: ${senderId.toShortString()}, '
        'recipient: ${recipientId?.toShortString() ?? "broadcast"}, '
        'payload: ${payload.length} bytes'
        ')';
  }
}
