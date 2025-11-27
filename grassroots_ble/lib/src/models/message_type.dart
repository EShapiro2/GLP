/// Message types for the Grassroots BLE protocol.
///
/// Each message type has a specific byte value used in packet headers.
library;

/// Enumeration of all protocol message types.
enum MessageType {
  /// Chat message (0x01).
  message(0x01),

  /// Delivery acknowledgment (0x02).
  deliveryAck(0x02),

  /// Read receipt (0x03).
  readReceipt(0x03),

  /// Noise XX handshake message 1 (0x10).
  noiseHandshakeInit(0x10),

  /// Noise XX handshake message 2 (0x11).
  noiseHandshakeResp(0x11),

  /// Noise XX handshake message 3 (0x12).
  noiseHandshakeFinal(0x12),

  /// Friendship request (0x20).
  friendRequest(0x20),

  /// Friendship accepted (0x21).
  friendAccept(0x21),

  /// Friendship rejected (0x22).
  friendReject(0x22),

  /// Friend introduction (0x30).
  introduction(0x30),

  /// Friend list share - Level 4 (0x31).
  friendListShare(0x31),

  /// First fragment (0xF0).
  fragmentStart(0xF0),

  /// Middle fragment (0xF1).
  fragmentContinue(0xF1),

  /// Last fragment (0xF2).
  fragmentEnd(0xF2);

  /// The byte value for this message type.
  final int value;

  const MessageType(this.value);

  /// Returns the [MessageType] for the given byte value.
  ///
  /// Throws [ArgumentError] if the value is not a valid message type.
  static MessageType fromValue(int value) {
    for (final type in MessageType.values) {
      if (type.value == value) {
        return type;
      }
    }
    throw ArgumentError('Unknown message type: 0x${value.toRadixString(16)}');
  }

  /// Returns the [MessageType] for the given byte value, or null if invalid.
  static MessageType? tryFromValue(int value) {
    for (final type in MessageType.values) {
      if (type.value == value) {
        return type;
      }
    }
    return null;
  }

  /// Whether this is a handshake message type.
  bool get isHandshake =>
      this == MessageType.noiseHandshakeInit ||
      this == MessageType.noiseHandshakeResp ||
      this == MessageType.noiseHandshakeFinal;

  /// Whether this is a fragment message type.
  bool get isFragment =>
      this == MessageType.fragmentStart ||
      this == MessageType.fragmentContinue ||
      this == MessageType.fragmentEnd;

  /// Whether this is a friendship-related message type.
  bool get isFriendship =>
      this == MessageType.friendRequest ||
      this == MessageType.friendAccept ||
      this == MessageType.friendReject ||
      this == MessageType.introduction ||
      this == MessageType.friendListShare;
}
