/// Message model for the Grassroots BLE protocol.
///
/// Represents a chat message with delivery tracking.
library;

import 'dart:typed_data';

import 'connection_state.dart';
import 'peer_id.dart';

/// A chat message.
class Message {
  /// Unique message ID (UUID, 16 bytes).
  final Uint8List messageId;

  /// Sender's peer ID.
  final PeerId senderId;

  /// Recipient's peer ID (null for broadcast).
  final PeerId? recipientId;

  /// Message content (UTF-8 text).
  final String content;

  /// Creation timestamp (milliseconds since epoch).
  final int timestamp;

  /// Current delivery status.
  final MessageStatus status;

  /// Number of send attempts.
  final int retryCount;

  /// Creates a new message.
  Message({
    required this.messageId,
    required this.senderId,
    this.recipientId,
    required this.content,
    int? timestamp,
    this.status = MessageStatus.pending,
    this.retryCount = 0,
  }) : timestamp = timestamp ?? DateTime.now().millisecondsSinceEpoch {
    if (messageId.length != 16) {
      throw ArgumentError('messageId must be 16 bytes (UUID)');
    }
  }

  /// Whether this is a broadcast message.
  bool get isBroadcast => recipientId == null;

  /// Returns the message ID as a hex string.
  String get messageIdHex {
    return messageId.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Returns a copy with updated status.
  Message withStatus(MessageStatus newStatus) {
    return Message(
      messageId: messageId,
      senderId: senderId,
      recipientId: recipientId,
      content: content,
      timestamp: timestamp,
      status: newStatus,
      retryCount: retryCount,
    );
  }

  /// Returns a copy with incremented retry count.
  Message incrementRetry() {
    return Message(
      messageId: messageId,
      senderId: senderId,
      recipientId: recipientId,
      content: content,
      timestamp: timestamp,
      status: status,
      retryCount: retryCount + 1,
    );
  }

  /// Returns the age of this message.
  Duration get age {
    return Duration(
      milliseconds: DateTime.now().millisecondsSinceEpoch - timestamp,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Message &&
          runtimeType == other.runtimeType &&
          messageIdHex == other.messageIdHex;

  @override
  int get hashCode => messageIdHex.hashCode;

  @override
  String toString() {
    final target = recipientId?.toShortString() ?? 'broadcast';
    return 'Message(${messageIdHex.substring(0, 8)}... '
        'from ${senderId.toShortString()} to $target, '
        'status: $status)';
  }
}
