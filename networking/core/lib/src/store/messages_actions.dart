import 'dart:typed_data';

/// Transport type for message tracking
enum MessageTransport {
  ble,
  udp,
}

/// Message delivery status
enum MessageStatus {
  /// Message is being sent (clock icon)
  sending,

  /// Message is queued until the peer becomes reachable
  queued,

  /// Message failed to send (red !)
  failed,

  /// Message sent from device (1 green ✓)
  sent,

  /// Message delivered to recipient's device (2 green ✓✓)
  delivered,

  /// Message read by recipient (2 blue ✓✓)
  read,
}

/// Base class for message-related actions
abstract class MessageAction {}

// ===== Outgoing Message Actions =====

/// A message is being sent (user pressed send, sending in progress)
/// Status: sending (clock icon)
class MessageSendingAction extends MessageAction {
  /// Unique message ID for tracking status
  final String messageId;
  final MessageTransport transport;
  final Uint8List recipientPubkey;
  final int payloadSize;
  final DateTime timestamp;

  MessageSendingAction({
    required this.messageId,
    required this.transport,
    required this.recipientPubkey,
    required this.payloadSize,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// A message failed to send
/// Status: failed (red !)
class MessageFailedAction extends MessageAction {
  /// Message ID that failed
  final String messageId;
  final DateTime timestamp;

  MessageFailedAction({
    required this.messageId,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// A message is queued until the recipient becomes reachable
/// Status: queued (clock icon)
class MessageQueuedAction extends MessageAction {
  /// Message ID that was queued
  final String messageId;
  final DateTime timestamp;

  MessageQueuedAction({
    required this.messageId,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// A message was sent (user pressed send, message left device)
/// Status: sent (1 green ✓)
class MessageSentAction extends MessageAction {
  /// Unique message ID for tracking status
  final String messageId;
  final MessageTransport transport;
  final Uint8List recipientPubkey;
  final int payloadSize;
  final DateTime timestamp;

  MessageSentAction({
    required this.messageId,
    required this.transport,
    required this.recipientPubkey,
    required this.payloadSize,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// A message was delivered to recipient's device
/// Status: delivered (2 green ✓✓)
class MessageDeliveredAction extends MessageAction {
  /// Message ID that was delivered
  final String messageId;
  final DateTime timestamp;

  MessageDeliveredAction({
    required this.messageId,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// A message was read by recipient
/// Status: read (2 blue ✓✓)
class MessageReadAction extends MessageAction {
  /// Message ID that was read
  final String messageId;
  final DateTime timestamp;

  MessageReadAction({
    required this.messageId,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

// ===== Incoming Message Actions =====

/// A message was received on this device
class MessageReceivedAction extends MessageAction {
  /// Unique message ID (from sender)
  final String messageId;
  final MessageTransport transport;
  final Uint8List senderPubkey;
  final int payloadSize;
  final DateTime timestamp;

  MessageReceivedAction({
    required this.messageId,
    required this.transport,
    required this.senderPubkey,
    required this.payloadSize,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();
}

// ===== Conversation Actions =====

/// Save a chat message to a conversation
class SaveChatMessageAction extends MessageAction {
  final String senderPubkeyHex;
  final String recipientPubkeyHex;
  final String content;
  final bool isOutgoing;
  final DateTime timestamp;

  /// Message ID for tracking delivery/read status (outgoing only)
  final String? messageId;

  /// Message type (text, friend request, etc.)
  final int messageType; // ChatMessageType.index

  /// For friendship messages: the UDP address involved
  final String? udpAddress;

  /// Picture-message fields. All null/false for text messages.
  final String? mediaPath;
  final String? mediaMime;
  final bool viewOnce;

  SaveChatMessageAction({
    required this.senderPubkeyHex,
    required this.recipientPubkeyHex,
    required this.content,
    required this.isOutgoing,
    DateTime? timestamp,
    this.messageId,
    this.messageType = 0, // ChatMessageType.text
    this.udpAddress,
    this.mediaPath,
    this.mediaMime,
    this.viewOnce = false,
  }) : timestamp = timestamp ?? DateTime.now();
}

/// Mark a view-once picture as viewed and clear its on-disk path. The caller
/// is responsible for the actual file deletion via
/// `MediaService.deleteMediaFile` — this action only updates Redux state.
class MarkPictureViewedAction extends MessageAction {
  final String peerHex;
  final String messageId;

  MarkPictureViewedAction({
    required this.peerHex,
    required this.messageId,
  });
}

/// Delete the entire conversation with [peerHex] — drops the message history
/// and the unread count. Media files attached to messages in the conversation
/// are deleted out-of-band by the middleware that observes this action.
class DeleteConversationAction extends MessageAction {
  final String peerHex;

  DeleteConversationAction(this.peerHex);
}

/// Mark all messages from a peer as read
class MarkMessagesReadAction extends MessageAction {
  final String peerHex;

  MarkMessagesReadAction(this.peerHex);
}

/// Hydrate conversations from persistence
class HydrateConversationsAction extends MessageAction {
  final Map<String, List<dynamic>> conversations;
  final Map<String, int> unreadCounts;

  HydrateConversationsAction({
    required this.conversations,
    required this.unreadCounts,
  });
}
