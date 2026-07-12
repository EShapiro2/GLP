import 'package:flutter/foundation.dart';
import 'messages_actions.dart';

/// Type of chat message content
enum ChatMessageType {
  /// Regular text message
  text,

  /// Friend request sent
  friendRequestSent,

  /// Friend request received
  friendRequestReceived,

  /// Friend request accepted by them
  friendRequestAccepted,

  /// Friend request accepted by us
  friendRequestAcceptedByUs,

  /// Picture message — the file lives on disk at `mediaPath`. May be a
  /// "1-time view" picture if `viewOnce` is true; the recipient deletes
  /// the file after viewing, the sender deletes when delivery confirms.
  picture,
}

/// Record of an outgoing message with delivery status
@immutable
class OutgoingMessage {
  final String messageId;
  final MessageTransport transport;
  final Uint8List recipientPubkey;
  final int payloadSize;
  final DateTime sentAt;
  final MessageStatus status;
  final DateTime? deliveredAt;
  final DateTime? readAt;

  const OutgoingMessage({
    required this.messageId,
    required this.transport,
    required this.recipientPubkey,
    required this.payloadSize,
    required this.sentAt,
    this.status = MessageStatus.sent,
    this.deliveredAt,
    this.readAt,
  });

  String get recipientPubkeyHex =>
      recipientPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  OutgoingMessage copyWith({
    MessageTransport? transport,
    Uint8List? recipientPubkey,
    int? payloadSize,
    MessageStatus? status,
    DateTime? deliveredAt,
    DateTime? readAt,
  }) {
    return OutgoingMessage(
      messageId: messageId,
      transport: transport ?? this.transport,
      recipientPubkey: recipientPubkey ?? this.recipientPubkey,
      payloadSize: payloadSize ?? this.payloadSize,
      sentAt: sentAt,
      status: status ?? this.status,
      deliveredAt: deliveredAt ?? this.deliveredAt,
      readAt: readAt ?? this.readAt,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is OutgoingMessage &&
          runtimeType == other.runtimeType &&
          messageId == other.messageId;

  @override
  int get hashCode => messageId.hashCode;
}

/// Record of an incoming message
@immutable
class IncomingMessage {
  final String messageId;
  final MessageTransport transport;
  final Uint8List senderPubkey;
  final int payloadSize;
  final DateTime receivedAt;

  const IncomingMessage({
    required this.messageId,
    required this.transport,
    required this.senderPubkey,
    required this.payloadSize,
    required this.receivedAt,
  });

  String get senderPubkeyHex =>
      senderPubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is IncomingMessage &&
          runtimeType == other.runtimeType &&
          messageId == other.messageId;

  @override
  int get hashCode => messageId.hashCode;
}

/// A chat message stored in Redux (for conversation history)
@immutable
class ChatMessageState {
  final String senderPubkeyHex;
  final String recipientPubkeyHex;
  final String content;
  final DateTime timestamp;
  final bool isOutgoing;
  final ChatMessageType messageType;

  /// For friendship messages: the UDP address involved
  final String? udpAddress;

  /// Message ID for tracking delivery/read status (outgoing messages only)
  final String? messageId;

  /// Absolute path to the on-disk media file for picture messages. Null for
  /// text/friendship messages, and also null after a view-once picture has
  /// been viewed (recipient) or delivered (sender).
  final String? mediaPath;

  /// MIME type of the media file (e.g. `image/jpeg`). Set whenever
  /// `mediaPath` is set; used to choose the right decoder/extension.
  final String? mediaMime;

  /// Whether this is a "1-time view" picture. The recipient bubble shows
  /// a blurred preview until the user taps; on dismiss the file is deleted.
  /// On the sender side, the file is deleted on `MessageStatus.delivered`.
  final bool viewOnce;

  /// Whether the view-once picture has been viewed (recipient) or expired
  /// (sender, post-delivery). Once true, `mediaPath` is null and the bubble
  /// renders the "expired" placeholder.
  final bool viewed;

  const ChatMessageState({
    required this.senderPubkeyHex,
    required this.recipientPubkeyHex,
    required this.content,
    required this.timestamp,
    required this.isOutgoing,
    this.messageType = ChatMessageType.text,
    this.udpAddress,
    this.messageId,
    this.mediaPath,
    this.mediaMime,
    this.viewOnce = false,
    this.viewed = false,
  });

  /// Whether this message carries an on-disk picture that hasn't expired.
  bool get hasMedia => mediaPath != null;

  /// Whether this is a picture message.
  bool get isPicture => messageType == ChatMessageType.picture;

  /// The peer's pubkey hex (the other party in the conversation)
  String get peerHex => isOutgoing ? recipientPubkeyHex : senderPubkeyHex;

  /// Whether this is a friendship-related message (request/accept).
  /// Picture and text messages are user content and should render as regular
  /// chat bubbles, not as the centered system-message style.
  bool get isFriendshipMessage =>
      messageType == ChatMessageType.friendRequestSent ||
      messageType == ChatMessageType.friendRequestReceived ||
      messageType == ChatMessageType.friendRequestAccepted ||
      messageType == ChatMessageType.friendRequestAcceptedByUs;

  /// Whether this is a pending friend request that can be accepted
  bool get canAccept => messageType == ChatMessageType.friendRequestReceived;

  /// Create a friend request sent message
  factory ChatMessageState.friendRequestSent({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    required String udpAddress,
    String? message,
  }) =>
      ChatMessageState(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: message ?? 'Sent a friend request',
        timestamp: DateTime.now(),
        isOutgoing: true,
        messageType: ChatMessageType.friendRequestSent,
        udpAddress: udpAddress,
      );

  /// Create a friend request received message
  factory ChatMessageState.friendRequestReceived({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    required String udpAddress,
    String? message,
  }) =>
      ChatMessageState(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: message ?? 'Wants to be friends',
        timestamp: DateTime.now(),
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestReceived,
        udpAddress: udpAddress,
      );

  /// Create a friend request accepted message (they accepted ours)
  factory ChatMessageState.friendRequestAccepted({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    required String udpAddress,
  }) =>
      ChatMessageState(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: 'Accepted your friend request',
        timestamp: DateTime.now(),
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestAccepted,
        udpAddress: udpAddress,
      );

  /// Create a friend request accepted by us message
  factory ChatMessageState.friendRequestAcceptedByUs({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
  }) =>
      ChatMessageState(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: 'You accepted the friend request',
        timestamp: DateTime.now(),
        isOutgoing: true,
        messageType: ChatMessageType.friendRequestAcceptedByUs,
      );

  Map<String, dynamic> toJson() => {
        'senderPubkeyHex': senderPubkeyHex,
        'recipientPubkeyHex': recipientPubkeyHex,
        'content': content,
        'timestamp': timestamp.toIso8601String(),
        'isOutgoing': isOutgoing,
        'messageType': messageType.index,
        'udpAddress': udpAddress,
        'messageId': messageId,
        'mediaPath': mediaPath,
        'mediaMime': mediaMime,
        'viewOnce': viewOnce,
        'viewed': viewed,
      };

  factory ChatMessageState.fromJson(Map<String, dynamic> json) {
    return ChatMessageState(
      senderPubkeyHex: json['senderPubkeyHex'] as String,
      recipientPubkeyHex: json['recipientPubkeyHex'] as String,
      content: json['content'] as String,
      timestamp: DateTime.parse(json['timestamp'] as String),
      isOutgoing: json['isOutgoing'] as bool,
      messageType: ChatMessageType.values[json['messageType'] as int],
      udpAddress: json['udpAddress'] as String?,
      messageId: json['messageId'] as String?,
      mediaPath: json['mediaPath'] as String?,
      mediaMime: json['mediaMime'] as String?,
      viewOnce: (json['viewOnce'] as bool?) ?? false,
      viewed: (json['viewed'] as bool?) ?? false,
    );
  }

  /// Copy with optional field overrides. `clearMediaPath: true` is used after
  /// a view-once picture is consumed (recipient view, or sender delivery) to
  /// drop the path while keeping the rest of the message intact.
  ChatMessageState copyWith({
    String? content,
    bool? viewed,
    String? mediaPath,
    bool clearMediaPath = false,
  }) {
    return ChatMessageState(
      senderPubkeyHex: senderPubkeyHex,
      recipientPubkeyHex: recipientPubkeyHex,
      content: content ?? this.content,
      timestamp: timestamp,
      isOutgoing: isOutgoing,
      messageType: messageType,
      udpAddress: udpAddress,
      messageId: messageId,
      mediaPath: clearMediaPath ? null : (mediaPath ?? this.mediaPath),
      mediaMime: mediaMime,
      viewOnce: viewOnce,
      viewed: viewed ?? this.viewed,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ChatMessageState &&
          runtimeType == other.runtimeType &&
          senderPubkeyHex == other.senderPubkeyHex &&
          recipientPubkeyHex == other.recipientPubkeyHex &&
          timestamp == other.timestamp &&
          messageType == other.messageType &&
          mediaPath == other.mediaPath &&
          viewed == other.viewed;

  @override
  int get hashCode => Object.hash(
        senderPubkeyHex,
        recipientPubkeyHex,
        timestamp,
        messageType,
        mediaPath,
        viewed,
      );
}

/// Messages state for Redux store
@immutable
class MessagesState {
  /// Outgoing messages keyed by messageId (for delivery status tracking)
  final Map<String, OutgoingMessage> outgoingMessages;

  /// Incoming messages keyed by messageId
  final Map<String, IncomingMessage> incomingMessages;

  /// Conversations keyed by peer hex, value is list of messages (oldest first)
  final Map<String, List<ChatMessageState>> conversations;

  /// Unread counts keyed by peer hex
  final Map<String, int> unreadCounts;

  /// Maximum number of delivery status records to keep per direction
  static const int maxMessages = 1000;

  /// Maximum number of chat messages to keep per conversation
  static const int maxMessagesPerConversation = 500;

  const MessagesState({
    this.outgoingMessages = const {},
    this.incomingMessages = const {},
    this.conversations = const {},
    this.unreadCounts = const {},
  });

  static const MessagesState initial = MessagesState();

  // ===== Delivery Status Getters =====

  /// Get outgoing message by ID
  OutgoingMessage? getOutgoingMessage(String messageId) =>
      outgoingMessages[messageId];

  /// Get incoming message by ID
  IncomingMessage? getIncomingMessage(String messageId) =>
      incomingMessages[messageId];

  /// All outgoing messages as list (sorted by sentAt, newest first)
  List<OutgoingMessage> get outgoingMessagesList {
    final list = outgoingMessages.values.toList();
    list.sort((a, b) => b.sentAt.compareTo(a.sentAt));
    return list;
  }

  /// All incoming messages as list (sorted by receivedAt, newest first)
  List<IncomingMessage> get incomingMessagesList {
    final list = incomingMessages.values.toList();
    list.sort((a, b) => b.receivedAt.compareTo(a.receivedAt));
    return list;
  }

  /// Outgoing messages with status 'sent' (pending delivery)
  List<OutgoingMessage> get pendingDeliveryMessages => outgoingMessages.values
      .where((m) => m.status == MessageStatus.sent)
      .toList();

  /// Outgoing messages queued until the recipient is reachable.
  List<OutgoingMessage> get queuedMessages => outgoingMessages.values
      .where((m) => m.status == MessageStatus.queued)
      .toList();

  /// Outgoing messages with status 'delivered' (pending read)
  List<OutgoingMessage> get deliveredMessages => outgoingMessages.values
      .where((m) => m.status == MessageStatus.delivered)
      .toList();

  /// Outgoing messages with status 'read'
  List<OutgoingMessage> get readMessages => outgoingMessages.values
      .where((m) => m.status == MessageStatus.read)
      .toList();

  /// Count of outgoing messages by transport
  int outgoingCountByTransport(MessageTransport transport) =>
      outgoingMessages.values.where((m) => m.transport == transport).length;

  /// Count of incoming messages by transport
  int incomingCountByTransport(MessageTransport transport) =>
      incomingMessages.values.where((m) => m.transport == transport).length;

  // ===== Conversation Getters =====

  /// Get conversation with a peer (list of messages, oldest first)
  List<ChatMessageState> getConversation(String peerHex) =>
      conversations[peerHex] ?? [];

  /// Get unread count for a peer
  int getUnreadCount(String peerHex) => unreadCounts[peerHex] ?? 0;

  /// Get all peers we have conversations with
  List<String> get conversationPeers => conversations.keys.toList();

  /// Check if we have a conversation with a peer
  bool hasConversation(String peerHex) => conversations.containsKey(peerHex);

  /// Get the last message with a peer (for chat preview)
  ChatMessageState? getLastMessage(String peerHex) {
    final conv = conversations[peerHex];
    return conv?.isNotEmpty == true ? conv!.last : null;
  }

  /// Total unread count across all conversations
  int get totalUnreadCount =>
      unreadCounts.values.fold(0, (sum, count) => sum + count);

  // ===== Copy With =====

  MessagesState copyWith({
    Map<String, OutgoingMessage>? outgoingMessages,
    Map<String, IncomingMessage>? incomingMessages,
    Map<String, List<ChatMessageState>>? conversations,
    Map<String, int>? unreadCounts,
  }) {
    return MessagesState(
      outgoingMessages: outgoingMessages ?? this.outgoingMessages,
      incomingMessages: incomingMessages ?? this.incomingMessages,
      conversations: conversations ?? this.conversations,
      unreadCounts: unreadCounts ?? this.unreadCounts,
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is MessagesState &&
          runtimeType == other.runtimeType &&
          mapEquals(outgoingMessages, other.outgoingMessages) &&
          mapEquals(incomingMessages, other.incomingMessages) &&
          _conversationsEqual(conversations, other.conversations) &&
          mapEquals(unreadCounts, other.unreadCounts);

  static bool _conversationsEqual(
    Map<String, List<ChatMessageState>> a,
    Map<String, List<ChatMessageState>> b,
  ) {
    if (a.length != b.length) return false;
    for (final key in a.keys) {
      if (!b.containsKey(key)) return false;
      if (!listEquals(a[key], b[key])) return false;
    }
    return true;
  }

  @override
  int get hashCode => Object.hash(
        outgoingMessages.length,
        incomingMessages.length,
        conversations.length,
        unreadCounts.length,
      );
}
