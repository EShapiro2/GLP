import 'package:flutter/foundation.dart';
import 'package:grassroots_networking/src/store/messages_state.dart';

// Re-export ChatMessageType from messages_state for backwards compatibility
export 'package:grassroots_networking/src/store/messages_state.dart' show ChatMessageType;

/// A chat message model for the demo app
class ChatMessage {
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

  ChatMessage({
    required this.senderPubkeyHex,
    required this.recipientPubkeyHex,
    required this.content,
    required this.isOutgoing,
    this.messageType = ChatMessageType.text,
    this.udpAddress,
    this.messageId,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();

  /// Whether this is a friendship-related message
  bool get isFriendshipMessage => messageType != ChatMessageType.text;

  /// Whether this is a pending friend request that can be accepted
  bool get canAccept => messageType == ChatMessageType.friendRequestReceived;

  /// Create a friend request sent message
  factory ChatMessage.friendRequestSent({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    String? udpAddress,
    String? message,
  }) =>
      ChatMessage(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: message ?? 'Sent a friend request',
        isOutgoing: true,
        messageType: ChatMessageType.friendRequestSent,
        udpAddress: udpAddress,
      );

  /// Create a friend request received message
  factory ChatMessage.friendRequestReceived({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    String? udpAddress,
    String? message,
  }) =>
      ChatMessage(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: message ?? 'Wants to be friends',
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestReceived,
        udpAddress: udpAddress,
      );

  /// Create a friend request accepted message (they accepted ours)
  factory ChatMessage.friendRequestAccepted({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
    String? udpAddress,
  }) =>
      ChatMessage(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: 'Accepted your friend request',
        isOutgoing: false,
        messageType: ChatMessageType.friendRequestAccepted,
        udpAddress: udpAddress,
      );

  /// Create a friend request accepted by us message
  factory ChatMessage.friendRequestAcceptedByUs({
    required String senderPubkeyHex,
    required String recipientPubkeyHex,
  }) =>
      ChatMessage(
        senderPubkeyHex: senderPubkeyHex,
        recipientPubkeyHex: recipientPubkeyHex,
        content: 'You accepted the friend request',
        isOutgoing: true,
        messageType: ChatMessageType.friendRequestAcceptedByUs,
      );

  /// Convert a public key to hex string
  static String pubkeyToHex(Uint8List pubkey) {
    return pubkey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Convert hex string back to public key
  static Uint8List hexToPubkey(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }
}
