import 'messages_state.dart';
import 'messages_actions.dart';

/// Outgoing-message status transitions are monotonic past `sent`. Once we
/// have proof of delivery (ACK → `delivered`) or proof of view (read receipt
/// → `read`), no later action may drag the status back. Same for `failed`:
/// the user explicitly has to retry, we don't transparently re-attempt.
///
/// Returns `true` if the action should be dropped (the existing status is
/// already at a terminal-ish state that this action would regress).
bool _isTerminalForRegression(MessageStatus status) =>
    status == MessageStatus.delivered ||
    status == MessageStatus.read ||
    status == MessageStatus.failed;

/// Reducer for messages state
MessagesState messagesReducer(MessagesState state, MessageAction action) {
  if (action is MessageSendingAction) {
    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    final existing = state.outgoingMessages[action.messageId];

    if (existing != null) {
      // Never regress from delivered/read/failed. The drain dispatches
      // MessageSendingAction blindly; if the message was already delivered
      // (e.g. ACK arrived before the drain picked it up), this would
      // otherwise wipe `deliveredAt` and flip the UI checkmarks backward.
      if (_isTerminalForRegression(existing.status)) {
        return state;
      }
      // Update in place so we preserve `sentAt` and any timestamps; only
      // bump the transport/payload metadata and the status.
      updated[action.messageId] = existing.copyWith(
        transport: action.transport,
        recipientPubkey: action.recipientPubkey,
        payloadSize: action.payloadSize,
        status: MessageStatus.sending,
      );
    } else {
      updated[action.messageId] = OutgoingMessage(
        messageId: action.messageId,
        transport: action.transport,
        recipientPubkey: action.recipientPubkey,
        payloadSize: action.payloadSize,
        sentAt: action.timestamp,
        status: MessageStatus.sending,
      );

      // Trim if exceeds max
      if (updated.length > MessagesState.maxMessages) {
        final sorted = updated.entries.toList()
          ..sort((a, b) => a.value.sentAt.compareTo(b.value.sentAt));
        final toRemove =
            sorted.take(updated.length - MessagesState.maxMessages);
        for (final entry in toRemove) {
          updated.remove(entry.key);
        }
      }
    }

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageFailedAction) {
    final existing = state.outgoingMessages[action.messageId];
    if (existing == null) return state;

    // Don't downgrade delivered/read to failed: by definition the recipient
    // already has it, so it's not really failed from their perspective.
    if (existing.status == MessageStatus.delivered ||
        existing.status == MessageStatus.read) {
      return state;
    }

    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    updated[action.messageId] = existing.copyWith(
      status: MessageStatus.failed,
    );

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageQueuedAction) {
    final existing = state.outgoingMessages[action.messageId];
    if (existing == null) return state;

    // `sent → queued` is legitimate (BLE-disconnect or ack-timeout re-queue);
    // `delivered/read/failed → queued` is a regression and gets dropped.
    if (_isTerminalForRegression(existing.status)) {
      return state;
    }

    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    updated[action.messageId] = existing.copyWith(
      status: MessageStatus.queued,
    );

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageSentAction) {
    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    final existing = state.outgoingMessages[action.messageId];

    if (existing != null) {
      // If the ACK won the race with `_markSentAndTrackForAck` (rare but
      // possible for fragmented BLE sends or hot-restart edge cases), don't
      // walk the status back from delivered/read to sent.
      if (_isTerminalForRegression(existing.status)) {
        return state;
      }
      // Update existing message (sending/queued -> sent)
      updated[action.messageId] = existing.copyWith(
        transport: action.transport,
        recipientPubkey: action.recipientPubkey,
        payloadSize: action.payloadSize,
        status: MessageStatus.sent,
      );
    } else {
      // Create new message (for backwards compatibility)
      final message = OutgoingMessage(
        messageId: action.messageId,
        transport: action.transport,
        recipientPubkey: action.recipientPubkey,
        payloadSize: action.payloadSize,
        sentAt: action.timestamp,
        status: MessageStatus.sent,
      );
      updated[action.messageId] = message;

      // Trim if exceeds max
      if (updated.length > MessagesState.maxMessages) {
        final sorted = updated.entries.toList()
          ..sort((a, b) => a.value.sentAt.compareTo(b.value.sentAt));
        final toRemove =
            sorted.take(updated.length - MessagesState.maxMessages);
        for (final entry in toRemove) {
          updated.remove(entry.key);
        }
      }
    }

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageDeliveredAction) {
    final existing = state.outgoingMessages[action.messageId];
    if (existing == null) return state;

    // Don't regress from read back to delivered if the read receipt landed
    // first (unlikely but cheap to defend against).
    if (existing.status == MessageStatus.read) {
      return state;
    }

    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    updated[action.messageId] = existing.copyWith(
      status: MessageStatus.delivered,
      deliveredAt: action.timestamp,
    );

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageReadAction) {
    final existing = state.outgoingMessages[action.messageId];
    if (existing == null) return state;

    final updated = Map<String, OutgoingMessage>.from(state.outgoingMessages);
    updated[action.messageId] = existing.copyWith(
      status: MessageStatus.read,
      readAt: action.timestamp,
    );

    return state.copyWith(outgoingMessages: updated);
  }

  if (action is MessageReceivedAction) {
    final message = IncomingMessage(
      messageId: action.messageId,
      transport: action.transport,
      senderPubkey: action.senderPubkey,
      payloadSize: action.payloadSize,
      receivedAt: action.timestamp,
    );

    final updated = Map<String, IncomingMessage>.from(state.incomingMessages);
    updated[action.messageId] = message;

    // Trim if exceeds max
    if (updated.length > MessagesState.maxMessages) {
      final sorted = updated.entries.toList()
        ..sort((a, b) => a.value.receivedAt.compareTo(b.value.receivedAt));
      final toRemove = sorted.take(updated.length - MessagesState.maxMessages);
      for (final entry in toRemove) {
        updated.remove(entry.key);
      }
    }

    return state.copyWith(incomingMessages: updated);
  }

  // ===== Conversation Actions =====

  if (action is SaveChatMessageAction) {
    final peerHex =
        action.isOutgoing ? action.recipientPubkeyHex : action.senderPubkeyHex;

    final chatMessage = ChatMessageState(
      senderPubkeyHex: action.senderPubkeyHex,
      recipientPubkeyHex: action.recipientPubkeyHex,
      content: action.content,
      timestamp: action.timestamp,
      isOutgoing: action.isOutgoing,
      messageType: ChatMessageType.values[action.messageType],
      udpAddress: action.udpAddress,
      messageId: action.messageId,
      mediaPath: action.mediaPath,
      mediaMime: action.mediaMime,
      viewOnce: action.viewOnce,
    );

    // Get existing conversation or create new.
    final existingConv = state.conversations[peerHex] ?? const [];

    // Dedupe by messageId. The MessageRouter Bloom filter normally drops
    // duplicate inbound packets before they reach `onMessageReceived`, but
    // the filter resets on dispose (e.g. hot restart), and outgoing sends
    // dispatch `SaveChatMessageAction` directly from `_sendMessage` /
    // `_sendPicture`, so a defensive id-check here keeps the chat thread
    // free of duplicate bubbles regardless.
    if (action.messageId != null &&
        existingConv.any((m) => m.messageId == action.messageId)) {
      return state;
    }

    final newConv = List<ChatMessageState>.from(existingConv)..add(chatMessage);

    // Trim if exceeds max per conversation
    while (newConv.length > MessagesState.maxMessagesPerConversation) {
      newConv.removeAt(0);
    }

    final updatedConversations = Map<String, List<ChatMessageState>>.from(
      state.conversations,
    )..[peerHex] = newConv;

    // Increment unread count for incoming messages
    Map<String, int> updatedUnreadCounts = state.unreadCounts;
    if (!action.isOutgoing) {
      updatedUnreadCounts = Map<String, int>.from(state.unreadCounts);
      updatedUnreadCounts[peerHex] = (updatedUnreadCounts[peerHex] ?? 0) + 1;
    }

    return state.copyWith(
      conversations: updatedConversations,
      unreadCounts: updatedUnreadCounts,
    );
  }

  if (action is MarkPictureViewedAction) {
    final conv = state.conversations[action.peerHex];
    if (conv == null) return state;

    final idx = conv.indexWhere((m) => m.messageId == action.messageId);
    if (idx < 0) return state;

    final updatedMessage = conv[idx].copyWith(
      viewed: true,
      clearMediaPath: true,
    );
    final newConv = List<ChatMessageState>.from(conv)..[idx] = updatedMessage;

    final updatedConversations = Map<String, List<ChatMessageState>>.from(
      state.conversations,
    )..[action.peerHex] = newConv;

    return state.copyWith(conversations: updatedConversations);
  }

  if (action is MarkMessagesReadAction) {
    final updatedUnreadCounts = Map<String, int>.from(state.unreadCounts);
    updatedUnreadCounts.remove(action.peerHex);
    return state.copyWith(unreadCounts: updatedUnreadCounts);
  }

  if (action is DeleteConversationAction) {
    // Drop the message thread + unread count. Outgoing/incoming delivery-status
    // records get pruned here too: with the chat history gone there is nothing
    // for those records to bind to, and they would otherwise leak forever.
    final messageIdsToDrop = <String>{
      for (final m in (state.conversations[action.peerHex] ??
          const <ChatMessageState>[]))
        if (m.messageId != null) m.messageId!,
    };

    final newConversations = Map<String, List<ChatMessageState>>.from(
      state.conversations,
    )..remove(action.peerHex);

    final newUnreadCounts = Map<String, int>.from(state.unreadCounts)
      ..remove(action.peerHex);

    final newOutgoing =
        Map<String, OutgoingMessage>.from(state.outgoingMessages)
          ..removeWhere((id, _) => messageIdsToDrop.contains(id));
    final newIncoming =
        Map<String, IncomingMessage>.from(state.incomingMessages)
          ..removeWhere((id, _) => messageIdsToDrop.contains(id));

    return state.copyWith(
      conversations: newConversations,
      unreadCounts: newUnreadCounts,
      outgoingMessages: newOutgoing,
      incomingMessages: newIncoming,
    );
  }

  if (action is HydrateConversationsAction) {
    // Convert dynamic lists to ChatMessageState lists
    final conversations = <String, List<ChatMessageState>>{};
    for (final entry in action.conversations.entries) {
      conversations[entry.key] = entry.value
          .map((m) => m is ChatMessageState
              ? m
              : ChatMessageState.fromJson(m as Map<String, dynamic>))
          .toList();
    }
    return state.copyWith(
      conversations: conversations,
      unreadCounts: action.unreadCounts,
    );
  }

  return state;
}
