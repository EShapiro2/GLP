import 'dart:typed_data';
import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/src/store/messages_state.dart';
import 'package:grassroots_networking/src/store/messages_actions.dart';
import 'package:grassroots_networking/src/store/messages_reducer.dart';

void main() {
  // Shared test helpers
  final testRecipientPubkey = Uint8List.fromList(List.filled(32, 0xAA));
  final testSenderPubkey = Uint8List.fromList(List.filled(32, 0xBB));
  final testTimestamp = DateTime(2025, 1, 15, 12, 0, 0);

  group('MessageSendingAction', () {
    test('creates outgoing message with status sending', () {
      const state = MessagesState.initial;
      final action = MessageSendingAction(
        messageId: 'msg-1',
        transport: MessageTransport.ble,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 128,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages.length, 1);
      final msg = newState.outgoingMessages['msg-1']!;
      expect(msg.messageId, 'msg-1');
      expect(msg.transport, MessageTransport.ble);
      expect(msg.recipientPubkey, testRecipientPubkey);
      expect(msg.payloadSize, 128);
      expect(msg.sentAt, testTimestamp);
      expect(msg.status, MessageStatus.sending);
      expect(msg.deliveredAt, isNull);
      expect(msg.readAt, isNull);
    });

    test('trims oldest messages when exceeding maxMessages', () {
      // Pre-populate state with maxMessages outgoing messages
      final existing = <String, OutgoingMessage>{};
      for (var i = 0; i < MessagesState.maxMessages; i++) {
        final id = 'existing-$i';
        existing[id] = OutgoingMessage(
          messageId: id,
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          sentAt: DateTime(2025, 1, 1, 0, 0, i), // incrementing timestamps
        );
      }
      final state = MessagesState(outgoingMessages: existing);
      expect(state.outgoingMessages.length, MessagesState.maxMessages);

      // Add one more message with a later timestamp
      final action = MessageSendingAction(
        messageId: 'new-msg',
        transport: MessageTransport.udp,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 256,
        timestamp: DateTime(2025, 6, 1),
      );

      final newState = messagesReducer(state, action);

      // Should still be at maxMessages (oldest trimmed)
      expect(newState.outgoingMessages.length, MessagesState.maxMessages);
      // New message should exist
      expect(newState.outgoingMessages.containsKey('new-msg'), isTrue);
      // The oldest message (existing-0) should have been trimmed
      expect(newState.outgoingMessages.containsKey('existing-0'), isFalse);
    });
  });

  group('MessageFailedAction', () {
    test('updates existing message status to failed', () {
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.sending,
          ),
        },
      );

      final action = MessageFailedAction(messageId: 'msg-1');
      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.failed);
    });

    test('returns same state for unknown messageId', () {
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.sending,
          ),
        },
      );

      final action = MessageFailedAction(messageId: 'unknown-msg');
      final newState = messagesReducer(state, action);

      expect(identical(newState, state), isTrue);
    });
  });

  group('MessageQueuedAction', () {
    test('updates existing message status to queued', () {
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.sending,
          ),
        },
      );

      final action = MessageQueuedAction(messageId: 'msg-1');
      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.queued);
      expect(newState.queuedMessages.map((m) => m.messageId), ['msg-1']);
    });

    test('returns same state for unknown messageId', () {
      const state = MessagesState.initial;

      final action = MessageQueuedAction(messageId: 'unknown-msg');
      final newState = messagesReducer(state, action);

      expect(identical(newState, state), isTrue);
    });
  });

  group('MessageSentAction', () {
    test('updates existing message to sent status', () {
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.sending,
          ),
        },
      );

      final action = MessageSentAction(
        messageId: 'msg-1',
        transport: MessageTransport.udp,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 128,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.sent);
      expect(
          newState.outgoingMessages['msg-1']!.transport, MessageTransport.udp);
    });

    test('creates new message if does not exist (backwards compat)', () {
      const state = MessagesState.initial;

      final action = MessageSentAction(
        messageId: 'msg-new',
        transport: MessageTransport.udp,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 256,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages.length, 1);
      final msg = newState.outgoingMessages['msg-new']!;
      expect(msg.status, MessageStatus.sent);
      expect(msg.transport, MessageTransport.udp);
      expect(msg.payloadSize, 256);
      expect(msg.sentAt, testTimestamp);
    });

    test('trims oldest when creating new message exceeds max', () {
      final existing = <String, OutgoingMessage>{};
      for (var i = 0; i < MessagesState.maxMessages; i++) {
        final id = 'existing-$i';
        existing[id] = OutgoingMessage(
          messageId: id,
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          sentAt: DateTime(2025, 1, 1, 0, 0, i),
        );
      }
      final state = MessagesState(outgoingMessages: existing);

      // MessageSentAction for a non-existing message should create + trim
      final action = MessageSentAction(
        messageId: 'brand-new',
        transport: MessageTransport.udp,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 100,
        timestamp: DateTime(2025, 6, 1),
      );

      final newState = messagesReducer(state, action);

      expect(newState.outgoingMessages.length, MessagesState.maxMessages);
      expect(newState.outgoingMessages.containsKey('brand-new'), isTrue);
      expect(newState.outgoingMessages.containsKey('existing-0'), isFalse);
    });
  });

  group('MessageDeliveredAction', () {
    test('updates to delivered status with timestamp', () {
      final deliveredTime = DateTime(2025, 1, 15, 12, 5, 0);
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.sent,
          ),
        },
      );

      final action = MessageDeliveredAction(
        messageId: 'msg-1',
        timestamp: deliveredTime,
      );

      final newState = messagesReducer(state, action);

      final msg = newState.outgoingMessages['msg-1']!;
      expect(msg.status, MessageStatus.delivered);
      expect(msg.deliveredAt, deliveredTime);
    });

    test('returns same state for unknown messageId', () {
      const state = MessagesState.initial;
      final action = MessageDeliveredAction(messageId: 'unknown');
      final newState = messagesReducer(state, action);

      expect(identical(newState, state), isTrue);
    });
  });

  group('MessageReadAction', () {
    test('updates to read status with timestamp', () {
      final readTime = DateTime(2025, 1, 15, 12, 10, 0);
      final state = MessagesState(
        outgoingMessages: {
          'msg-1': OutgoingMessage(
            messageId: 'msg-1',
            transport: MessageTransport.ble,
            recipientPubkey: testRecipientPubkey,
            payloadSize: 128,
            sentAt: testTimestamp,
            status: MessageStatus.delivered,
            deliveredAt: DateTime(2025, 1, 15, 12, 5, 0),
          ),
        },
      );

      final action = MessageReadAction(
        messageId: 'msg-1',
        timestamp: readTime,
      );

      final newState = messagesReducer(state, action);

      final msg = newState.outgoingMessages['msg-1']!;
      expect(msg.status, MessageStatus.read);
      expect(msg.readAt, readTime);
      // deliveredAt should be preserved
      expect(msg.deliveredAt, DateTime(2025, 1, 15, 12, 5, 0));
    });

    test('returns same state for unknown messageId', () {
      const state = MessagesState.initial;
      final action = MessageReadAction(messageId: 'unknown');
      final newState = messagesReducer(state, action);

      expect(identical(newState, state), isTrue);
    });
  });

  group('status-transition monotonicity', () {
    // Once a message reaches `delivered`, `read`, or `failed`, no earlier
    // transition (sending / queued / sent) is allowed to walk the status
    // back. This defends against three real races:
    //   - ACK arrives before `_markSentAndTrackForAck` registers the entry,
    //     so `MessageDeliveredAction` lands first and `MessageSentAction`
    //     follows.
    //   - Watchdog/disconnect re-queues a message that's already delivered
    //     (timer fired then ACK landed), dispatching `MessageQueuedAction`.
    //   - Drain dispatches `MessageSendingAction` for an already-delivered
    //     queue entry it hasn't yet trimmed.

    OutgoingMessage deliveredAt(MessageStatus status) => OutgoingMessage(
          messageId: 'msg-1',
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          sentAt: testTimestamp,
          status: status,
          deliveredAt:
              status == MessageStatus.delivered || status == MessageStatus.read
                  ? testTimestamp.add(const Duration(milliseconds: 100))
                  : null,
          readAt: status == MessageStatus.read
              ? testTimestamp.add(const Duration(milliseconds: 200))
              : null,
        );

    test('MessageSentAction does NOT regress from delivered to sent', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.delivered)},
      );
      final action = MessageSentAction(
        messageId: 'msg-1',
        transport: MessageTransport.ble,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 64,
        timestamp: testTimestamp,
      );
      final newState = messagesReducer(state, action);
      expect(newState.outgoingMessages['msg-1']!.status,
          MessageStatus.delivered);
      expect(newState.outgoingMessages['msg-1']!.deliveredAt,
          state.outgoingMessages['msg-1']!.deliveredAt,
          reason: 'deliveredAt must be preserved.');
    });

    test('MessageSentAction does NOT regress from read to sent', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.read)},
      );
      final action = MessageSentAction(
        messageId: 'msg-1',
        transport: MessageTransport.ble,
        recipientPubkey: testRecipientPubkey,
        payloadSize: 64,
        timestamp: testTimestamp,
      );
      final newState = messagesReducer(state, action);
      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.read);
      expect(newState.outgoingMessages['msg-1']!.readAt,
          state.outgoingMessages['msg-1']!.readAt);
    });

    test('MessageQueuedAction does NOT regress from delivered to queued', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.delivered)},
      );
      final newState =
          messagesReducer(state, MessageQueuedAction(messageId: 'msg-1'));
      expect(newState.outgoingMessages['msg-1']!.status,
          MessageStatus.delivered);
    });

    test('MessageQueuedAction DOES allow sent → queued (legitimate re-queue)',
        () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.sent)},
      );
      final newState =
          messagesReducer(state, MessageQueuedAction(messageId: 'msg-1'));
      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.queued,
          reason: 'BLE-disconnect / ack-timeout re-queue is allowed.');
    });

    test('MessageSendingAction does NOT regress from delivered, preserves '
        'deliveredAt', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.delivered)},
      );
      final newState = messagesReducer(
        state,
        MessageSendingAction(
          messageId: 'msg-1',
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          timestamp: testTimestamp,
        ),
      );
      expect(newState.outgoingMessages['msg-1']!.status,
          MessageStatus.delivered);
      expect(newState.outgoingMessages['msg-1']!.deliveredAt,
          state.outgoingMessages['msg-1']!.deliveredAt,
          reason: 'deliveredAt must survive an erroneous Sending re-dispatch.');
    });

    test('MessageSendingAction does allow queued → sending (drain step)', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.queued)},
      );
      final newState = messagesReducer(
        state,
        MessageSendingAction(
          messageId: 'msg-1',
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          timestamp: testTimestamp,
        ),
      );
      expect(
          newState.outgoingMessages['msg-1']!.status, MessageStatus.sending);
    });

    test('MessageFailedAction does NOT downgrade delivered to failed', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.delivered)},
      );
      final newState =
          messagesReducer(state, MessageFailedAction(messageId: 'msg-1'));
      expect(newState.outgoingMessages['msg-1']!.status,
          MessageStatus.delivered);
    });

    test('MessageDeliveredAction does NOT regress from read to delivered', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.read)},
      );
      final newState = messagesReducer(
        state,
        MessageDeliveredAction(messageId: 'msg-1', timestamp: testTimestamp),
      );
      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.read);
    });

    test('failed is terminal: no re-promotion on MessageSendingAction', () {
      final state = MessagesState(
        outgoingMessages: {'msg-1': deliveredAt(MessageStatus.failed)},
      );
      final newState = messagesReducer(
        state,
        MessageSendingAction(
          messageId: 'msg-1',
          transport: MessageTransport.ble,
          recipientPubkey: testRecipientPubkey,
          payloadSize: 64,
          timestamp: testTimestamp,
        ),
      );
      expect(newState.outgoingMessages['msg-1']!.status, MessageStatus.failed);
    });
  });

  group('MessageReceivedAction', () {
    test('creates incoming message record', () {
      const state = MessagesState.initial;
      final action = MessageReceivedAction(
        messageId: 'inc-1',
        transport: MessageTransport.udp,
        senderPubkey: testSenderPubkey,
        payloadSize: 512,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      expect(newState.incomingMessages.length, 1);
      final msg = newState.incomingMessages['inc-1']!;
      expect(msg.messageId, 'inc-1');
      expect(msg.transport, MessageTransport.udp);
      expect(msg.senderPubkey, testSenderPubkey);
      expect(msg.payloadSize, 512);
      expect(msg.receivedAt, testTimestamp);
    });

    test('trims oldest when exceeding max', () {
      final existing = <String, IncomingMessage>{};
      for (var i = 0; i < MessagesState.maxMessages; i++) {
        final id = 'inc-$i';
        existing[id] = IncomingMessage(
          messageId: id,
          transport: MessageTransport.ble,
          senderPubkey: testSenderPubkey,
          payloadSize: 64,
          receivedAt: DateTime(2025, 1, 1, 0, 0, i),
        );
      }
      final state = MessagesState(incomingMessages: existing);

      final action = MessageReceivedAction(
        messageId: 'inc-new',
        transport: MessageTransport.udp,
        senderPubkey: testSenderPubkey,
        payloadSize: 100,
        timestamp: DateTime(2025, 6, 1),
      );

      final newState = messagesReducer(state, action);

      expect(newState.incomingMessages.length, MessagesState.maxMessages);
      expect(newState.incomingMessages.containsKey('inc-new'), isTrue);
      expect(newState.incomingMessages.containsKey('inc-0'), isFalse);
    });
  });

  group('SaveChatMessageAction', () {
    const senderHex = 'aabbccdd';
    const recipientHex = '11223344';

    test('adds outgoing message to conversation keyed by recipientPubkeyHex',
        () {
      const state = MessagesState.initial;
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Hello!',
        isOutgoing: true,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      // Conversation should be keyed by recipient for outgoing
      expect(newState.conversations.containsKey(recipientHex), isTrue);
      expect(newState.conversations.containsKey(senderHex), isFalse);
      final conv = newState.conversations[recipientHex]!;
      expect(conv.length, 1);
      expect(conv.first.content, 'Hello!');
      expect(conv.first.isOutgoing, isTrue);
      expect(conv.first.senderPubkeyHex, senderHex);
      expect(conv.first.recipientPubkeyHex, recipientHex);
    });

    test('dedupes by messageId — duplicate SaveChatMessageAction is a no-op',
        () {
      const state = MessagesState.initial;
      final firstAction = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Hello!',
        isOutgoing: false,
        timestamp: testTimestamp,
        messageId: 'msg-dup-1',
      );

      final after1 = messagesReducer(state, firstAction);
      // Same messageId arriving again (Bloom filter cleared on hot restart,
      // or some other unusual path).
      final secondAction = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Hello!',
        isOutgoing: false,
        timestamp: testTimestamp.add(const Duration(seconds: 1)),
        messageId: 'msg-dup-1',
      );
      final after2 = messagesReducer(after1, secondAction);

      // Second action is dropped — no second bubble in the chat thread.
      expect(identical(after2, after1), isTrue,
          reason: 'Duplicate messageId must short-circuit the reducer.');
      expect(after2.conversations[senderHex]!.length, 1);
    });

    test('messages without a messageId still append (e.g. friendship blocks)',
        () {
      const state = MessagesState.initial;
      // Friendship messages typically don't carry a messageId. Dedupe only
      // engages when an id is present.
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Sent a friend request',
        isOutgoing: true,
        timestamp: testTimestamp,
      );
      final after1 = messagesReducer(state, action);
      final after2 = messagesReducer(after1, action);
      expect(after2.conversations[recipientHex]!.length, 2,
          reason: 'No messageId means no dedupe — both saves stick.');
    });

    test('adds incoming message to conversation keyed by senderPubkeyHex', () {
      const state = MessagesState.initial;
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Hi there!',
        isOutgoing: false,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      // Conversation should be keyed by sender for incoming
      expect(newState.conversations.containsKey(senderHex), isTrue);
      expect(newState.conversations.containsKey(recipientHex), isFalse);
      final conv = newState.conversations[senderHex]!;
      expect(conv.length, 1);
      expect(conv.first.content, 'Hi there!');
      expect(conv.first.isOutgoing, isFalse);
    });

    test('increments unread count for incoming messages', () {
      const state = MessagesState.initial;
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Incoming 1',
        isOutgoing: false,
        timestamp: testTimestamp,
      );

      final state1 = messagesReducer(state, action);
      expect(state1.unreadCounts[senderHex], 1);

      // Send a second incoming message
      final action2 = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Incoming 2',
        isOutgoing: false,
        timestamp: testTimestamp.add(const Duration(seconds: 1)),
      );

      final state2 = messagesReducer(state1, action2);
      expect(state2.unreadCounts[senderHex], 2);
    });

    test('does NOT increment unread count for outgoing messages', () {
      const state = MessagesState.initial;
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Outgoing',
        isOutgoing: true,
        timestamp: testTimestamp,
      );

      final newState = messagesReducer(state, action);

      expect(newState.unreadCounts.containsKey(recipientHex), isFalse);
      expect(newState.unreadCounts.containsKey(senderHex), isFalse);
    });

    test('trims conversation when exceeding maxMessagesPerConversation', () {
      // Pre-populate a conversation at the limit
      final existingMessages = <ChatMessageState>[];
      for (var i = 0; i < MessagesState.maxMessagesPerConversation; i++) {
        existingMessages.add(ChatMessageState(
          senderPubkeyHex: senderHex,
          recipientPubkeyHex: recipientHex,
          content: 'Message $i',
          timestamp: DateTime(2025, 1, 1, 0, 0, i),
          isOutgoing: true,
        ));
      }
      final state = MessagesState(
        conversations: {recipientHex: existingMessages},
      );

      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Overflow message',
        isOutgoing: true,
        timestamp: DateTime(2025, 6, 1),
      );

      final newState = messagesReducer(state, action);

      final conv = newState.conversations[recipientHex]!;
      expect(conv.length, MessagesState.maxMessagesPerConversation);
      // The newest message should be the one we just added
      expect(conv.last.content, 'Overflow message');
      // The oldest (Message 0) should have been removed
      expect(conv.first.content, 'Message 1');
    });

    test('preserves messageType and udpAddress fields', () {
      const state = MessagesState.initial;
      final action = SaveChatMessageAction(
        senderPubkeyHex: senderHex,
        recipientPubkeyHex: recipientHex,
        content: 'Sent a friend request',
        isOutgoing: true,
        timestamp: testTimestamp,
        messageType: ChatMessageType.friendRequestSent.index,
        udpAddress: '[::1]:4001',
        messageId: 'fr-1',
      );

      final newState = messagesReducer(state, action);

      final msg = newState.conversations[recipientHex]!.first;
      expect(msg.messageType, ChatMessageType.friendRequestSent);
      expect(msg.udpAddress, '[::1]:4001');
      expect(msg.messageId, 'fr-1');
    });
  });

  group('MarkMessagesReadAction', () {
    test('clears unread count for specific peer', () {
      const state = MessagesState(
        unreadCounts: {'peer-a': 5, 'peer-b': 3},
      );

      final action = MarkMessagesReadAction('peer-a');
      final newState = messagesReducer(state, action);

      expect(newState.unreadCounts.containsKey('peer-a'), isFalse);
      expect(newState.unreadCounts['peer-b'], 3);
    });

    test('no-op for peer with no unread count', () {
      const state = MessagesState(
        unreadCounts: {'peer-a': 5},
      );

      final action = MarkMessagesReadAction('peer-nonexistent');
      final newState = messagesReducer(state, action);

      // unreadCounts should be unchanged in content
      expect(newState.unreadCounts['peer-a'], 5);
      expect(newState.unreadCounts.length, 1);
    });
  });

  group('HydrateConversationsAction', () {
    test('restores conversations and unread counts', () {
      const state = MessagesState.initial;

      final msg1 = ChatMessageState(
        senderPubkeyHex: 'aabb',
        recipientPubkeyHex: '1122',
        content: 'Restored message 1',
        timestamp: testTimestamp,
        isOutgoing: true,
      );
      final msg2 = ChatMessageState(
        senderPubkeyHex: 'ccdd',
        recipientPubkeyHex: 'aabb',
        content: 'Restored message 2',
        timestamp: testTimestamp.add(const Duration(minutes: 1)),
        isOutgoing: false,
      );

      final action = HydrateConversationsAction(
        conversations: {
          '1122': [msg1],
          'ccdd': [msg2],
        },
        unreadCounts: {'ccdd': 1},
      );

      final newState = messagesReducer(state, action);

      expect(newState.conversations.length, 2);
      expect(newState.conversations['1122']!.length, 1);
      expect(
          newState.conversations['1122']!.first.content, 'Restored message 1');
      expect(newState.conversations['ccdd']!.length, 1);
      expect(
          newState.conversations['ccdd']!.first.content, 'Restored message 2');
      expect(newState.unreadCounts['ccdd'], 1);
    });

    test('restores from JSON-serialized data via fromJson', () {
      const state = MessagesState.initial;

      final jsonMsg = {
        'senderPubkeyHex': 'aabb',
        'recipientPubkeyHex': '1122',
        'content': 'From JSON',
        'timestamp': '2025-01-15T12:00:00.000',
        'isOutgoing': true,
        'messageType': 0,
        'udpAddress': null,
        'messageId': null,
      };

      final action = HydrateConversationsAction(
        conversations: {
          '1122': [jsonMsg],
        },
        unreadCounts: {},
      );

      final newState = messagesReducer(state, action);

      expect(newState.conversations['1122']!.length, 1);
      final msg = newState.conversations['1122']!.first;
      expect(msg, isA<ChatMessageState>());
      expect(msg.content, 'From JSON');
      expect(msg.senderPubkeyHex, 'aabb');
      expect(msg.isOutgoing, isTrue);
    });

    test('overwrites existing conversations', () {
      final existingMsg = ChatMessageState(
        senderPubkeyHex: 'old',
        recipientPubkeyHex: 'old-r',
        content: 'Old message',
        timestamp: testTimestamp,
        isOutgoing: true,
      );
      final state = MessagesState(
        conversations: {
          'old-r': [existingMsg]
        },
        unreadCounts: const {'old-r': 2},
      );

      final newMsg = ChatMessageState(
        senderPubkeyHex: 'new',
        recipientPubkeyHex: 'new-r',
        content: 'New message',
        timestamp: testTimestamp,
        isOutgoing: false,
      );

      final action = HydrateConversationsAction(
        conversations: {
          'new-r': [newMsg]
        },
        unreadCounts: {'new-r': 1},
      );

      final newState = messagesReducer(state, action);

      // Old conversation should be gone, replaced by hydrated data
      expect(newState.conversations.containsKey('old-r'), isFalse);
      expect(newState.conversations.containsKey('new-r'), isTrue);
      expect(newState.unreadCounts.containsKey('old-r'), isFalse);
      expect(newState.unreadCounts['new-r'], 1);
    });
  });

  group('unknown action', () {
    test('returns the same state for unhandled action types', () {
      const state = MessagesState.initial;

      // Create a trivial subclass to represent an unhandled action
      final action = _UnknownMessageAction();
      final newState = messagesReducer(state, action);

      expect(identical(newState, state), isTrue);
    });
  });
}

/// Test helper: an action subclass not handled by the reducer.
class _UnknownMessageAction extends MessageAction {}
