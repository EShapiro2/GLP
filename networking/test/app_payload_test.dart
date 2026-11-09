import 'dart:typed_data';

import 'package:flutter_test/flutter_test.dart';
import 'package:grassroots_networking/app_payload.dart';
import 'package:grassroots_networking_core/src/models/underlay.dart';
import 'package:grassroots_networking_core/src/store/app_state.dart';
import 'package:grassroots_networking_core/src/store/messages_actions.dart';
import 'package:grassroots_networking_core/src/store/messages_state.dart';
import 'package:grassroots_networking_core/src/store/reducers.dart';

String _hex(Uint8List key) =>
    key.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

void main() {
  final reader = Uint8List.fromList(List.filled(32, 0x0b));
  final stranger = Uint8List.fromList(List.filled(32, 0x0c));
  const chatId = '6f1c2b9e-0d4a-4c1e-8f3a-2b7d9e5c1a40';

  const wireId = '0a9e3c1d-5b2f-4e7a-9c8d-1f2e3a4b5c6d';
  final readerHex = _hex(reader);

  // The sender's state once chat message [chatId] is saved to the
  // conversation with `reader`, sent, and linked to the id the layer gave it.
  final MessagesState sent = [
    SaveChatMessageAction(
      senderPubkeyHex: 'aa' * 32,
      recipientPubkeyHex: readerHex,
      content: 'read me',
      isOutgoing: true,
      messageId: chatId,
    ),
    MessageSendingAction(
      messageId: wireId,
      underlay: Underlay.pan,
      recipientPubkey: reader,
      payloadSize: 10,
    ),
    LinkWireMessageAction(
      peerHex: readerHex,
      messageId: chatId,
      wireMessageId: wireId,
    ),
  ].fold(AppState.initial, appReducer).messages;

  group('receiptReadWireId', () {
    test(
        'a receipt from the peer the message was sent to marks it read, by '
        'the id the layer gave it', () {
      expect(receiptReadWireId(sent, chatId, readerHex), wireId);
    });

    test('a receipt from a different peer marks nothing read', () {
      expect(receiptReadWireId(sent, chatId, _hex(stranger)), isNull);
      expect(sent.outgoingMessages[wireId]?.status, MessageStatus.sending);
    });

    test('a receipt naming no message sent to its sender marks nothing read',
        () {
      expect(receiptReadWireId(sent, 'no-such-message', readerHex), isNull);
    });

    test('a receipt for a message the layer has not yet named marks nothing',
        () {
      final unlinked = appReducer(
        AppState.initial,
        SaveChatMessageAction(
          senderPubkeyHex: 'aa' * 32,
          recipientPubkeyHex: readerHex,
          content: 'read me',
          isOutgoing: true,
          messageId: chatId,
        ),
      ).messages;
      expect(receiptReadWireId(unlinked, chatId, readerHex), isNull);
    });
  });
}
