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
  // The id the sender's send returned for the chat message.
  const sentId = '6f1c2b9e-0d4a-4c1e-8f3a-2b7d9e5c1a40';

  // The sender's state once the chat message is sent to `reader`.
  final MessagesState sent = appReducer(
    AppState.initial,
    MessageSendingAction(
      messageId: sentId,
      underlay: Underlay.pan,
      recipientPubkey: reader,
      payloadSize: 10,
    ),
  ).messages;

  group('receiptMarksRead', () {
    test('a receipt from the peer the message was sent to marks it read', () {
      expect(receiptMarksRead(sent, sentId, _hex(reader)), isTrue);
    });

    test('a receipt from a different peer marks nothing read', () {
      expect(receiptMarksRead(sent, sentId, _hex(stranger)), isFalse);
      expect(sent.outgoingMessages[sentId]?.status, MessageStatus.sending);
    });

    test('a receipt naming no message sent to its sender marks nothing read',
        () {
      expect(receiptMarksRead(sent, 'no-such-message', _hex(reader)), isFalse);
    });
  });
}
