import 'dart:convert';
import 'dart:typed_data';

import 'package:grassroots_networking/grassroots_networking.dart';

/// The payloads the demo app hands to `send`.
///
/// Every payload is one kind byte and its body. A chat block carries the id
/// the sender gave the message, so the reader can name that message back in a
/// read receipt; the layer knows nothing of either.
///
///   block:         [0x01][Block bytes]
///   chat:          [0x02][36-byte message id][Block bytes]
///   read receipt:  [0x03][36-byte message id]
sealed class AppPayload {
  const AppPayload();

  Uint8List encode();

  /// The payload [bytes] carries, or null when it is not one of the above.
  static AppPayload? decode(Uint8List bytes) {
    if (bytes.isEmpty) return null;
    final body = bytes.sublist(1);
    switch (bytes[0]) {
      case _blockKind:
        final block = Block.tryDeserialize(body);
        return block == null ? null : BlockPayload(block);
      case _chatKind:
        if (body.length < _idLength) return null;
        final block = Block.tryDeserialize(body.sublist(_idLength));
        if (block == null) return null;
        return ChatPayload(
          messageId: latin1.decode(body.sublist(0, _idLength)),
          block: block,
        );
      case _readReceiptKind:
        if (body.length != _idLength) return null;
        return ReadReceiptPayload(
          messageId: latin1.decode(body),
        );
      default:
        return null;
    }
  }
}

const int _blockKind = 0x01;
const int _chatKind = 0x02;
const int _readReceiptKind = 0x03;

/// A UUID as its 36 ASCII characters.
const int _idLength = 36;

/// A block that is not a chat message: a friendship block.
class BlockPayload extends AppPayload {
  final Block block;

  const BlockPayload(this.block);

  @override
  Uint8List encode() => _join(_blockKind, const [], block.serialize());
}

/// A chat block with the id the sender gave its message.
class ChatPayload extends AppPayload {
  final String messageId;
  final Block block;

  ChatPayload({required this.messageId, required this.block})
      : assert(messageId.length == _idLength);

  @override
  Uint8List encode() =>
      _join(_chatKind, ascii.encode(messageId), block.serialize());
}

/// The reader's acknowledgement that it has read the chat message named.
class ReadReceiptPayload extends AppPayload {
  final String messageId;

  ReadReceiptPayload({required this.messageId})
      : assert(messageId.length == _idLength);

  @override
  Uint8List encode() => _join(_readReceiptKind, ascii.encode(messageId), null);
}

Uint8List _join(int kind, List<int> id, Uint8List? block) {
  final out = BytesBuilder(copy: false)..addByte(kind);
  out.add(id);
  if (block != null) out.add(block);
  return out.toBytes();
}

/// Whether a receipt from [readerHex] may mark outgoing message [messageId]
/// read: only a receipt from the peer that message was sent to. A receipt
/// naming no message sent to its sender marks nothing read (F07).
bool receiptMarksRead(
  MessagesState messages,
  String messageId,
  String readerHex,
) =>
    messages.outgoingMessages[messageId]?.recipientPubkeyHex == readerHex;
