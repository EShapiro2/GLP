/// Chat message payload for the Grassroots BLE protocol.
library;

import 'dart:convert';
import 'dart:typed_data';

/// Payload for chat messages (type 0x01).
///
/// Format:
/// - Message ID: 16 bytes (UUID)
/// - Content Length: 2 bytes (big-endian)
/// - Content: variable (UTF-8 encoded text)
class ChatPayload {
  /// Unique message ID (16 bytes).
  final Uint8List messageId;

  /// Message content (UTF-8 text).
  final String content;

  /// Creates a chat payload.
  ChatPayload({
    required this.messageId,
    required this.content,
  }) {
    if (messageId.length != 16) {
      throw ArgumentError('messageId must be 16 bytes');
    }
  }

  /// Encodes this payload to bytes.
  Uint8List encode() {
    final contentBytes = utf8.encode(content);
    final length = contentBytes.length;

    if (length > 65535) {
      throw ArgumentError('Content too long: $length bytes (max 65535)');
    }

    final buffer = BytesBuilder();
    buffer.add(messageId);
    buffer.addByte((length >> 8) & 0xFF);
    buffer.addByte(length & 0xFF);
    buffer.add(contentBytes);

    return buffer.toBytes();
  }

  /// Decodes a chat payload from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static ChatPayload decode(Uint8List data) {
    if (data.length < 18) {
      throw FormatException(
        'Chat payload too short: ${data.length} bytes (min 18)',
      );
    }

    final messageId = Uint8List.fromList(data.sublist(0, 16));
    final contentLength = (data[16] << 8) | data[17];

    if (data.length < 18 + contentLength) {
      throw FormatException(
        'Chat payload truncated: expected ${18 + contentLength} bytes, got ${data.length}',
      );
    }

    final contentBytes = data.sublist(18, 18 + contentLength);
    final content = utf8.decode(contentBytes);

    return ChatPayload(
      messageId: messageId,
      content: content,
    );
  }

  @override
  String toString() {
    final idHex = messageId.sublist(0, 4)
        .map((b) => b.toRadixString(16).padLeft(2, '0'))
        .join();
    return 'ChatPayload($idHex..., "${content.length > 20 ? '${content.substring(0, 20)}...' : content}")';
  }
}
