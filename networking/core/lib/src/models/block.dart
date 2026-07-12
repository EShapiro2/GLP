import 'dart:convert';
import 'package:flutter/foundation.dart';

/// Block types for the Grassroots protocol
enum BlockType {
  /// Regular text message
  say(0x01),

  /// Friend request (no transport info)
  friendshipOffer(0x02),

  /// Accept friend request (no transport info)
  friendshipAccept(0x03),

  /// Revoke friendship (unfriend)
  friendshipRevoke(0x05);

  final int value;
  const BlockType(this.value);

  static BlockType fromValue(int value) {
    return BlockType.values.firstWhere(
      (t) => t.value == value,
      orElse: () => throw ArgumentError('Unknown block type: $value'),
    );
  }

  /// Check if value is a valid block type
  static bool isValidType(int value) {
    return BlockType.values.any((t) => t.value == value);
  }
}

/// A Block represents a message unit in the Grassroots protocol.
///
/// Block types:
/// - Say: Regular text message
/// - FriendshipOffer: Send friend request (transport-agnostic)
/// - FriendshipAccept: Accept friend request (transport-agnostic)
/// - FriendshipRevoke: Unfriend notification
///
/// Presence and address distribution are handled at the transport layer
/// via unified ANNOUNCE messages and signaling (ADDR_REGISTER/QUERY/RESPONSE).
abstract class Block {
  /// The type of this block
  BlockType get type;

  /// Serialize the block to bytes for transmission
  Uint8List serialize();

  /// Deserialize a block from bytes
  static Block deserialize(Uint8List data) {
    if (data.isEmpty) {
      throw FormatException('Block data is empty');
    }

    final blockType = BlockType.fromValue(data[0]);
    final payload = data.sublist(1);

    switch (blockType) {
      case BlockType.say:
        return SayBlock.fromPayload(payload);
      case BlockType.friendshipOffer:
        return FriendshipOfferBlock.fromPayload(payload);
      case BlockType.friendshipAccept:
        return FriendshipAcceptBlock.fromPayload(payload);
      case BlockType.friendshipRevoke:
        return FriendshipRevokeBlock.fromPayload(payload);
    }
  }

  /// Try to deserialize, returns null if data is not a valid block
  /// (e.g., legacy plain text message)
  static Block? tryDeserialize(Uint8List data) {
    try {
      // Check if first byte is a valid block type
      if (data.isEmpty) {
        debugPrint('Block data is empty');
        return null;
      }
      final typeValue = data[0];
      if (!BlockType.isValidType(typeValue)) {
        // Not a block - treat as legacy plain text
        debugPrint('Data is not a valid block type: $typeValue');
        return null;
      }
      return deserialize(data);
    } catch (e) {
      debugPrint('Failed to deserialize block: $e');
      return null;
    }
  }
}

/// User-authored content sent in a chat. The umbrella type covers any kind
/// of "thing the user said" — text, picture, future audio/video — and the
/// concrete subclass is determined by a one-byte subtype tag.
///
/// Wire format: [0x01] + [subtype: 1 byte] + <subtype-specific payload>
abstract class SayBlock extends Block {
  /// Generative default constructor so concrete subtypes (TextSayBlock,
  /// PictureSayBlock) can extend this class. The factory `fromPayload`
  /// handles inbound parsing.
  SayBlock();

  @override
  BlockType get type => BlockType.say;

  /// One-byte tag distinguishing the kind of "say" content.
  SaySubtype get subtype;

  /// Subtype-specific bytes (excluding the [type] and [subtype] header bytes).
  Uint8List serializeSubtypePayload();

  @override
  Uint8List serialize() {
    final body = serializeSubtypePayload();
    final data = Uint8List(2 + body.length);
    data[0] = type.value;
    data[1] = subtype.value;
    data.setRange(2, data.length, body);
    return data;
  }

  /// Parse the SayBlock body (everything after the [type] byte) and dispatch
  /// to the right concrete subtype.
  factory SayBlock.fromPayload(Uint8List payload) {
    if (payload.isEmpty) {
      throw FormatException('SayBlock payload missing subtype byte');
    }
    final subtype = SaySubtype.fromValue(payload[0]);
    final body = payload.sublist(1);
    switch (subtype) {
      case SaySubtype.text:
        return TextSayBlock.fromBody(body);
      case SaySubtype.picture:
        return PictureSayBlock.fromBody(body);
    }
  }
}

/// SayBlock subtypes. New media kinds extend this without claiming a new
/// top-level [BlockType].
enum SaySubtype {
  text(0x00),
  picture(0x01);

  final int value;
  const SaySubtype(this.value);

  static SaySubtype fromValue(int value) {
    return SaySubtype.values.firstWhere(
      (s) => s.value == value,
      orElse: () => throw ArgumentError('Unknown say subtype: $value'),
    );
  }
}

/// Plain text message — the historical SayBlock content.
class TextSayBlock extends SayBlock {
  @override
  SaySubtype get subtype => SaySubtype.text;

  /// The text content of the message.
  final String content;

  TextSayBlock({required this.content});

  @override
  Uint8List serializeSubtypePayload() => utf8.encode(content);

  factory TextSayBlock.fromBody(Uint8List body) {
    return TextSayBlock(content: utf8.decode(body));
  }
}

/// Picture message. Wire body:
///   [viewOnce: 1 byte, 0/1] +
///   [mimeLen: 1 byte] +
///   [mime: utf8] +
///   [imageBytes: rest of payload]
///
/// `viewOnce == true` flags the recipient bubble to render a blurred preview
/// and delete the file on first view; the sender's local copy is deleted when
/// the message reaches MessageStatus.delivered.
class PictureSayBlock extends SayBlock {
  @override
  SaySubtype get subtype => SaySubtype.picture;

  final bool viewOnce;
  final String mime;
  final Uint8List imageBytes;

  PictureSayBlock({
    required this.viewOnce,
    required this.mime,
    required this.imageBytes,
  });

  @override
  Uint8List serializeSubtypePayload() {
    final mimeBytes = utf8.encode(mime);
    if (mimeBytes.length > 255) {
      throw ArgumentError('mime too long (max 255 bytes): $mime');
    }
    final out = Uint8List(2 + mimeBytes.length + imageBytes.length);
    out[0] = viewOnce ? 1 : 0;
    out[1] = mimeBytes.length;
    out.setRange(2, 2 + mimeBytes.length, mimeBytes);
    out.setRange(2 + mimeBytes.length, out.length, imageBytes);
    return out;
  }

  factory PictureSayBlock.fromBody(Uint8List body) {
    if (body.length < 2) {
      throw const FormatException('PictureSayBlock body too short');
    }
    final viewOnce = body[0] != 0;
    final mimeLen = body[1];
    if (body.length < 2 + mimeLen) {
      throw const FormatException('PictureSayBlock body truncated in mime');
    }
    final mime = utf8.decode(body.sublist(2, 2 + mimeLen));
    final image = Uint8List.fromList(body.sublist(2 + mimeLen));
    return PictureSayBlock(viewOnce: viewOnce, mime: mime, imageBytes: image);
  }
}

/// A friendship offer block (friend request)
class FriendshipOfferBlock extends Block {
  @override
  BlockType get type => BlockType.friendshipOffer;

  /// Optional message with the friend request
  final String? message;

  FriendshipOfferBlock({this.message});

  @override
  Uint8List serialize() {
    // Format: type (1) + message_len (2) + message
    final messageBytes = message != null ? utf8.encode(message!) : Uint8List(0);
    final data = ByteData(1 + 2 + messageBytes.length);
    var offset = 0;

    data.setUint8(offset++, type.value);
    data.setUint16(offset, messageBytes.length, Endian.big);
    offset += 2;

    final bytes = data.buffer.asUint8List();
    if (messageBytes.isNotEmpty) {
      bytes.setRange(offset, offset + messageBytes.length, messageBytes);
    }

    return bytes;
  }

  factory FriendshipOfferBlock.fromPayload(Uint8List payload) {
    final data = ByteData.view(payload.buffer, payload.offsetInBytes);
    var offset = 0;

    final messageLen = data.getUint16(offset, Endian.big);
    offset += 2;

    String? message;
    if (messageLen > 0) {
      message = utf8.decode(payload.sublist(offset, offset + messageLen));
    }

    return FriendshipOfferBlock(message: message);
  }
}

/// A friendship accept block
class FriendshipAcceptBlock extends Block {
  @override
  BlockType get type => BlockType.friendshipAccept;

  FriendshipAcceptBlock();

  @override
  Uint8List serialize() {
    // Just the type byte - no additional data
    return Uint8List.fromList([type.value]);
  }

  factory FriendshipAcceptBlock.fromPayload(Uint8List payload) {
    return FriendshipAcceptBlock();
  }
}

/// A friendship revoke block (unfriend notification)
///
/// This is sent when a user unfriends someone. The recipient should:
/// 1. Remove the sender from their friend list
/// 2. Delete any stored addresses for the sender
///
/// The message is intentionally minimal to not reveal the reason.
class FriendshipRevokeBlock extends Block {
  @override
  BlockType get type => BlockType.friendshipRevoke;

  FriendshipRevokeBlock();

  @override
  Uint8List serialize() {
    // Just the type byte - no additional data needed
    return Uint8List.fromList([type.value]);
  }

  factory FriendshipRevokeBlock.fromPayload(Uint8List payload) {
    // No payload to parse
    return FriendshipRevokeBlock();
  }
}
