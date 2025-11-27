/// Friend request payload for the Grassroots BLE protocol.
library;

import 'dart:convert';
import 'dart:typed_data';

import '../../constants.dart';

/// Payload for friend requests (type 0x20).
///
/// Format:
/// - Requester PK: 32 bytes (Curve25519 public key)
/// - Requester Sign PK: 32 bytes (Ed25519 public key)
/// - Name Length: 1 byte
/// - Display Name: variable (UTF-8, max 63 bytes)
/// - Signature: 64 bytes (Ed25519 signature over above fields)
class FriendRequestPayload {
  /// Requester's Curve25519 public key.
  final Uint8List noisePublicKey;

  /// Requester's Ed25519 public key.
  final Uint8List signPublicKey;

  /// Requester's display name.
  final String displayName;

  /// Ed25519 signature over the above fields.
  final Uint8List signature;

  /// Creates a friend request payload.
  FriendRequestPayload({
    required this.noisePublicKey,
    required this.signPublicKey,
    required this.displayName,
    required this.signature,
  }) {
    if (noisePublicKey.length != curve25519KeySize) {
      throw ArgumentError('noisePublicKey must be $curve25519KeySize bytes');
    }
    if (signPublicKey.length != ed25519KeySize) {
      throw ArgumentError('signPublicKey must be $ed25519KeySize bytes');
    }
    final nameBytes = utf8.encode(displayName);
    if (nameBytes.length > maxDisplayName) {
      throw ArgumentError(
        'displayName too long: ${nameBytes.length} bytes (max $maxDisplayName)',
      );
    }
    if (signature.length != signatureSize) {
      throw ArgumentError('signature must be $signatureSize bytes');
    }
  }

  /// Creates a friend request payload without signature (for signing).
  factory FriendRequestPayload.unsigned({
    required Uint8List noisePublicKey,
    required Uint8List signPublicKey,
    required String displayName,
  }) {
    return FriendRequestPayload(
      noisePublicKey: noisePublicKey,
      signPublicKey: signPublicKey,
      displayName: displayName,
      signature: Uint8List(signatureSize), // Placeholder
    );
  }

  /// Returns the data to be signed (everything except signature).
  Uint8List get dataToSign {
    final nameBytes = utf8.encode(displayName);
    final buffer = BytesBuilder();
    buffer.add(noisePublicKey);
    buffer.add(signPublicKey);
    buffer.addByte(nameBytes.length);
    buffer.add(nameBytes);
    return buffer.toBytes();
  }

  /// Returns a copy with the given signature.
  FriendRequestPayload withSignature(Uint8List sig) {
    return FriendRequestPayload(
      noisePublicKey: noisePublicKey,
      signPublicKey: signPublicKey,
      displayName: displayName,
      signature: sig,
    );
  }

  /// Encodes this payload to bytes.
  Uint8List encode() {
    final nameBytes = utf8.encode(displayName);
    final buffer = BytesBuilder();
    buffer.add(noisePublicKey);
    buffer.add(signPublicKey);
    buffer.addByte(nameBytes.length);
    buffer.add(nameBytes);
    buffer.add(signature);
    return buffer.toBytes();
  }

  /// Decodes a friend request payload from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static FriendRequestPayload decode(Uint8List data) {
    // Minimum size: 32 + 32 + 1 + 0 + 64 = 129 bytes
    if (data.length < 129) {
      throw FormatException(
        'Friend request payload too short: ${data.length} bytes (min 129)',
      );
    }

    var offset = 0;

    final noisePublicKey = Uint8List.fromList(
      data.sublist(offset, offset + curve25519KeySize),
    );
    offset += curve25519KeySize;

    final signPublicKey = Uint8List.fromList(
      data.sublist(offset, offset + ed25519KeySize),
    );
    offset += ed25519KeySize;

    final nameLength = data[offset];
    offset += 1;

    if (nameLength > maxDisplayName) {
      throw FormatException(
        'Display name too long: $nameLength bytes (max $maxDisplayName)',
      );
    }

    if (data.length < offset + nameLength + signatureSize) {
      throw FormatException('Friend request payload truncated');
    }

    final nameBytes = data.sublist(offset, offset + nameLength);
    final displayName = utf8.decode(nameBytes);
    offset += nameLength;

    final signature = Uint8List.fromList(
      data.sublist(offset, offset + signatureSize),
    );

    return FriendRequestPayload(
      noisePublicKey: noisePublicKey,
      signPublicKey: signPublicKey,
      displayName: displayName,
      signature: signature,
    );
  }

  @override
  String toString() => 'FriendRequestPayload($displayName)';
}
