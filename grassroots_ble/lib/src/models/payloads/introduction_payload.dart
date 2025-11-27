/// Introduction payload for the Grassroots BLE protocol.
library;

import 'dart:convert';
import 'dart:typed_data';

import '../../constants.dart';

/// Payload for introductions (type 0x30).
///
/// Sent by a mutual friend R to introduce P to Q.
///
/// Format:
/// - Introducer PK: 32 bytes (R's Curve25519 public key)
/// - Introducee PK: 32 bytes (P's Curve25519 public key)
/// - Introducee Sign PK: 32 bytes (P's Ed25519 public key)
/// - Name Length: 1 byte
/// - Introducee Name: variable (UTF-8, max 63 bytes)
/// - Signature: 64 bytes (R's Ed25519 signature over above fields)
class IntroductionPayload {
  /// Introducer's (R's) Curve25519 public key.
  final Uint8List introducerPublicKey;

  /// Introducee's (P's) Curve25519 public key.
  final Uint8List introduceeNoiseKey;

  /// Introducee's (P's) Ed25519 public key.
  final Uint8List introduceeSignKey;

  /// Introducee's display name.
  final String introduceeName;

  /// R's Ed25519 signature over the above fields.
  final Uint8List signature;

  /// Creates an introduction payload.
  IntroductionPayload({
    required this.introducerPublicKey,
    required this.introduceeNoiseKey,
    required this.introduceeSignKey,
    required this.introduceeName,
    required this.signature,
  }) {
    if (introducerPublicKey.length != curve25519KeySize) {
      throw ArgumentError(
        'introducerPublicKey must be $curve25519KeySize bytes',
      );
    }
    if (introduceeNoiseKey.length != curve25519KeySize) {
      throw ArgumentError(
        'introduceeNoiseKey must be $curve25519KeySize bytes',
      );
    }
    if (introduceeSignKey.length != ed25519KeySize) {
      throw ArgumentError('introduceeSignKey must be $ed25519KeySize bytes');
    }
    final nameBytes = utf8.encode(introduceeName);
    if (nameBytes.length > maxDisplayName) {
      throw ArgumentError(
        'introduceeName too long: ${nameBytes.length} bytes (max $maxDisplayName)',
      );
    }
    if (signature.length != signatureSize) {
      throw ArgumentError('signature must be $signatureSize bytes');
    }
  }

  /// Creates an introduction payload without signature (for signing).
  factory IntroductionPayload.unsigned({
    required Uint8List introducerPublicKey,
    required Uint8List introduceeNoiseKey,
    required Uint8List introduceeSignKey,
    required String introduceeName,
  }) {
    return IntroductionPayload(
      introducerPublicKey: introducerPublicKey,
      introduceeNoiseKey: introduceeNoiseKey,
      introduceeSignKey: introduceeSignKey,
      introduceeName: introduceeName,
      signature: Uint8List(signatureSize), // Placeholder
    );
  }

  /// Returns the data to be signed (everything except signature).
  Uint8List get dataToSign {
    final nameBytes = utf8.encode(introduceeName);
    final buffer = BytesBuilder();
    buffer.add(introducerPublicKey);
    buffer.add(introduceeNoiseKey);
    buffer.add(introduceeSignKey);
    buffer.addByte(nameBytes.length);
    buffer.add(nameBytes);
    return buffer.toBytes();
  }

  /// Returns a copy with the given signature.
  IntroductionPayload withSignature(Uint8List sig) {
    return IntroductionPayload(
      introducerPublicKey: introducerPublicKey,
      introduceeNoiseKey: introduceeNoiseKey,
      introduceeSignKey: introduceeSignKey,
      introduceeName: introduceeName,
      signature: sig,
    );
  }

  /// Encodes this payload to bytes.
  Uint8List encode() {
    final nameBytes = utf8.encode(introduceeName);
    final buffer = BytesBuilder();
    buffer.add(introducerPublicKey);
    buffer.add(introduceeNoiseKey);
    buffer.add(introduceeSignKey);
    buffer.addByte(nameBytes.length);
    buffer.add(nameBytes);
    buffer.add(signature);
    return buffer.toBytes();
  }

  /// Decodes an introduction payload from bytes.
  ///
  /// Throws [FormatException] if the data is invalid.
  static IntroductionPayload decode(Uint8List data) {
    // Minimum size: 32 + 32 + 32 + 1 + 0 + 64 = 161 bytes
    if (data.length < 161) {
      throw FormatException(
        'Introduction payload too short: ${data.length} bytes (min 161)',
      );
    }

    var offset = 0;

    final introducerPublicKey = Uint8List.fromList(
      data.sublist(offset, offset + curve25519KeySize),
    );
    offset += curve25519KeySize;

    final introduceeNoiseKey = Uint8List.fromList(
      data.sublist(offset, offset + curve25519KeySize),
    );
    offset += curve25519KeySize;

    final introduceeSignKey = Uint8List.fromList(
      data.sublist(offset, offset + ed25519KeySize),
    );
    offset += ed25519KeySize;

    final nameLength = data[offset];
    offset += 1;

    if (nameLength > maxDisplayName) {
      throw FormatException(
        'Introducee name too long: $nameLength bytes (max $maxDisplayName)',
      );
    }

    if (data.length < offset + nameLength + signatureSize) {
      throw FormatException('Introduction payload truncated');
    }

    final nameBytes = data.sublist(offset, offset + nameLength);
    final introduceeName = utf8.decode(nameBytes);
    offset += nameLength;

    final signature = Uint8List.fromList(
      data.sublist(offset, offset + signatureSize),
    );

    return IntroductionPayload(
      introducerPublicKey: introducerPublicKey,
      introduceeNoiseKey: introduceeNoiseKey,
      introduceeSignKey: introduceeSignKey,
      introduceeName: introduceeName,
      signature: signature,
    );
  }

  @override
  String toString() => 'IntroductionPayload(introducing: $introduceeName)';
}
