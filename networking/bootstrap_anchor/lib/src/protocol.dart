import 'dart:typed_data';
import 'package:cryptography/cryptography.dart';

import 'identity.dart';
import 'packet.dart';

/// Protocol handler for the bootstrap anchor.
///
/// Handles the self-signed ANNOUNCE record and record-level Ed25519
/// signing/verification. Wire-compatible with the Flutter client's
/// ProtocolHandler.
class Protocol {
  final AnchorIdentity identity;
  static const int protocolVersion = 2;

  /// Length of the trailing Ed25519 signature on an ANNOUNCE payload.
  static const int announceSignatureLength = 64;

  /// Exact length of an ANNOUNCE payload: pubkey(32) + version(2) + sig(64).
  static const int announcePayloadLength = 98;

  const Protocol({required this.identity});

  // ===== Signing & Verification =====

  /// Detached Ed25519 signature over arbitrary [message] bytes under the
  /// anchor identity. Used for self-contained signed records (ANNOUNCE).
  Future<Uint8List> signBytes(Uint8List message) async {
    final signature =
        await Ed25519().sign(message, keyPair: identity.keyPair);
    return Uint8List.fromList(signature.bytes);
  }

  /// Verify a detached Ed25519 [signature] over [message] against
  /// [publicKey]. Returns false on any error.
  Future<bool> verifyBytes({
    required Uint8List signature,
    required Uint8List message,
    required Uint8List publicKey,
  }) async {
    try {
      return await Ed25519().verify(
        message,
        signature: Signature(
          signature,
          publicKey: SimplePublicKey(publicKey, type: KeyPairType.ed25519),
        ),
      );
    } catch (e) {
      return false;
    }
  }

  // ===== ANNOUNCE =====

  /// Create a self-signed ANNOUNCE payload. Must match the client's format —
  /// the irreducible record of spec §ANNOUNCE and Liveness:
  ///
  /// Format: pubkey(32) + version(2) + signature(64) over all preceding bytes
  Future<Uint8List> createAnnouncePayload() async {
    final buffer = BytesBuilder();

    // Pubkey (32 bytes)
    buffer.add(identity.publicKey);

    // Protocol version (2 bytes, big-endian)
    final versionBytes = ByteData(2);
    versionBytes.setUint16(0, protocolVersion, Endian.big);
    buffer.add(versionBytes.buffer.asUint8List());

    buffer.add(await signBytes(buffer.toBytes()));
    return buffer.toBytes();
  }

  /// Decode and verify a self-signed ANNOUNCE payload. Throws
  /// [FormatException] on malformed input or a bad signature.
  Future<AnnounceData> decodeAnnounce(Uint8List data) async {
    if (data.length != announcePayloadLength) {
      throw const FormatException(
          'ANNOUNCE payload must be exactly pubkey(32) + version(2) + '
          'signature(64) bytes');
    }
    var offset = 0;

    final pubkey = data.sublist(offset, offset + 32);
    offset += 32;

    final version = ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
        .getUint16(0, Endian.big);
    offset += 2;

    final signature = data.sublist(offset, offset + announceSignatureLength);
    final signedBytes = data.sublist(0, offset);
    if (!await verifyBytes(
      signature: signature,
      message: signedBytes,
      publicKey: Uint8List.fromList(pubkey),
    )) {
      throw const FormatException('ANNOUNCE signature verification failed');
    }

    return AnnounceData(
      publicKey: Uint8List.fromList(pubkey),
      protocolVersion: version,
    );
  }

  /// Create an ANNOUNCE packet (self-signed payload).
  Future<GrassrootsPacket> createAnnouncePacket() async {
    return GrassrootsPacket(
      type: PacketType.announce,
      payload: await createAnnouncePayload(),
    );
  }

  /// Create ACK packet.
  GrassrootsPacket createAckPacket({required String messageId}) {
    return GrassrootsPacket(
      type: PacketType.ack,
      payload: Uint8List.fromList(messageId.codeUnits),
    );
  }

  /// Create a signaling packet.
  GrassrootsPacket createSignalingPacket({
    required Uint8List signalingPayload,
  }) {
    return GrassrootsPacket(
      type: PacketType.signaling,
      payload: signalingPayload,
    );
  }
}

/// Decoded ANNOUNCE data.
class AnnounceData {
  final Uint8List publicKey;
  final int protocolVersion;

  const AnnounceData({
    required this.publicKey,
    required this.protocolVersion,
  });

  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  String toString() =>
      'AnnounceData(${pubkeyHex.substring(0, 8)}..., v$protocolVersion)';
}
