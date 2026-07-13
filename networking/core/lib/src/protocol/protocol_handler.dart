import 'dart:convert';
import 'dart:typed_data';
import '../models/identity.dart';
import '../models/packet.dart';
import 'package:sodium/sodium.dart';

/// Exact length of an ANNOUNCE payload: pubkey(32) + version(2) + sig(64).
const int announcePayloadLength = 98;

/// Handles Grassroots protocol logic: packet encoding/decoding,
/// ANNOUNCE parsing, MESSAGE handling, etc.
///
/// Pure functions - no state, no I/O, fully testable.
/// Extracted from transport layer to achieve separation of concerns.
class ProtocolHandler {
  final GrassrootsIdentity identity;
  final Sodium _sodium;
  /// Version 3: the shared message transport (spec §Message Transport) —
  /// UDX streams removed, session-encrypted fragments ride UDP datagrams
  /// with per-fragment ACK; self-contained fragment format; self-ordering
  /// handshake. Mixed versions are refused at ANNOUNCE.
  static const int protocolVersion = 3;

  /// Length of the trailing Ed25519 signature on an ANNOUNCE payload.
  static const int announceSignatureLength = 64;

  /// Length of the messageId prefix on a MESSAGE payload (a UUID string,
  /// matching the id fragments carry in FRAGMENT_START).
  static const int messageIdLength = 36;

  ProtocolHandler({
    required this.identity,
    required Sodium sodium,
  }) : _sodium = sodium;

  // ===== Encoding =====

  /// Create a self-signed ANNOUNCE payload.
  ///
  /// The wire frame carries no identity or signature, so ANNOUNCE is a
  /// self-contained signed identity record: the trailing Ed25519 signature
  /// covers every preceding byte and is verified against the pubkey the
  /// record itself carries. The payload is the irreducible record of spec
  /// §ANNOUNCE and Liveness — identity beacon and heartbeat only; address
  /// distribution is not ANNOUNCE's role (addresses reach the layer via
  /// `putPeerAddress`, fed by GLP).
  ///
  /// Format: [pubkey(32) + version(2) + signature(64)]
  Uint8List createAnnouncePayload() {
    final buffer = BytesBuilder();

    // Pubkey (32 bytes)
    buffer.add(identity.publicKey);

    // Protocol version (2 bytes)
    final versionBytes = ByteData(2);
    versionBytes.setUint16(0, protocolVersion, Endian.big);
    buffer.add(versionBytes.buffer.asUint8List());

    buffer.add(signBytes(buffer.toBytes()));
    return buffer.toBytes();
  }

  /// Create READ_RECEIPT packet
  GrassrootsPacket createReadReceiptPacket({required String messageId}) {
    return GrassrootsPacket(
      type: PacketType.readReceipt,
      payload: utf8.encode(messageId),
    );
  }

  // ===== Decoding =====

  /// Decode and verify a self-signed ANNOUNCE payload.
  ///
  /// Throws [FormatException] on malformed input or a bad signature — an
  /// ANNOUNCE whose trailing signature does not verify against the pubkey it
  /// carries is forged or corrupted and must not identify anyone.
  AnnounceData decodeAnnounce(Uint8List data) {
    if (data.length != announcePayloadLength) {
      throw const FormatException(
          'ANNOUNCE payload must be exactly pubkey(32) + version(2) + '
          'signature(64) bytes');
    }
    var offset = 0;

    // Pubkey (32 bytes)
    final pubkey = data.sublist(offset, offset + 32);
    offset += 32;

    // Version (2 bytes)
    final version = ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
        .getUint16(0, Endian.big);
    offset += 2;

    // Refuse mixed protocol versions: the wire protocol changed
    // incompatibly (spec §Message Transport); a peer speaking another
    // version cannot interoperate and its contact is refused here.
    if (version != protocolVersion) {
      throw FormatException(
          'ANNOUNCE protocol version $version != $protocolVersion; refused');
    }

    final signature = data.sublist(offset, offset + announceSignatureLength);
    final signedBytes = data.sublist(0, offset);
    if (!verifyBytes(
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

  /// Decode READ_RECEIPT payload
  String decodeReadReceipt(Uint8List payload) {
    return utf8.decode(payload);
  }

  /// Create ACK packet (for delivery confirmation)
  GrassrootsPacket createAckPacket({required String messageId}) {
    return GrassrootsPacket(
      type: PacketType.ack,
      payload: utf8.encode(messageId),
    );
  }

  // ===== Signing & Verification =====

  /// Detached Ed25519 signature over arbitrary [message] bytes under the
  /// identity key. Used for the self-contained signed records that carry
  /// identity now that the wire frame does not: ANNOUNCE payloads, Noise
  /// handshake identity claims, and cold-call invites (spec §IP Cold-Call).
  /// Native libsodium (~5-10ms on Android, ~1-3ms on iOS).
  Uint8List signBytes(Uint8List message) {
    // The identity's `privateKey` is the standard 64-byte Ed25519 secret
    // key (32-byte seed concatenated with the 32-byte public key) — exactly
    // what libsodium's `crypto_sign_detached` expects.
    final secretKey = SecureKey.fromList(_sodium, identity.privateKey);
    try {
      return _sodium.crypto.sign.detached(
        message: message,
        secretKey: secretKey,
      );
    } finally {
      secretKey.dispose();
    }
  }

  /// Verify a detached Ed25519 [signature] over [message] against
  /// [publicKey]. Returns false on any error.
  bool verifyBytes({
    required Uint8List signature,
    required Uint8List message,
    required Uint8List publicKey,
  }) {
    try {
      return _sodium.crypto.sign.verifyDetached(
        signature: signature,
        message: message,
        publicKey: publicKey,
      );
    } catch (_) {
      return false;
    }
  }

}

/// Decoded ANNOUNCE data
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
