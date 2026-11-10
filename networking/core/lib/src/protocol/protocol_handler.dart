import 'dart:typed_data';
import 'layer_version.dart';
import '../models/identity.dart';
import 'package:sodium/sodium.dart';

/// Exact length of an ANNOUNCE payload: pubkey(32) + version declaration(9)
/// + sig(64) (GNA api.tex, "Versions").
const int announcePayloadLength = 32 + VersionDeclaration.length + 64;

/// Handles Grassroots protocol logic: packet encoding/decoding,
/// ANNOUNCE parsing, MESSAGE handling, etc.
///
/// Pure functions - no state, no I/O, fully testable.
/// Extracted from transport layer to achieve separation of concerns.
class ProtocolHandler {
  final GrassrootsIdentity identity;
  final Sodium _sodium;

  /// The layer's versions, which every ANNOUNCE declares (GNA api.tex,
  /// "Versions"; [VersionDeclaration]).  Until 2026-11-10 one protocol
  /// version, 9 at the last, was shared by both kinds and any other refused.
  final VersionDeclaration declaration;

  /// Length of the trailing Ed25519 signature on an ANNOUNCE payload.
  static const int announceSignatureLength = 64;

  /// Length of the messageId prefix on a MESSAGE payload (a UUID string,
  /// matching the id fragments carry in FRAGMENT_START).
  static const int messageIdLength = 36;

  ProtocolHandler({
    required this.identity,
    required Sodium sodium,
    this.declaration = VersionDeclaration.smartphone,
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
  /// Format: [pubkey(32) + version declaration(9) + signature(64)], the
  /// declaration the layer's kind, version, earliest compatible version and
  /// the versions of each kind it works with (GNA api.tex, "Versions").
  Uint8List createAnnouncePayload() {
    final buffer = BytesBuilder();

    // Pubkey (32 bytes)
    buffer.add(identity.publicKey);

    // The version declaration (9 bytes)
    buffer.add(declaration.encode());

    buffer.add(signBytes(buffer.toBytes()));
    return buffer.toBytes();
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
          'ANNOUNCE payload must be exactly pubkey(32) + version '
          'declaration(9) + signature(64) bytes');
    }
    var offset = 0;

    // Pubkey (32 bytes)
    final pubkey = data.sublist(offset, offset + 32);
    offset += 32;

    // The version declaration (9 bytes), checked once the signature is:
    // "each peer checks that the version the other works with lies between
    // its own earliest compatible version and its own version ... otherwise
    // the connection is refused" (GNA api.tex, "Versions").
    final declared = VersionDeclaration.decode(data, offset);
    offset += VersionDeclaration.length;

    final signature = data.sublist(offset, offset + announceSignatureLength);
    final signedBytes = data.sublist(0, offset);
    if (!verifyBytes(
      signature: signature,
      message: signedBytes,
      publicKey: Uint8List.fromList(pubkey),
    )) {
      throw const FormatException('ANNOUNCE signature verification failed');
    }
    final talksAs = declaration.accepts(declared);
    if (talksAs == null) {
      throw VersionDeclaration.refusal(declaration, declared);
    }

    return AnnounceData(
      publicKey: Uint8List.fromList(pubkey),
      declaration: declared,
      talksAs: talksAs,
    );
  }

  // ===== Signing & Verification =====

  /// Detached Ed25519 signature over arbitrary [message] bytes under the
  /// identity key. Used for the self-contained signed records that carry
  /// identity now that the wire frame does not: ANNOUNCE payloads and Noise
  /// handshake identity claims.
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
  /// The peer's version declaration (GNA api.tex, "Versions").
  final VersionDeclaration declaration;

  /// The version of this layer's kind it talks to the peer as: the one the
  /// peer works with.
  final int talksAs;

  /// The peer's version.
  int get protocolVersion => declaration.version;

  const AnnounceData({
    required this.publicKey,
    required this.declaration,
    required this.talksAs,
  });

  String get pubkeyHex =>
      publicKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  @override
  String toString() =>
      'AnnounceData(${pubkeyHex.substring(0, 8)}..., v$protocolVersion)';
}
