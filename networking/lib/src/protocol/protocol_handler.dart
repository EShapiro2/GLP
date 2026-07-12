import 'dart:convert';
import 'dart:typed_data';
import 'package:grassroots_networking/src/models/identity.dart';
import 'package:grassroots_networking/src/models/packet.dart';
import 'package:sodium_libs/sodium_libs.dart';

/// Handles Grassroots protocol logic: packet encoding/decoding,
/// ANNOUNCE parsing, MESSAGE handling, etc.
///
/// Pure functions - no state, no I/O, fully testable.
/// Extracted from transport layer to achieve separation of concerns.
class ProtocolHandler {
  final GrassrootsIdentity identity;
  final Sodium _sodium;
  static const int protocolVersion = 2;

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
  /// record itself carries.
  ///
  /// Format:
  /// [pubkey(32) + version(2) + nickLen(1) + nick
  ///  + candidateCount(2) + repeated(candidateLen(2) + candidate)
  ///  + signature(64)]
  Uint8List createAnnouncePayload({
    String? address,
    String? linkLocalAddress,
    Iterable<String> addressCandidates = const [],
  }) {
    final nicknameBytes = _encodeNickname(identity.nickname);
    final candidates = <String>{
      if (address != null && address.isNotEmpty) address,
      if (linkLocalAddress != null && linkLocalAddress.isNotEmpty)
        linkLocalAddress,
      ...addressCandidates.where((candidate) => candidate.isNotEmpty),
    };
    final buffer = BytesBuilder();

    // Pubkey (32 bytes)
    buffer.add(identity.publicKey);

    // Protocol version (2 bytes)
    final versionBytes = ByteData(2);
    versionBytes.setUint16(0, protocolVersion, Endian.big);
    buffer.add(versionBytes.buffer.asUint8List());

    // Nickname length (1 byte) + nickname
    buffer.addByte(nicknameBytes.length);
    buffer.add(nicknameBytes);

    // Candidate address set.
    final candidateCountBytes = ByteData(2);
    candidateCountBytes.setUint16(0, candidates.length, Endian.big);
    buffer.add(candidateCountBytes.buffer.asUint8List());
    for (final candidate in candidates) {
      final candidateBytes = Uint8List.fromList(candidate.codeUnits);
      final candidateLenBytes = ByteData(2);
      candidateLenBytes.setUint16(0, candidateBytes.length, Endian.big);
      buffer.add(candidateLenBytes.buffer.asUint8List());
      buffer.add(candidateBytes);
    }

    buffer.add(signBytes(buffer.toBytes()));
    return buffer.toBytes();
  }

  /// Create MESSAGE packet.
  ///
  /// The payload opens with the 36-character [messageId] — the delivery
  /// identity, stable across retries and media — followed by the message
  /// body, mirroring how fragment payloads carry it. The recipient
  /// deduplicates by it and echoes it back in its ACK; pass the same id you
  /// stored in `MessageSendingAction` so `MessageDeliveredAction`
  /// (dispatched on ACK receipt) can match the outgoing message in the
  /// Redux store and flip ✓ → ✓✓.
  GrassrootsPacket createMessagePacket({
    required Uint8List payload,
    required String messageId,
  }) {
    if (messageId.length != messageIdLength) {
      throw ArgumentError(
          'messageId must be a $messageIdLength-character UUID: $messageId');
    }
    return GrassrootsPacket(
      type: PacketType.message,
      payload: Uint8List.fromList([...utf8.encode(messageId), ...payload]),
    );
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
    var offset = 0;

    // Pubkey (32 bytes)
    final pubkey = data.sublist(offset, offset + 32);
    offset += 32;

    // Version (2 bytes)
    final version = ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
        .getUint16(0, Endian.big);
    offset += 2;

    // Nickname length (1 byte) + nickname
    final nicknameLength = data[offset];
    offset += 1;
    final nickname = utf8.decode(
      data.sublist(offset, offset + nicknameLength),
      allowMalformed: true,
    );
    offset += nicknameLength;

    if (offset + 2 > data.length) {
      throw const FormatException('ANNOUNCE payload missing candidates');
    }

    final addressCandidates = <String>{};
    final candidateCount =
        ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
            .getUint16(0, Endian.big);
    offset += 2;
    for (var i = 0; i < candidateCount; i++) {
      if (offset + 2 > data.length) {
        throw const FormatException('ANNOUNCE candidate length missing');
      }
      final candidateLength =
          ByteData.view(data.buffer, data.offsetInBytes + offset, 2)
              .getUint16(0, Endian.big);
      offset += 2;
      if (offset + candidateLength > data.length) {
        throw const FormatException('ANNOUNCE candidate truncated');
      }
      if (candidateLength > 0) {
        addressCandidates.add(
          String.fromCharCodes(
            data.sublist(offset, offset + candidateLength),
          ),
        );
      }
      offset += candidateLength;
    }

    if (data.length != offset + announceSignatureLength) {
      throw const FormatException('ANNOUNCE signature missing or malformed');
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

    final address = _firstNonLinkLocalCandidate(addressCandidates);
    final linkLocalAddress = _firstLinkLocalCandidate(addressCandidates);

    return AnnounceData(
      publicKey: Uint8List.fromList(pubkey),
      nickname: nickname,
      protocolVersion: version,
      udpAddress: address,
      linkLocalAddress: linkLocalAddress,
      addressCandidates: addressCandidates,
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

  /// Encode a nickname as UTF-8 for the ANNOUNCE payload, truncated to fit the
  /// 1-byte length prefix (max 255 bytes) on a code-point boundary so
  /// multi-byte characters (emoji, non-ASCII names) survive the round-trip.
  /// The matching decode is `utf8.decode(...)` in [decodeAnnounce].
  static Uint8List _encodeNickname(String nickname) {
    final encoded = utf8.encode(nickname);
    if (encoded.length <= 255) return Uint8List.fromList(encoded);
    final truncated = <int>[];
    for (final rune in nickname.runes) {
      final runeBytes = utf8.encode(String.fromCharCode(rune));
      if (truncated.length + runeBytes.length > 255) break;
      truncated.addAll(runeBytes);
    }
    return Uint8List.fromList(truncated);
  }

  String? _firstNonLinkLocalCandidate(Iterable<String> candidates) {
    for (final candidate in candidates) {
      if (!_isLinkLocalCandidate(candidate)) return candidate;
    }
    return null;
  }

  String? _firstLinkLocalCandidate(Iterable<String> candidates) {
    for (final candidate in candidates) {
      if (_isLinkLocalCandidate(candidate)) return candidate;
    }
    return null;
  }

  bool _isLinkLocalCandidate(String candidate) {
    final lower = candidate.toLowerCase();
    if (lower.startsWith('[')) {
      final end = lower.indexOf(']');
      final host = end == -1 ? lower.substring(1) : lower.substring(1, end);
      return host.startsWith('fe80:');
    }
    final colon = lower.lastIndexOf(':');
    final host = colon == -1 ? lower : lower.substring(0, colon);
    return host.startsWith('169.254.');
  }
}

/// Decoded ANNOUNCE data
class AnnounceData {
  final Uint8List publicKey;
  final String nickname;
  final int protocolVersion;
  final String? udpAddress;
  final String? linkLocalAddress;
  final Set<String> addressCandidates;

  const AnnounceData({
    required this.publicKey,
    required this.nickname,
    required this.protocolVersion,
    this.udpAddress,
    this.linkLocalAddress,
    this.addressCandidates = const {},
  });

  @override
  String toString() => 'AnnounceData($nickname, v$protocolVersion'
      '${udpAddress != null ? ", addr: $udpAddress" : ""}'
      '${linkLocalAddress != null ? ", ll: $linkLocalAddress" : ""}'
      '${addressCandidates.isNotEmpty ? ", candidates: $addressCandidates" : ""})';
}
