import 'dart:convert' show base64Url, utf8;
import 'dart:typed_data';

import 'package:cryptography/cryptography.dart';
import 'package:cryptography/dart.dart' show DartSha256;
import 'package:sodium_libs/sodium_libs_sumo.dart';

/// IP cold-call deep-link invites — spec `GLP_Networking_API` §IP Cold-Call.
///
/// An invite is a signed bearer invitation from inviting agent A: at creation
/// A need not know the holder's public key, and possession of a valid unused
/// link authorizes exactly one first-contact attempt, redeemed through the
/// rendezvous server named in the link. The raw nonce never travels over the
/// IP network: redemption uses the derived [deriveInviteId] and MACs under
/// [deriveInviteKey], so passive observers and off-path attackers cannot
/// learn or guess the nonce.

/// URI scheme + host for invite links: `grassroots://invite/<base64url>`.
const String peerLinkScheme = 'grassroots';
const String peerLinkHost = 'invite';

/// Domain-separation label for the invite identifier:
/// InviteId = SHA-256("glp invite id" | Nonce256).
const String inviteIdLabel = 'glp invite id';

/// Domain-separation label for the invite proof key:
/// InviteKey = HKDF(Nonce256, "glp invite proof" | A_pk | RvServer | ExpiresAt).
const String inviteProofLabel = 'glp invite proof';

/// Invite lifetime: ExpiresAt is one hour after creation (spec).
const Duration inviteLifetime = Duration(hours: 1);

/// Wire version of the canonical invite record encoding.
const int peerLinkVersion = 1;

/// The signed invite record carried (base64url) in a peer link URI.
///
/// Canonical byte layout (all fields big-endian):
///   version(1) | A_pk(32) | rvPubkey(32) | rvAddrLen(2) | rvAddr(n)
///   | nonce(32) | expiresAtMs(8) | signature(64)
/// [signature] is Ed25519 by A over every preceding byte.
class PeerLinkInvite {
  /// Inviter A's Ed25519 public key (32 bytes).
  final Uint8List inviterPubkey;

  /// The named rendezvous server's Ed25519 public key (32 bytes).
  final Uint8List rvPubkey;

  /// The named rendezvous server's public `ip:port`.
  final String rvAddress;

  /// 256 uniformly random bits. Never sent over the IP network — it travels
  /// only inside the out-of-band link itself.
  final Uint8List nonce;

  /// Expiry, milliseconds since epoch (UTC).
  final int expiresAtMs;

  /// Ed25519 signature by the inviter over the preceding canonical bytes.
  final Uint8List signature;

  PeerLinkInvite({
    required this.inviterPubkey,
    required this.rvPubkey,
    required this.rvAddress,
    required this.nonce,
    required this.expiresAtMs,
    required this.signature,
  }) {
    if (inviterPubkey.length != 32) {
      throw ArgumentError('inviterPubkey must be 32 bytes');
    }
    if (rvPubkey.length != 32) throw ArgumentError('rvPubkey must be 32 bytes');
    if (nonce.length != 32) throw ArgumentError('nonce must be 32 bytes');
    if (signature.length != 64) {
      throw ArgumentError('signature must be 64 bytes');
    }
  }

  bool get isExpired => DateTime.now().millisecondsSinceEpoch > expiresAtMs;

  /// The canonical signable prefix: every field except the signature.
  Uint8List signableBytes() => _encodeBody(
        inviterPubkey: inviterPubkey,
        rvPubkey: rvPubkey,
        rvAddress: rvAddress,
        nonce: nonce,
        expiresAtMs: expiresAtMs,
      );

  /// Encode the full record (signable prefix + signature).
  Uint8List encode() {
    final body = signableBytes();
    final out = Uint8List(body.length + 64);
    out.setAll(0, body);
    out.setAll(body.length, signature);
    return out;
  }

  /// Render as the out-of-band URI: `grassroots://invite/<base64url>`.
  String toUri() {
    final encoded = base64Url.encode(encode()).replaceAll('=', '');
    return '$peerLinkScheme://$peerLinkHost/$encoded';
  }

  /// Parse a peer link URI back into an invite record. Throws
  /// [FormatException] on anything malformed. Signature and expiry are NOT
  /// checked here — callers must verify both before acting on the record.
  static PeerLinkInvite fromUri(String uri) {
    final parsed = Uri.tryParse(uri.trim());
    if (parsed == null ||
        parsed.scheme != peerLinkScheme ||
        parsed.host != peerLinkHost ||
        parsed.pathSegments.length != 1) {
      throw const FormatException('Not a grassroots://invite/... link');
    }
    var b64 = parsed.pathSegments.single;
    // Restore stripped base64url padding.
    final rem = b64.length % 4;
    if (rem != 0) b64 = b64.padRight(b64.length + (4 - rem), '=');
    final Uint8List bytes;
    try {
      bytes = base64Url.decode(b64);
    } on FormatException {
      throw const FormatException('Invite payload is not valid base64url');
    }
    return decode(bytes);
  }

  /// Decode the canonical record bytes. Throws [FormatException] when
  /// malformed. Per the no-compatibility rule, an unknown version is
  /// malformed — there is no tolerant path.
  static PeerLinkInvite decode(Uint8List bytes) {
    var o = 0;
    int need(int n) {
      if (bytes.length - o < n) {
        throw const FormatException('Invite record truncated');
      }
      final at = o;
      o += n;
      return at;
    }

    final version = bytes[need(1)];
    if (version != peerLinkVersion) {
      throw FormatException('Unknown invite version: $version');
    }
    final inviter = Uint8List.fromList(bytes.sublist(need(32), o));
    final rvKey = Uint8List.fromList(bytes.sublist(need(32), o));
    final addrLenAt = need(2);
    final addrLen = (bytes[addrLenAt] << 8) | bytes[addrLenAt + 1];
    final addr = String.fromCharCodes(bytes.sublist(need(addrLen), o));
    final nonce = Uint8List.fromList(bytes.sublist(need(32), o));
    final expAt = need(8);
    var expiresAtMs = 0;
    for (var i = 0; i < 8; i++) {
      expiresAtMs = (expiresAtMs << 8) | bytes[expAt + i];
    }
    final signature = Uint8List.fromList(bytes.sublist(need(64), o));
    if (o != bytes.length) {
      throw const FormatException('Invite record has trailing bytes');
    }
    return PeerLinkInvite(
      inviterPubkey: inviter,
      rvPubkey: rvKey,
      rvAddress: addr,
      nonce: nonce,
      expiresAtMs: expiresAtMs,
      signature: signature,
    );
  }

  static Uint8List _encodeBody({
    required Uint8List inviterPubkey,
    required Uint8List rvPubkey,
    required String rvAddress,
    required Uint8List nonce,
    required int expiresAtMs,
  }) {
    final addrBytes = rvAddress.codeUnits;
    if (addrBytes.length > 0xffff) {
      throw ArgumentError('rvAddress too long');
    }
    final out = BytesBuilder(copy: false);
    out.addByte(peerLinkVersion);
    out.add(inviterPubkey);
    out.add(rvPubkey);
    out.addByte((addrBytes.length >> 8) & 0xff);
    out.addByte(addrBytes.length & 0xff);
    out.add(addrBytes);
    out.add(nonce);
    out.add(_uint64be(expiresAtMs));
    return out.toBytes();
  }

  /// Create and sign a fresh invite for [identityPubkey], naming the given
  /// rendezvous server. [sign] produces a detached Ed25519 signature over the
  /// canonical signable bytes (injected so this module stays free of key
  /// material handling).
  static PeerLinkInvite create({
    required Uint8List identityPubkey,
    required Uint8List rvPubkey,
    required String rvAddress,
    required Uint8List nonce,
    required int expiresAtMs,
    required Uint8List Function(Uint8List signable) sign,
  }) {
    final body = _encodeBody(
      inviterPubkey: identityPubkey,
      rvPubkey: rvPubkey,
      rvAddress: rvAddress,
      nonce: nonce,
      expiresAtMs: expiresAtMs,
    );
    return PeerLinkInvite(
      inviterPubkey: identityPubkey,
      rvPubkey: rvPubkey,
      rvAddress: rvAddress,
      nonce: nonce,
      expiresAtMs: expiresAtMs,
      signature: sign(body),
    );
  }

  /// Verify the inviter's Ed25519 signature over the canonical bytes.
  bool verifySignature(SodiumSumo sodium) {
    try {
      return sodium.crypto.sign.verifyDetached(
        signature: signature,
        message: signableBytes(),
        publicKey: inviterPubkey,
      );
    } catch (_) {
      return false;
    }
  }
}

/// InviteId = SHA-256("glp invite id" | Nonce256) — the opaque identifier
/// redemption traffic uses instead of the raw nonce.
Uint8List deriveInviteId(Uint8List nonce) {
  final input = <int>[...utf8.encode(inviteIdLabel), ...nonce];
  return Uint8List.fromList(const DartSha256().hashSync(input).bytes);
}

/// InviteKey = HKDF-SHA256(Nonce256,
///   info = "glp invite proof" | A_pk | RvServer(pubkey, address) | ExpiresAt).
/// The proof key under which redemption MACs are computed.
Future<Uint8List> deriveInviteKey({
  required Uint8List nonce,
  required Uint8List inviterPubkey,
  required Uint8List rvPubkey,
  required String rvAddress,
  required int expiresAtMs,
}) async {
  final info = <int>[
    ...utf8.encode(inviteProofLabel),
    ...inviterPubkey,
    ...rvPubkey,
    ...rvAddress.codeUnits,
    ..._uint64be(expiresAtMs),
  ];
  final hkdf = Hkdf(hmac: Hmac.sha256(), outputLength: 32);
  final key = await hkdf.deriveKey(
    secretKey: SecretKey(nonce),
    nonce: const <int>[],
    info: info,
  );
  return Uint8List.fromList(await key.extractBytes());
}

/// The redemption claim MAC: HMAC-SHA256 under InviteKey over A's and B's
/// public keys, the InviteId, and B's fresh nonce (spec §Redemption).
Future<Uint8List> computeRedemptionMac({
  required Uint8List inviteKey,
  required Uint8List inviterPubkey,
  required Uint8List redeemerPubkey,
  required Uint8List inviteId,
  required Uint8List redeemerNonce,
}) async {
  final mac = await Hmac.sha256().calculateMac(
    <int>[
      ...inviterPubkey,
      ...redeemerPubkey,
      ...inviteId,
      ...redeemerNonce,
    ],
    secretKey: SecretKey(inviteKey),
  );
  return Uint8List.fromList(mac.bytes);
}

Uint8List _uint64be(int value) {
  final out = Uint8List(8);
  for (var i = 0; i < 8; i++) {
    out[i] = (value >> (8 * (7 - i))) & 0xff;
  }
  return out;
}
