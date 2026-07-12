import 'dart:typed_data';

/// Tracks peers that have presented valid friendship proofs.
///
/// The rendezvous agent has no friends list of its own and no "owner".
/// It serves any pair of agents that can cryptographically prove they
/// are friends — i.e. each side presents a friendship attestation signed
/// by the other.
///
/// Authorization flow:
/// 1. Agent A connects and sends a reconnect request for friend B,
///    including A's friendship proof (signed by B).
/// 2. The server verifies the proof via Ed25519 signature check.
/// 3. On success, A is registered as an authorized peer and its address
///    is recorded.
///
/// The peer table is purely volatile — it is populated at runtime from
/// verified proofs and cleared on restart.
class PeerTable {
  /// Peers that have connected and been verified via friendship proof.
  /// Keyed by pubkey hex.
  final Map<String, PeerEntry> _verified = {};

  /// Transient entries for peers that connected but haven't been verified.
  /// Kept briefly for diagnostics.
  final Map<String, PeerEntry> _unverified = {};

  /// Register a verified peer (one that has presented a valid friendship proof).
  void addVerified(String pubkeyHex, {String? nickname}) {
    _verified.putIfAbsent(
      pubkeyHex,
      () => PeerEntry(
        publicKey: _hexDecode(pubkeyHex),
        nickname: nickname ?? pubkeyHex.substring(0, 8),
        pubkeyHex: pubkeyHex,
        lastSeen: DateTime.now(),
        firstSeen: DateTime.now(),
      ),
    );
    // Promote from unverified if present
    _unverified.remove(pubkeyHex);
  }

  /// Whether a pubkey hex belongs to a verified peer.
  bool isVerified(String pubkeyHex) => _verified.containsKey(pubkeyHex);

  /// Update a peer's info from an ANNOUNCE.
  void upsert({
    required Uint8List publicKey,
    required String nickname,
    required String pubkeyHex,
    String? udpAddress,
  }) {
    if (isVerified(pubkeyHex)) {
      final existing = _verified[pubkeyHex];
      _verified[pubkeyHex] = PeerEntry(
        publicKey: publicKey,
        nickname: nickname,
        pubkeyHex: pubkeyHex,
        udpAddress: udpAddress ?? existing?.udpAddress,
        lastSeen: DateTime.now(),
        firstSeen: existing?.firstSeen ?? DateTime.now(),
      );
    } else {
      _unverified[pubkeyHex] = PeerEntry(
        publicKey: publicKey,
        nickname: nickname,
        pubkeyHex: pubkeyHex,
        udpAddress: udpAddress,
        lastSeen: DateTime.now(),
        firstSeen: DateTime.now(),
      );
    }
  }

  PeerEntry? lookupVerified(String pubkeyHex) => _verified[pubkeyHex];

  /// Remove peers not seen for [maxAge].
  void removeStale(Duration maxAge) {
    final cutoff = DateTime.now().subtract(maxAge);
    _unverified.removeWhere((_, peer) => peer.lastSeen.isBefore(cutoff));
    // Verified peers also go stale — the proof is session-scoped.
    _verified.removeWhere((_, peer) => peer.lastSeen.isBefore(cutoff));
  }

  int get verifiedCount => _verified.length;
  int get unverifiedCount => _unverified.length;
  Iterable<PeerEntry> get verifiedPeers => _verified.values;
  Iterable<String> get verifiedPubkeyHexes => _verified.keys;

  static Uint8List _hexDecode(String hex) {
    final bytes = <int>[];
    for (var i = 0; i < hex.length; i += 2) {
      bytes.add(int.parse(hex.substring(i, i + 2), radix: 16));
    }
    return Uint8List.fromList(bytes);
  }
}

class PeerEntry {
  final Uint8List publicKey;
  final String nickname;
  final String pubkeyHex;
  final String? udpAddress;
  final DateTime lastSeen;
  final DateTime firstSeen;

  const PeerEntry({
    required this.publicKey,
    required this.nickname,
    required this.pubkeyHex,
    this.udpAddress,
    required this.lastSeen,
    required this.firstSeen,
  });

  @override
  String toString() => 'PeerEntry($nickname, $pubkeyHex'
      '${udpAddress != null ? ", addr: $udpAddress" : ""})';
}
