import 'dart:typed_data';

/// A registered cold-call invite (spec `GLP_Networking_API` §IP Cold-Call).
///
/// Stored by the rendezvous server on REGISTER_INVITE from inviter A, keyed
/// by the opaque InviteId. The server never sees the raw invite nonce — only
/// the derived id and the InviteKey proof key it verifies redemption MACs
/// against.
class InviteEntry {
  /// Inviter A's public key — the authenticated sender of the registration.
  final Uint8List inviterPubkey;

  /// The HKDF-derived proof key redemption MACs are verified against.
  final Uint8List inviteKey;

  /// Expiry, milliseconds since epoch.
  final int expiresAtMs;

  /// Single-use flag: flipped exactly once, atomically, on the first valid
  /// redemption claim.
  bool used = false;

  /// The redeemer's public key when the INVITE_REDEEMED notification to A
  /// could not be delivered yet (A offline). Cleared once delivered; retried
  /// by the anchor's cleanup timer while the entry lives.
  Uint8List? undeliveredRedeemer;

  InviteEntry({
    required this.inviterPubkey,
    required this.inviteKey,
    required this.expiresAtMs,
  });

  bool isExpired(DateTime now) => now.millisecondsSinceEpoch > expiresAtMs;
}

/// In-memory invite store, keyed by lowercase InviteId hex.
///
/// Volatile like [PeerTable]/[AddressTable]: invites do not survive an anchor
/// restart. The one-hour invite lifetime bounds the impact; an inviter can
/// always mint a fresh link.
class InviteTable {
  /// Cap on concurrently-registered invites per inviter. Bounds the memory a
  /// single authenticated peer can pin (each invite lives up to its 1-hour
  /// expiry). Generous for legitimate use; a flood is rejected past this.
  static const int maxInvitesPerInviter = 256;

  /// Global cap across all inviters — a coarse backstop against many peers
  /// each staying under the per-inviter cap.
  static const int maxTotalInvites = 100000;

  final Map<String, InviteEntry> _entries = {};
  final Map<String, int> _countByInviter = {};

  /// Store (or idempotently refresh) an invite. Re-registration of the same
  /// InviteId is accepted only from the same inviter and never resurrects a
  /// used invite; a different sender claiming an existing id is rejected.
  /// A new registration is rejected when it would exceed [maxInvitesPerInviter]
  /// for the sender or [maxTotalInvites] overall. Returns whether the entry is
  /// registered after the call.
  bool register({
    required String inviteIdHex,
    required Uint8List inviterPubkey,
    required Uint8List inviteKey,
    required int expiresAtMs,
  }) {
    final existing = _entries[inviteIdHex];
    if (existing != null) {
      final samePubkey = _bytesEqual(existing.inviterPubkey, inviterPubkey);
      return samePubkey && !existing.used;
    }
    final inviterHex = _hex(inviterPubkey);
    final inviterCount = _countByInviter[inviterHex] ?? 0;
    if (inviterCount >= maxInvitesPerInviter ||
        _entries.length >= maxTotalInvites) {
      return false;
    }
    _entries[inviteIdHex] = InviteEntry(
      inviterPubkey: inviterPubkey,
      inviteKey: inviteKey,
      expiresAtMs: expiresAtMs,
    );
    _countByInviter[inviterHex] = inviterCount + 1;
    return true;
  }

  InviteEntry? lookup(String inviteIdHex) => _entries[inviteIdHex];

  /// Entries with an undelivered redeemed-notification, as
  /// (inviteIdHex, entry) pairs — for the anchor's retry sweep.
  Iterable<MapEntry<String, InviteEntry>> get undelivered =>
      _entries.entries.where((e) => e.value.undeliveredRedeemer != null);

  /// Drop expired invites. A used invite whose notification is still
  /// undelivered is retained until expiry so the retry sweep can deliver it.
  void removeExpired({DateTime? now}) {
    final at = now ?? DateTime.now();
    _entries.removeWhere((_, entry) {
      if (!entry.isExpired(at)) return false;
      final inviterHex = _hex(entry.inviterPubkey);
      final remaining = (_countByInviter[inviterHex] ?? 1) - 1;
      if (remaining <= 0) {
        _countByInviter.remove(inviterHex);
      } else {
        _countByInviter[inviterHex] = remaining;
      }
      return true;
    });
  }

  int get count => _entries.length;

  static String _hex(Uint8List bytes) =>
      bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();

  static bool _bytesEqual(Uint8List a, Uint8List b) {
    if (a.length != b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
}
