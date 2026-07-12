import 'package:flutter/foundation.dart';

/// Keys supplied to the layer through its API, with each peer's last-known
/// dial address.
///
/// This is the layer's recognition set and persisted dial book. The layer
/// holds no social graph (spec §Trust Gating: the transport has no concept
/// of friend — GLP holds the social graph): BLE Closed mode recognizes
/// exactly the keys GLP supplied (spec §Cold-Call Trust Levels,
/// "already-known peers"), and reconnection sweeps target exactly these
/// peers at their stored addresses.
///
/// Entries are created by `putKnownPeer` and `putPeerAddress`, and removed
/// only by `removeKnownPeer` — the layer never drops a GLP-supplied key on
/// its own. Addresses are supply-only: updated when a new valid address
/// arrives, never cleared unilaterally.
@immutable
class KnownPeersState {
  /// Known peers keyed by lowercase pubkey hex; value is the last-known
  /// "ip:port" dial address, or null when none has been supplied or
  /// observed yet (e.g. a BLE-only peer).
  final Map<String, String?> known;

  const KnownPeersState({this.known = const {}});

  static const KnownPeersState initial = KnownPeersState();

  // ===== Getters =====

  /// Whether [pubkeyHex] was supplied via the API.
  bool isKnown(String pubkeyHex) => known.containsKey(pubkeyHex.toLowerCase());

  /// All known-peer pubkey hexes.
  Set<String> get knownPubkeyHexes => known.keys.toSet();

  /// The stored dial address for [pubkeyHex], if any.
  String? addressOf(String pubkeyHex) => known[pubkeyHex.toLowerCase()];

  /// Known peers that have a dial address (the reconnectable set).
  Map<String, String> get dialBook => Map.unmodifiable({
        for (final e in known.entries)
          if (e.value != null && e.value!.isNotEmpty) e.key: e.value!,
      });

  // ===== Copy With =====

  KnownPeersState copyWith({Map<String, String?>? known}) {
    return KnownPeersState(known: known ?? this.known);
  }

  // ===== Persistence =====

  Map<String, dynamic> toJson() => {'known': known};

  factory KnownPeersState.fromJson(Map<String, dynamic> json) {
    final knownJson = json['known'] as Map<String, dynamic>?;
    if (knownJson == null) return const KnownPeersState();
    return KnownPeersState(
      known: {for (final e in knownJson.entries) e.key: e.value as String?},
    );
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is KnownPeersState &&
          runtimeType == other.runtimeType &&
          mapEquals(known, other.known);

  @override
  int get hashCode => Object.hashAll(
        (known.entries.toList()..sort((a, b) => a.key.compareTo(b.key)))
            .map((e) => Object.hash(e.key, e.value)),
      );
}
