/// In-memory address table for well-connected friends.
///
/// When this device can act as a friend mediator, it maintains a table of
/// friend addresses learned from ANNOUNCE traffic. The mediation path uses
/// this table to tell two friends which UDP addresses to punch toward.
///
/// The table is volatile — entries are lost on restart. Friends re-register
/// their addresses on startup, so this is fine.
class AddressTable {
  final Map<String, Map<String, AddressEntry>> _entries = {};

  /// Register (or update) a friend's address.
  void register(String pubkeyHex, String ip, int port) {
    final key = '$ip:$port';
    _entries.putIfAbsent(pubkeyHex, () => {})[key] = AddressEntry(
      ip: ip,
      port: port,
      registeredAt: DateTime.now(),
    );
  }

  /// Look up a friend's address by pubkey hex.
  ///
  /// Returns null if the friend hasn't registered or was removed.
  AddressEntry? lookup(String pubkeyHex) {
    final entries = lookupAll(pubkeyHex);
    if (entries.isEmpty) return null;
    return entries.first;
  }

  /// Look up all addresses by pubkey hex, newest first.
  List<AddressEntry> lookupAll(String pubkeyHex) {
    final entries = _entries[pubkeyHex];
    if (entries == null || entries.isEmpty) return const [];
    return entries.values.toList()
      ..sort((a, b) => b.registeredAt.compareTo(a.registeredAt));
  }

  /// Remove a specific entry.
  void remove(String pubkeyHex) => _entries.remove(pubkeyHex);

  /// Remove entries older than [maxAge].
  ///
  /// Call this periodically (e.g. every 60s) to evict stale entries
  /// from friends that went offline without deregistering.
  void removeStale(Duration maxAge) {
    final cutoff = DateTime.now().subtract(maxAge);
    _entries.removeWhere((_, entries) {
      entries.removeWhere((_, entry) => entry.registeredAt.isBefore(cutoff));
      return entries.isEmpty;
    });
  }

  /// Number of registered entries.
  int get length =>
      _entries.values.fold(0, (count, entries) => count + entries.length);

  /// All registered pubkey hexes.
  Iterable<String> get registeredPubkeys => _entries.keys;

  /// Clear all entries.
  void clear() => _entries.clear();
}

/// A registered address entry in the [AddressTable].
class AddressEntry {
  final String ip;
  final int port;
  final DateTime registeredAt;

  const AddressEntry({
    required this.ip,
    required this.port,
    required this.registeredAt,
  });

  @override
  String toString() => 'AddressEntry($ip:$port, registered: $registeredAt)';
}
