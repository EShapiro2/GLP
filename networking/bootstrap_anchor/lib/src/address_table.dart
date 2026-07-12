import 'dart:io';

/// In-memory address table for friend addresses.
///
/// Identical to the client-side AddressTable. The anchor maintains this
/// to answer ADDR_QUERY requests from friends.
class AddressTable {
  final Map<String, Map<InternetAddressType, AddressEntry>> _entries = {};

  void register(String pubkeyHex, String ip, int port) {
    final family = InternetAddress.tryParse(ip)?.type;
    if (family == null) return;

    _entries.putIfAbsent(pubkeyHex, () => {})[family] = AddressEntry(
      ip: ip,
      port: port,
      family: family,
      registeredAt: DateTime.now(),
    );
  }

  AddressEntry? lookup(
    String pubkeyHex, {
    InternetAddressType? family,
  }) {
    final entries = _entries[pubkeyHex];
    if (entries == null || entries.isEmpty) return null;
    if (family != null) return entries[family];

    final values = entries.values.toList()
      ..sort((a, b) => b.registeredAt.compareTo(a.registeredAt));
    return values.first;
  }

  List<AddressEntry> lookupAll(String pubkeyHex) {
    final entries = _entries[pubkeyHex];
    if (entries == null || entries.isEmpty) return const [];
    final values = entries.values.toList()
      ..sort((a, b) => b.registeredAt.compareTo(a.registeredAt));
    return values;
  }

  void remove(String pubkeyHex) => _entries.remove(pubkeyHex);

  void removeStale(
    Duration maxAge, {
    Set<String> protectedPubkeys = const {},
  }) {
    final cutoff = DateTime.now().subtract(maxAge);
    _entries.removeWhere((pubkeyHex, familyEntries) {
      if (protectedPubkeys.contains(pubkeyHex)) {
        return false;
      }
      familyEntries.removeWhere(
        (_, entry) => entry.registeredAt.isBefore(cutoff),
      );
      return familyEntries.isEmpty;
    });
  }

  int get length =>
      _entries.values.fold(0, (count, entries) => count + entries.length);
  Iterable<String> get registeredPubkeys => _entries.keys;
  void clear() => _entries.clear();
}

class AddressEntry {
  final String ip;
  final int port;
  final InternetAddressType family;
  final DateTime registeredAt;

  const AddressEntry({
    required this.ip,
    required this.port,
    required this.family,
    required this.registeredAt,
  });

  @override
  String toString() =>
      'AddressEntry($ip:$port, ${family == InternetAddressType.IPv6 ? "IPv6" : "IPv4"})';
}
