import 'package:shared_preferences/shared_preferences.dart';

import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';

/// The Flutter profile's [OutboundQueueStore]: the application's
/// SharedPreferences, where the known-peer set is kept too
/// (`PersistenceService`), one key per agent, so that the fair-delivery queue
/// survives the termination of the application (GNA §2.10).  An empty queue
/// leaves no key behind.
class SharedPreferencesOutboundQueueStore implements OutboundQueueStore {
  const SharedPreferencesOutboundQueueStore();

  static const String _keyPrefix = 'grassroots_outbound_queue_v1:';

  /// The key [ownerHex]'s queue is kept under.
  static String keyFor(String ownerHex) => '$_keyPrefix$ownerHex';

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async {
    final prefs = await SharedPreferences.getInstance();
    final text = prefs.getString(keyFor(ownerHex));
    return text == null ? [] : decodeOutboundQueue(text);
  }

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) async {
    final prefs = await SharedPreferences.getInstance();
    final key = keyFor(ownerHex);
    final saved = entries.isEmpty
        ? await prefs.remove(key)
        : await prefs.setString(key, encodeOutboundQueue(entries));
    if (!saved) {
      throw StateError('SharedPreferences did not save the outbound queue');
    }
  }
}
