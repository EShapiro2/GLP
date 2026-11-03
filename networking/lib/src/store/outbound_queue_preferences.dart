import 'package:shared_preferences/shared_preferences.dart';

import 'package:grassroots_networking_core/src/protocol/outbound_queue_store.dart';

/// The Flutter profile's [OutboundQueueStore]: the application's
/// SharedPreferences, where the known-peer set is kept too
/// (`PersistenceService`), two keys per agent, one for the messages it sent
/// and one for the messages it delivered, so that the fair-delivery queue
/// survives the termination of the application (GNA §2.11).  An empty part
/// leaves no key behind.
class SharedPreferencesOutboundQueueStore implements OutboundQueueStore {
  const SharedPreferencesOutboundQueueStore();

  static const String _keyPrefix = 'grassroots_outbound_queue_v1:';
  static const String _deliveredKeyPrefix = 'grassroots_delivered_queue_v1:';

  /// The key [ownerHex]'s sent messages are kept under.
  static String keyFor(String ownerHex) => '$_keyPrefix$ownerHex';

  /// The key [ownerHex]'s delivered messages are kept under.
  static String deliveredKeyFor(String ownerHex) =>
      '$_deliveredKeyPrefix$ownerHex';

  @override
  Future<List<OutboundQueueEntry>> load(String ownerHex) async {
    final text = await _read(keyFor(ownerHex));
    return text == null ? [] : decodeOutboundQueue(text);
  }

  @override
  Future<void> save(String ownerHex, List<OutboundQueueEntry> entries) =>
      _write(keyFor(ownerHex),
          entries.isEmpty ? null : encodeOutboundQueue(entries));

  @override
  Future<List<DeliveredQueueEntry>> loadDelivered(String ownerHex) async {
    final text = await _read(deliveredKeyFor(ownerHex));
    return text == null ? [] : decodeDeliveredQueue(text);
  }

  @override
  Future<void> saveDelivered(
          String ownerHex, List<DeliveredQueueEntry> entries) =>
      _write(deliveredKeyFor(ownerHex),
          entries.isEmpty ? null : encodeDeliveredQueue(entries));

  Future<String?> _read(String key) async =>
      (await SharedPreferences.getInstance()).getString(key);

  /// Write [text] under [key], or remove the key when [text] is null.
  Future<void> _write(String key, String? text) async {
    final prefs = await SharedPreferences.getInstance();
    final saved = text == null
        ? await prefs.remove(key)
        : await prefs.setString(key, text);
    if (!saved) {
      throw StateError('SharedPreferences did not save the queue');
    }
  }
}
