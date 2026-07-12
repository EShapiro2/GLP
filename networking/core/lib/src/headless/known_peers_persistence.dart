import 'dart:convert';
import 'dart:io';

import '../platform/compat.dart';

/// Persistence seam for the headless known-peers set (spec `GLP_Networking_API`
/// §Known Peers: keys GLP supplies plus the persisted dial book). The Flutter
/// package persists through SharedPreferences; a server persists to a file or
/// runs in memory.
abstract class HeadlessKnownPeersStore {
  /// Load the persisted known set: pubkey hex → last-known `ip:port` (null
  /// when no address is on file). Empty when nothing was persisted.
  Map<String, String?> load();

  /// Persist the known set. Called on every change; implementations should
  /// be cheap and atomic.
  void save(Map<String, String?> known);
}

/// In-memory store: known peers live for the process only.
class MemoryKnownPeersStore implements HeadlessKnownPeersStore {
  Map<String, String?> _known = const {};

  @override
  Map<String, String?> load() => Map.of(_known);

  @override
  void save(Map<String, String?> known) => _known = Map.of(known);
}

/// JSON-file store: survives service restarts. Writes are atomic
/// (temp file + rename).
class FileKnownPeersStore implements HeadlessKnownPeersStore {
  FileKnownPeersStore(this.path);

  final String path;

  @override
  Map<String, String?> load() {
    final file = File(path);
    if (!file.existsSync()) return {};
    try {
      final decoded = jsonDecode(file.readAsStringSync());
      if (decoded is! Map) return {};
      return decoded.map(
        (key, value) => MapEntry(key as String, value as String?),
      );
    } catch (e) {
      debugPrint('[known-peers] Failed to read $path: $e');
      return {};
    }
  }

  @override
  void save(Map<String, String?> known) {
    final file = File(path);
    file.parent.createSync(recursive: true);
    final tmp = File('$path.tmp');
    tmp.writeAsStringSync(jsonEncode(known), flush: true);
    tmp.renameSync(path);
  }
}
