import 'dart:collection';
import 'package:logger/logger.dart' show Level;

/// A log entry captured from the logger.
class LogEntry {
  final Level level;
  final String message;
  final DateTime timestamp;

  LogEntry({
    required this.level,
    required this.message,
    required this.timestamp,
  });

  String get levelLabel {
    switch (level) {
      case Level.trace:
        return 'TRACE';
      case Level.debug:
        return 'DEBUG';
      case Level.info:
        return 'INFO';
      case Level.warning:
        return 'WARN';
      case Level.error:
        return 'ERROR';
      case Level.fatal:
        return 'FATAL';
      default:
        return level.name.toUpperCase();
    }
  }
}

/// Global in-memory log buffer that captures logger output.
///
/// Install as a [LogOutput] on all Logger instances to capture log lines.
/// The buffer is ring-buffered to avoid unbounded memory growth.
class LogBuffer {
  static final LogBuffer instance = LogBuffer._();

  /// Maximum number of log entries to keep.
  static const int maxEntries = 2000;

  final _entries = Queue<LogEntry>();
  final _listeners = <void Function()>[];

  LogBuffer._();

  /// All captured log entries (oldest first).
  List<LogEntry> get entries => _entries.toList();

  /// Number of entries currently stored.
  int get length => _entries.length;

  /// Register a listener that's called when new entries arrive.
  void addListener(void Function() listener) => _listeners.add(listener);

  /// Remove a previously registered listener.
  void removeListener(void Function() listener) => _listeners.remove(listener);

  /// Add a single log entry directly.
  void addEntry(LogEntry entry) {
    _entries.addLast(entry);
    while (_entries.length > maxEntries) {
      _entries.removeFirst();
    }
    _notifyListeners();
  }

  /// Clear all entries.
  void clear() {
    _entries.clear();
    _notifyListeners();
  }


  void _notifyListeners() {
    for (final listener in _listeners) {
      listener();
    }
  }
}
