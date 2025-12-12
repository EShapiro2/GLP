/// Import vector: array of streams, one per imported module
/// Following FCP's make_vector pattern (domain_server.cp)
///
/// Each import in a module's -import([...]) declaration gets a stream.
/// Messages written to Vector[i] are read by serve_import[i] and
/// forwarded to the target module.

import 'dart:async';
import 'module_messages.dart';

/// Result of creating an import vector
/// Contains both the vector (writer side) and the readers (consumer side)
class ImportVectorResult {
  final ImportVector vector;
  final List<Stream<ExportMessage>> readers;

  ImportVectorResult(this.vector, this.readers);
}

/// Import vector: array of streams, one per imported module
class ImportVector {
  final List<StreamController<ExportMessage>> _streams;

  ImportVector._(this._streams);

  /// Create vector of N streams (FCP make_vector)
  /// Returns (Vector, List<Stream>) where:
  /// - Vector[i] = writer end of stream i (1-indexed)
  /// - readers[i] = reader end of stream i (0-indexed)
  static ImportVectorResult make(int size) {
    final streams = <StreamController<ExportMessage>>[];
    final readers = <Stream<ExportMessage>>[];

    for (int i = 0; i < size; i++) {
      // Use broadcast stream for flexibility (multiple listeners allowed)
      final controller = StreamController<ExportMessage>.broadcast();
      streams.add(controller);
      readers.add(controller.stream);
    }

    return ImportVectorResult(ImportVector._(streams), readers);
  }

  /// Write message to stream at index (FCP write_vector)
  /// Index is 1-based following FCP convention
  void write(int index, ExportMessage message) {
    if (index < 1 || index > _streams.length) {
      throw RangeError('Import vector index $index out of range [1, ${_streams.length}]');
    }
    _streams[index - 1].add(message);  // Convert 1-indexed to 0-indexed
  }

  /// Get the number of import slots
  int get size => _streams.length;

  /// Check if vector has any imports
  bool get isEmpty => _streams.isEmpty;

  /// Close all streams (for cleanup)
  void close() {
    for (final controller in _streams) {
      controller.close();
    }
  }

  @override
  String toString() => 'ImportVector(size: $size)';
}
