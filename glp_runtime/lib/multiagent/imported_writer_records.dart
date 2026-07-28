/// Imported-Writer Records U_p for madGLP
///
/// Normative source: the IGLP paper — Definition Imported-Writer Records
/// (madglp-spec.tex), madGLP Local State (the tuple `(R_p, W_p, U_p, M_p)`),
/// Definition Localize case 1, Definition Globalize case 4, and "Imported
/// writers" under Heap in the Implementation Notes appendix.
///
/// A writer localized from a writer global name `_w(o, k)` is an ordinary local
/// writer that carries a record of the link it is the sending end of: the
/// anchor `o`, the index `k`, and the `global_send` goal watching its reader.
/// The record is what makes re-export a forwarding rather than a re-anchoring:
/// Globalize reads it from the writer's cell, emits `_w(o, k)`, and removes
/// both the record and the goal, so the exporting agent leaves the link.
library;

/// A record `(Y, o, k)` marking the local writer [writerAddr] as the sending
/// end of the link anchored by `_w(anchor, index)`.
///
/// The `global_send` goal watching `Y?` is registered in the agent's
/// [GlobalSendRegistry] under the same writer address, so the record reaches
/// its goal in O(1); the two are created together by Localize and removed
/// together — when the value is sent, or when `Y` is re-exported.
class ImportedWriterRecord {
  /// Local writer `Y` — also the key of the `global_send` goal watching `Y?`.
  final int writerAddr;

  /// The link's anchor `o`: the agent that globalized the writer.
  final String anchor;

  /// The index `k` of the link at its anchor.
  final int index;

  ImportedWriterRecord({
    required this.writerAddr,
    required this.anchor,
    required this.index,
  });

  @override
  String toString() =>
      'ImportedWriterRecord(writer=$writerAddr, _w($anchor, $index))';
}

/// The imported-writer records `U_p` of agent `p` (Definition Imported-Writer
/// Records): a set of triples keyed by the local writer, so the record is
/// reachable from the writer's cell in O(1) — as an imported reader's cell
/// holds its global writers table entry.
class ImportedWriterRecords {
  /// Agent ID that owns these records.
  final String agentId;

  final Map<int, ImportedWriterRecord> _byWriter = {};

  ImportedWriterRecords(this.agentId);

  /// Add the record `(writerAddr, anchor, index)`.
  ///
  /// Called by Localize case 1 alongside the spawned `global_send` goal.
  /// Localize allocates a fresh pair for every global name, so the writer is
  /// new; a repeat would mean two links sharing one sending end.
  void add(int writerAddr, String anchor, int index) {
    final existing = _byWriter[writerAddr];
    if (existing != null) {
      throw ArgumentError(
        'Duplicate imported-writer record for writer $writerAddr: '
        'existing $existing, new _w($anchor, $index)',
      );
    }
    _byWriter[writerAddr] =
        ImportedWriterRecord(writerAddr: writerAddr, anchor: anchor, index: index);
  }

  /// The record of [writerAddr], or null when the writer is not imported.
  ///
  /// The presence of a record is the condition of Globalize case 4.
  ImportedWriterRecord? lookup(int writerAddr) => _byWriter[writerAddr];

  /// Remove and return the record of [writerAddr], or null if there is none.
  ///
  /// Removed when the value is sent, and when the writer is re-exported and
  /// the name forwarded (Definition Imported-Writer Records).
  ImportedWriterRecord? remove(int writerAddr) => _byWriter.remove(writerAddr);

  /// Number of records held.
  int get count => _byWriter.length;

  @override
  String toString() {
    final buf = StringBuffer('ImportedWriterRecords($agentId)\n');
    for (final rec in _byWriter.values) {
      buf.writeln('  $rec');
    }
    return buf.toString();
  }
}
