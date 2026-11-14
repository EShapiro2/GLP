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

import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/saved_state.dart';

import 'mad_helpers.dart' show GlobalName;

/// A record `(Y, o, k, s)` marking the local writer [writerAddr] as the sending
/// end of the link anchored by `_w(anchor, index)`, received from agent [sender].
///
/// The `global_send` goal watching `Y?` is registered in the agent's
/// [GlobalSendRegistry] under the same writer address, so the record reaches
/// its goal in O(1); the two are created together by Localize and removed
/// together — when the value is sent, or when `Y` is re-exported.
class ImportedWriterRecord {
  /// Local writer `Y` — also the key of the `global_send` goal watching `Y?`.
  final HeapCell writerAddr;

  /// The link's anchor `o`: the agent that globalized the writer.
  final String anchor;

  /// The epoch in the name, `_w(o, e, k)`: the epoch this end records of
  /// the anchor (IGLP cec4682, after Definition "madGLP Local State").
  final int epoch;

  /// The index `k` of the link at its anchor.
  final int index;

  /// The agent `s` this name came from. Equal to [anchor] when the name was
  /// received from the agent that globalized it; otherwise the name was
  /// forwarded and the value this writer produces is held (Definition Held
  /// Link, case 2), reported with `s` as the sender — the hold happens at
  /// binding, when the message being processed is long gone, so the sender
  /// must be carried here.
  final String sender;

  ImportedWriterRecord({
    required this.writerAddr,
    required this.anchor,
    required this.epoch,
    required this.index,
    required this.sender,
  });

  /// The writer name of the link, `_w(o, e, k)`.
  GlobalName get name => GlobalName.writer(anchor, index, epoch: epoch);

  /// Whether this writer's link was forwarded to us rather than exported to us
  /// by its anchor — the condition of Definition Held Link, case 2.
  bool get isForwarded => sender != anchor;

  @override
  String toString() =>
      'ImportedWriterRecord(writer=$writerAddr, $name, from=$sender)';
}

/// The imported-writer records `U_p` of agent `p` (Definition Imported-Writer
/// Records): a set of triples keyed by the local writer, so the record is
/// reachable from the writer's cell in O(1) — as an imported reader's cell
/// holds its global writers table entry.
class ImportedWriterRecords {
  /// Agent ID that owns these records.
  final String agentId;

  final Map<HeapCell, ImportedWriterRecord> _byWriter = {};

  ImportedWriterRecords(this.agentId);

  /// Add the record `(writerAddr, name, sender)`, [name] the writer name
  /// `_w(o, e, k)`.
  ///
  /// Called by Localize case 1 alongside the spawned `global_send` goal.
  /// Localize allocates a fresh pair for every global name, so the writer is
  /// new; a repeat would mean two links sharing one sending end.
  void add(HeapCell writerAddr, GlobalName name, String sender) {
    assert(name.isWriter, 'an imported-writer record is of a writer name');
    final existing = _byWriter[writerAddr];
    if (existing != null) {
      throw ArgumentError(
        'Duplicate imported-writer record for writer $writerAddr: '
        'existing $existing, new $name',
      );
    }
    _byWriter[writerAddr] = ImportedWriterRecord(
      writerAddr: writerAddr,
      anchor: name.agent,
      epoch: name.epoch,
      index: name.index,
      sender: sender,
    );
  }

  /// The record of the link of the writer name [name], or null.
  ///
  /// Used by `'_authorise_link'` to reach a record from the reported name;
  /// there is at most one, a link having one sending end at a time.
  ImportedWriterRecord? findByLink(GlobalName name) {
    for (final rec in _byWriter.values) {
      if (rec.anchor == name.agent &&
          rec.epoch == name.epoch &&
          rec.index == name.index) {
        return rec;
      }
    }
    return null;
  }

  /// The records held.
  Iterable<ImportedWriterRecord> get all => _byWriter.values;

  /// Retire every record, as a restart after a crash does (IGLP db0a117,
  /// Definition "Restart after a Crash"); the records retired.
  List<ImportedWriterRecord> retireAll() {
    final all = List<ImportedWriterRecord>.from(_byWriter.values);
    _byWriter.clear();
    return all;
  }

  /// Retire every record whose anchor is [agent] in an epoch below [epoch]
  /// (IGLP db0a117, Definition "madGLP Send Transaction": the ends "whose
  /// other end is at p and whose recorded epoch of p is below it"); the
  /// records retired.
  List<ImportedWriterRecord> retireAt(String agent, int epoch) {
    final gone = [
      for (final rec in _byWriter.values)
        if (rec.anchor == agent && rec.epoch < epoch) rec
    ];
    for (final rec in gone) {
      _byWriter.remove(rec.writerAddr);
    }
    return gone;
  }

  /// The record of [writerAddr], or null when the writer is not imported.
  ///
  /// The presence of a record is the condition of Globalize case 4.
  ImportedWriterRecord? lookup(HeapCell writerAddr) => _byWriter[writerAddr];

  /// Remove and return the record of [writerAddr], or null if there is none.
  ///
  /// Removed when the value is sent, and when the writer is re-exported and
  /// the name forwarded (Definition Imported-Writer Records).
  ImportedWriterRecord? remove(HeapCell writerAddr) => _byWriter.remove(writerAddr);

  /// Number of records held.
  int get count => _byWriter.length;

  /// Write the records into the agent's saved state (IGLP, Implementation
  /// Notes, "Persistence": "the imported-writer records").
  void saveState(StateWriter w) {
    w.uint(_byWriter.length);
    for (final rec in _byWriter.values) {
      w.cell(rec.writerAddr);
      rec.name.save(w);
      w.str(rec.sender);
    }
  }

  /// Read back what [saveState] wrote, into records fresh from construction.
  void restoreState(StateReader r) {
    _byWriter.clear();
    for (var i = r.uint(); i > 0; i--) {
      final writer = r.cell();
      final name = GlobalName.restore(r);
      add(writer, name, r.str());
    }
  }

  @override
  String toString() {
    final buf = StringBuffer('ImportedWriterRecords($agentId)\n');
    for (final rec in _byWriter.values) {
      buf.writeln('  $rec');
    }
    return buf.toString();
  }
}
