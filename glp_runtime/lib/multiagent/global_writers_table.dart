/// Global Writers Table for madGLP
///
/// Tracks local writers that await incoming assignments from remote agents.
/// Each entry maps a global name to the local writer that will be assigned
/// when a message arrives.
///
/// See: IGLP Definitions Global Writers Table Entry and Global Writers Table (app:global-writers-table).
library;

import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/saved_state.dart';

import 'mad_helpers.dart' show GlobalName;

/// Entry created by Globalize (when exporting a writer)
///
/// Stored at index i in the table, corresponds to global name `_w(p, i)`.
/// When agent q sends a message `_w(p, i) := T`, this entry
/// identifies the local writer X that should be assigned.
class GlobalizeEntry {
  /// Local writer address that will be assigned when callback arrives
  final HeapCell writerAddr;

  /// Agent who will send the callback
  final String remoteAgent;

  /// The epoch this end records of the agent at the other end, the holder:
  /// "for an end at the anchor, the epoch carried by the first request,
  /// acknowledgement or value from the holder, and no epoch until one
  /// arrives" (IGLP db0a117, after Definition "madGLP Local State").  Null
  /// until one arrives; an end with none is retired by no epoch the agent
  /// learns, and by the agent's own crash.
  int? holderEpoch;

  GlobalizeEntry({
    required this.writerAddr,
    required this.remoteAgent,
    this.holderEpoch,
  });

  @override
  String toString() => 'GlobalizeEntry(writer=$writerAddr, remote=$remoteAgent'
      '${holderEpoch == null ? '' : ', holder epoch $holderEpoch'})';
}

/// Entry created by Localize (when importing a reader global name)
///
/// Must search by (remoteAgent, remoteIndex) to find the entry.
/// When message `_r(p, i) := T` arrives, search for entry matching (p, i).
class LocalizeEntry {
  /// Local writer address that will be assigned
  final HeapCell writerAddr;

  /// Agent who created the global name (p in `_r(p, e, i)`)
  final String remoteAgent;

  /// The epoch in the name, e in `_r(p, e, i)`: the epoch this end records
  /// of the anchor, "for an end whose other end is at the anchor, the epoch
  /// in the name" (IGLP db0a117, after Definition "madGLP Local State").
  final int remoteEpoch;

  /// Index from the remote agent's global name (i in `_r(p, e, i)`)
  final int remoteIndex;

  LocalizeEntry({
    required this.writerAddr,
    required this.remoteAgent,
    required this.remoteEpoch,
    required this.remoteIndex,
  });

  /// The reader name this entry was made for, `_r(p, e, i)`.
  GlobalName get name =>
      GlobalName.reader(remoteAgent, remoteIndex, epoch: remoteEpoch);

  @override
  String toString() => 'LocalizeEntry(writer=$writerAddr, $name)';
}

/// Global Writers Table (W_p)
///
/// Tracks writers awaiting incoming assignments from remote agents.
/// Two entry types are stored:
/// - GlobalizeEntry: created by Globalize, direct index lookup
/// - LocalizeEntry: created by Localize, search by (agent, index)
///
/// Index 0 is reserved for the network input serializer (IGLP Definition
/// Index-0 Serializer).
/// This entry is permanent and supports many-to-one cold-call reception.
///
/// See: IGLP Definitions Global Writers Table Entry and Global Writers Table (app:global-writers-table).
class GlobalWritersTable {
  /// Agent ID that owns this table
  final String agentId;

  /// Single counter for index allocation (IGLP: the global-name counter)
  /// Shared across both Globalize and Localize operations.
  /// Index 0 is reserved for the serializer, so counter starts at 1.
  /// Indices are never reused.
  int _nextIndex = 1;

  /// Serializer entry at index 0 (IGLP Definition Index-0 Serializer)
  /// Maps to the local writer for the network input stream.
  /// This entry is permanent - never removed.
  HeapCell? _serializerWriterAddr;

  /// The D_p of the serializer entry `(N_p, D_p)` (IGLP db0a117, Definition
  /// Index-0 Serializer): "the set of (sender, epoch, serial) triples of the
  /// cold calls appended so far: a cold call carries a serial its sender
  /// allocates in its epoch, and one delivered twice is appended once", empty
  /// at boot.  A serial is unique by (sender, epoch, serial): a sender that
  /// crashed allocates serials again in its next epoch.
  final Set<(String, int, int)> _serializerAppended = {};

  /// GlobalizeEntries: direct index lookup
  /// Key is the index i, value is the entry at that index.
  /// Corresponds to global name `_w(agentId, i)`.
  final Map<int, GlobalizeEntry> _globalizeEntries = {};

  /// LocalizeEntries: search by (agent, index)
  /// These entries are created when localizing `_r(p, i)` names.
  final List<LocalizeEntry> _localizeEntries = [];

  GlobalWritersTable(this.agentId);

  // ===========================================================================
  // Serializer Entry (Index 0) - Network Input Stream
  // ===========================================================================

  /// Initialize the permanent serializer entry at index 0.
  ///
  /// Called at boot time to set up the network input stream writer.
  /// This entry is never removed - it receives cold-call messages from
  /// any agent via `_w(agentId, 0)`.
  ///
  /// IGLP Definition Index-0 Serializer: "At boot time, each agent p creates
  /// a permanent entry at index 0 mapping `_r(p, 0)` to the local writer N_p
  /// for p's network input stream."
  void initializeSerializerEntry(HeapCell netInWriterAddr) {
    if (_serializerWriterAddr != null) {
      throw StateError('Serializer entry already initialized');
    }
    _serializerWriterAddr = netInWriterAddr;
  }

  /// Get the current serializer writer address.
  ///
  /// Returns null if not yet initialized.
  HeapCell? get serializerWriterAddr => _serializerWriterAddr;

  /// Update the serializer writer to a fresh address.
  ///
  /// Called by Receive transaction after binding the current writer
  /// to extend the network input stream.
  ///
  /// IGLP, madGLP Receive Transaction, Serializer case: "Assign N_q := [T_q↓ |
  /// N'_q] where N'_q is a fresh writer. Update the entry to `(N'_q, *)` at
  /// index 0."
  void updateSerializerWriter(HeapCell newWriterAddr) {
    if (_serializerWriterAddr == null) {
      throw StateError('Cannot update serializer: not initialized');
    }
    _serializerWriterAddr = newWriterAddr;
  }

  /// Check if the serializer entry is initialized.
  bool get hasSerializerEntry => _serializerWriterAddr != null;

  /// Whether the cold call of serial [serial] from [sender] in its epoch
  /// [epoch] is in the serializer entry's D_p: appended before, so a repeat
  /// (IGLP db0a117, Definition madGLP Send Transaction, Serializer case: "If
  /// (p, e, i) ∈ D_q, m is a repeat").
  bool serializerHasAppended(String sender, int epoch, int serial) =>
      _serializerAppended.contains((sender, epoch, serial));

  /// Add the triple ([sender], [epoch], [serial]) to the serializer entry's
  /// D_p, as the Serializer case does when it appends the cold call: "The
  /// entry at index 0 in W'_q becomes (N'_q, D_q ∪ {(p, e, i)})".
  void recordSerializerAppend(String sender, int epoch, int serial) {
    if (_serializerWriterAddr == null) {
      throw StateError('Cannot record a cold call: serializer not initialized');
    }
    _serializerAppended.add((sender, epoch, serial));
  }

  /// The number of pairs in the serializer entry's D_p.
  int get serializerAppendedCount => _serializerAppended.length;

  // ===========================================================================
  // GlobalizeEntry Operations
  // ===========================================================================

  /// Allocate next index and add GlobalizeEntry.
  ///
  /// Called by Globalize when exporting a writer Y:
  /// - Allocate index i
  /// - Create entry (Y, q) at index i
  /// - Replace Y with `_w(p, i)` in globalized term
  ///
  /// Returns the allocated index.
  int addGlobalizeEntry(HeapCell writerAddr, String remoteAgent) {
    final index = _nextIndex++;
    _globalizeEntries[index] = GlobalizeEntry(
      writerAddr: writerAddr,
      remoteAgent: remoteAgent,
    );
    return index;
  }

  /// Add LocalizeEntry.
  ///
  /// Called by Localize when importing the reader name [name], `_r(p, e, i)`:
  /// - Create fresh pair (Z, Z?)
  /// - Add entry (Z, p, e, i)
  /// - Replace `_r(p, e, i)` with Z? in localized term
  ///
  /// Throws [ArgumentError] if a duplicate entry for [name] exists.
  void addLocalizeEntry(HeapCell writerAddr, GlobalName name) {
    assert(name.isReader, 'a LocalizeEntry is made for a reader name');
    // Check for duplicates (IGLP, the entry lifecycle: one entry per name,
    // names never reused)
    final existing = findByRemote(name);
    if (existing != null) {
      throw ArgumentError(
        'Duplicate LocalizeEntry for $name: '
        'existing writer=${existing.writerAddr}, new writer=$writerAddr',
      );
    }

    _localizeEntries.add(LocalizeEntry(
      writerAddr: writerAddr,
      remoteAgent: name.agent,
      remoteEpoch: name.epoch,
      remoteIndex: name.index,
    ));
  }

  /// Lookup GlobalizeEntry by index.
  ///
  /// Used when receiving `_w(p, i) := T` where we are agent p.
  /// The entry at index i identifies the local writer to assign.
  GlobalizeEntry? lookupByIndex(int index) {
    return _globalizeEntries[index];
  }

  /// Search for the LocalizeEntry whose local writer is [writerAddr].
  ///
  /// Used by Globalize's forwarding case: an unbound imported reader is one
  /// whose paired writer carries a LocalizeEntry (X, o, k) — the entry's
  /// presence is the condition, since applying the incoming value removes it.
  LocalizeEntry? findLocalizeEntryByWriter(HeapCell writerAddr) {
    for (final entry in _localizeEntries) {
      if (entry.writerAddr == writerAddr) {
        return entry;
      }
    }
    return null;
  }

  /// Search for LocalizeEntry matching the reader name [name], `_r(p, e, i)`.
  ///
  /// Used when receiving `_r(p, e, i) := T`:
  /// - Search for entry with remoteAgent=p, remoteEpoch=e and remoteIndex=i
  /// - That entry's writerAddr is the local writer to assign
  LocalizeEntry? findByRemote(GlobalName name) {
    for (final entry in _localizeEntries) {
      if (entry.remoteAgent == name.agent &&
          entry.remoteEpoch == name.epoch &&
          entry.remoteIndex == name.index) {
        return entry;
      }
    }
    return null;
  }

  /// The GlobalizeEntries standing, by index.
  Iterable<MapEntry<int, GlobalizeEntry>> get globalizeEntries =>
      _globalizeEntries.entries;

  /// The LocalizeEntries standing.
  Iterable<LocalizeEntry> get localizeEntries => _localizeEntries;

  /// Remove GlobalizeEntry at index.
  ///
  /// Called after receiving `_w(p, i) := T` and assigning the writer.
  /// Leaves a gap in the index space; indices are not reused.
  ///
  /// Index 0 (serializer) cannot be removed - use updateSerializerWriter instead.
  ///
  /// Safe to call on non-existent index (idempotent).
  void removeGlobalizeEntry(int index) {
    if (index == 0) {
      // Serializer entry is permanent - cannot be removed
      // IGLP Definition Index-0 Serializer: "This entry is never removed."
      return;
    }
    _globalizeEntries.remove(index);
  }

  /// Remove LocalizeEntry by the reader name [name].
  ///
  /// Called after receiving `_r(p, e, i) := T` and assigning the writer.
  ///
  /// Safe to call on non-existent entry (idempotent).
  void removeLocalizeEntry(GlobalName name) {
    _localizeEntries.removeWhere(
      (e) =>
          e.remoteAgent == name.agent &&
          e.remoteEpoch == name.epoch &&
          e.remoteIndex == name.index,
    );
  }

  /// Retire every entry but the serializer's, as a restart after a crash
  /// does (IGLP db0a117, Definition "Restart after a Crash": "every link end
  /// of p retired ... but the serializer entry"; Definition "Retire": "removing
  /// from W_q the entry of the link").  Returns the entries retired, the
  /// Globalize ones with their indices.
  ({List<(int, GlobalizeEntry)> globalize, List<LocalizeEntry> localize})
      retireAllButSerializer() {
    final globalize = [
      for (final MapEntry(key: i, value: e) in _globalizeEntries.entries)
        (i, e)
    ];
    final localize = List<LocalizeEntry>.from(_localizeEntries);
    _globalizeEntries.clear();
    _localizeEntries.clear();
    return (globalize: globalize, localize: localize);
  }

  /// Retire every entry whose other end is at [agent] and whose recorded
  /// epoch of [agent] is below [epoch] (IGLP db0a117, Definition "madGLP Send
  /// Transaction": "q retires every link end whose other end is at p and
  /// whose recorded epoch of p is below it"): a LocalizeEntry of a name
  /// anchored at [agent] in an earlier epoch, and a GlobalizeEntry exported
  /// to [agent] that recorded a holder epoch below it.  Returns the entries
  /// retired.
  ({List<(int, GlobalizeEntry)> globalize, List<LocalizeEntry> localize})
      retireEndsAt(String agent, int epoch) {
    final globalize = <(int, GlobalizeEntry)>[];
    for (final MapEntry(key: i, value: e) in _globalizeEntries.entries) {
      final h = e.holderEpoch;
      if (e.remoteAgent == agent && h != null && h < epoch) globalize.add((i, e));
    }
    for (final (i, _) in globalize) {
      _globalizeEntries.remove(i);
    }
    final localize = [
      for (final e in _localizeEntries)
        if (e.remoteAgent == agent && e.remoteEpoch < epoch) e
    ];
    _localizeEntries.removeWhere(localize.contains);
    return (globalize: globalize, localize: localize);
  }


  /// Allocate next index without creating an entry.
  ///
  /// Called by Globalize when exporting a reader Y?:
  /// - Allocate index i for use in `_r(p, i)`
  /// - No entry is created (outgoing link handled by global_send)
  ///
  /// IGLP Definition Globalize, reader case: "No entry is created—the
  /// global_send goal handles outgoing communication."
  ///
  /// Returns the allocated index.
  int allocateIndex() {
    return _nextIndex++;
  }

  /// Current index counter (for testing/debugging)
  int get nextIndex => _nextIndex;

  /// Number of GlobalizeEntries currently in the table
  int get globalizeEntryCount => _globalizeEntries.length;

  /// Number of LocalizeEntries currently in the table
  int get localizeEntryCount => _localizeEntries.length;

  /// Write the table into the agent's saved state (IGLP, Implementation
  /// Notes, "Persistence": "the global writers table with the serializer's set
  /// of appended cold calls ... the global-name counter").
  void saveState(StateWriter w) {
    w.uint(_nextIndex);
    w.optCell(_serializerWriterAddr);
    w.uint(_serializerAppended.length);
    for (final (sender, epoch, serial) in _serializerAppended) {
      w.str(sender);
      w.uint(epoch);
      w.uint(serial);
    }
    w.uint(_globalizeEntries.length);
    for (final MapEntry(key: index, value: e) in _globalizeEntries.entries) {
      w.uint(index);
      w.cell(e.writerAddr);
      w.str(e.remoteAgent);
      w.boolean(e.holderEpoch != null);
      if (e.holderEpoch != null) w.uint(e.holderEpoch!);
    }
    w.uint(_localizeEntries.length);
    for (final e in _localizeEntries) {
      w.cell(e.writerAddr);
      w.str(e.remoteAgent);
      w.uint(e.remoteEpoch);
      w.uint(e.remoteIndex);
    }
  }

  /// Read back what [saveState] wrote, into a table fresh from construction.
  void restoreState(StateReader r) {
    _nextIndex = r.uint();
    _serializerWriterAddr = r.optCell();
    _serializerAppended.clear();
    for (var i = r.uint(); i > 0; i--) {
      final sender = r.str();
      final epoch = r.uint();
      _serializerAppended.add((sender, epoch, r.uint()));
    }
    _globalizeEntries.clear();
    for (var i = r.uint(); i > 0; i--) {
      final index = r.uint();
      final writer = r.cell();
      final agent = r.str();
      final holderEpoch = r.boolean() ? r.uint() : null;
      _globalizeEntries[index] = GlobalizeEntry(
          writerAddr: writer, remoteAgent: agent, holderEpoch: holderEpoch);
    }
    _localizeEntries.clear();
    for (var i = r.uint(); i > 0; i--) {
      final writer = r.cell();
      final agent = r.str();
      final epoch = r.uint();
      _localizeEntries.add(LocalizeEntry(
          writerAddr: writer,
          remoteAgent: agent,
          remoteEpoch: epoch,
          remoteIndex: r.uint()));
    }
  }

  @override
  String toString() {
    final buf = StringBuffer('GlobalWritersTable($agentId, nextIndex=$_nextIndex)\n');
    buf.writeln('  Serializer[0]: ${_serializerWriterAddr ?? "(not initialized)"}'
        ', ${_serializerAppended.length} cold calls appended');
    buf.writeln('  GlobalizeEntries:');
    for (final entry in _globalizeEntries.entries) {
      buf.writeln('    [${entry.key}] ${entry.value}');
    }
    buf.writeln('  LocalizeEntries:');
    for (final entry in _localizeEntries) {
      buf.writeln('    $entry');
    }
    return buf.toString();
  }
}
