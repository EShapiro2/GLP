/// Global Writers Table for madGLP
///
/// Tracks local writers that await incoming assignments from remote agents.
/// Each entry maps a global name to the local writer that will be assigned
/// when a message arrives.
///
/// See: docs/ma/madGLP-spec.md Section 3
library;

/// Entry created by Globalize (when exporting a writer)
///
/// Stored at index i in the table, corresponds to global name `_w(p, i)`.
/// When agent q sends a message `_w(p, i) := T`, this entry
/// identifies the local writer X that should be assigned.
class GlobalizeEntry {
  /// Local writer address that will be assigned when callback arrives
  final int writerAddr;

  /// Agent who will send the callback
  final String remoteAgent;

  GlobalizeEntry({
    required this.writerAddr,
    required this.remoteAgent,
  });

  @override
  String toString() => 'GlobalizeEntry(writer=$writerAddr, remote=$remoteAgent)';
}

/// Entry created by Localize (when importing a reader global name)
///
/// Must search by (remoteAgent, remoteIndex) to find the entry.
/// When message `_r(p, i) := T` arrives, search for entry matching (p, i).
class LocalizeEntry {
  /// Local writer address that will be assigned
  final int writerAddr;

  /// Agent who created the global name (p in `_r(p, i)`)
  final String remoteAgent;

  /// Index from the remote agent's global name (i in `_r(p, i)`)
  final int remoteIndex;

  LocalizeEntry({
    required this.writerAddr,
    required this.remoteAgent,
    required this.remoteIndex,
  });

  @override
  String toString() =>
      'LocalizeEntry(writer=$writerAddr, remote=$remoteAgent, index=$remoteIndex)';
}

/// Global Writers Table (W_p)
///
/// Tracks writers awaiting incoming assignments from remote agents.
/// Two entry types are stored:
/// - GlobalizeEntry: created by Globalize, direct index lookup
/// - LocalizeEntry: created by Localize, search by (agent, index)
///
/// Index 0 is reserved for the network input serializer (spec Section 4.1).
/// This entry is permanent and supports many-to-one cold-call reception.
///
/// See: docs/ma/madGLP-spec.md Section 3
class GlobalWritersTable {
  /// Agent ID that owns this table
  final String agentId;

  /// Single counter for index allocation (spec Section 3.2)
  /// Shared across both Globalize and Localize operations.
  /// Index 0 is reserved for the serializer, so counter starts at 1.
  /// Indices are never reused.
  int _nextIndex = 1;

  /// Serializer entry at index 0 (spec Section 4.1)
  /// Maps to the local writer for the network input stream.
  /// This entry is permanent - never removed.
  int? _serializerWriterAddr;

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
  /// Spec Section 4.1: "At boot time, each agent p creates a permanent entry
  /// at index 0 mapping `_r(p, 0)` to the local writer N_p for p's network
  /// input stream."
  void initializeSerializerEntry(int netInWriterAddr) {
    if (_serializerWriterAddr != null) {
      throw StateError('Serializer entry already initialized');
    }
    _serializerWriterAddr = netInWriterAddr;
  }

  /// Get the current serializer writer address.
  ///
  /// Returns null if not yet initialized.
  int? get serializerWriterAddr => _serializerWriterAddr;

  /// Update the serializer writer to a fresh address.
  ///
  /// Called by Receive transaction after binding the current writer
  /// to extend the network input stream.
  ///
  /// Spec Section 8.3: "Assign N_q := [T_q↓ | N'_q] where N'_q is a fresh
  /// writer. Update the entry to `(N'_q, *)` at index 0."
  void updateSerializerWriter(int newWriterAddr) {
    if (_serializerWriterAddr == null) {
      throw StateError('Cannot update serializer: not initialized');
    }
    _serializerWriterAddr = newWriterAddr;
  }

  /// Check if the serializer entry is initialized.
  bool get hasSerializerEntry => _serializerWriterAddr != null;

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
  int addGlobalizeEntry(int writerAddr, String remoteAgent) {
    final index = _nextIndex++;
    _globalizeEntries[index] = GlobalizeEntry(
      writerAddr: writerAddr,
      remoteAgent: remoteAgent,
    );
    return index;
  }

  /// Add LocalizeEntry.
  ///
  /// Called by Localize when importing `_r(p, i)`:
  /// - Create fresh pair (Z, Z?)
  /// - Add entry (Z, p, i)
  /// - Replace `_r(p, i)` with Z? in localized term
  ///
  /// Throws [ArgumentError] if duplicate (remoteAgent, remoteIndex) exists.
  void addLocalizeEntry(int writerAddr, String remoteAgent, int remoteIndex) {
    // Check for duplicates (spec Section 12: Entry Lifecycle invariant)
    final existing = findByRemote(remoteAgent, remoteIndex);
    if (existing != null) {
      throw ArgumentError(
        'Duplicate LocalizeEntry for ($remoteAgent, $remoteIndex): '
        'existing writer=${existing.writerAddr}, new writer=$writerAddr',
      );
    }

    _localizeEntries.add(LocalizeEntry(
      writerAddr: writerAddr,
      remoteAgent: remoteAgent,
      remoteIndex: remoteIndex,
    ));
  }

  /// Lookup GlobalizeEntry by index.
  ///
  /// Used when receiving `_w(p, i) := T` where we are agent p.
  /// The entry at index i identifies the local writer to assign.
  GlobalizeEntry? lookupByIndex(int index) {
    return _globalizeEntries[index];
  }

  /// Search for LocalizeEntry matching remote (agent, index).
  ///
  /// Used when receiving `_r(p, i) := T`:
  /// - Search for entry with remoteAgent=p and remoteIndex=i
  /// - That entry's writerAddr is the local writer to assign
  LocalizeEntry? findByRemote(String agent, int index) {
    for (final entry in _localizeEntries) {
      if (entry.remoteAgent == agent && entry.remoteIndex == index) {
        return entry;
      }
    }
    return null;
  }

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
      // Spec Section 4.1: "This entry is never removed."
      return;
    }
    _globalizeEntries.remove(index);
  }

  /// Remove LocalizeEntry by remote agent and index.
  ///
  /// Called after receiving `_r(p, i) := T` and assigning the writer.
  ///
  /// Safe to call on non-existent entry (idempotent).
  void removeLocalizeEntry(String agent, int index) {
    _localizeEntries.removeWhere(
      (e) => e.remoteAgent == agent && e.remoteIndex == index,
    );
  }


  /// Allocate next index without creating an entry.
  ///
  /// Called by Globalize when exporting a reader Y?:
  /// - Allocate index i for use in `_r(p, i)`
  /// - No entry is created (outgoing link handled by global_send)
  ///
  /// Spec Section 5.1: "No entry is created—the global_send goal handles
  /// outgoing communication."
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

  @override
  String toString() {
    final buf = StringBuffer('GlobalWritersTable($agentId, nextIndex=$_nextIndex)\n');
    buf.writeln('  Serializer[0]: ${_serializerWriterAddr ?? "(not initialized)"}');
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
