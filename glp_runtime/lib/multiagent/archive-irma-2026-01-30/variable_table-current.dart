/// Variable Table (V_p) for irmaGLP
/// 
/// Tracks variables whose paired counterparts are non-local.
/// 
/// Core Invariant: V_p contains exactly those variables whose paired 
/// counterparts are non-local (i.e., in a remote agent's resolvent).
/// 
/// Per spec, V_p ⊆ 𝒱 × Π × (𝒯 ∪ Π ∪ {⊥}) × 𝒮* where 𝒱 includes both X (writers)
/// and X? (readers) as distinct elements. The fourth component Σ is the
/// suspension list for goals waiting on this variable. For imported readers,
/// V_p serves as the "virtual writer" that holds suspensions since there is
/// no local writer cell.
/// 
/// Specification: /docs/ma/irmaGLP-spec.md Section 3.1.2
library;

import '../runtime/suspension.dart';
import '../runtime/terms.dart';

/// Key for variable table entries
/// 
/// Per spec, X and X? are distinct elements of 𝒱. A single heap variable ID
/// can have separate entries for its reader and writer views.
class VarKey {
  final int varId;
  final bool isReader;
  
  const VarKey(this.varId, this.isReader);
  
  @override
  bool operator ==(Object other) =>
      other is VarKey && other.varId == varId && other.isReader == isReader;
  
  @override
  int get hashCode => Object.hash(varId, isReader);
  
  @override
  String toString() => isReader ? 'R$varId?' : 'W$varId';
}

/// Role of a variable in the variable table
enum VariableRole {
  /// We created and hold writer X, paired reader X? is remote
  createdWriter,
  
  /// We received writer X from another agent, paired reader X? is remote
  importedWriter,
  
  /// We created reader X? (and paired writer X), writer is remote
  createdReader,
  
  /// We received reader X? from another agent
  importedReader,
}

/// Entry in the variable table
///
/// Uses typed fields for state management per Issue 12 of audit:
/// - [requester]: Who is waiting for this variable's value (for created variables)
/// - [requestSent]: Whether a read request has been sent (for imported readers)
/// - [boundValue]: Cached bound value (for any role)
class VariableEntry {
  /// Variable ID (local heap ID)
  final int varId;

  /// Whether this entry is for the reader view (true) or writer view (false)
  final bool isReader;

  /// Agent who created this variable
  final String creator;

  /// Creator's original local ID for this variable
  ///
  /// For imported variables, this is the creator's varId that we need to use
  /// when sending messages back to the creator. For created variables, this
  /// is the same as varId.
  final int creatorLocalId;

  /// Role of this variable
  final VariableRole role;

  /// Who is waiting for the value of this variable
  ///
  /// - For createdWriter: the agent who sent a read request for the paired reader
  /// - For createdReader: the agent who sent a read request for this reader
  /// - Not used for imported variables (they don't receive requests)
  String? requester;

  /// Whether a read request has been sent for this imported reader
  ///
  /// Only meaningful for importedReader role. When true, indicates we have
  /// sent a read request to the creator and are waiting for an assignment.
  bool requestSent;

  /// Cached bound value
  ///
  /// - For createdWriter/importedWriter: the value this writer was bound to
  /// - For createdReader: the value received from assignment (before forwarding)
  /// - For importedReader: not used (entry is removed when assignment arrives)
  Term? boundValue;

  /// For imported writers: the creator's local ID of the paired reader.
  ///
  /// Per spec Section 5.3, assignments are addressed to the reader (W?:=T).
  /// When an imported writer is bound, we need to send the assignment using
  /// the reader's creatorLocalId, not the writer's. This field stores that.
  ///
  /// Only meaningful for importedWriter role.
  int? pairedReaderCreatorLocalId;

  /// Suspension list for goals waiting on this variable (Σ in spec).
  ///
  /// For imported readers, V_p serves as the "virtual writer" since there
  /// is no local writer cell to hold suspensions. When an assignment arrives,
  /// goals are resumed from this list.
  SuspensionListNode? suspensions;

  VariableEntry({
    required this.varId,
    required this.isReader,
    required this.creator,
    required this.role,
    int? creatorLocalId,
    this.requester,
    this.requestSent = false,
    this.boundValue,
  }) : creatorLocalId = creatorLocalId ?? varId;

  /// Get the key for this entry
  VarKey get key => VarKey(varId, isReader);

  @override
  String toString() {
    final keyStr = isReader ? 'R$varId?' : 'W$varId';
    final creatorIdStr = creatorLocalId != varId ? ', creatorLocalId=$creatorLocalId' : '';
    final stateStr = _stateString();
    return 'VarEntry($keyStr, creator=$creator, role=$role$creatorIdStr$stateStr)';
  }

  String _stateString() {
    final parts = <String>[];
    if (requester != null) parts.add('requester=$requester');
    if (requestSent) parts.add('requestSent');
    if (boundValue != null) parts.add('boundValue=$boundValue');
    return parts.isEmpty ? '' : ', ${parts.join(', ')}';
  }
}

/// Variable Table (V_p) for tracking non-local variables
/// 
/// Maintains the invariant: V_p contains exactly those variables whose
/// paired counterparts are non-local.
/// 
/// Per spec Section 3.1.2, V_p can contain both (X, p, s) for a created writer
/// and (X?, p, s) for a created reader with the same underlying varId.
class VariableTable {
  /// Current agent ID
  final String agentId;
  
  /// Map from (varId, isReader) to entry
  final Map<VarKey, VariableEntry> _entries = {};
  
  VariableTable(this.agentId);
  
  /// Add a variable entry to the table
  void add(VarKey key, VariableEntry entry) {
    _entries[key] = entry;
  }
  
  /// Remove a variable entry from the table
  void remove(VarKey key) {
    _entries.remove(key);
  }
  
  /// Lookup a variable entry by key
  VariableEntry? lookup(VarKey key) {
    return _entries[key];
  }
  
  /// Lookup by varId and isReader separately
  VariableEntry? lookupByIdAndRole(int varId, bool isReader) {
    return _entries[VarKey(varId, isReader)];
  }
  
  /// Get all entries created by a specific agent
  List<VariableEntry> getByCreator(String creator) {
    return _entries.values
        .where((entry) => entry.creator == creator)
        .toList();
  }
  
  /// Update the requester of an existing entry
  ///
  /// For created writers/readers: sets who is waiting for the value.
  /// Throws if entry doesn't exist.
  void updateRequester(VarKey key, String? requester) {
    final entry = _entries[key];
    if (entry == null) {
      throw ArgumentError('Variable $key not in table');
    }
    entry.requester = requester;
  }

  /// Mark that a read request has been sent for an imported reader
  ///
  /// Throws if entry doesn't exist.
  void markRequestSent(VarKey key) {
    final entry = _entries[key];
    if (entry == null) {
      throw ArgumentError('Variable $key not in table');
    }
    entry.requestSent = true;
  }

  /// Update the bound value of an existing entry
  ///
  /// Throws if entry doesn't exist.
  void updateBoundValue(VarKey key, Term? value) {
    final entry = _entries[key];
    if (entry == null) {
      throw ArgumentError('Variable $key not in table');
    }
    entry.boundValue = value;
  }
  
  /// Check if a variable key is in the table
  bool contains(VarKey key) {
    return _entries.containsKey(key);
  }
  
  /// Check if varId with given isReader is in table
  bool containsByIdAndRole(int varId, bool isReader) {
    return _entries.containsKey(VarKey(varId, isReader));
  }
  
  /// Get all keys in the table
  Iterable<VarKey> get keys => _entries.keys;
  
  /// Get all entries
  Iterable<VariableEntry> get entries => _entries.values;
  
  /// Number of entries
  int get length => _entries.length;
  
  /// Check if table is empty
  bool get isEmpty => _entries.isEmpty;
  
  /// Check if table is not empty
  bool get isNotEmpty => _entries.isNotEmpty;
  
  /// Find entry by creator's global ID (creator + creatorLocalId)
  /// 
  /// Used when receiving assignment messages that contain global IDs
  /// but we need to find our local varId for heap binding.
  VariableEntry? findByCreatorLocalId(String creator, int creatorLocalId, {required bool isReader}) {
    for (final entry in _entries.values) {
      if (entry.creator == creator && 
          entry.creatorLocalId == creatorLocalId && 
          entry.isReader == isReader) {
        return entry;
      }
    }
    return null;
  }
  
  /// Clear all entries
  void clear() {
    _entries.clear();
  }
  
  @override
  String toString() {
    if (_entries.isEmpty) {
      return 'VariableTable($agentId): empty';
    }
    
    final buffer = StringBuffer('VariableTable($agentId):\n');
    for (final entry in _entries.values) {
      buffer.writeln('  $entry');
    }
    return buffer.toString();
  }
}
