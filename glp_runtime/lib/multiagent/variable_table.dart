/// Variable Table (V_p) for irmaGLP
/// 
/// Tracks variables whose paired counterparts are non-local.
/// 
/// Core Invariant: V_p contains exactly those variables whose paired 
/// counterparts are non-local (i.e., in a remote agent's resolvent).
/// 
/// Specification: /docs/ma/irmaGLP-spec.md Section 3.2
library;

/// Role of a variable in the variable table
enum VariableRole {
  /// We hold writer X, paired reader X? is remote
  writer,
  
  /// We created reader X? (and paired writer X), writer is remote
  createdReader,
  
  /// We received reader X? from another agent
  importedReader,
}

/// Entry in the variable table
class VariableEntry {
  /// Variable ID (local heap ID)
  final int varId;
  
  /// Agent who created this variable
  final String creator;
  
  /// Role of this variable
  final VariableRole role;
  
  /// State depends on role:
  /// - Writer: bound value (dynamic) or null if unbound
  /// - Created reader: requester agent ID (String) or null if no request
  /// - Imported reader: creator ID (String) if request sent, null otherwise
  dynamic state;
  
  VariableEntry({
    required this.varId,
    required this.creator,
    required this.role,
    this.state,
  });
  
  @override
  String toString() {
    return 'VarEntry($varId, creator=$creator, role=$role, state=$state)';
  }
}

/// Variable Table (V_p) for tracking non-local variables
/// 
/// Maintains the invariant: V_p contains exactly those variables whose
/// paired counterparts are non-local.
class VariableTable {
  /// Current agent ID
  final String agentId;
  
  /// Map from varId to entry
  final Map<int, VariableEntry> _entries = {};
  
  VariableTable(this.agentId);
  
  /// Add a variable entry to the table
  /// 
  /// INVARIANT: For writer entries, creator must equal agentId.
  /// Writers are never imported - only readers can be imported.
  void add(int varId, VariableEntry entry) {
    // Enforce writer invariant
    if (entry.role == VariableRole.writer && entry.creator != agentId) {
      throw AssertionError(
        'INVARIANT VIOLATION: Writer entry must have creator = $agentId, '
        'got creator = ${entry.creator} for varId $varId'
      );
    }
    
    _entries[varId] = entry;
  }
  
  /// Remove a variable entry from the table
  void remove(int varId) {
    _entries.remove(varId);
  }
  
  /// Lookup a variable entry by ID
  VariableEntry? lookup(int varId) {
    return _entries[varId];
  }
  
  /// Get all entries created by a specific agent
  List<VariableEntry> getByCreator(String creator) {
    return _entries.values
        .where((entry) => entry.creator == creator)
        .toList();
  }
  
  /// Update the state of an existing entry
  /// 
  /// Throws if entry doesn't exist.
  void updateState(int varId, dynamic newState) {
    final entry = _entries[varId];
    if (entry == null) {
      throw ArgumentError('Variable $varId not in table');
    }
    entry.state = newState;
  }
  
  /// Check if a variable is in the table
  bool contains(int varId) {
    return _entries.containsKey(varId);
  }
  
  /// Get all variable IDs in the table
  Iterable<int> get varIds => _entries.keys;
  
  /// Get all entries
  Iterable<VariableEntry> get entries => _entries.values;
  
  /// Number of entries
  int get length => _entries.length;
  
  /// Check if table is empty
  bool get isEmpty => _entries.isEmpty;
  
  /// Check if table is not empty
  bool get isNotEmpty => _entries.isNotEmpty;
  
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
