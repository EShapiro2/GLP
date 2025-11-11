/// Heap with single ID variable system
/// Aligns with FCP design: one ID per variable, accessed as writer or reader
library;

import 'terms.dart';
import 'cells.dart';

/// Heap: Single ID per variable (FCP-aligned design)
/// One ID per variable, accessed as writer or reader mode
class Heap {
  // Single ID for each variable
  final Map<int, VariableCell> _vars = {};
  int _nextId = 1000;

  // ROQ: reader suspensions (varId -> Set<goalId>)
  final Map<int, Set<int>> _roq = {};

  // Compatibility: Track writer<->reader pairing from old two-ID system
  // In single-ID, we treat the writer ID as the canonical variable ID
  final Map<int, int> _readerToWriter = {};
  final Map<int, int> _writerToReader = {};

  /// Get variable value (null if unbound)
  Term? getValue(int varId) {
    return _vars[varId]?.value;
  }

  /// Check if variable is bound
  bool isBound(int varId) {
    return _vars[varId]?.value != null;
  }

  /// Bind variable to a term
  void bindVariable(int varId, Term value) {
    final cell = _vars[varId];
    if (cell != null && cell.value == null) {
      cell.value = value;
      _processROQ(varId);
    }
  }

  /// Bind variable to constant
  void bindVariableConst(int varId, Object? v) {
    bindVariable(varId, ConstTerm(v));
  }

  /// Bind variable to structure
  void bindVariableStruct(int varId, String f, List<Term> args) {
    bindVariable(varId, StructTerm(f, args));
  }

  /// Process ROQ when variable is bound
  void _processROQ(int varId) {
    final suspended = _roq[varId];
    if (suspended != null) {
      // Wake suspended goals (handled by runtime)
      _roq.remove(varId);
    }
  }

  /// Add goal suspension on reader
  void addSuspension(int varId, int goalId) {
    _roq.putIfAbsent(varId, () => {}).add(goalId);
  }

  /// Get suspended goals for reader
  Set<int>? getSuspensions(int varId) {
    return _roq[varId];
  }

  /// Remove suspension
  void removeSuspension(int varId, int goalId) {
    _roq[varId]?.remove(goalId);
    if (_roq[varId]?.isEmpty ?? false) {
      _roq.remove(varId);
    }
  }

  /// Dereference a term (follow variable chains)
  Term dereference(Term term) {
    if (term is! VarRef) return term;

    Set<int> visited = {};
    Term current = term;

    while (current is VarRef) {
      if (!isBound(current.varId)) {
        return current; // Unbound - stop here
      }

      if (visited.contains(current.varId)) {
        throw StateError('Cycle detected in dereferencing - SRSW violation!');
      }
      visited.add(current.varId);

      current = getValue(current.varId)!;

      // Early termination for ground terms
      if (current is ConstTerm || current is StructTerm) {
        break;
      }
    }
    return current;
  }

  /// Dereference with path compression (FCP optimization)
  Term dereferenceWithCompression(Term term) {
    if (term is! VarRef) return term;

    List<int> path = [];
    Term current = term;

    // Follow chain, recording path
    while (current is VarRef && isBound(current.varId)) {
      path.add(current.varId);
      current = getValue(current.varId)!;
    }

    // Compress path - make intermediate vars point to final value
    if (path.length > 1 && current is! VarRef) {
      for (int varId in path.sublist(0, path.length - 1)) {
        final cell = _vars[varId];
        if (cell != null) {
          cell.dereferencedCache = current;
        }
      }
    }

    return current;
  }

  /// Check if term is ground (no unbound variables)
  bool isGround(Term term) {
    final deref = dereference(term);
    if (deref is VarRef) return false;
    if (deref is ConstTerm) return true;
    if (deref is StructTerm) {
      return deref.args.every((arg) => isGround(arg));
    }
    return true;
  }

  /// Get all variable IDs
  Iterable<int> get allVarIds => _vars.keys;

  /// Get variable count
  int get varCount => _vars.length;

  /// Clear all variables (for testing)
  void clear() {
    _vars.clear();
    _roq.clear();
    _nextId = 1000;
  }

  // ===== COMPATIBILITY LAYER FOR MIGRATION =====
  // These methods provide the old two-ID API during migration
  // In single-ID system: writerId == readerId == varId

  /// Compatibility: Check if writer is bound (writerID == varId)
  
  bool isWriterBound(int writerId) => isBound(writerId);

  /// Compatibility: Get value of writer (writerId == varId)
  
  Term? valueOfWriter(int writerId) => getValue(writerId);

  /// Compatibility: Bind writer to constant (writerId == varId)
  
  void bindWriterConst(int writerId, Object? v) => bindVariableConst(writerId, v);

  /// Compatibility: Bind writer to structure (writerId == varId)
  
  void bindWriterStruct(int writerId, String f, List<Term> args) {
    bindVariableStruct(writerId, f, args);
  }

  /// Compatibility: Get WriterCell for varId
  /// In single-ID from fresh allocation: readerId == writerId == varId
  /// From old two-ID tests: use the pairing maps
  
  WriterCell? writer(int varId) {
    if (_vars.containsKey(varId)) {
      // Check if we have an explicit pairing from old two-ID code
      final readerId = _writerToReader[varId];
      if (readerId != null) {
        return WriterCell(varId, readerId);
      }
      // Otherwise, in pure single-ID system, they're the same
      return WriterCell(varId, varId);
    }
    return null;
  }

  /// Override allocateFreshVar from base Heap
  
  int allocateFreshVar() {
    return _nextId++;
  }

  /// Override allocateFreshPair to return SAME ID for both writer and reader
  /// This is the true single-ID FCP design: one variable, two access modes
  
  (int, int) allocateFreshPair() {
    final varId = allocateFreshVar();
    return (varId, varId);  // Writer and reader use the SAME ID
  }

  /// Override addVariable from base Heap
  
  void addVariable(int varId) {
    _vars[varId] = VariableCell(varId);
  }

  /// Compatibility: addWriter from old two-ID system
  /// In single-ID: just add the variable with the writer's ID
  
  void addWriter(WriterCell w) {
    // Don't call super - we don't use the old writers map
    addVariable(w.writerId);
    // Track the pairing for lookups
    _readerToWriter[w.readerId] = w.writerId;
    _writerToReader[w.writerId] = w.readerId;
  }

  /// Compatibility: addReader from old two-ID system
  /// In single-ID: reader and writer are the same variable
  
  void addReader(ReaderCell r) {
    // Don't call super - we don't use the old readers map
    // The pairing was established by addWriter
    // This is a no-op in single-ID system
  }

  /// Compatibility: Get writer ID for reader ID
  /// In true single-ID system: writerId == readerId == varId
  /// Check if variable exists, return the ID itself
  
  int? writerIdForReader(int readerId) {
    // In single-ID, reader and writer are the same ID
    // Just check if the variable exists
    return _vars.containsKey(readerId) ? readerId : null;
  }
}

/// Single variable cell (replaces WriterCell/ReaderCell)
class VariableCell {
  final int varId;
  Term? value;
  Term? dereferencedCache; // Cache for dereferencing optimization

  VariableCell(this.varId);
}
