/// New heap with single ID variable system
/// Aligns with FCP design: one ID per variable, accessed as writer or reader
library;

import 'terms.dart';

/// Heap V2: Single ID per variable
/// Replaces dual Writer/Reader ID system with unified approach
class HeapV2 {
  // Single ID for each variable
  final Map<int, VariableCell> _vars = {};
  int _nextId = 1000;

  // ROQ: reader suspensions (varId -> Set<goalId>)
  final Map<int, Set<int>> _roq = {};

  /// Allocate a fresh variable (returns single ID)
  int allocateFreshVar() {
    return _nextId++;
  }

  /// Add variable cell to heap
  void addVariable(int varId) {
    _vars[varId] = VariableCell(varId);
  }

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
}

/// Single variable cell (replaces WriterCell/ReaderCell)
class VariableCell {
  final int varId;
  Term? value;
  Term? dereferencedCache; // Cache for dereferencing optimization

  VariableCell(this.varId);
}

/// Unified variable reference (replaces WriterTerm/ReaderTerm)
class VarRef implements Term {
  final int varId;
  final bool isReader;

  VarRef(this.varId, this.isReader);

  @override
  String toString() => isReader ? 'R$varId?' : 'W$varId';

  @override
  bool operator ==(Object other) =>
      other is VarRef &&
      other.varId == varId &&
      other.isReader == isReader;

  @override
  int get hashCode => Object.hash(varId, isReader);
}
