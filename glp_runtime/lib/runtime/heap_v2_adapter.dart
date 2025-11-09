/// Adapter that presents Heap interface but uses HeapV2 internally
///
/// This adapter allows HeapV2 to be used with the existing bytecode runner
/// and runtime without modifying production code. It maintains the dual
/// writer/reader ID API while using single variable IDs internally.
///
/// Performance: Should maintain >90% of direct HeapV2 performance, with
/// small overhead from ID mapping.
library;

import 'heap.dart';
import 'heap_v2.dart';
import 'cells.dart';
import 'terms.dart';

class HeapV2Adapter extends Heap {
  final HeapV2 _v2 = HeapV2();

  // Mapping between old API (separate writer/reader IDs) and new API (single var IDs)
  final Map<int, int> _writerToVar = {};
  final Map<int, int> _readerToVar = {};
  final Map<int, int> _varToWriter = {}; // Reverse mapping
  final Map<int, int> _varToReader = {}; // Reverse mapping

  int _nextSyntheticId = 1000; // Synthetic IDs for writer/reader

  @override
  (int, int) allocateFreshPair() {
    // Allocate single variable in V2
    final varId = _v2.allocateFreshVar();
    _v2.addVariable(varId);

    // Create synthetic writer/reader IDs for compatibility
    final writerId = _nextSyntheticId++;
    final readerId = _nextSyntheticId++;

    // Maintain bidirectional mappings
    _writerToVar[writerId] = varId;
    _readerToVar[readerId] = varId;
    _varToWriter[varId] = writerId;
    _varToReader[varId] = readerId;

    return (writerId, readerId);
  }

  @override
  void addWriter(WriterCell w) {
    super.addWriter(w); // Keep parent's writer map for compatibility

    // If this writer isn't mapped yet, map it
    if (!_writerToVar.containsKey(w.writerId)) {
      // Add variable if it doesn't exist
      // Use getValue to check existence (returns null if not exists)
      if (_v2.getValue(w.writerId) == null && !_v2.isBound(w.writerId)) {
        _v2.addVariable(w.writerId);
      }
      _writerToVar[w.writerId] = w.writerId;
      _varToWriter[w.writerId] = w.writerId;
    }
  }

  @override
  void addReader(ReaderCell r) {
    super.addReader(r); // Keep parent's reader map for compatibility

    // Map reader to same variable as its paired writer
    final writerId = writerIdForReader(r.readerId);
    if (writerId != null) {
      final varId = _writerToVar[writerId];
      if (varId != null) {
        _readerToVar[r.readerId] = varId;
        _varToReader[varId] = r.readerId;
      }
    }
  }

  @override
  bool isWriterBound(int writerId) {
    final varId = _writerToVar[writerId];
    if (varId == null) {
      return super.isWriterBound(writerId); // Fall back to parent
    }
    return _v2.isBound(varId);
  }

  @override
  Term? valueOfWriter(int writerId) {
    final varId = _writerToVar[writerId];
    if (varId == null) {
      return super.valueOfWriter(writerId); // Fall back to parent
    }

    final value = _v2.getValue(varId);
    if (value == null) return null;

    // Convert VarRef back to WriterTerm/ReaderTerm for compatibility
    return _convertFromV2(value);
  }

  @override
  void bindWriterConst(int writerId, Object? v) {
    final varId = _writerToVar[writerId];
    if (varId == null) {
      super.bindWriterConst(writerId, v); // Fall back to parent
      return;
    }

    _v2.bindVariableConst(varId, v);

    // Also update parent's map for compatibility
    super.bindWriterConst(writerId, v);
  }

  @override
  void bindWriterStruct(int writerId, String f, List<Term> args) {
    final varId = _writerToVar[writerId];
    if (varId == null) {
      super.bindWriterStruct(writerId, f, args); // Fall back to parent
      return;
    }

    // Convert args from old format to V2 format
    final v2Args = args.map((arg) => _convertToV2(arg)).toList();
    _v2.bindVariableStruct(varId, f, v2Args);

    // Also update parent's map for compatibility
    super.bindWriterStruct(writerId, f, args);
  }

  /// Convert old Term types to V2 VarRef
  Term _convertToV2(Term term) {
    if (term is WriterTerm) {
      final varId = _writerToVar[term.writerId];
      if (varId != null) {
        return VarRef(varId, false);
      }
    } else if (term is ReaderTerm) {
      final varId = _readerToVar[term.readerId];
      if (varId != null) {
        return VarRef(varId, true);
      }
    } else if (term is StructTerm) {
      // Recursively convert structure arguments
      final v2Args = term.args.map((arg) => _convertToV2(arg)).toList();
      return StructTerm(term.functor, v2Args);
    }
    // ConstTerm and others pass through unchanged
    return term;
  }

  /// Convert V2 VarRef back to old WriterTerm/ReaderTerm
  Term _convertFromV2(Term term) {
    if (term is VarRef) {
      if (term.isReader) {
        final readerId = _varToReader[term.varId];
        if (readerId != null) {
          return ReaderTerm(readerId);
        }
      } else {
        final writerId = _varToWriter[term.varId];
        if (writerId != null) {
          return WriterTerm(writerId);
        }
      }
      // If no mapping exists, return as-is (shouldn't happen)
      return term;
    } else if (term is StructTerm) {
      // Recursively convert structure arguments
      final oldArgs = term.args.map((arg) => _convertFromV2(arg)).toList();
      return StructTerm(term.functor, oldArgs);
    }
    // ConstTerm and others pass through unchanged
    return term;
  }

  // Performance metrics for validation
  int get v2VarCount => _v2.varCount;
  int get mappingOverhead => _writerToVar.length + _readerToVar.length;
}
