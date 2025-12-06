abstract class Term {}

class ConstTerm implements Term {
  final Object? value;
  ConstTerm(this.value);
  @override
  String toString() => 'Const($value)';
}

class StructTerm implements Term {
  final String functor;
  final List<Term> args;
  StructTerm(this.functor, this.args);
  @override
  String toString() => '$functor(${args.join(",")})';
}

/// Unified variable reference (single-ID system)
/// Replaces the old two-ID WriterTerm/ReaderTerm system
///
/// In the single-ID system:
/// - Each variable has ONE ID (varId)
/// - isReader flag indicates access mode:
///   - false (writer): Can bind the variable
///   - true (reader): Can read or suspend on the variable
class VarRef implements Term {
  final int varId;
  final bool isReader;

  VarRef(this.varId, {required this.isReader});

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

/// Mutable reference to an unbound writer - enables O(1) stream append
///
/// MutualRef holds a mutable pointer to the current "end" of a stream.
/// Multiple goals can share a MutualRef and append to the same stream
/// in constant time, without traversing the stream.
///
/// Usage:
/// - Create with mutual_ref(StreamEnd, Ref) where StreamEnd is unbound writer
/// - Append with stream_append(Ref, Value, NewEnd) - O(1) operation
/// - Close with mutual_ref_close(Ref) to terminate stream with []
///
/// SRSW: MutualRefTerm is treated as ground (can be read multiple times)
class MutualRefTerm implements Term {
  int _currentWriterId;  // varId of current unbound tail
  final int id;          // unique ID for this MutualRef

  static int _nextId = 0;

  MutualRefTerm(this._currentWriterId) : id = _nextId++;

  int get currentWriterId => _currentWriterId;
  set currentWriterId(int varId) => _currentWriterId = varId;

  @override
  String toString() => 'MutualRef#$id(@W$_currentWriterId)';

  @override
  bool operator ==(Object other) =>
      other is MutualRefTerm && other.id == id;

  @override
  int get hashCode => id.hashCode;
}
