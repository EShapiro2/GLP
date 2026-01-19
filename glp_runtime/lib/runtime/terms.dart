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

/// Variable reference - holds varId and isReader flag
///
/// Migration notes:
/// - During Phase 1, varId IS the writerAddr (address of writer cell)
/// - The isReader flag is stored explicitly for backward compatibility
/// - The addr getter computes the heap address from (varId, isReader)
/// - After migration completes, VarRef can be simplified to hold just addr
class VarRef implements Term {
  /// The variable ID (which equals writerAddr during migration)
  final int varId;
  
  /// Whether this reference is to the reader cell
  final bool isReader;

  /// Primary constructor
  VarRef(this.varId, {required this.isReader});

  /// Get the heap address
  /// During migration: writerAddr for writer, writerAddr+1 for reader
  int get addr => isReader ? varId + 1 : varId;

  /// Alternate constructor from heap address (for migrated code)
  /// Requires knowing whether the address is writer or reader
  VarRef.fromAddr(int address, {required bool reader})
      : varId = reader ? address - 1 : address,
        isReader = reader;

  @override
  String toString() => isReader ? 'R$varId?' : 'W$varId';

  @override
  bool operator ==(Object other) =>
      other is VarRef && other.varId == varId && other.isReader == isReader;

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
///
/// Migration note: During Phase 1, _currentWriterId holds the writerAddr.
class MutualRefTerm implements Term {
  int _currentWriterId;  // heap address of current unbound tail writer
  final int id;          // unique ID for this MutualRef

  static int _nextId = 0;

  MutualRefTerm(this._currentWriterId) : id = _nextId++;

  /// Get/set the current writer ID (which equals writerAddr during migration)
  int get currentWriterId => _currentWriterId;
  set currentWriterId(int varId) => _currentWriterId = varId;

  /// Alias for clarity during migration
  int get currentWriterAddr => _currentWriterId;
  set currentWriterAddr(int addr) => _currentWriterId = addr;

  @override
  String toString() => 'MutualRef#$id(@W$_currentWriterId)';

  @override
  bool operator ==(Object other) =>
      other is MutualRefTerm && other.id == id;

  @override
  int get hashCode => id.hashCode;
}
