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

/// Variable reference - holds heap address only
///
/// Per heap-pointer-architecture-spec.md v3.0:
/// A VarRef contains only an address. The cell's tag (WrtTag or RoTag)
/// determines whether it is a writer or reader. Use heap.isWriter(addr)
/// or heap.isReader(addr) to check.
class VarRef implements Term {
  /// The heap address of this variable reference
  final int addr;

  VarRef(this.addr);

  @override
  String toString() => 'Var@$addr';

  @override
  bool operator ==(Object other) =>
      other is VarRef && other.addr == addr;

  @override
  int get hashCode => addr.hashCode;
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
/// Per heap-pointer-architecture-spec.md v3.0:
/// _currentWriterAddr holds the heap address of the current unbound tail writer.
class MutualRefTerm implements Term {
  int _currentWriterAddr;  // heap address of current unbound tail writer
  final int id;            // unique ID for this MutualRef

  static int _nextId = 0;

  MutualRefTerm(this._currentWriterAddr) : id = _nextId++;

  /// Get/set the current writer address
  int get currentWriterAddr => _currentWriterAddr;
  set currentWriterAddr(int addr) => _currentWriterAddr = addr;

  @override
  String toString() => 'MutualRef#$id(@$_currentWriterAddr)';

  @override
  bool operator ==(Object other) =>
      other is MutualRefTerm && other.id == id;

  @override
  int get hashCode => id.hashCode;
}
