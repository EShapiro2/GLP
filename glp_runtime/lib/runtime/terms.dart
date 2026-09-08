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
/// Per irmaGLP-spec.md Section 3.2.1:
/// A variable's reader/writer identity is determined by its heap cell tag
/// (RoTag or WrtTag), NOT by address arithmetic. Use heap.isWriter(addr)
/// or heap.isReader(addr) to check.
///
/// MUST NOT: Code must not assume reader_addr == writer_addr + 1 or
/// derive reader/writer identity from address parity.
class VarRef implements Term {
  /// The heap address of this variable reference
  final int addr;

  VarRef(this.addr);

  // NOTE: isReader and varId computed properties have been REMOVED per
  // irmaGLP-spec.md Section 3.2.1. Use heap.isReader(addr) to check type
  // and raw addr as the identifier.

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

/// Module value — an app's compiled artefact: h(M) and code.
///
/// The `Module` constant of the type system. Per IGLP appendix §Self-Module,
/// the Module constant carries the artefact — h(M) and code — not code alone:
/// the adopter checks h(M) against the offer and then runs the code, and code
/// alone has no h(M). A goal carries the module value of the app it belongs to;
/// `self_module`/1 returns it and `run`/2 launches a goal on it. Ground, and
/// stored opaquely on the heap, following FCP's module-as-value convention.
class ModuleTerm implements Term {
  /// This module's compiled artefact: body (interface, symbol table, code) and
  /// certificate (compiler's key, source identity h(M), compiled identity).
  final Object artefact;  // Artefact (untyped to avoid a circular import)

  /// Module name (for display/debugging)
  final String name;

  /// The module's declared type-identity table (TGLP, Implementation Notes,
  /// "The tables"): every procedure declared in the module's scope, keyed
  /// `p/n` as the compiled module carries it. What `find_type/2` reads. Set by
  /// the compiler at load; null for a module that arrived as a value, whose
  /// artefact carries its interface and not its scope.
  final Object? declaredTypes;  // TypeIdentityTables (untyped, same reason)

  /// The exported table `run/3` compares against, derived on first use from
  /// the artefact's interface text and cached here (a shipped table can
  /// disagree with the source it describes; one recomputed from it cannot).
  Object? exportedTypesCache;  // TypeIdentityTables

  ModuleTerm(this.artefact, {this.name = '', this.declaredTypes});

  @override
  String toString() => 'Module($name)';
}
