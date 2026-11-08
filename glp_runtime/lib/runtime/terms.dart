import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

abstract class Term {}

/// The empty list `[]`, a constant of its own and no string: GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax" ("a constant (numbers,
/// strings, or the empty list `[]`)"), and IGLP's code format, which gives it
/// a constant tag of its own (constant tag 0 nil; strings tag 3).  [nil] is
/// the runtime's one value of it, held as a [ConstTerm]'s value.  Its type is
/// `String` (TGLP appendix-root-self.tex: "The empty list is a String, hence
/// a Constant"), so the `string` and `constant` guards hold of it; as a value
/// it equals no string.  Until 2026-10-07 the runtime held `[]` as the string
/// 'nil', so `X = nil.` showed `[]` and `nil =?= []` succeeded.
final class Nil {
  const Nil._();

  /// Every [Nil] is the one empty list, a copy made in passing a term to
  /// another isolate among them.
  @override
  bool operator ==(Object other) => other is Nil;

  @override
  int get hashCode => 0x5B5D; // "[]"

  /// `[]`, as the reader reads it.
  @override
  String toString() => '[]';
}

/// The empty list, the one value of [Nil].
const Nil nil = Nil._();

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

  /// The ground mark (IGLP appendix-implementation-notes.tex, "Ground mark"):
  /// set when a ground test finds this structure ground on the heap's
  /// bindings, a list cell or a compound term, after which a ground test of it
  /// succeeds at once.  A term that is ground stays ground, every variable
  /// being assigned once, so the mark is never cleared.  A copy of the term,
  /// as a message to another isolate makes, starts unmarked.
  bool groundMark = false;

  /// `functor(arg,...)`, each argument as its own toString gives it.  The
  /// text is written with a stack of its own, a frame for each structure
  /// being written, piece by piece in the order `'$functor(${args.join(",")})'`
  /// wrote it: until 2026-10-02 that recursed once a structure argument, and
  /// the madGLP traces, whose text is made of every term a message carries
  /// whether a trace is on or not, overflowed the Dart stack on a long list.
  @override
  String toString() {
    final out = StringBuffer();
    // Each frame: a structure being written, and its next argument's index.
    final frames = <(StructTerm, int)>[];
    void open(StructTerm s) {
      out
        ..write(s.functor)
        ..write('(');
      frames.add((s, 0));
    }

    open(this);
    while (frames.isNotEmpty) {
      final (s, i) = frames.removeLast();
      if (i == s.args.length) {
        out.write(')');
        continue;
      }
      if (i > 0) out.write(',');
      frames.add((s, i + 1));
      final a = s.args[i];
      if (a is StructTerm) {
        open(a);
      } else {
        out.write(a);
      }
    }
    return out.toString();
  }
}

/// Variable reference - holds heap address only
///
/// Per IGLP app:in-heap, Variable pairs:
/// A variable's reader/writer identity is determined by its heap cell tag
/// (RoTag or WrtTag), NOT by address arithmetic. Use heap.isWriter(addr)
/// or heap.isReader(addr) to check.
///
/// MUST NOT: Code must not assume reader_addr == writer_addr + 1 or
/// derive reader/writer identity from address parity.
class VarRef implements Term {
  /// The cell of this variable occurrence: the cell itself, a reference and
  /// not an address (IGLP app:in-heap, Variable pairs).
  final HeapCell addr;

  VarRef(this.addr);

  // The cell's tag gives its polarity (IGLP app:in-heap, Variable pairs):
  // heap.isReader(addr) checks it, and the cell is the identifier.

  @override
  String toString() => 'Var@$addr';

  @override
  bool operator ==(Object other) =>
      other is VarRef && identical(other.addr, addr);

  @override
  int get hashCode => addr.id;
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
/// _currentWriterAddr holds the heap address of the current unbound tail writer.
class MutualRefTerm implements Term {
  HeapCell _currentWriterAddr;  // the cell of the current unbound tail writer
  final int id;            // unique ID for this MutualRef

  static int _nextId = 0;

  MutualRefTerm(this._currentWriterAddr) : id = _nextId++;

  /// A MutualRef of a restarted agent, under the id it had when its state was
  /// saved (saved_state.dart); later ones are numbered past it.
  MutualRefTerm.restored(this._currentWriterAddr, this.id) {
    if (_nextId <= id) _nextId = id + 1;
  }

  /// Get/set the current writer address
  HeapCell get currentWriterAddr => _currentWriterAddr;
  set currentWriterAddr(HeapCell addr) => _currentWriterAddr = addr;

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

  /// The module's own directory, which compilation assigns it (GLP-Spec
  /// appendix-guards, "Compilation and file reading": the module's path from
  /// the root): where `load_file/2` resolves a name, and the caller can neither
  /// escape it nor choose otherwise. Null for a module that arrived as a value
  /// or was read from a file, whose artefact names no directory.
  final String? directory;

  ModuleTerm(this.artefact,
      {this.name = '', this.declaredTypes, this.directory});

  @override
  String toString() => 'Module($name)';
}

/// A global name in the place of a variable: the runtime's form of a variable
/// in a term that crosses agents, between its globalization by the sender and
/// its localization by the receiver (IGLP Definitions Globalize and Localize).
/// It is the code format's tag-2 variable, polarity, agent and index (IGLP
/// code-format-fragment.tex, Terms, tag 2), and it never stands for a variable
/// in a resolvent (IGLP madglp-spec.tex, Global Variable Names).
///
/// It is no structure.  "A ground structure `_w(P, I)` or `_r(P, I)` that a
/// program puts in a message is a term, encoded as a structure and arriving
/// as one ...; only a variable is encoded as a global name" (IGLP
/// madglp-spec.tex:17, at IGLP 1e01638).  So a program's ground `'_w'(p, 2)`,
/// a [StructTerm], is never taken for this name.  Until 2026-11-02 the
/// globalized variable was the structure `'_w'(p, i)` itself, and a program's
/// structure of that form was sent as a tag-2 variable and localized.
class GlobalNameTerm implements Term {
  /// The polarity: a reader `_r(p, i)`, or a writer `_w(p, i)`.
  final bool isReader;

  /// The allocating agent p, the link's anchor.
  final String agent;

  /// The index i, allocated by p.
  final int index;

  GlobalNameTerm(
      {required this.isReader, required this.agent, required this.index});

  @override
  String toString() => isReader ? '_r($agent, $index)' : '_w($agent, $index)';

  @override
  bool operator ==(Object other) =>
      other is GlobalNameTerm &&
      other.isReader == isReader &&
      other.agent == agent &&
      other.index == index;

  @override
  int get hashCode => Object.hash(isReader, agent, index);
}
