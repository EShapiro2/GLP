/// Direct bytecode execution — the loaded code image (B2).
///
/// Goal (IGLP `app:code-format`): the engine executes the §cf code-format byte
/// string DIRECTLY. This is the loaded, in-memory form the byte interpreter
/// (B3, `interp.dart`) runs over: the artefact's code section as a raw byte
/// array addressed by a byte program counter, plus the symbol table mapping a
/// `proc` index (and a procedure signature) to its entry byte offset, length,
/// and kind (compiled vs codeless kernel/guard).
///
/// Unlike `wire/artefact.dart`'s `fromBytes`, this does NOT decode procedure
/// bodies into `Op` objects — there is no second in-memory instruction form.
/// Operands are read inline from the byte stream at execution time (FCP model:
/// `switch (*PC++)` with operands fetched by PC-advancing reads).
///
/// The header/interface/symbol-table framing is parsed exactly as
/// `Artefact.toBytes` writes it (§cf-artefact); only the code-body decode is
/// elided. The two readers share `wire/codec.dart` primitives, so the framing
/// stays one source of truth.
library;

import 'dart:typed_data';
import 'package:glp_runtime/wire/artefact.dart'
    show artefactMagic, wireFormatVersion, ArtefactExport;
import 'package:glp_runtime/wire/codec.dart';

/// A symbol-table entry in the loaded image. A `proc` operand indexes the
/// [CodeImage.symbols] list. Compiled symbols carry a byte range into the code
/// section; codeless symbols (runtime kernels / builtin guards) carry none and
/// are bound by name at execution.
class ImageSymbol {
  final String name;
  final int arity;
  final bool compiled;

  /// Byte offset and length into [CodeImage.code] for a compiled symbol; both 0
  /// for a codeless symbol.
  final int codeOffset;
  final int codeLength;

  const ImageSymbol.compiled(
      this.name, this.arity, this.codeOffset, this.codeLength)
      : compiled = true;
  const ImageSymbol.codeless(this.name, this.arity)
      : compiled = false,
        codeOffset = 0,
        codeLength = 0;

  String get signature => '$name/$arity';

  @override
  String toString() => compiled
      ? 'compiled $signature @$codeOffset+$codeLength'
      : 'codeless $signature';
}

/// A loaded code-format module ready for direct byte execution.
///
/// [code] is the concatenated compiled procedure bodies (§cf code section), in
/// symbol-table order; a byte PC indexes it. A compiled symbol's body occupies
/// `code[codeOffset .. codeOffset+codeLength)`. [entryOffsetOf] gives a
/// procedure's first instruction's byte offset; [symbolIndexOf] resolves a
/// signature to its `proc` index.
class CodeImage {
  final String isaVersion;
  final Uint8List hM;
  final String moduleName;
  final String typeDefsText;
  final List<ArtefactExport> exports;
  final List<ImageSymbol> symbols;
  final Uint8List code;

  // Keep the FIRST occurrence of each signature, matching
  // BytecodeProgram.labels (first-occurrence) and the proc indices baked into
  // Spawn/Requeue/Guard operands. A program may carry duplicate signatures
  // (e.g. a user predicate that shadows a self.glp one like merge/3); query
  // entry resolution (entryOffsetOf/symbolIndexOf) must pick the same
  // definition the index operands do, or a goal and its recursive spawn run
  // different procedures.
  late final Map<String, int> _sigToIndex = {
    for (var i = symbols.length - 1; i >= 0; i--) symbols[i].signature: i
  };

  CodeImage({
    required this.isaVersion,
    required this.hM,
    required this.moduleName,
    required this.typeDefsText,
    required this.exports,
    required this.symbols,
    required this.code,
  });

  /// The `proc`-table index of a signature, or null if absent.
  int? symbolIndexOf(String signature) => _sigToIndex[signature];

  /// The symbol at a `proc` index.
  ImageSymbol symbolAt(int index) => symbols[index];

  /// The byte offset of a compiled procedure's first instruction, by signature;
  /// null if the signature is absent or codeless.
  int? entryOffsetOf(String signature) {
    final i = _sigToIndex[signature];
    if (i == null) return null;
    final s = symbols[i];
    return s.compiled ? s.codeOffset : null;
  }

  /// The byte offset just past a compiled procedure's last instruction (its
  /// body end), by signature; null if absent or codeless.
  int? entryEndOf(String signature) {
    final i = _sigToIndex[signature];
    if (i == null) return null;
    final s = symbols[i];
    return s.compiled ? s.codeOffset + s.codeLength : null;
  }

  /// The exported signatures the loader aliases unqualified.
  Set<String> get exportAliases =>
      {for (final e in exports) '${e.name}/${e.arity}'};

  /// Parse a §cf artefact byte string into a runnable image WITHOUT decoding
  /// procedure bodies. Framing matches `Artefact.toBytes` (§cf-artefact).
  static CodeImage fromArtefactBytes(Uint8List bytes) {
    final r = WireReader(bytes);
    // 1. Header
    for (final b in artefactMagic) {
      if (r.u8() != b) throw WireFormatException('bad magic (not GLPW)');
    }
    final ver = r.u8();
    if (ver != wireFormatVersion) {
      throw WireFormatException('unsupported code-format version: $ver');
    }
    final isaVersion = r.string();
    final hM = r.hash();
    final moduleName = r.string();
    // 2. Interface table
    final typeDefsText = r.string();
    final exportCount = r.clen();
    final exports = <ArtefactExport>[];
    for (var i = 0; i < exportCount; i++) {
      final name = r.string();
      final arity = r.clen();
      final declText = r.string();
      exports.add(ArtefactExport(name, arity, declText));
    }
    // 3. Symbol table
    final symCount = r.clen();
    final symbols = <ImageSymbol>[];
    for (var i = 0; i < symCount; i++) {
      final name = r.string();
      final arity = r.clen();
      final kind = r.u8();
      if (kind != 0 && kind != 1) {
        throw WireFormatException('bad symbol kind: $kind');
      }
      if (kind == 0) {
        final off = r.clen();
        final len = r.clen();
        symbols.add(ImageSymbol.compiled(name, arity, off, len));
      } else {
        symbols.add(ImageSymbol.codeless(name, arity));
      }
    }
    // 4. Code section
    final code = r.bytes();
    r.expectEnd();

    return CodeImage(
      isaVersion: isaVersion,
      hM: hM,
      moduleName: moduleName,
      typeDefsText: typeDefsText,
      exports: exports,
      symbols: symbols,
      code: code,
    );
  }
}
