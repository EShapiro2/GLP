/// GLP module artefact + loader (D3 wire format, §§5, 7).
///
/// Normative source: the IGLP paper appendix `app:wire-format`, §wf-artefact
/// and §wf-loader. The artefact is the byte string a compiler produces from a
/// flattened (statically-linked, pruned) module; its sections, in order:
///
///   1. Header        — magic `GLPW`, u8 wire-format version, string ISA
///                      version, 32-byte h(M), string module name.
///   2. Interface     — string (reachable type definitions, canonical print);
///                      clen export count; per export: string name, clen arity,
///                      string declaration text.
///   3. Symbol table  — clen count; per symbol: string name, clen arity, u8 kind
///                      (0 compiled → clen code offset + clen code length;
///                       1 codeless → bound by name at load: a runtime kernel or
///                       builtin guard). `proc` operands index this table.
///   4. Code section  — clen byte count, then the concatenated compiled bodies
///                      in the §4 instruction encoding.
///
/// The artefact identity is the SHA-256 of the entire artefact byte string.
/// h(M) — the source identity — is provided as input here; computing it (the
/// canonical print of the flattened source) is S5.
library;

import 'dart:typed_data';
import 'package:crypto/crypto.dart' as crypto;
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';

/// Magic `GLPW`.
const List<int> artefactMagic = [0x47, 0x4C, 0x50, 0x57];

/// Wire-format version this implementation writes and accepts.
const int wireFormatVersion = 1;

/// An exported procedure recorded in the interface table.
class ArtefactExport {
  final String name;
  final int arity;
  final String declarationText;
  const ArtefactExport(this.name, this.arity, this.declarationText);

  @override
  bool operator ==(Object other) =>
      other is ArtefactExport &&
      other.name == name &&
      other.arity == arity &&
      other.declarationText == declarationText;
  @override
  int get hashCode => Object.hash(name, arity, declarationText);
}

/// A symbol-table entry. `compiled` carries this module's own procedure code;
/// `codeless` is a runtime kernel or builtin guard bound by name at load.
class ArtefactSymbol {
  final String name;
  final int arity;
  final bool compiled;

  /// Procedure body for a compiled symbol (decoded, label-free); empty for a
  /// codeless symbol.
  final List<Object> ops;

  ArtefactSymbol.compiled(this.name, this.arity, this.ops) : compiled = true;
  ArtefactSymbol.codeless(this.name, this.arity)
      : compiled = false,
        ops = const [];

  String get signature => '$name/$arity';
}

/// A parsed/constructed module artefact.
class Artefact {
  final String isaVersion;
  final Uint8List hM; // 32 bytes
  final String moduleName;
  final String typeDefsText;
  final List<ArtefactExport> exports;

  /// The unified symbol table; a `proc` operand is an index into this list.
  final List<ArtefactSymbol> symbols;

  Artefact({
    required this.isaVersion,
    required this.hM,
    required this.moduleName,
    required this.typeDefsText,
    required this.exports,
    required this.symbols,
  }) {
    if (hM.length != 32) {
      throw WireFormatException('h(M) must be 32 bytes, got ${hM.length}');
    }
  }

  int _indexOfSignature(String sig) {
    for (var i = 0; i < symbols.length; i++) {
      if (symbols[i].signature == sig) return i;
    }
    throw WireFormatException('symbol not in table: $sig');
  }

  /// Serialize to artefact bytes (§5).
  Uint8List toBytes() {
    int procIndexOf(String sig) => _indexOfSignature(sig);

    // Encode each compiled body and record its (offset, length) in the code
    // section (concatenated compiled bodies, in symbol-table order).
    final codeBuf = BytesBuilder(copy: false);
    final offsets = <int>[];
    final lengths = <int>[];
    var cursor = 0;
    for (final s in symbols) {
      if (!s.compiled) {
        offsets.add(0);
        lengths.add(0);
        continue;
      }
      final body = encodeCode(s.ops, procIndexOf: procIndexOf);
      offsets.add(cursor);
      lengths.add(body.length);
      codeBuf.add(body);
      cursor += body.length;
    }
    final codeBytes = codeBuf.toBytes();

    final w = WireWriter();
    // 1. Header
    for (final b in artefactMagic) {
      w.u8(b);
    }
    w.u8(wireFormatVersion);
    w.string(isaVersion);
    w.hash(hM);
    w.string(moduleName);
    // 2. Interface table
    w.string(typeDefsText);
    w.clen(exports.length);
    for (final e in exports) {
      w.string(e.name);
      w.clen(e.arity);
      w.string(e.declarationText);
    }
    // 3. Symbol table
    w.clen(symbols.length);
    for (var i = 0; i < symbols.length; i++) {
      final s = symbols[i];
      w.string(s.name);
      w.clen(s.arity);
      w.u8(s.compiled ? 0 : 1);
      if (s.compiled) {
        w.clen(offsets[i]);
        w.clen(lengths[i]);
      }
    }
    // 4. Code section (clen byte count, then concatenated bodies)
    w.bytes(codeBytes);
    return w.toBytes();
  }

  /// The artefact identity: SHA-256 of the entire artefact byte string.
  static Uint8List identityOf(List<int> artefactBytes) =>
      Uint8List.fromList(crypto.sha256.convert(artefactBytes).bytes);

  /// Parse artefact bytes (§5). Verifies the magic and wire-format version.
  static Artefact fromBytes(Uint8List bytes) {
    final r = WireReader(bytes);
    // 1. Header
    for (final b in artefactMagic) {
      if (r.u8() != b) throw WireFormatException('bad magic (not GLPW)');
    }
    final ver = r.u8();
    if (ver != wireFormatVersion) {
      throw WireFormatException('unsupported wire-format version: $ver');
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
    // 3. Symbol table (headers first; bodies decoded after the code section)
    final symCount = r.clen();
    final names = <String>[];
    final arities = <int>[];
    final compiledFlags = <bool>[];
    final offs = <int>[];
    final lens = <int>[];
    for (var i = 0; i < symCount; i++) {
      names.add(r.string());
      arities.add(r.clen());
      final kind = r.u8();
      if (kind != 0 && kind != 1) {
        throw WireFormatException('bad symbol kind: $kind');
      }
      final compiled = kind == 0;
      compiledFlags.add(compiled);
      offs.add(compiled ? r.clen() : 0);
      lens.add(compiled ? r.clen() : 0);
    }
    // 4. Code section
    final codeBytes = r.bytes();
    r.expectEnd();

    // Decode compiled bodies; `proc` indices resolve to symbol signatures.
    String procNameOf(int i) => '${names[i]}/${arities[i]}';
    final symbols = <ArtefactSymbol>[];
    for (var i = 0; i < symCount; i++) {
      if (compiledFlags[i]) {
        final slice = Uint8List.sublistView(
            codeBytes, offs[i], offs[i] + lens[i]);
        final ops = decodeCode(Uint8List.fromList(slice), procNameOf: procNameOf);
        symbols.add(ArtefactSymbol.compiled(names[i], arities[i], ops));
      } else {
        symbols.add(ArtefactSymbol.codeless(names[i], arities[i]));
      }
    }

    return Artefact(
      isaVersion: isaVersion,
      hM: hM,
      moduleName: moduleName,
      typeDefsText: typeDefsText,
      exports: exports,
      symbols: symbols,
    );
  }

  /// Reconstruct a runnable program: each compiled symbol contributes its entry
  /// `Label(signature)` followed by its body, so `Spawn`/`Requeue`/`Guard`
  /// targets resolve via the program's label map. Codeless symbols contribute
  /// no code — the runtime resolves their names to local kernels/guards (the
  /// `Spawn`/`Guard` handlers already fall back to kernel/guard lookup).
  BytecodeProgram toProgram() {
    final ops = <Object>[];
    for (final s in symbols) {
      if (!s.compiled) continue;
      ops.add(Label(s.signature));
      ops.addAll(s.ops);
    }
    return BytecodeProgram(ops);
  }
}

/// A module loaded from an artefact (§7).
class LoadedModule {
  final Uint8List hM;
  final Uint8List artefactId;
  final Artefact artefact;
  final BytecodeProgram program;

  /// The exported procedure signatures the loader aliases unqualified.
  final Set<String> exportAliases;

  LoadedModule({
    required this.hM,
    required this.artefactId,
    required this.artefact,
    required this.program,
    required this.exportAliases,
  });
}

/// The artefact loader (§7): verifies identities and versions, decodes, aliases
/// exports, and caches/deduplicates by artefact identity.
class ArtefactLoader {
  final Map<String, LoadedModule> _byId = {};

  /// Load an artefact in its adoption context: the offered source identity
  /// h(M) and the certified artefact identity (the sender's attestation). The
  /// type-automata derivation and the load-time interface check of §7 step 2
  /// are the type system's (TGLP); the interface text is carried in
  /// `artefact.exports` / `artefact.typeDefsText` for that step.
  LoadedModule load(
    Uint8List artefactBytes, {
    required Uint8List offeredHM,
    required Uint8List certifiedArtefactId,
    Set<int> supportedWireVersions = const {wireFormatVersion},
    Set<String>? supportedIsaVersions,
  }) {
    // 1. Verify the artefact identity against the certified one.
    final id = Artefact.identityOf(artefactBytes);
    if (!_bytesEqual(id, certifiedArtefactId)) {
      throw WireFormatException('artefact identity mismatch');
    }
    // Cache/dedup by artefact identity.
    final key = _hex(id);
    final cached = _byId[key];
    if (cached != null) return cached;

    final art = Artefact.fromBytes(artefactBytes); // checks magic + wire version
    if (!supportedWireVersions.contains(wireFormatVersion)) {
      throw WireFormatException('unsupported wire-format version');
    }
    // 1 (cont). Verify the header h(M) equals the offered h(M).
    if (!_bytesEqual(art.hM, offeredHM)) {
      throw WireFormatException('h(M) mismatch with the adoption offer');
    }
    // Refuse an unsupported ISA version.
    if (supportedIsaVersions != null &&
        !supportedIsaVersions.contains(art.isaVersion)) {
      throw WireFormatException('unsupported ISA version: ${art.isaVersion}');
    }
    // 2. (TGLP) derive type automata from the interface text + run the
    //    load-time interface checks. Deferred to the type system; the interface
    //    is available on `art` for that hook.
    // 3. Decode + reconstruct the runnable program; alias exports only.
    final program = art.toProgram();
    final aliases = {for (final e in art.exports) '${e.name}/${e.arity}'};
    // 4. Register under (h(M), artefact identity); cache by artefact identity.
    final m = LoadedModule(
      hM: art.hM,
      artefactId: id,
      artefact: art,
      program: program,
      exportAliases: aliases,
    );
    _byId[key] = m;
    return m;
  }
}

bool _bytesEqual(List<int> a, List<int> b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

String _hex(List<int> b) {
  final sb = StringBuffer();
  for (final x in b) {
    sb.write(x.toRadixString(16).padLeft(2, '0'));
  }
  return sb.toString();
}
