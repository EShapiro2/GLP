/// GLP module artefact + loader (code format §Program Artefact, §Loader).
///
/// Normative source: the IGLP paper, sections/code-format-fragment.tex,
/// §Program Artefact and §Loader. The artefact is the byte string a compiler
/// produces from a flat program: a BODY followed by a CERTIFICATE. The body's
/// sections, in order:
///
///   1. Header        — magic `GLPW`, u8 code-format version (2), string
///                      instruction-set version, string program name. Neither
///                      identity is in the header: both are in the certificate,
///                      where they are signed.
///   2. Interface     — string (reachable type definitions, canonical print);
///                      clen export count; per export: string name, clen arity,
///                      string declaration text.
///   3. Symbol table  — clen count; per symbol: string name, clen arity, u8 kind
///                      (0 compiled → clen code offset + clen code length;
///                       1 codeless → bound by name at load: a runtime kernel or
///                       builtin guard). `proc` operands index this table.
///   4. Code section  — clen byte count, then the concatenated compiled bodies
///                      in the instruction encoding.
///
/// The CERTIFICATE follows the body: agent — the public key of the person who
/// compiled the module; hash HSrc, the source identity h(M) (the hash of the
/// flattened source, §Deterministic Flattening); hash HBin, the compiled
/// identity, SHA-256 of the body; then bytes, that person's signature over
/// e(ids(HSrc, HBin)), the canonical encoding of the 2-ary structure `ids`
/// whose arguments are the two identities as bytes constants. It is written at
/// every compilation, whether or not the module is ever sent, so a module
/// states which source it was compiled from and who is answerable for it
/// (Secure GLP, core.tex §Setting). A certificate is refused to a module that
/// calls an OS-privileged predicate (compiler/certification.dart): the
/// artefact then carries the two identities under an empty agent and an empty
/// signature, which no loader admits and `decompose_module/4` does not read.
/// The compiled identity is the hash of the body alone, so the certificate
/// sits inside the artefact without hashing itself.
library;

import 'dart:typed_data';
import 'package:crypto/crypto.dart' as crypto;
import 'package:glp_runtime/analysis/type_checker/type_identity.dart'
    show TypeIdentityTables, interfaceTypeIdentityTables;
import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/bytecode/runner.dart';
import 'package:glp_runtime/multiagent/glp_network.dart' show PubKey;
import 'package:glp_runtime/multiagent/identity.dart' show PersonIdentity;
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/instruction_codec.dart';

/// Magic `GLPW`.
const List<int> artefactMagic = [0x47, 0x4C, 0x50, 0x57];

/// Code-format version this implementation writes and accepts. Version 2
/// introduces the message kind byte (0 value, 1 request, 2 acknowledgement)
/// and the certified artefact layout above; the loader refuses a version it
/// does not support.
const int wireFormatVersion = 2;

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

/// The certificate that follows an artefact's body (§Program Artefact): the
/// compiler's key, the two identities, and the signature over them.
class Certificate {
  /// The public key of the person who compiled the module; empty where the
  /// certificate was refused.
  final Uint8List agent;

  /// The source identity h(M) — SHA-256 of the flattened source.
  final Uint8List hSrc;

  /// The compiled identity — SHA-256 of the artefact body.
  final Uint8List hBin;

  /// The person's signature over [signedContent]; empty where refused.
  final Uint8List signature;

  Certificate({
    required this.agent,
    required this.hSrc,
    required this.hBin,
    required this.signature,
  }) {
    if (hSrc.length != 32 || hBin.length != 32) {
      throw WireFormatException(
          'certificate identities must be 32 bytes each');
    }
  }

  /// A certificate written by [signer] over the two identities.
  factory Certificate.signed(
      {required PersonIdentity signer,
      required Uint8List hSrc,
      required Uint8List hBin}) {
    return Certificate(
      agent: Uint8List.fromList(signer.pub.bytes),
      hSrc: hSrc,
      hBin: hBin,
      signature: signer.sign(signedContent(hSrc, hBin)),
    );
  }

  /// The certificate of a module that was refused one, or compiled where no
  /// one vouches: the identities stand, and nobody signs them.
  factory Certificate.refused(
          {required Uint8List hSrc, required Uint8List hBin}) =>
      Certificate(
          agent: Uint8List(0), hSrc: hSrc, hBin: hBin, signature: Uint8List(0));

  /// Whether this module carries no signature — refused, or never certified.
  bool get isRefused => agent.isEmpty || signature.isEmpty;

  /// The compiler's key as a [PubKey], or null where refused.
  PubKey? get compilerKey => isRefused ? null : PubKey(agent);

  /// e(ids(HSrc, HBin)): the bytes the certificate signs. The functor `ids` is
  /// fixed by the code format; the identities are bytes constants.
  static Uint8List signedContent(Uint8List hSrc, Uint8List hBin) =>
      encodeTermToBytes(
          WStruct('ids', [WConst(WBlob(hSrc)), WConst(WBlob(hBin))]));

  /// Whether the signature verifies under the key the certificate carries.
  bool verifies() {
    final key = compilerKey;
    if (key == null) return false;
    return PersonIdentity.verify(key, signedContent(hSrc, hBin), signature);
  }

  void write(WireWriter w) {
    w.bytes(agent);
    w.hash(hSrc);
    w.hash(hBin);
    w.bytes(signature);
  }

  static Certificate read(WireReader r) {
    final agent = r.bytes();
    final hSrc = r.hash();
    final hBin = r.hash();
    final signature = r.bytes();
    return Certificate(
        agent: agent,
        hSrc: Uint8List.fromList(hSrc),
        hBin: Uint8List.fromList(hBin),
        signature: signature);
  }
}

/// A parsed/constructed module artefact.
class Artefact {
  final String isaVersion;
  final String moduleName;
  final String typeDefsText;
  final List<ArtefactExport> exports;

  /// The unified symbol table; a `proc` operand is an index into this list.
  final List<ArtefactSymbol> symbols;

  /// The source identity this artefact was compiled from.
  final Uint8List _hSrc;

  /// The compiling person, or null where no one certifies the module.
  final PersonIdentity? _signer;

  /// A certificate read from bytes, or written once from the body.
  Certificate? _certificate;

  /// Construct an artefact over [symbols]. The certificate is written from the
  /// body on first use: signed by [signer], or refused where none is given.
  Artefact({
    required this.isaVersion,
    required Uint8List hM,
    required this.moduleName,
    required this.typeDefsText,
    required this.exports,
    required this.symbols,
    PersonIdentity? signer,
  })  : _hSrc = hM,
        _signer = signer {
    if (hM.length != 32) {
      throw WireFormatException('h(M) must be 32 bytes, got ${hM.length}');
    }
  }

  Artefact._parsed({
    required this.isaVersion,
    required this.moduleName,
    required this.typeDefsText,
    required this.exports,
    required this.symbols,
    required Certificate certificate,
  })  : _hSrc = certificate.hSrc,
        _signer = null,
        _certificate = certificate;

  /// The source identity h(M).
  Uint8List get hM => _hSrc;

  /// The certificate: compiler's key, both identities, signature.
  Certificate get certificate {
    final c = _certificate;
    if (c != null) return c;
    final hBin = compiledIdentityOfBody(bodyBytes());
    final signer = _signer;
    final made = signer == null
        ? Certificate.refused(hSrc: _hSrc, hBin: hBin)
        : Certificate.signed(signer: signer, hSrc: _hSrc, hBin: hBin);
    _certificate = made;
    return made;
  }

  /// The compiled identity: SHA-256 of the body.
  Uint8List get compiledIdentity => certificate.hBin;

  int _indexOfSignature(String sig) {
    for (var i = 0; i < symbols.length; i++) {
      if (symbols[i].signature == sig) return i;
    }
    throw WireFormatException('symbol not in table: $sig');
  }

  /// Serialize the body (§Program Artefact, sections 1–4).
  Uint8List bodyBytes() {
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

  /// Serialize to artefact bytes: the body, then the certificate.
  Uint8List toBytes() {
    final body = bodyBytes();
    final cert = certificate;
    final w = WireWriter();
    for (final b in body) {
      w.u8(b);
    }
    cert.write(w);
    return w.toBytes();
  }

  /// Build an artefact from a flattened (statically-linked) program's
  /// instruction stream. Procedure entries are `Label`s of the form `name/arity`
  /// (internal clause/end labels `_cN`/`_end` are not entries). Each procedure
  /// becomes a compiled symbol; every `Spawn`/`Requeue`/`Guard` target that is
  /// not a compiled procedure becomes a codeless symbol (a runtime kernel or
  /// builtin guard, bound by name at load). Symbol order — compiled in program
  /// order, then codeless sorted — is deterministic, so equal programs yield
  /// equal bodies. The certificate is [signer]'s, or refused where none.
  static Artefact fromCompiled({
    required List<Object> ops,
    required Uint8List hM,
    required String moduleName,
    required String isaVersion,
    String typeDefsText = '',
    List<ArtefactExport> exports = const [],
    PersonIdentity? signer,
  }) {
    // Procedure-entry labels: a `name/arity` signature, not an internal label.
    final internalSuffix = RegExp(r'_c\d+$');
    bool isEntry(String n) =>
        n.contains('/') && !n.endsWith('_end') && !internalSuffix.hasMatch(n);

    ({String name, int arity}) splitSig(String sig) {
      final slash = sig.lastIndexOf('/');
      return (name: sig.substring(0, slash), arity: int.parse(sig.substring(slash + 1)));
    }

    // Segment into procedures by consecutive entry labels.
    final entryStarts = <int>[];
    final entrySigs = <String>[];
    for (var i = 0; i < ops.length; i++) {
      final op = ops[i];
      if (op is Label && isEntry(op.name)) {
        entryStarts.add(i);
        entrySigs.add(op.name);
      }
    }
    final compiledSigs = entrySigs.toSet();
    final compiled = <ArtefactSymbol>[];
    for (var e = 0; e < entryStarts.length; e++) {
      final start = entryStarts[e];
      final end = e + 1 < entryStarts.length ? entryStarts[e + 1] : ops.length;
      final body = ops.sublist(start + 1, end); // exclude the entry label
      final s = splitSig(entrySigs[e]);
      compiled.add(ArtefactSymbol.compiled(s.name, s.arity, body));
    }

    // Codeless targets: Spawn/Requeue (name/arity) and Guard (name + arity)
    // not satisfied by a compiled procedure.
    final codelessSigs = <String>{};
    for (final op in ops) {
      if (op is Spawn && !compiledSigs.contains(op.procedureLabel)) {
        codelessSigs.add(op.procedureLabel);
      } else if (op is Requeue && !compiledSigs.contains(op.procedureLabel)) {
        codelessSigs.add(op.procedureLabel);
      } else if (op is Guard) {
        final sig = '${op.procedureLabel}/${op.arity}';
        if (!compiledSigs.contains(sig)) codelessSigs.add(sig);
      }
    }
    final codeless = (codelessSigs.toList()..sort()).map((sig) {
      final s = splitSig(sig);
      return ArtefactSymbol.codeless(s.name, s.arity);
    }).toList();

    return Artefact(
      isaVersion: isaVersion,
      hM: hM,
      moduleName: moduleName,
      typeDefsText: typeDefsText,
      exports: exports,
      symbols: [...compiled, ...codeless],
      signer: signer,
    );
  }

  /// SHA-256 of a body's bytes — the compiled identity.
  static Uint8List compiledIdentityOfBody(List<int> bodyBytes) =>
      Uint8List.fromList(crypto.sha256.convert(bodyBytes).bytes);

  /// The length of the body within [artefactBytes] — every byte before the
  /// certificate. Walks the body's framing without decoding procedure bodies.
  static int bodyLength(Uint8List artefactBytes) {
    final r = WireReader(artefactBytes);
    for (final b in artefactMagic) {
      if (r.u8() != b) throw WireFormatException('bad magic (not GLPW)');
    }
    final ver = r.u8();
    if (ver != wireFormatVersion) {
      throw WireFormatException('unsupported code-format version: $ver');
    }
    r.string(); // isa version
    r.string(); // module name
    r.string(); // type definitions
    final exportCount = r.clen();
    for (var i = 0; i < exportCount; i++) {
      r.string();
      r.clen();
      r.string();
    }
    final symCount = r.clen();
    for (var i = 0; i < symCount; i++) {
      r.string();
      r.clen();
      final kind = r.u8();
      if (kind != 0 && kind != 1) {
        throw WireFormatException('bad symbol kind: $kind');
      }
      if (kind == 0) {
        r.clen();
        r.clen();
      }
    }
    r.bytes(); // code section
    return r.offset;
  }

  /// The compiled identity of an artefact: SHA-256 of its body (§Loader, step
  /// 1: computed by the receiver and compared with the certificate's).
  static Uint8List compiledIdentityOf(Uint8List artefactBytes) =>
      compiledIdentityOfBody(
          Uint8List.sublistView(artefactBytes, 0, bodyLength(artefactBytes)));

  /// The artefact [bytes] hold, where they are a certified compiled program:
  /// they parse as an artefact, the body hashes to the certificate's compiled
  /// identity, and the certificate's signature verifies under the key it
  /// carries — the loader's step 1 without an adoption offer (§Loader).
  /// Null otherwise: a file that is not an artefact, or whose certificate does
  /// not check, is text and not a Module (GLP-Spec appendix-guards,
  /// "Compilation and file reading"), and a forged artefact is never a Module.
  static Artefact? certifiedFromBytes(Uint8List bytes) {
    final Artefact art;
    try {
      art = fromBytes(bytes);
    } catch (_) {
      return null;
    }
    final cert = art.certificate;
    if (cert.isRefused) return null;
    final Uint8List id;
    try {
      id = compiledIdentityOf(bytes);
    } catch (_) {
      return null;
    }
    if (!_bytesEqual(id, cert.hBin)) return null;
    if (!cert.verifies()) return null;
    return art;
  }

  /// Parse artefact bytes (§Program Artefact). Verifies the magic and
  /// code-format version, and the framing; not the certificate — that is the
  /// loader's.
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
    // The certificate
    final certificate = Certificate.read(r);
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

    return Artefact._parsed(
      isaVersion: isaVersion,
      moduleName: moduleName,
      typeDefsText: typeDefsText,
      exports: exports,
      symbols: symbols,
      certificate: certificate,
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

/// A module loaded from an artefact (§Loader).
class LoadedModule {
  final Uint8List hM;

  /// The compiled identity: SHA-256 of the body, verified against the
  /// certificate at load.
  final Uint8List compiledIdentity;
  final Artefact artefact;
  final BytecodeProgram program;

  /// The exported procedure signatures the loader aliases unqualified.
  final Set<String> exportAliases;

  /// The module's exported type-identity table, derived at load from the
  /// interface text the artefact carries (§Loader step 2), not shipped beside
  /// it: a shipped table can disagree with the source it describes, one
  /// recomputed from the attested source cannot. `run/3` compares a posted
  /// goal against it.
  final TypeIdentityTables exportedTypes;

  LoadedModule({
    required this.hM,
    required this.compiledIdentity,
    required this.artefact,
    required this.program,
    required this.exportAliases,
    required this.exportedTypes,
  });

  /// The compiled identity, under the loader's former name for it.
  Uint8List get artefactId => compiledIdentity;
}

/// The artefact loader (§Loader): verifies the certificate and versions,
/// decodes, aliases exports, and caches/deduplicates by compiled identity.
class ArtefactLoader {
  final Map<String, LoadedModule> _byId = {};

  /// Load an artefact in its adoption context: the offered source identity
  /// h(M). Step 1 computes the compiled identity from the body and verifies it
  /// equals the certificate's, verifies the certificate's signature under the
  /// key the certificate carries, and verifies the certificate's source
  /// identity equals the offer. Step 2 derives the type automata from the
  /// interface text the artefact carries; the derivation is the type system's
  /// (TGLP) and the result is the loaded module's `exportedTypes`.
  LoadedModule load(
    Uint8List artefactBytes, {
    required Uint8List offeredHM,
    Set<int> supportedWireVersions = const {wireFormatVersion},
    Set<String>? supportedIsaVersions,
  }) {
    final art = Artefact.fromBytes(artefactBytes); // checks magic + wire version
    if (!supportedWireVersions.contains(wireFormatVersion)) {
      throw WireFormatException('unsupported wire-format version');
    }
    // 1. The compiled identity, computed here and compared with the
    //    certificate's; the certificate's signature; the source identity
    //    against the offer.
    final id = Artefact.compiledIdentityOf(artefactBytes);
    final cert = art.certificate;
    if (!_bytesEqual(id, cert.hBin)) {
      throw WireFormatException(
          'compiled identity mismatch: the body does not hash to the '
          'certificate\'s compiled identity');
    }
    if (cert.isRefused) {
      throw WireFormatException('module carries no certificate');
    }
    if (!cert.verifies()) {
      throw WireFormatException(
          'certificate signature does not verify under the key it carries');
    }
    if (!_bytesEqual(cert.hSrc, offeredHM)) {
      throw WireFormatException('h(M) mismatch with the adoption offer');
    }
    // Cache/dedup by compiled identity.
    final key = _hex(id);
    final cached = _byId[key];
    if (cached != null) return cached;

    // Refuse an unsupported ISA version.
    if (supportedIsaVersions != null &&
        !supportedIsaVersions.contains(art.isaVersion)) {
      throw WireFormatException('unsupported ISA version: ${art.isaVersion}');
    }
    // 2. Derive the type automata from the interface table's declaration text.
    //    Text that will not parse is a failsafe refusal, as an unknown symbol
    //    name is (§versioning): the interface is what the load-time check
    //    compares against, so a module whose interface cannot be read is not
    //    loadable.
    final TypeIdentityTables exportedTypes;
    try {
      exportedTypes = interfaceTypeIdentityTables(
        typeDefsText: art.typeDefsText,
        exportDeclarationTexts: art.exports.map((e) => e.declarationText),
      );
    } catch (e) {
      throw WireFormatException('interface text does not parse: $e');
    }
    // 3. Decode + reconstruct the runnable program; alias exports only.
    final program = art.toProgram();
    final aliases = {for (final e in art.exports) '${e.name}/${e.arity}'};
    // 4. Register under (h(M), compiled identity); cache by compiled identity.
    final m = LoadedModule(
      hM: art.hM,
      compiledIdentity: id,
      artefact: art,
      program: program,
      exportAliases: aliases,
      exportedTypes: exportedTypes,
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
