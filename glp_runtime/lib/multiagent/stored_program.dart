/// The compiled program an agent runs, as its store keeps it beside the key
/// pair (IGLP 61f3283, Implementation Notes, "Persistence": "the agent's key
/// pair, the compiled program it runs, the resolvent ..."), so that a restart
/// that refuses an update restarts the agent under the version it terminated
/// under (IGLP 61f3283, madglp-spec.tex, Termination and Restart: "otherwise
/// the update is refused, and the agent restarts under the version it
/// terminated under").
///
/// What the agent runs is three things, each kept:
///
///   - The code its goals run, whose byte offsets their program counters are:
///     the program and the root linked as one, a §cf artefact (IGLP, Code
///     Format, "Program Artefact"; `codeArtefactFromProgram`).  Its identity
///     ([codeIdentityOf]) is the one a saved state is held to.
///   - Its own module value, the artefact `self_module` returns and every goal
///     of the program carries (IGLP, Implementation Notes, "Self-Module"),
///     with its name and its directory, where `load_file/2` resolves a name.
///   - The type scope the module's type-identity tables are built from
///     ([TypeIdentityTables.source]; wire/type_scope_codec.dart), which the
///     goals read through `find_type/2` and `find_type/3`, and from which the
///     tables are built again by the construction that built them.
///
/// The format: the magic `GLPP`, a version byte, the code's artefact bytes,
/// then a flag and, where the program has a module value, its artefact bytes,
/// its name, its directory or none, its type scope or none, and its opaque
/// entries.
library;

import 'dart:io';
import 'dart:typed_data';

import 'package:crypto/crypto.dart' as crypto;

import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;
import 'package:glp_runtime/engine_v2/code_image.dart' show CodeImage;
import 'package:glp_runtime/multiagent/glp_network.dart' show Application;
import 'package:glp_runtime/runtime/module_hierarchy.dart' show primitiveScope;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/type_scope_codec.dart';

/// The four magic bytes, `GLPP`, and the format version.
const List<int> storedProgramMagic = [0x47, 0x4C, 0x50, 0x50];
/// Version 2 (2026-11-09) writes after the code the app whose program it is,
/// its developer, its name and its version number, or none, which
/// `self_app/2` gives after a refused update (GLP-Spec cce13ce,
/// appendix-guards, "Networking seam"; IGLP 59c40e2); a version-1 program is
/// not read.
/// Version 3 (2026-11-09) writes after the app the files of the program's
/// directory, each by its path relative to it, which `load_file/2` reads after
/// a refused update (IGLP 96cd44a, Implementation Notes, "Persistence" and "An
/// update"); a version-2 program is not read.
const int storedProgramVersion = 3;

/// The identity of a code image: SHA-256 over its instruction-set version,
/// its name, its symbols and its code, which the goals' program counters
/// index.  A saved state names the code it was saved under by it.
Uint8List codeIdentityOf(CodeImage image) {
  final w = WireWriter()
    ..string(image.isaVersion)
    ..string(image.moduleName)
    ..clen(image.symbols.length);
  for (final sym in image.symbols) {
    w
      ..string(sym.signature)
      ..u8(sym.compiled ? 1 : 0)
      ..clen(sym.codeOffset)
      ..clen(sym.codeLength);
  }
  w.bytes(image.code);
  return Uint8List.fromList(crypto.sha256.convert(w.toBytes()).bytes);
}

/// A compiled program an agent runs: its code, and its module value with the
/// scope of its tables.
class StoredProgram {
  /// The artefact bytes of the code the goals run.
  final Uint8List code;

  /// The code, loaded for direct byte execution.
  late final CodeImage image = CodeImage.fromArtefactBytes(code);

  /// [image]'s identity ([codeIdentityOf]).
  late final Uint8List identity = codeIdentityOf(image);

  /// The app whose program this is, as the host gave it when the program was
  /// kept: its developer, its name and its version number; null where the
  /// host gave none.
  final Application? app;

  /// The files of the program's directory, by path relative to it, as they
  /// were when the program was kept: what `load_file/2` reads where the agent
  /// runs this program after a refused update ([rt.ModuleTerm.keptFiles]).
  final Map<String, Uint8List> files;

  final Uint8List? _moduleArtefact;
  final String _moduleName;
  final String? _moduleDirectory;
  final ast.Module? _typeScope;
  final Map<String, OpaqueEntry> _opaque;

  StoredProgram._(this.code, this.app, this.files, this._moduleArtefact,
      this._moduleName, this._moduleDirectory, this._typeScope, this._opaque);

  /// The program an engine compiled: [code] the bytes of its code
  /// (`codeArtefactFromProgram`), and [module] its module value, whose tables
  /// carry the scope they were built from.  Throws where they carry none:
  /// tables derived from an interface say nothing of a scope, and a compiled
  /// program's goals read its scope.  [app] is the app whose program it is,
  /// as the host gives it, and [files] the files of its directory.
  factory StoredProgram.compiled(Uint8List code, rt.ModuleTerm? module,
      {Application? app, Map<String, Uint8List> files = const {}}) {
    if (module == null) {
      return StoredProgram._(code, app, files, null, '', null, null, const {});
    }
    final tables = module.declaredTypes;
    ast.Module? scope;
    var opaque = const <String, OpaqueEntry>{};
    if (tables != null) {
      if (tables is! TypeIdentityTables || tables.source == null) {
        throw StateError('the module ${module.name} carries type-identity '
            'tables built from no scope, and the store keeps the scope of a '
            'compiled program');
      }
      scope = tables.source;
      opaque = tables.opaque;
    }
    return StoredProgram._(code, app, files,
        (module.artefact as Artefact).toBytes(), module.name, module.directory,
        scope, opaque);
  }

  /// The program the store keeps as [bytes] ([toBytes]).
  factory StoredProgram.fromBytes(Uint8List bytes) {
    final r = WireReader(bytes);
    for (final b in storedProgramMagic) {
      if (r.u8() != b) {
        throw WireFormatException('no stored program (bad magic)');
      }
    }
    final version = r.u8();
    if (version != storedProgramVersion) {
      throw WireFormatException('stored program version $version; this '
          'runtime reads $storedProgramVersion');
    }
    final code = r.bytes();
    final app = r.u8() == 1
        ? Application(
            developer: r.string(),
            name: r.string(),
            version: int.parse(r.string()))
        : null;
    final files = <String, Uint8List>{};
    for (var i = r.clen(); i > 0; i--) {
      final path = r.string();
      files[path] = r.bytes();
    }
    if (r.u8() == 0) {
      _end(r);
      return StoredProgram._(code, app, files, null, '', null, null, const {});
    }
    final artefact = r.bytes();
    final name = r.string();
    final directory = r.u8() == 1 ? r.string() : null;
    final scope = r.u8() == 1 ? readTypeScope(r) : null;
    final opaque = <String, OpaqueEntry>{};
    for (var i = r.clen(); i > 0; i--) {
      final key = r.string();
      final entry = r.string();
      opaque[key] = OpaqueEntry(
          entry: entry, types: [for (var j = r.clen(); j > 0; j--) r.string()]);
    }
    _end(r);
    return StoredProgram._(
        code, app, files, artefact, name, directory, scope, opaque);
  }

  static void _end(WireReader r) {
    if (!r.atEnd) {
      throw WireFormatException('the stored program has bytes past its end');
    }
  }

  Uint8List toBytes() {
    final w = WireWriter();
    for (final b in storedProgramMagic) {
      w.u8(b);
    }
    w
      ..u8(storedProgramVersion)
      ..bytes(code);
    final app = this.app;
    w.u8(app == null ? 0 : 1);
    if (app != null) {
      w
        ..string(app.developer)
        ..string(app.name)
        ..string('${app.version}');
    }
    w.clen(files.length);
    for (final MapEntry(key: path, value: bytes) in files.entries) {
      w
        ..string(path)
        ..bytes(bytes);
    }
    final artefact = _moduleArtefact;
    if (artefact == null) {
      w.u8(0);
      return w.toBytes();
    }
    w
      ..u8(1)
      ..bytes(artefact)
      ..string(_moduleName);
    final directory = _moduleDirectory;
    w.u8(directory == null ? 0 : 1);
    if (directory != null) w.string(directory);
    final scope = _typeScope;
    w.u8(scope == null ? 0 : 1);
    if (scope != null) writeTypeScope(w, scope);
    w.clen(_opaque.length);
    for (final MapEntry(:key, :value) in _opaque.entries) {
      w
        ..string(key)
        ..string(value.entry)
        ..clen(value.types.length);
      for (final t in value.types) {
        w.string(t);
      }
    }
    return w.toBytes();
  }

  /// The program's module value, its tables built again from its scope by
  /// the construction that built them ([typeIdentityTablesForModule], over
  /// the language primitives, as [linkedTypeIdentityTables] builds them);
  /// null where the program has none.
  late final rt.ModuleTerm? module = _module();

  rt.ModuleTerm? _module() {
    final artefact = _moduleArtefact;
    if (artefact == null) return null;
    final scope = _typeScope;
    TypeIdentityTables? tables;
    if (scope != null) {
      tables = typeIdentityTablesForModule(scope,
          ancestorScope: primitiveScope());
      if (_opaque.isNotEmpty) tables = tables.withOpaque(_opaque);
    }
    return rt.ModuleTerm(Artefact.fromBytes(artefact),
        name: _moduleName,
        declaredTypes: tables,
        directory: _moduleDirectory,
        keptFiles: files);
  }

  /// The type-identity tables of the program, where its module carries them.
  /// The files under [directory], by path relative to it with `/` between
  /// its parts, as `load_file/2` names them: each regular file, a link being
  /// none, which `load_file/2` refuses where it leaves the directory.  None
  /// where [directory] is null or does not exist.
  static Map<String, Uint8List> filesOf(String? directory) {
    if (directory == null) return const {};
    final dir = Directory(directory);
    if (!dir.existsSync()) return const {};
    final base = dir.absolute.path;
    final files = <String, Uint8List>{};
    for (final e in dir.listSync(recursive: true, followLinks: false)) {
      if (e is! File) continue;
      final abs = e.absolute.path;
      if (!abs.startsWith('$base${Platform.pathSeparator}')) continue;
      final rel = abs
          .substring(base.length + 1)
          .split(Platform.pathSeparator)
          .join('/');
      files[rel] = e.readAsBytesSync();
    }
    return files;
  }

  TypeIdentityTables? get tables {
    final t = module?.declaredTypes;
    return t is TypeIdentityTables ? t : null;
  }
}
