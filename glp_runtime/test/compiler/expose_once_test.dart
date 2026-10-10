/// A module the directory walk collects and an `-expose` names is one module
/// of the program, its procedures emitted once.
///
/// TGLP modules.tex, Compilation: the first step "collects every .glp file of
/// the program's directory tree", the third renames "every procedure p/n ...
/// in every .glp file" to M:p/n, M the module's path from the root; a file is
/// one module however many routes reach it.  Until 2026-10-03 a file both in
/// the walk and named by an ancestor's `-expose` was discovered twice, under
/// one name, and the linker emitted its procedures twice: tests/expose/basic
/// gave util/strutil:twice/2 and util/plist:pmerge/3 two procedures each, and
/// system/mad_predicates.glp loaded alone, which the root self.glp then
/// exposed, all of its own (GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:01. 5":
/// "faults, fix them").  The root exposes it no longer, send_to_net/1 being
/// the root self.glp's own (GLP-Spec appendix-guards, "Output to the
/// network"); the single-file case is held on tests/expose/basic/util/
/// strutil.glp, which expose/basic/self.glp, on its ancestor chain, exposes.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

final _rootSelf = File('../programs/self.glp').absolute.path;

/// The procedure keys (name/arity) of [linked] that occur more than once.
List<String> _twice(LinkResult linked) {
  final seen = <String>{};
  final twice = <String>[];
  for (final p in linked.program.procedures) {
    final k = '${p.name}/${p.arity}';
    if (!seen.add(k)) twice.add(k);
  }
  return twice;
}

/// The modules of [modules] by file, any file named more than once.
List<String> _filesTwice(List<DiscoveredModule> modules) {
  final seen = <String>{};
  return [
    for (final m in modules)
      if (!seen.add(File(m.filePath).absolute.path)) m.filePath
  ];
}

void main() {
  group('tests/expose/basic', () {
    final dir = Directory('../programs/tests/expose/basic').absolute.path;

    test('each file of the program is one module', () {
      final modules = discoverProgram(dir, rootSelfGlpPath: _rootSelf);
      expect(_filesTwice(modules), isEmpty);
    });

    test('the exposed modules are still exposed', () {
      final modules = discoverProgram(dir, rootSelfGlpPath: _rootSelf);
      final strutil = modules.singleWhere(
          (m) => m.filePath.endsWith('util/strutil.glp'));
      expect(strutil.exposingDir, isNotNull);
    });

    test('each procedure is emitted once', () {
      final modules = discoverProgram(dir, rootSelfGlpPath: _rootSelf);
      final linked = linkAndResolveModules(modules, rootDir: dir);
      expect(_twice(linked), isEmpty);
      expect(
          linked.program.procedures
              .where((p) => p.name.endsWith('util/strutil:twice'))
              .length,
          1);
      expect(
          linked.program.procedures
              .where((p) => p.name.endsWith('util/plist:pmerge'))
              .length,
          1);
    });

    test('it loads and runs as before', () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(engine.loadProgram(dir), isTrue);
      final r = await engine.runGoal('use_exposed(R).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    });
  });

  group('tests/expose/basic/util/strutil.glp loaded alone', () {
    final file =
        File('../programs/tests/expose/basic/util/strutil.glp').absolute.path;

    test('is one module, though an ancestor self.glp exposes it', () {
      final modules =
          discoverSingleModule(file, rootSelfGlpPath: _rootSelf);
      expect(_filesTwice(modules), isEmpty);
      // The case is the one the group names: the module loaded alone is the
      // module the ancestor's -expose reaches, marked exposed.
      final strutil = modules.singleWhere(
          (m) => File(m.filePath).absolute.path == File(file).absolute.path);
      expect(strutil.exposingDir, isNotNull);
    });

    test('each of its procedures is emitted once', () {
      final modules =
          discoverSingleModule(file, rootSelfGlpPath: _rootSelf);
      final linked = linkAndResolveModules(modules,
          rootDir: File(file).parent.path, singleModulePath: file);
      expect(_twice(linked), isEmpty);
    });
  });
}
