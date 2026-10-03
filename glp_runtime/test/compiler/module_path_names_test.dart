// glp_runtime/test/compiler/module_path_names_test.dart
//
// Every module is named by its path from the root, and a cross-module call
// is resolved from the caller's directory (TGLP modules.tex, Compilation, third
// and fourth steps: every procedure and type is renamed M:p and M:T "where M is
// the module's path from the root, eliminating name collisions", and a call
// M'#p, "whose qualifier is a single child directory or module file relative
// to the caller's directory, resolves to the procedure p that the qualifier
// exports"; Cross-module type checking, the same rule for the qualifier).
//
// The two cases are sGLP's coins program's (Integration #4 Code to GLP,
// 2026-10-02 18:26 UTC), made fixtures under programs/tests/module_paths/:
//
//   d/    a directory's self.glp forwards p to d # p, d.glp defining p by a
//         local call to q.  The self.glp is tests/module_paths/d and d.glp is
//         tests/module_paths/d/d.  Until 2026-10-02 both were the module "d",
//         and the load was refused, "Undefined procedure: q/2".
//   top/  a/h.glp and b/h.glp, each forwarded by its own directory's self.glp.
//         They are tests/module_paths/top/a/h and .../top/b/h.  Until
//         2026-10-02 both were the module "h", and the load was refused,
//         "Undefined procedure: one/1".
//
// And two refusals of the rule for the qualifier: not_child/ calls h # run
// where h.glp lies in a subdirectory with no self.glp, and not_exported/ calls
// m # helper where m.glp does not export helper/1.
//
// A directory with no self.glp is not a program ("Entry and the absence of a
// boot module"): no_self/ is refused as one, its m.glp loading alone.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  String fixture(String name) =>
      Directory('../programs/tests/module_paths/$name').absolute.path;

  late GlpEngine engine;
  setUp(() => engine = GlpEngine(rootSelfGlpPath: rootSelf));

  Matcher throwsContaining(String needle) => throwsA(predicate(
      (e) => e.toString().contains(needle), 'message contains "$needle"'));

  Matcher isInteger(int n) =>
      isA<rt.ConstTerm>().having((c) => c.value, 'value', n);

  Set<String> linkedNames(String dir) {
    final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
    return linkAndResolveModules(modules, rootDir: dir)
        .program
        .procedures
        .map((p) => p.name)
        .toSet();
  }

  group('a directory self.glp and a module of its name (d/)', () {
    test('are two modules, each named by its path from the root', () {
      final modules = discoverProgram(fixture('d'), rootSelfGlpPath: rootSelf);
      final byFile = {
        for (final m in modules)
          if (m.filePath.contains('module_paths'))
            m.filePath.split('module_paths/').last: m.moduleName
      };
      expect(byFile['d/self.glp'], 'tests/module_paths/d');
      expect(byFile['d/d.glp'], 'tests/module_paths/d/d');
    });

    test('their procedures are renamed apart', () {
      final names = linkedNames(fixture('d'));
      expect(names, contains('tests/module_paths/d:p'));
      expect(names, contains('tests/module_paths/d/d:p'));
      expect(names, contains('tests/module_paths/d/d:q'));
    });

    test('d # p reaches d.glp and its local call reaches its q', () async {
      engine.loadProgram(fixture('d'));
      final r = await engine.runGoal('p(1, Y)');
      expect(r.error, isNull);
      expect(r.succeeded, isTrue);
      expect(r.bindings['Y'], isInteger(2));
    });
  });

  group('two modules of one file name in two directories (top/)', () {
    test('are two modules, renamed apart', () {
      final names = linkedNames(fixture('top'));
      expect(names, contains('tests/module_paths/top/a/h:run'));
      expect(names, contains('tests/module_paths/top/b/h:run'));
      expect(names, contains('tests/module_paths/top/a/h:one'));
      expect(names, contains('tests/module_paths/top/b/h:two'));
    });

    test("each self.glp's h # run reaches its own directory's h", () async {
      engine.loadProgram(fixture('top'));
      final a = await engine.runGoal('ra(X)');
      expect(a.error, isNull);
      expect(a.bindings['X'], isInteger(1));
      final b = await engine.runGoal('rb(Y)');
      expect(b.error, isNull);
      expect(b.bindings['Y'], isInteger(2));
    });
  });

  group('a directory with no self.glp', () {
    test('is not a program and is refused as one', () {
      expect(() => engine.loadProgram(fixture('no_self')),
          throwsContaining('has no self.glp'));
    });

    test('its module loads alone', () async {
      engine.loadFile('${fixture('no_self')}/m.glp');
      final r = await engine.runGoal('run(X)');
      expect(r.bindings['X'], isInteger(1));
    });
  });

  group('the qualifier is resolved from the caller\'s directory', () {
    test('a qualifier naming no child of it is refused, naming the call', () {
      expect(() => engine.loadProgram(fixture('not_child')),
          throwsContaining('h # run/1: h is neither a child directory'));
    });

    test('a call to a procedure its module does not export is refused', () {
      expect(() => engine.loadProgram(fixture('not_exported')),
          throwsContaining('does not export helper/1'));
    });
  });
}
