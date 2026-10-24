/// An exposed module outside the program's tree is collected with the
/// self.glp of each directory from the root down to it and compiled in its
/// own scope, and a call it makes to a procedure of a self.glp on its own
/// chain resolves there.
///
/// TGLP 47d62b2, modules.tex, Compilation, first step: "the compiler collects
/// every .glp file of the program's directory tree, together with the self.glp
/// of each directory from the root down to the program, and every module a
/// self.glp among them exposes, with the self.glp of each directory from the
/// root down to that module, and constructs the scope of each module
/// (def:scope); an exposed module outside the program's tree is compiled in
/// its own scope, as every module is, and a call it makes to a procedure of a
/// self.glp on its own chain resolves there."  GLP #10 Cowork, 2026-10-23
/// 23:30 UTC, answering Code's case 6 of 2026-10-04 12:00 UTC.
///
/// The fixture, programs/tests/expose/outer_chain: its self.glp exposes
/// lib/util.glp, whose quad/2 calls double/2 of lib/self.glp and whose
/// quarter/2 calls half/2, which lib/self.glp exposes from tools/half.glp
/// ("The -expose directive": lifted "into that directory's scope, as if
/// defined in its self.glp"); app/ and neg/ are programs, and lib/self.glp is
/// on neither's chain.  app/ defines a double/2 of its own.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  const fixture = '../programs/tests/expose/outer_chain';
  const lib = 'tests/expose/outer_chain/lib';
  const app = 'tests/expose/outer_chain/app';

  List<DiscoveredModule> discover(String dir) => discoverProgram(
      Directory(dir).absolute.path,
      rootSelfGlpPath: rootSelf);

  test('lib/self.glp, on the chain of the exposed util.glp, is a module of '
      'the program', () {
    final modules = discover('$fixture/app');
    final libSelf =
        modules.singleWhere((m) => m.filePath.endsWith('/outer_chain/lib/self.glp'));
    expect(libSelf.moduleName, lib);
    expect(libSelf.isSelfGlp, isTrue);
    expect(libSelf.collectedByExpose, isTrue);
    expect(libSelf.ast.procedures.map((p) => '${p.name}/${p.arity}'),
        contains('double/2'));
    final util =
        modules.singleWhere((m) => m.filePath.endsWith('/outer_chain/lib/util.glp'));
    expect(util.collectedByExpose, isTrue);
    // Compiled in its own scope: lib/self.glp's double/2, and the half/2
    // lib/self.glp exposes, are declared there.
    expect(util.ancestorScope.procedures.keys,
        containsAll(['double/2', 'half/2']));
    final half = modules
        .singleWhere((m) => m.filePath.endsWith('/outer_chain/lib/tools/half.glp'));
    expect(half.collectedByExpose, isTrue);
    // Neither is in the scope of app/'s self.glp.
    final appSelf =
        modules.singleWhere((m) => m.filePath.endsWith('/outer_chain/app/self.glp'));
    expect(appSelf.ancestorScope.procedures.keys, isNot(contains('half/2')));
    expect(appSelf.ancestorScope.procedures.keys, isNot(contains('double/2')));
  });

  test("util.glp's calls of double/2 resolve to lib/self.glp's, and app's "
      'own double/2 is another procedure', () {
    final dir = Directory('$fixture/app').absolute.path;
    final modules = discover(dir);
    final linked = checkedLinkedProgram(modules, rootDir: dir);
    final quad = linked.program.procedures
        .singleWhere((p) => p.name == '$lib/util:quad');
    expect([for (final g in quad.clauses.single.body!) g.functor],
        ['$lib:double', '$lib:double']);
    final quarter = linked.program.procedures
        .singleWhere((p) => p.name == '$lib/util:quarter');
    expect([for (final g in quarter.clauses.single.body!) g.functor],
        ['$lib/tools/half:half', '$lib/tools/half:half']);
    final names = {for (final p in linked.program.procedures) p.name};
    expect(names, containsAll(['$lib:double', '$app:double']));
    final triple = linked.program.procedures
        .singleWhere((p) => p.name == '$app:triple');
    expect(triple.clauses.single.body!.single.functor, '$app:double');
  });

  test('app/ loads and runs', () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(engine.loadProgram('$fixture/app'), isTrue);
    final run = await engine.runGoal('run(1, Y)');
    expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
    expect(run.bindings['Y'].toString(), 'Const(4)');
    final fourth = await engine.runGoal('fourth(8, Y)');
    expect(fourth.succeeded, isTrue, reason: 'Error: ${fourth.error}');
    expect(fourth.bindings['Y'].toString(), 'Const(2)');
    final triple = await engine.runGoal('triple(1, Y)');
    expect(triple.succeeded, isTrue, reason: 'Error: ${triple.error}');
    expect(triple.bindings['Y'].toString(), 'Const(3)');
  });

  test("neg/, calling double/2, is refused: lib/self.glp is not on its "
      'chain', () {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(
        () => engine.loadProgram('$fixture/neg'),
        throwsA(predicate((e) =>
            e.toString().contains('double/2') &&
            e.toString().contains('outer_chain/neg/self.glp'))));
  });
}
