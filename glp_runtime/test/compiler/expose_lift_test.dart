/// What `-expose` lifts: the exported procedures of the exposed module and the
/// types their signatures carry, read in the exposed module's own scope, and
/// nothing else of it.
///
/// TGLP modules.tex, "The -expose directive": `-expose(M).` "lifts the
/// exported procedures of module M (and the types their signatures carry) into
/// that directory's scope, as if defined in its self.glp"; "Procedure
/// declarations": "A declaration carries the transitive closure of the types
/// its signature references, so types are not exported separately";
/// Definition (Root, Scope): the scope of M runs from the root down to M.
/// GLP #3 Cowork, 2026-10-04 09:06 UTC, "21:20" (the lift carries the types the
/// exported signatures carry and no other) and "00:34" (a program-root self.glp
/// names the types its -expose lifts); Integration, 2026-10-04 11:28 UTC (the
/// root's -expose(social#graph#routing#intro) and a signature naming a type of
/// social/graph/self.glp).
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;

  List<DiscoveredModule> discover(String dir) =>
      discoverProgram(Directory(dir).absolute.path, rootSelfGlpPath: rootSelf);

  DiscoveredModule module(List<DiscoveredModule> modules, String suffix) =>
      modules.firstWhere((m) => m.filePath.endsWith(suffix));

  group('programs/sglp: -expose(monitor)', () {
    final sglp = File('../programs/sglp/self.glp').absolute.path;

    test('lifts what monitor/4 carries, and not Queue or Clock', () {
      final lift = module(discover('../programs/sglp'), '/sglp/monitor.glp').lift!;
      expect(lift.scope.procedures.keys, contains('monitor/4'));
      for (final t in ['Request', 'Rate', 'Token', 'TimeUnit']) {
        expect(lift.scope.types.keys, contains(t), reason: t);
      }
      expect(lift.scope.typeOrigins['TimeUnit'], 'sglp/monitor');
      expect(lift.scope.types.keys, isNot(contains('Queue')));
      expect(lift.scope.types.keys, isNot(contains('Clock')));
    });

    test('a scope below sglp/self.glp has TimeUnit, not Queue or Clock', () {
      final scope = buildAncestorScope(chain: [sglp], rootSelfGlpPath: rootSelf);
      expect(scope.types.keys, contains('TimeUnit'));
      expect(scope.types.keys, isNot(contains('Queue')));
      expect(scope.types.keys, isNot(contains('Clock')));
      final self = module(discover('../programs/sglp'), '/sglp/self.glp');
      expect(self.ancestorScope.types.keys, contains('TimeUnit'));
      expect(self.ancestorScope.types.keys, isNot(contains('Queue')));
      expect(self.ancestorScope.types.keys, isNot(contains('Clock')));
    });
  });

  group('tests/expose/own_scope: a type from the exposed module\'s chain', () {
    const app = '../programs/tests/expose/own_scope/app';

    test('Level is read in rates.glp\'s scope and lifted', () {
      final modules = discover(app);
      final rates = module(modules, '/rates.glp');
      expect(rates.lift!.scope.types.keys, contains('Level'));
      expect(rates.lift!.scope.typeOrigins['Level'],
          'tests/expose/own_scope/lib');
      final self = module(modules, '/own_scope/app/self.glp');
      expect(self.ancestorScope.procedures.keys, contains('rate/2'));
      expect(self.ancestorScope.types.keys, contains('Level'));
    });

    test('the linked program carries lib/self.glp\'s Level', () {
      final modules = discover(app);
      final flat = linkedFlatModule(
          modules, linkProgram(modules, rootDir: Directory(app).absolute.path));
      expect(flat.typeDefs.map((t) => t.name),
          contains('tests/expose/own_scope/lib:Level'));
    });

    test('loads and runs', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram(app), isTrue);
      final run = await engine.runGoal('run(N)');
      expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
      expect(run.bindings['N'].toString(), 'Const(2)');
      final rateOf = await engine.runGoal('rate_of(low, M)');
      expect(rateOf.succeeded, isTrue, reason: 'Error: ${rateOf.error}');
      expect(rateOf.bindings['M'].toString(), 'Const(1)');
    });
  });

  group('tests/expose/unlifted: a type no signature carries', () {
    test('clock.glp\'s lift carries Unit and not Tick', () {
      final lift =
          module(discover('../programs/tests/expose/unlifted/pos'), '/clock.glp')
              .lift!;
      expect(lift.scope.types.keys, contains('Unit'));
      expect(lift.scope.types.keys, isNot(contains('Tick')));
    });

    test('pos/, naming Unit, loads and runs', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram('../programs/tests/expose/unlifted/pos'),
          isTrue);
      final r = await engine.runGoal('run(min, N)');
      expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
      expect(r.bindings['N'].toString(), 'Const(60)');
    });

    test('neg/, naming Tick, is refused at its declaration', () {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(
          () => engine.loadProgram('../programs/tests/expose/unlifted/neg'),
          throwsA(predicate((e) =>
              e.toString().contains('undefined type "Tick"') &&
              e.toString().contains('run/1'))));
    });
  });

  group('tests/expose/names_lifted: the exposing self.glp names T', () {
    const dir = '../programs/tests/expose/names_lifted';

    test('loads, and p/1 is declared over the lifted T', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram(dir), isTrue);
      final r = await engine.runGoal('p(a)');
      expect(r.succeeded, isTrue, reason: 'Error: ${r.error}');
    });

    test('a goal outside T is refused', () async {
      final engine = GlpEngine(rootSelfGlpPath: rootSelf);
      expect(engine.loadProgram(dir), isTrue);
      final r = await engine.runGoal('p(c)');
      expect(r.succeeded, isFalse);
    });
  });
}
