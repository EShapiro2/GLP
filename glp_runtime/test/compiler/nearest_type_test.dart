/// A type reference resolves to the renamed type of the nearest scope defining
/// it, an expansion instance's arguments among them.
///
/// TGLP modules.tex, Compilation, fourth step: "every type reference is
/// resolved the same way, to the renamed type of the nearest scope defining
/// it"; Definition "Root, Scope", the module's own definitions shadowing its
/// ancestors'.
///
/// The fixture, programs/tests/nearest_type: its self.glp defines Peers, never
/// empty, and names Stream(Peers) in count/2's declaration, so the scope of
/// every module below it holds the expansion instance Stream<Peers> at that
/// Peers.  m.glp defines a Peers of its own, which may be empty, and calls its
/// parameterised app/3 with lists of its Peers, [] among them.  Until
/// 2026-11-07 the module's scope kept the enclosing Stream<Peers> under its
/// bare name, its Peers renamed tests/nearest_type:Peers, and the call's
/// instantiation at X = Peers found that instance already defined, so m.glp
/// was refused: "No alternative of tests/nearest_type:Peers? matches the
/// constant []".  The same fault refused sGLP's run of the canonical social
/// graph under programs/sglp, whose self.glp defines Peers ::= [Peer |
/// List(Peer)] and names Stream(Peers), at the platform's subsets/2.
library;

import 'dart:io';

import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final dir = Directory('../programs/tests/nearest_type').absolute.path;

  test('m.glp\'s call of app/3 is checked at its own Peers, and runs',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(engine.loadProgram(dir), isTrue);
    final run = await engine.runGoal('run(N)');
    expect(run.succeeded, isTrue, reason: 'Error: ${run.error}');
    expect(run.bindings['N'].toString(), 'Const(3)');
  });

  test('m.glp checks on its own against its scope', () {
    final modules = discoverProgram(dir, rootSelfGlpPath: rootSelf);
    expect(() => checkModulesIndependently(modules), returnsNormally);
  });

  test('a shadowed argument renames the instance named by it', () {
    final outer = TypeEnvironment({
      'Peers': TypeDef('Peers', [TypeRef('Integer', 0, 0)], 0, 0),
      'Stream<Peers>': TypeDef('Stream<Peers>', [
        ListNilAlt(0, 0),
        ListConsAlt(
            TypeRef('Peers', 0, 0), TypeRef('Stream<Peers>', 0, 0), 0, 0)
      ], 0, 0),
    }, {}, typeOrigins: {'Peers': 'o', 'Stream<Peers>': 'o'});
    final inner = TypeEnvironment({
      'Peers': TypeDef('Peers', [TypeRef('String', 0, 0)], 0, 0),
    }, {});
    final kept = outer.shadowedBy(inner);
    expect(kept.types.keys, containsAll(['o:Peers', 'Stream<o:Peers>']));
    expect(kept.types.keys, isNot(contains('Stream<Peers>')));
    final cons = kept.types['Stream<o:Peers>']!.alternatives[1] as ListConsAlt;
    expect((cons.head as TypeRef).name, 'o:Peers');
    expect((cons.tail as TypeRef).name, 'Stream<o:Peers>');
  });

  test('renameInstanceName renames nested arguments and keeps input marks',
      () {
    final rename = {'Peers': 'o:Peers'};
    expect(renameInstanceName('Stream<Peers>', rename), 'Stream<o:Peers>');
    expect(renameInstanceName('Pair<Peers?,Integer>', rename),
        'Pair<o:Peers?,Integer>');
    expect(renameInstanceName('Stream<List<Peers>>', rename),
        'Stream<List<o:Peers>>');
    expect(renameInstanceName('Stream<Integer>', rename), 'Stream<Integer>');
    expect(renameInstanceName('Peers', rename), 'Peers');
  });
}
