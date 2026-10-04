/// A type an ancestor exposes is in the declaration scope of every layer.
///
/// TGLP modules.tex, "The -expose directive": `-expose(M).` "lifts the
/// exported procedures of module M (and the types their signatures carry) into
/// that directory's scope, as if defined in its self.glp".  The self.glp of
/// expose/root_types_decl exposes lib#nets, which defines NetStream, so
/// NetStream is in the scope of every self.glp below it --- decl/self.glp's ---
/// at the moment that self.glp is layered (Definition (Root, Scope)).
///
/// Until this test the root-scope environment realised the root's definitions
/// but not its exposes, and they were lifted only after every ancestor scope
/// had been layered: a self.glp declaration naming NetStream with its
/// parameters named was refused as naming an undefined type, and one naming no
/// parameters read NetStream as a parameter (cert_refused's leak/1, at its
/// self.glp:7 and :10).  The exposer was the root self.glp, whose
/// -expose(system#mad_predicates) lifted NetStream into every scope, until
/// send_to_net/1 became the root self.glp's own (GLP-Spec appendix-guards,
/// "Output to the network"); the fixture now carries its own exposing
/// ancestor, and cert_refused's leak/1 takes send_to_net/1's Stream(_).
library;

import 'dart:io';

import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;

  DiscoveredModule module(String dir, String file) =>
      discoverProgram(Directory(dir).absolute.path, rootSelfGlpPath: rootSelf)
          .firstWhere((m) => m.filePath.endsWith(file));

  const decl = '../programs/tests/expose/root_types_decl/decl';

  test('a named-parameter declaration in a self.glp resolves NetStream',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    expect(engine.loadProgram(decl), isTrue);
    final result = await engine.runGoal('count([], N)');
    expect(result.succeeded, isTrue, reason: 'Error: ${result.error}');
    expect(result.bindings['N'].toString(), 'Const(0)');
  });

  test('an unnamed declaration in a self.glp takes NetStream as a type', () {
    final scope = module(decl, 'app.glp').ancestorScope;
    expect(scope.paramProcDecls.containsKey('count/2'), isFalse,
        reason: 'NetStream is a type, not a parameter');
    expect(scope.procedures['count/2']!.typeParams, isEmpty);
    expect(scope.procedures['count/2']!.argTypes.first.toString(),
        'NetStream?');
    expect(scope.types.containsKey('NetStream'), isTrue);
  });

  test('count/2, exported and imported, is monomorphic in its self.glp', () {
    final scope = module(decl, 'app.glp').ancestorScope;
    for (final key in ['count/2', 'app#count/2']) {
      expect(scope.paramProcDecls.containsKey(key), isFalse, reason: key);
      expect(scope.procedures[key]!.typeParams, isEmpty, reason: key);
    }
  });
}
