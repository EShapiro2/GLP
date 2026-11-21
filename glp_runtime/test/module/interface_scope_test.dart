/// A root type whose part a nearer scope shadows keeps the root's part, on
/// both sides of run/3's check: the declared table find_type/2 reads, and the
/// exported table the loader derives from an artefact's interface text.
///
/// TGLP modules.tex, Compilation, steps three and four: every type of every
/// module, self.glp included, is renamed M:T, and "every type reference is
/// resolved the same way, to the renamed type of the nearest scope defining
/// it"; Dynamic Activation: the type identity expands "every type the
/// signature references" as so resolved.  So the root's Args is built of the
/// root's Arg in every program, and an Arg of the caller's scope does not
/// enter its identity.  Until 2026-11-21 the interface text read every name
/// in the entry module's scope, and the derived identity of an export at a
/// root type took the program's own part: run(contract(_, P), Tc, M) was
/// refused on cssn, gsn and gkk, whose self.glp defines an Atom of its own.
///
/// programs/tests/interface_scope/shadowed defines an Arg of its own beside
/// the root's Args; ../plain defines none.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/wire/artefact.dart';

const _root = '../programs/self.glp';
const _dir = '../programs/tests/interface_scope';

void main() {
  late Artefact shadowed, plain;
  late TypeIdentityTables shadowedCompiled, plainCompiled;

  setUpAll(() {
    (Artefact, TypeIdentityTables) load(String g) {
      final e = GlpEngine(rootSelfGlpPath: _root)
        ..loadProgram(File('$_dir/$g').absolute.path);
      return (
        e.appModule!.artefact as Artefact,
        e.appModule!.declaredTypes as TypeIdentityTables
      );
    }

    (shadowed, shadowedCompiled) = load('shadowed');
    (plain, plainCompiled) = load('plain');
  });

  TypeIdentityTables derived(Artefact a) => interfaceTypeIdentityTables(
      typeDefsText: a.typeDefsText,
      exportDeclarationTexts: a.exports.map((e) => e.declarationText));

  String declared(TypeIdentityTables t, String suffix) =>
      t.declared.entries.firstWhere((e) => e.key.endsWith(suffix)).value;

  test('find_type side: a declaration at the root\'s Args has the root\'s '
      'Arg in a scope that shadows Arg', () {
    expect(declared(shadowedCompiled, 'host/1'),
        declared(plainCompiled, 'host/1'));
    expect(shadowedCompiled.exported['given/1'],
        plainCompiled.exported['given/1']);
  });

  test('exports side: the identity derived from the interface text is the '
      'compiled program\'s, the root\'s Args built of the root\'s Arg', () {
    expect(derived(shadowed).exported['given/1'], isNotNull);
    expect(derived(shadowed).exported['given/1'],
        shadowedCompiled.exported['given/1']);
    expect(derived(shadowed).exported['given/1'],
        derived(plain).exported['given/1']);
  });

  test('the scope\'s own Arg keeps its own automaton, bare in the text, and '
      'is not the root\'s', () {
    expect(shadowed.typeDefsText, contains('Arg ::= other(Integer).'));
    expect(derived(shadowed).exported['own/1'],
        shadowedCompiled.exported['own/1']);
    expect(derived(shadowed).exported['own/1'],
        isNot(derived(plain).exported['root_arg/1']));
    expect(derived(plain).exported['root_arg/1'],
        plainCompiled.exported['root_arg/1']);
  });
}
