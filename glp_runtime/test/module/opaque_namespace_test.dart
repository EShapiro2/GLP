/// The interface text an opaque guest's record carries keeps the template's
/// types apart from the guest's own: each type of its entry's declaration and
/// of the template's is qualified by the scope that defines it, so a guest's
/// own type of the name of a type the template reaches is another name there
/// (GLP #6, 2026-10-13 11:57 UTC: "it is the implementation's to keep the
/// template's types and the guest's apart in the interface text (qualify the
/// template's by their scope), compliance with TGLP's identity, which is of
/// automata, not names"; TGLP modules.tex, Definition "Type Identity of a
/// Type, Opaque Instance"; IGLP code-format-fragment.tex, "Interface table").
///
/// programs/tests/opaque_ns/self.glp declares the template entry/2, which
/// reaches Box(X) and, through it, Inner(X); guest_own is a guest at Move that
/// defines a type Inner of its own and exports other/2 at it, guest_plain one
/// that defines none.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/wire/artefact.dart';

const _root = '../programs/self.glp';
const _dir = '../programs/tests/opaque_ns';

void main() {
  late Artefact own, plain;
  late TypeIdentityTables ownCompiled, plainCompiled;

  setUpAll(() {
    (Artefact, TypeIdentityTables) load(String g) {
      final e = GlpEngine(rootSelfGlpPath: _root)
        ..loadProgram(File('$_dir/$g').absolute.path);
      return (
        e.appModule!.artefact as Artefact,
        e.appModule!.declaredTypes as TypeIdentityTables
      );
    }

    (own, ownCompiled) = load('guest_own');
    (plain, plainCompiled) = load('guest_plain');
  });

  TypeIdentityTables derived(Artefact a) => interfaceTypeIdentityTables(
      typeDefsText: a.typeDefsText,
      exportDeclarationTexts: a.exports.map((e) => e.declarationText));

  test('the entry and the template name the host\'s types qualified by '
      'their scope, and the guest\'s own Inner bare', () {
    final entry = own.exports.firstWhere((e) => e.name == 'entry');
    expect(entry.declarationText,
        'exported procedure entry(Box__tests_opaque_ns(Move__tests_opaque_ns_guest_own)?, '
        'Box__tests_opaque_ns(Move__tests_opaque_ns_guest_own)).\n'
        'procedure(X) entry(Box__tests_opaque_ns(X)?, Box__tests_opaque_ns(X)).');
    expect(own.typeDefsText,
        contains('Inner__tests_opaque_ns(X) ::= inner(X).'));
    expect(own.typeDefsText, contains('Inner ::= other(Integer).'));
    final other = own.exports.firstWhere((e) => e.name == 'other');
    expect(other.declarationText, 'exported procedure other(Inner?, Inner).');
  });

  test('the record derived from the text is the compiled program\'s, and a '
      'guest\'s own type of a name the template reaches changes neither', () {
    final d = derived(own);
    expect(d.opaque['entry/2'], isNotNull);
    expect(d.opaque['entry/2'], ownCompiled.opaque['entry/2']);
    expect(d.opaque['entry/2'], derived(plain).opaque['entry/2']);
    expect(d.opaque['entry/2'], plainCompiled.opaque['entry/2']);
    expect(d.exported['entry/2'], ownCompiled.exported['entry/2']);
  });

  test('the guest\'s own Inner keeps its own automaton in the derived table',
      () {
    expect(derived(own).exported['other/2'], isNotNull);
    expect(derived(own).exported['other/2'], ownCompiled.exported['other/2']);
  });
}
