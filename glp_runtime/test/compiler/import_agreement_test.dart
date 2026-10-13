/// An imported declaration is compared with the declaration its module
/// exports.
///
/// TGLP modules.tex, "Cross-module type checking": "The imported declaration
/// must agree with the declaration M exports: their type automata are equal
/// (structural type compatibility) once the exported declaration's type
/// parameters, if any, are instantiated (Definition Instantiation); subtyping
/// applies to the arguments of a call, not between the two declarations."
/// And: "a parameter the importing module holds open stays open across the
/// module boundary and is fixed at the call".
///
/// Each fixture under `programs/tests/import_agreement/` is its own root, a
/// `self.glp` calling `lib # p` in the `lib.glp` beside it, so the test turns
/// on the two declarations alone.  A refusal is the linker's, raised where the
/// call is resolved, naming the importing module and line, both declarations
/// and the argument where they differ.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

String _fixture(String name) =>
    Directory('../programs/tests/$name').absolute.path;

List<DiscoveredModule> _modules(String dir) =>
    discoverProgram(dir, rootSelfGlpPath: '$dir/self.glp');

void _admitted(String name) {
  final dir = _fixture(name);
  expect(() => typeCheckProgram(_modules(dir), rootDir: dir), returnsNormally);
}

void _refused(String name, List<String> mentions) {
  final dir = _fixture(name);
  expect(
    () => typeCheckProgram(_modules(dir), rootDir: dir),
    throwsA(predicate((e) {
      final s = e.toString();
      return s.contains('Imported declarations that do not agree') &&
          s.contains('$dir/self.glp:') &&
          mentions.every(s.contains);
    }, 'refuses the import, naming ${mentions.join(' and ')}')),
  );
}

void main() {
  group('an import agreeing with its export is admitted', () {
    test('equal to a monomorphic export, under another type name', () {
      _admitted('import_agreement/mono_equal');
    });

    test('at an instance of a parameterised export', () {
      _admitted('import_agreement/param_instance');
    });

    test('at an instance of a parameter occurring only as a bare alternative',
        () async {
      // The link, where the import is compared with its export, and the
      // load: the whole-program check instantiates the call lib:get(O?, P),
      // binding X as the import check binds it (TGLP
      // appendix-implementation-notes.tex, "The instantiation of a call").
      final dir = _fixture('import_agreement/bare_param_instance');
      expect(() => linkProgram(_modules(dir), rootDir: dir), returnsNormally);
      final engine =
          GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(engine.loadProgram(dir), isTrue);
      final r = await engine.runGoal('go(3, P)');
      expect(r.succeeded, isTrue, reason: r.error ?? '');
      final p = engine.runtime.heap.dereference(r.bindings['P']!);
      expect(p, isA<rt.ConstTerm>().having((t) => t.value, 'value', 3));
    });

    test('holding a parameter of a parameterised export open', () {
      _admitted('param_import_linked');
    });
  });

  group('an import disagreeing with its export is refused', () {
    test('a subtype of the export', () {
      _refused('import_agreement/subtype_neg', [
        'imported procedure lib#pick(Warm?, Warm).',
        'exported procedure pick(Colour?, Colour).',
        'argument 1: the import has Warm? where the export has Colour?',
      ]);
    });

    test('at _? against a concrete export type', () {
      _refused('import_agreement/wildcard_neg', [
        'imported procedure lib#pick(_?, Colour).',
        'argument 1: the import has _? where the export has Colour?',
      ]);
    });

    test('a mode difference', () {
      _refused('import_agreement/mode_neg', [
        'imported procedure lib#pick(Colour, Colour).',
        'argument 1: the import declares it produced where the export '
            'declares it consumed (?)',
      ]);
    });

    test('at no instance of a parameter occurring only as a bare alternative',
        () {
      _refused('import_agreement/bare_param_neg', [
        'imported procedure lib#get(Colour?, Colour).',
        'exported procedure(X) get(Opt(X)?, Opt(X)).',
        'argument 1: the import has Colour?',
        'only the export\'s has',
      ]);
    });

    test('one parameter of the export instantiated two ways', () {
      _refused('import_agreement/param_inconsistent_neg', [
        'exported procedure(X) pass(Stream(X)?, Stream(X)).',
        'argument 2: the import has Constant',
        'its parameter X, instantiated to Integer',
      ]);
    });
  });
}
