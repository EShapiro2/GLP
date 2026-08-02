// glp_runtime/test/compiler/per_module_check_test.dart
//
// Step 2 of static linking (TGLP modules.tex §Static Linking): "each module is
// type-checked independently against its ancestor scope, exactly as for
// single-file compilation".
//
// Regression guard: only the linked check ran, and step 5 (dead-code
// elimination) restricts the linked program to the procedures the root
// self.glp's exports reach. A module no entry point reaches was therefore
// type-checked by nothing, and a directory load reported "Loaded program" with
// no diagnostics over a file that called a procedure at an arity that does not
// exist and constructed a term of a type the scope no longer declared (vGLP,
// 2026-07-23, against programs/grassapp).
//
// Fixture: programs/tests/unreachable_neg/. Its root self.glp exports one entry
// point, which reaches reached.glp and nothing else; unreached_arity.glp and
// unreached_type.glp carry one of vGLP's two cases each.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;
  final fixtureDir =
      Directory('../programs/tests/unreachable_neg').absolute.path;

  late GlpEngine engine;

  setUp(() => engine = GlpEngine(rootSelfGlpPath: rootSelf));

  String loadError() {
    try {
      engine.loadProgram(fixtureDir);
      return '';
    } catch (e) {
      return e.toString();
    }
  }

  group('Per-module type check of a directory program', () {
    test('a module no entry point reaches is checked, and the load fails', () {
      expect(loadError(), contains('Type checking failed'));
    });

    test('the unreachable stale call is rejected, naming its own file', () {
      final err = loadError();
      expect(err, contains('unreached_arity.glp'));
      expect(err, contains('reached#mint/3'));
    });

    test('the unreachable stale type is rejected, naming its own file', () {
      final err = loadError();
      expect(err, contains('unreached_type.glp'));
      expect(err, contains('holding'));
    });

    test('errors carry the module\'s own path, not the linked M:p rename', () {
      final err = loadError();
      expect(err, contains('${Platform.pathSeparator}unreached_type.glp:'));
      expect(err, isNot(contains('unreached_type:stale_type')));
    });

    test('the reachable modules are not implicated', () {
      final err = loadError();
      expect(err, isNot(contains('reached.glp')));
      expect(err, isNot(contains('self.glp')));
    });

    test('programs whose every module is well-typed still load', () {
      // Control: the same machinery over a directory program with no
      // unreachable breakage. module_self_procs is a three-file directory
      // program (self.glp + boot.glp + worker.glp) whose root self.glp forwards
      // an export, so it has an entry point and links cleanly. It replaced
      // co_load_neg here on 2026-08-02, when the entry-point rejection
      // (modules.tex §Static Linking) made that directory a negative fixture.
      expect(
          engine.loadProgram(
              Directory('../programs/tests/module_self_procs').absolute.path),
          isTrue);
    });
  });
}
