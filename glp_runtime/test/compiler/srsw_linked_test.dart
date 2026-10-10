/// SRSW on a linked program.
///
/// TGLP (modules.tex §Compilation): the flat program is the linked program and
/// it is the object checked; every clause in it satisfies SRSW, as def:glp-program
/// requires of a GLP program, the alias clauses of the fifth step included ---
/// each argument threaded at its declared polarity, a writer in the head and its
/// paired reader in the body at each consumed argument, a reader in the head and
/// its paired writer in the body at each produced one.
///
/// Until 2026-09-18 `compileProgram` defaulted to skipping the SRSW pass for a
/// linked program, and nothing else performed it, so a directory program was
/// compiled and run with no SRSW check. The pass runs for every program now.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

void main() {
  late GlpEngine engine;

  setUp(() {
    engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  });

  test('a linked program with an unwritten reader in a head output argument is rejected',
      () {
    final dir = Directory('../programs/tests/srsw_linked_neg').absolute.path;
    expect(
      () => engine.loadProgram(dir),
      throwsA(predicate((e) {
        final s = e.toString();
        return s.contains('SRSW violation') && s.contains('"Xs"');
      }, 'names the SRSW violation and the variable')),
    );
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('the alias clauses of a program with produced arguments satisfy SRSW',
      () async {
    // expose/basic exports use_exposed(Integer): its alias clause threads the
    // produced argument as a reader in the head and a writer in the body, and
    // the pass admits it.
    final dir = Directory('../programs/tests/expose/basic').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
    final result = await engine.runGoal('use_exposed(R)');
    expect(result.succeeded, isTrue, reason: 'Error: ${result.error}');
    expect(result.bindings['R'].toString(), contains('100'));
  });
}
