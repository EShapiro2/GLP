// glp_runtime/test/compiler/removed_guards_test.dart
//
// `tuple/1` and `atom/1` are not guards.  GLP-Spec sections/appendix-guards.tex,
// the catalogue, lists every guard, and neither is there; GLP's task of
// 2026-10-01 13:05 UTC, approved by Udi, removes them from the runtime: the
// analyzer's grounding table and negatable set, and the partial evaluator's
// redundant-guard cases.  Until then each marked its argument grounded, so a
// clause guarded by `atom(X?)` or `tuple(X?)` could read X? as often as it
// liked although the language gave neither guard a meaning.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';

void main() {
  for (final name in const ['atom', 'tuple']) {
    test('$name/1 licenses no repeated reader: it is not a guard', () {
      expect(
          () => GlpCompiler().compile('''
q(_).
p(X) :- $name(X?) | q(X?), q(X?).
'''),
          throwsA(predicate((e) => e
              .toString()
              .contains('Reader variable "X?" occurs 2 times'))));
    });
  }

  test('a catalogue guard that is "Ground: yes" still licenses it', () {
    // The control: `ground/1` is in the catalogue with "Ground: yes" (GLP-Spec
    // appendix-guards.tex; TGLP glp.tex, Remark "Guards and SRSW").
    expect(GlpCompiler().compile('''
q(_).
p(X) :- ground(X?) | q(X?), q(X?).
'''), isNotNull);
  });
}
