// glp_runtime/test/compiler/srsw_head_writer_test.dart
//
// A writer occurring twice in a clause is an SRSW violation, refused by the
// analyzer at compile time whatever the guards; twice in the HEAD, the
// diagnostic names the clause.
//
// Spec: GLP-Spec sections/glp.tex, Definition "GLP Program": a clause
// satisfies SRSW "if it satisfies SO", and Definition "Single-Occurrence (SO)
// Invariant": "every variable occurs in it at most once".  Remark "Guards and
// SRSW" (bbff21d) relaxes it for the reader alone: "if the success of a guard
// implies that X? is bound to a ground term, then X? may occur multiple times
// in the clause; X occurs once, as ever."  The head is matched before the
// guard is tried, by term matching (appendix-term-matching.tex), defined for
// terms that jointly satisfy SO; GLP's ruling (GLP #3 Cowork, 2026-10-02 08:40
// UTC, G).
//
// Until 2026-10-02 a groundness-implying guard licensed a repeated writer.  In
// the head the second occurrence's get_variable overwrote the first:
// h1(same(To), To) :- ground(To?) | true reduced h1(same(4), 3).  In the head
// and again in the body, p(X, Y?) :- integer(X?) | Y = s(X) loaded, until the
// Remark of bbff21d.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

/// The compiler's diagnostic for [source], or '' if it compiles.
String _compileError(String source) {
  try {
    GlpCompiler().compile(source);
  } catch (e) {
    return e.toString();
  }
  return '';
}

void main() {
  group('a writer twice in the head is refused', () {
    final refused = {
      // The finding: in a structure and at top level, under ground/1.
      'h1(same(To), To) :- ground(To?) | true.\n':
          'Writer variable "To" occurs 2 times in the head of the clause '
              'h1(same(To), To)',
      // Twice at top level, under ground/1.
      'p(X, X) :- ground(X?) | true.\n':
          'Writer variable "X" occurs 2 times in the head of the clause p(X, X)',
      // Twice nested, under integer/1, also "Ground: yes".
      'p(f(X), g(X)) :- integer(X?) | true.\n':
          'Writer variable "X" occurs 2 times in the head of the clause '
              'p(f(X), g(X))',
      // In a list, under an arithmetic comparison.
      'p([X, X]) :- X? > 0 | true.\n':
          'Writer variable "X" occurs 2 times in the head of the clause',
      // With no guard at all, the head diagnostic is the one given.
      'p(X, X).\n':
          'Writer variable "X" occurs 2 times in the head of the clause p(X, X)',
      // Three times.
      'p(X, f(X), [X]) :- ground(X?) | true.\n':
          'Writer variable "X" occurs 3 times in the head',
    };

    for (final MapEntry(key: clause, value: diagnostic) in refused.entries) {
      test(clause.trim(), () {
        final e = _compileError(clause);
        expect(e, contains('SRSW violation'), reason: e);
        expect(e, contains(diagnostic), reason: e);
      });
    }

    test('the diagnostic names the procedure and the line', () {
      final e = _compileError('q(a).\n\nh1(same(To), To) :- ground(To?) | true.\n');
      expect(e, contains('h1/2: Line 3: Writer variable "To"'), reason: e);
    });

    test('the engine refuses to load the finding', () {
      final engine =
          GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      const source = '''
Same ::= same(Integer).
procedure h1(Same?, Integer?).
h1(same(To), To) :- ground(To?) | true.
''';
      Object? error;
      bool? loaded;
      try {
        loaded = engine.loadSource(source, filename: 'h1.glp');
      } catch (e) {
        error = e;
      }
      expect(loaded, isNot(isTrue));
      expect('$error', contains('occurs 2 times in the head'), reason: '$error');
    });
  });

  group('a writer twice in the clause is refused, whatever the guards', () {
    // GLP-Spec glp.tex, Remark "Guards and SRSW" (bbff21d): "X occurs once, as
    // ever".
    final refused = {
      // In the head and again in the body, under integer/1, "Ground: yes".
      'p(X, Y?) :- integer(X?) | Y = s(X).\n': 'X',
      // The same under ground/1, at a produced position of a body goal.
      'p(X, Y?) :- ground(X?) | q(X, Y).\nq(A?, b) :- A = c.\n': 'X',
      // Under =?=, which grounds both of its arguments.
      'p(X, Z, Y?) :- X? =?= Z? | Y = s(X).\n': 'X',
      // Under an arithmetic comparison.
      'p(X, Y?) :- X? > 0 | Y = s(X).\n': 'X',
      // Twice in the body under a guard on its reader, the head holding the
      // reader.
      'p(X?) :- ground(X?) | X = a, X = b.\n': 'X',
      // In the head and again as a guard's argument, which is a reader's
      // place (glp.tex, Definition "Guarded Clause").
      'p(X, Y?) :- ground(X) | Y = s(X?).\n': 'X',
    };

    for (final MapEntry(key: clause, value: name) in refused.entries) {
      test(clause.trim().replaceAll('\n', ' '), () {
        final e = _compileError(clause);
        expect(e, contains('SRSW violation'), reason: e);
        expect(e, contains('Writer variable "$name" occurs '), reason: e);
        expect(e, contains('a writer occurs once, whatever the guards'),
            reason: e);
      });
    }
  });

  group('what the rule leaves licensed', () {
    test('a reader twice under a groundness-implying guard, its writer once',
        () {
      expect(_compileError('p(X, Y?) :- integer(X?) | Y = s(X?, X?).\n'),
          isEmpty);
    });

    test('a writer and its reader in the head', () {
      expect(_compileError('p(X, X?).\n'), isEmpty);
    });

    test('anonymous variables, each a writer of its own', () {
      expect(_compileError('p(_, _, f(_)).\n'), isEmpty);
    });

    test('a writer once in the head and once in a guard-free body is refused '
        'by the general count, not the head\'s', () {
      final e = _compileError('p(X) :- q(X).\n');
      expect(e, contains('a writer occurs once, whatever the guards'),
          reason: e);
      expect(e, isNot(contains('in the head')), reason: e);
    });
  });
}
