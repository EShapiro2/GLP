// glp_runtime/test/compiler/srsw_head_writer_test.dart
//
// A writer occurring twice in a clause HEAD is an SRSW violation, refused by
// the analyzer at compile time whatever the guards.
//
// Spec: GLP-Spec sections/glp.tex, Definition "GLP Program": a clause
// satisfies SRSW "if it satisfies SO", and Definition "Single-Occurrence (SO)
// Invariant": "every variable occurs in it at most once".  The head is matched
// before the guard is tried, by term matching (appendix-term-matching.tex),
// defined for terms that jointly satisfy SO; so the relaxation of Remark
// "Guards and SRSW" does not reach a second head occurrence.  GLP's ruling
// (GLP #3 Cowork, 2026-10-02 08:40 UTC, G).
//
// Until 2026-10-02 a groundness-implying guard licensed the repeated head
// writer, and the second occurrence's get_variable overwrote the first:
// h1(same(To), To) :- ground(To?) | true reduced h1(same(4), 3).

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

  group('what the head rule leaves licensed', () {
    test('a writer in the head and again in the body, under a groundness-'
        'implying guard (Remark "Guards and SRSW")', () {
      expect(_compileError('p(X, Y?) :- integer(X?) | Y = s(X).\n'), isEmpty);
    });

    test('a writer and its reader in the head', () {
      expect(_compileError('p(X, X?).\n'), isEmpty);
    });

    test('anonymous variables, each a writer of its own', () {
      expect(_compileError('p(_, _, f(_)).\n'), isEmpty);
    });

    test('a writer once in the head and once in a guard-free body is still '
        'refused as before, by the general count', () {
      final e = _compileError('p(X) :- q(X).\n');
      expect(e, contains('without a groundness-implying guard'), reason: e);
      expect(e, isNot(contains('in the head')), reason: e);
    });
  });
}
