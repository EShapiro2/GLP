// glp_runtime/test/compiler/no_guard_negation_test.dart
//
// GLP has no guard negation.  GLP-Spec glp.tex, Definition "Guarded Clause": a
// guarded clause has the form H :- G | B, "where H is the head, G is a
// conjunction of guard predicates, and B is the body"; guard negation left the
// language on 2026-10-01 (GLP-Spec 98913b4, TGLP 44f2778, IGLP 9b45225).  `~`
// begins no GLP construct, so the parser refuses `~G` in a guard as a syntax
// error, whatever G is: a built-in guard, an infix guard, a defined guard.

import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:test/test.dart';

/// The CompileError the parser raises on [source]; fails the test if it parses.
CompileError _refusal(String source) {
  try {
    Parser(Lexer(source).tokenize()).parse();
  } on CompileError catch (e) {
    return e;
  }
  fail('parsed: $source');
}

void main() {
  group('~G in a guard is a syntax error', () {
    for (final (what, source) in [
      ('a built-in guard', 'p(X, Y?) :- ~ground(X?) | Y = no.'),
      ('ground equality', 'p(X, Y, Z?) :- ~(X? =?= Y?) | Z = no.'),
      ('an arithmetic comparison', 'p(X, Y?) :- integer(X?), ~(X? > 0) | Y = no.'),
      ('a defined guard', 'p(X, Y?) :- ~d(X?) | Y = no.'),
      ('negation twice', 'p(X, Y?) :- ~~ground(X?) | Y = no.'),
      ('a guard in parentheses', 'p(X, Y?) :- (~ground(X?)) | Y = no.'),
    ]) {
      test(what, () {
        final e = _refusal(source);
        expect(e.category, ErrorCategory.syntax, reason: '$e');
        expect(e.message, contains('"~" is not GLP syntax'));
        // The error points at the `~`.
        expect(e.line, 1);
        expect(source[e.column - 1], '~', reason: '$e');
      });
    }

    test('the whole compiler refuses it at the parser', () {
      expect(
          () => GlpCompiler().compile('''
procedure d(Integer?).
d(1).
procedure use(Integer?, _).
use(X, R?) :- ~d(X?) | R = no.
'''),
          throwsA(predicate(
              (e) =>
                  e is CompileError &&
                  e.message.contains('"~" is not GLP syntax') &&
                  e.line == 4,
              'the parser\'s refusal at line 4')));
    });
  });

  group('the same guards, unnegated, parse', () {
    for (final source in [
      'p(X, Y?) :- ground(X?) | Y = no.',
      'p(X, Y, Z?) :- X? =?= Y? | Z = no.',
      'p(X, Y, Z?) :- X? =?\\= Y? | Z = no.',
      'p(X, Y?) :- integer(X?), X? > 0 | Y = no.',
      'p(X, Y?) :- (ground(X?)) | Y = no.',
      'p(X, Y?) :- otherwise | Y = no.',
    ]) {
      test(source, () {
        final module = Parser(Lexer(source).tokenize()).parse();
        expect(module.procedures.single.clauses.single.guards, isNotEmpty);
      });
    }
  });
}
