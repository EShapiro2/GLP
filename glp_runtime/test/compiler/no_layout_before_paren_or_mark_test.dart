// glp_runtime/test/compiler/no_layout_before_paren_or_mark_test.dart
//
// "A functor and its opening parenthesis, and a type and its ?, are written
// with nothing between them, and the reader refuses a program that puts white
// space or a line break there" (GLP-Spec ef7c2cb, appendix-guards.tex,
// "Predefined types"; GLP #11 Cowork, 2026-11-01 22:33 UTC).  The refusal names
// the line of the "(" or the "?".  Until 2026-11-02 `p (X)`, `p\n(X)`,
// `Integer ?` and `Integer\n?` were read as `p(X)` and `Integer?`.

import 'dart:io';

import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The CompileError the parser raises on [source]; fails the test if it parses.
CompileError _refusal(String source) {
  try {
    Parser(Lexer(source).tokenize()).parseModule();
  } on CompileError catch (e) {
    return e;
  }
  fail('parsed: $source');
}

void _parses(String source) {
  Parser(Lexer(source).tokenize()).parseModule();
}

const _functorRule =
    'a functor and its opening parenthesis are written with nothing between them';
const _markRule = 'a type and its "?" are written with nothing between them';

/// [source] with [gap] at the one place marked `^`, and with nothing there.
(String, String) _at(String source, String gap) =>
    (source.replaceFirst('^', gap), source.replaceFirst('^', ''));

/// The line and the column of the first character after the `^` of [source]
/// once [gap] stands there.
(int, int) _position(String source, String gap) {
  final text = source.replaceFirst('^', gap);
  final at = source.indexOf('^') + gap.length;
  final before = text.substring(0, at);
  final line = '\n'.allMatches(before).length + 1;
  final column = at - (before.lastIndexOf('\n') + 1) + 1;
  return (line, column);
}

void main() {
  group('layout between a functor and its "(" is refused', () {
    for (final (what, source, functor) in [
      ('a clause head', 'p^(a).', 'p'),
      ('a goal', 'p(X) :- q^(X?).\nq(_).', 'q'),
      ('a structure in a term', 'p(f^(a)).', 'f'),
      ('a structure in a goal argument', 'p(X) :- q(X?, g^(1)).\nq(_, _).',
          'g'),
      ('an operator name as a functor', 'p(-^(1, 2)).', '-'),
      ('mod as a functor', 'p(X) :- X := mod^(7, 2).', 'mod'),
      ('a quoted name as a functor', "p('f g'^(1)).", 'f g'),
      ('a procedure declaration', 'procedure p^(Integer?).\np(_).', 'p'),
      ('an operator procedure declaration',
          'procedure =?=^(Integer?, Integer?).', '=?='),
      ('a type argument in a declaration',
          'procedure p(Stream^(Integer)?).\np(_).', 'Stream'),
      ('a type definition head', 'T^(X) ::= f(X).', 'T'),
      ('a type definition alternative', 'T ::= f^(Integer).', 'f'),
      ('a type argument in a type definition', 'T ::= f(List^(Integer)).',
          'List'),
      ('a mode directive', '-mode^(system).', 'mode'),
    ]) {
      for (final (gap, kind) in [
        ('\n', 'a line break'),
        (' ', 'a space'),
        ('\t', 'a tab'),
        ('/* c */', 'a comment'),
      ]) {
        test('in $what, $kind', () {
          final (spaced, joined) = _at(source, gap);
          final e = _refusal(spaced);
          expect(e.category, ErrorCategory.syntax, reason: '$e');
          expect(e.message, contains(_functorRule), reason: '$e');
          expect(e.message, contains('the functor "$functor"'), reason: '$e');
          final (line, column) = _position(source, gap);
          expect((e.line, e.column), (line, column), reason: '$e');
          if (what != 'a mode directive' &&
              what != 'an operator procedure declaration') {
            _parses(joined);
          }
        });
      }
    }

    test('the refusal names the functor\'s line where the "(" is on the next',
        () {
      final e = _refusal('p(a).\nq\n(b).');
      expect(e.line, 3);
      expect(e.message, contains('"q" (line 2)'));
    });
  });

  group('layout between a type and its "?" is refused', () {
    for (final (what, source, type) in [
      ('a type name in a declaration', 'procedure p(Integer^?).\np(_).',
          'Integer'),
      ('a parameterised type in a declaration',
          'procedure p(Stream(Integer)^?).\np(_).', 'Stream(...)'),
      ('the open type in a declaration', 'procedure p(_^?).\np(_).', '_'),
      ('a qualified type in a declaration',
          'imported procedure m#p(m#T^?).', 'T'),
      ('a type name in a type definition', 'T ::= f(Integer^?).', 'Integer'),
      ('a parameterised type in a type definition',
          'T ::= f(List(Integer)^?).', 'List(...)'),
      ('the open type in a type definition', 'T ::= f(_^?).', '_'),
    ]) {
      for (final (gap, kind) in [
        ('\n', 'a line break'),
        (' ', 'a space'),
        ('/* c */', 'a comment'),
      ]) {
        test('$what, $kind', () {
          final (spaced, joined) = _at(source, gap);
          final e = _refusal(spaced);
          expect(e.category, ErrorCategory.syntax, reason: '$e');
          expect(e.message, contains(_markRule), reason: '$e');
          expect(e.message, contains('the type "$type"'), reason: '$e');
          final (line, column) = _position(source, gap);
          expect((e.line, e.column), (line, column), reason: '$e');
          _parses(joined);
        });
      }
    }
  });

  group('a program with layout there does not load', () {
    late GlpEngine engine;
    setUp(() => engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path));

    const joined = 'procedure p(Integer?, Integer).\n'
        'p(X, Y?) :- Y := X? + 1.\n';

    test('written joined, it loads and runs', () async {
      expect(engine.loadSource(joined), isTrue);
      final r = await engine.runGoal('p(3, Y).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(r.bindings['Y'].toString(), 'Const(4)');
    });

    for (final (what, source, rule, line) in [
      ('a newline between a functor and its "("',
          'procedure p(Integer?, Integer).\np\n(X, Y?) :- Y := X? + 1.\n',
          _functorRule, 3),
      ('a newline between a type and its "?"',
          'procedure p(Integer\n?, Integer).\np(X, Y?) :- Y := X? + 1.\n',
          _markRule, 2),
    ]) {
      test('$what: refused, naming line $line', () {
        expect(
            () => engine.loadSource(source),
            throwsA(predicate((e) =>
                '$e'.contains(rule) &&
                ('$e'.contains('line $line') || '$e'.contains('Line $line')))));
      });
    }
  });
}
