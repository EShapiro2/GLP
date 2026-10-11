/// One kind of constant besides numbers and `[]`: the string.  GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax": "A term is a variable,
/// a constant (numbers, strings, or the empty list `[]`) ...  A unit goal is a
/// compound term or a string, also commonly referred to as an atom."  So a
/// quoted name and a double-quoted string are two spellings of one string,
/// `'x'` and `"x"` one value; `'"x"'` is the three-character string, another;
/// the reader keeps the characters as written.  One canonical print: single
/// quoted, bare where the lexer reads it so; the double-quoted form is read
/// and never printed (GLP #3 Cowork, 2026-10-04 15:19 UTC, FAULT 2 (b)).
///
/// Until 2026-10-10 the parser kept a double-quoted string's quotes in its
/// value: `'x'` and `"x"` were two values, `"x"` and `'"x"'` one, and the
/// REPL showed `"x"`.
library;

import 'dart:convert';
import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

const _source = r'''
exported procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.
eq(_, _, no) :- otherwise | true.

exported procedure is_x(_?, Constant).
is_x("x", yes).
is_x(_, no) :- otherwise | true.

exported procedure str(_?, Constant).
str(X, yes) :- string(X?) | true.
str(_, no) :- otherwise | true.
''';

/// The values of the arguments of `t(...)` as the reader reads [args].
List<Object?> _values(String args) => [
      for (final a in Parser(Lexer('t($args).').tokenize())
          .parse()
          .procedures
          .single
          .clauses
          .single
          .head
          .args)
        (a as ConstTerm).value
    ];

Future<String> _answer(String goal) async {
  final e = GlpEngine(rootSelfGlpPath: _root);
  expect(e.loadSource(_source, filename: 'one_string.glp'), isTrue);
  final r = await e.runGoal(goal);
  expect(r.error, isNull, reason: goal);
  expect(r.status, ExecutionStatus.succeeded, reason: goal);
  return '${(r.bindings['R'] as rt.ConstTerm).value}';
}

void main() {
  group('the reader keeps the characters as written', () {
    test("'x' and \"x\" read as the one string x", () {
      expect(_values(r'''x, 'x', "x"'''), ['x', 'x', 'x']);
    });
    test("'\"x\"' is the three-character string", () {
      expect(_values(r''''"x"', "x"'''), ['"x"', 'x']);
      expect((_values(r''''"x"' ''').single as String).length, 3);
    });
    test('escapes read alike in both spellings', () {
      expect(_values(r'''"it's", 'it\'s', "say \"hi\"", 'say "hi"' '''),
          ["it's", "it's", 'say "hi"', 'say "hi"']);
    });
  });

  group('one value', () {
    test("'x' =?= \"x\" succeeds, '\"x\"' =?= \"x\" fails", () async {
      expect(await _answer(r'''eq('x', "x", R)'''), 'yes');
      expect(await _answer(r'''eq(x, "x", R)'''), 'yes');
      expect(await _answer(r'''eq('"x"', "x", R)'''), 'no');
      expect(await _answer(r'''eq(f("A b"), f('A b'), R)'''), 'yes');
    });
    test('a head "x" matches x and not \'"x"\'', () async {
      expect(await _answer('is_x(x, R)'), 'yes');
      expect(await _answer(r'''is_x('"x"', R)'''), 'no');
    });
    test('string holds of every spelling', () async {
      expect(await _answer(r'''str("x", R)'''), 'yes');
      expect(await _answer(r'''str('x', R)'''), 'yes');
      expect(await _answer('str(x, R)'), 'yes');
    });
  });

  test(
      'the REPL shows a string single-quoted or bare, never in double quotes '
      '(bin/glp_repl.dart, _formatTerm)', () async {
    final repl =
        await Process.start(Platform.resolvedExecutable, ['bin/glp_repl.dart']);
    repl.stdin.write('A = "x".\nB = "Hello world".\n'
        'C = \'"x"\'.\nD = [x, "Y", \'z\'].\n:quit\n');
    await repl.stdin.close();
    final err = repl.stderr.transform(utf8.decoder).join();
    final out = await repl.stdout.transform(utf8.decoder).join();
    await repl.exitCode;
    expect(out, contains('A = x\n'), reason: '$out\n${await err}');
    expect(out, contains("B = 'Hello world'\n"), reason: out);
    expect(out, contains('C = \'"x"\'\n'), reason: out);
    expect(out, contains("D = [x, 'Y', z]\n"), reason: out);
  }, timeout: Timeout(Duration(minutes: 3)));
}
