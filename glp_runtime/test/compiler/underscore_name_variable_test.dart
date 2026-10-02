/// An unquoted name beginning with an underscore is an anonymous variable.
///
/// GLP-Spec appendix-guards.tex, "Naming and admission of body kernels":
/// "Quoting is necessary: an unquoted name beginning with an underscore is an
/// anonymous variable"; glp.tex, Remark "Anonymous Variables": "An anonymous
/// variable is any variable whose name begins with `_`".  Until 2026-10-02 the
/// lexer read a name of `_` and a capital (`_X`) as a variable and any other
/// (`_x`, `_add`, `_1`, `__`) as an atom (Integration's report of 2026-10-02
/// 09:54 UTC, F; GLP #3 Cowork's task of 2026-10-02 13:00 UTC).  It now reads
/// every one as a variable, and the rest of the compiler, which takes any
/// variable whose name begins with `_` as anonymous --- the analyzer, codegen,
/// and the checker giving each occurrence a fresh name (moded_head.dart,
/// 38a9a67b, d87f0ed3) --- treats `_x` as it treats `_X`
/// (named_anonymous_variable_test.dart).  A quoted name, `'_add'`, is an atom
/// as before: a body kernel is named so.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/token.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// The type and lexeme of the first token of [source].
(TokenType, String) _first(String source) {
  final t = Lexer(source).tokenize().first;
  return (t.type, t.lexeme);
}

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

GlpEngine _load(String source) {
  final engine = _engine();
  expect(engine.loadSource(source), isTrue);
  return engine;
}

/// The value [t] is bound to: a constant by its value.
String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return '${d.value}';
  return '$d';
}

void main() {
  group('the lexer', () {
    test('reads an unquoted name beginning with `_` as a variable', () {
      for (final name in ['_x', '_add', '_1', '__', '_x_Y2', '_X', '_Out']) {
        expect(_first(name), (TokenType.VARIABLE, name), reason: name);
      }
    });

    test('and with `?` after it as a reader', () {
      for (final name in ['_x', '_add', '_X']) {
        expect(_first('$name?'), (TokenType.READER, name), reason: name);
      }
    });

    test('`_` alone is the anonymous variable as before', () {
      expect(_first('_').$1, TokenType.UNDERSCORE);
      expect(_first('_?').$1, TokenType.UNDERSCORE);
    });

    test("a quoted name is an atom: '_add', the body kernel's name", () {
      expect(_first("'_add'"), (TokenType.ATOM, '_add'));
    });

    test('other names are as before: x_y an atom, X_y a variable', () {
      expect(_first('x_y'), (TokenType.ATOM, 'x_y'));
      expect(_first('X_y'), (TokenType.VARIABLE, 'X_y'));
    });

    test('an unquoted `_add(...)` is not a call: the parser refuses it', () {
      CompileError? e;
      try {
        Parser(Lexer("p(X?) :- _add(1, 2, X).\n").tokenize()).parse();
      } on CompileError catch (err) {
        e = err;
      }
      expect(e, isNotNull, reason: 'parsed');
      expect(e!.category, ErrorCategory.syntax, reason: '$e');
    });
  });

  // Each case loads and runs as it does written with `_X`
  // (named_anonymous_variable_test.dart); read as atoms, `_x` and `_a` were
  // constants at an Integer or String position, and each program was refused.
  group('`_x` is compiled and typed as `_X` is', () {
    test('at a head argument: f(_x, 0)', () async {
      final engine = _load('''
procedure f(Integer?, Integer).
f(_x, 0).
''');
      final r = await engine.runGoal('f(5, Y)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['Y']), '0');
    });

    test('two occurrences of one name, at two types: h(_a, _a, 1)', () async {
      final engine = _load('''
procedure h(Integer?, String?, Integer).
h(_a, _a, 1).
''');
      final r = await engine.runGoal('h(1, "s", N)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['N']), '1');
    });

    test('at a body argument: k(N?) :- two(_discard, N)', () async {
      final engine = _load('''
procedure two(Integer, Integer).
two(1, 2).

procedure k(Integer).
k(N?) :- two(_discard, N).
''');
      final r = await engine.runGoal('k(N)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['N']), '2');
    });

    test('in a head structure: g([_first | _rest], 1)', () async {
      final engine = _load('''
procedure g(Stream(Integer)?, Integer).
g([_first | _rest], 1).
g([], 0).
''');
      final r = await engine.runGoal('g([7, 8], N)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['N']), '1');
    });

    test('out(_r) is refused as out(_R) is, naming _r', () {
      final engine = _engine();
      expect(() => engine.loadSource('procedure out(Integer).\nout(_r).\n'),
          throwsA(predicate((Object e) => '$e'.contains('(_r#1?'))));
    });
  });

  // The engine parses a REPL goal as the body of a clause whose head is named
  // '_glp_query_', and a conjunction under '_conj_wrapper_'.  Written
  // unquoted, each head was a variable once `_x` was read as one: the goal
  // check did not parse and was passed by, and no conjunction ran.
  group("the REPL's goals, parsed under the engine's own heads", () {
    const source = '''
procedure f(Integer?, Integer).
f(_x, 0).
''';

    test('an ill-typed goal is still refused by the goal check', () async {
      final engine = _load(source);
      final r = await engine.runGoal('f(W, Y)');
      expect(r.status, ExecutionStatus.failed);
      expect(r.error, contains('Goal is not well-typed'));
    });

    test('a conjunction runs', () async {
      final engine = _load(source);
      final r = await engine.runGoal('f(5, Y), f(6, Z)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['Y']), '0');
      expect(_show(engine, r.bindings['Z']), '0');
    });
  });

  test("a quoted '_hidden' is a constant, in data", () async {
    final engine = _load('''
procedure tag(Constant).
tag('_hidden').
''');
    final r = await engine.runGoal('tag(T)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['T']), '_hidden');
  });
}
