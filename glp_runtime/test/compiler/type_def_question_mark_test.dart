// glp_runtime/test/compiler/type_def_question_mark_test.dart
//
// A `?` standing apart in a type definition marks the type name before it:
// `T ?` is `T?`, the dual of T, as a procedure declaration reads it.  TGLP
// typed-glp.tex, "Type Declarations": "GLP types are specified using BNF rules
// with the complementation operator ?", and "its dual (for example Stream?) is
// an input type".  A `?` that marks nothing is refused with a message, not
// dropped (GLP #3 Cowork, 2026-10-10 07:48 UTC, "00:26": "A parser that drops
// a mark silently is at fault whatever the syntax").  Until 2026-10-10 the
// type-definition parser consumed a `?` standing apart and dropped it, so
// `Q ::= f(R ?).` was read as `f(R)`, while `procedure p(R ?).` read `R?`.

import 'dart:io';

import 'package:glp_runtime/compiler/error.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The type definitions of [source], printed.
List<String> _typeDefs(String source) => [
      for (final td in Parser(Lexer(source).tokenize()).parseModule().typeDefs)
        '$td'
    ];

/// The CompileError the parser raises on [source]; fails the test if it parses.
CompileError _refusal(String source) {
  try {
    Parser(Lexer(source).tokenize()).parseModule();
  } on CompileError catch (e) {
    return e;
  }
  fail('parsed: $source');
}

/// GLP's case (Code #6, 2026-10-10 00:26 UTC), its dual written [dual].
String _case(String dual) => '''
R ::= g(Integer).
Q ::= f($dual).
exported procedure p(Stream(Q)?).
p([f(X?)|Qs]) :- w(X), p(Qs?).
p([]).
procedure w(R).
w(g(1)).
''';

void main() {
  group('"T ?" in a type definition is "T?"', () {
    for (final (what, apart, joined) in [
      ('an argument of a structure', 'Q ::= f(R ?).', 'Q ::= f(R?).'),
      ('an alternative, the alias of a dual', 'Q ::= R ?.', 'Q ::= R?.'),
      ('a list element', 'Q ::= [] ; [R ?|Q].', 'Q ::= [] ; [R?|Q].'),
      ('a list tail', 'Q ::= [R|Q ?].', 'Q ::= [R|Q?].'),
      ('a type argument', 'Q ::= f(Stream(R ?)).', 'Q ::= f(Stream(R?)).'),
      ('a parameterised type', 'Q ::= f(Stream(R) ?).', 'Q ::= f(Stream(R)?).'),
      ('a primitive type', 'Q ::= f(Integer ?, String).',
          'Q ::= f(Integer?, String).'),
      ('a type parameter', 'Q(X) ::= f(X ?).', 'Q(X) ::= f(X?).'),
      ('an operand of an operator', r'Q ::= Stream ? \ Stream.',
          r'Q ::= Stream? \ Stream.'),
      ('the right operand of an operator', r'Q ::= Stream \ Stream ?.',
          r'Q ::= Stream \ Stream?.'),
      ('several, apart and joined', 'Q ::= f(R ?, R, R?) ; g(R, R ?).',
          'Q ::= f(R?, R, R?) ; g(R, R?).'),
    ]) {
      test(what, () {
        expect(_typeDefs(apart), _typeDefs(joined));
      });
    }

    test('as a procedure declaration reads it', () {
      final m = Parser(Lexer('R ::= g(Integer).\nQ ::= f(R ?).\n'
              'imported procedure m#p(R ?, Q).')
          .tokenize())
          .parseModule();
      expect('${m.procDeclarations.single.argTypes.first}', 'R?');
      expect(m.typeDefs.last.toString(), 'Q ::= f(R?).');
    });
  });

  group('a "?" that marks nothing is refused, not dropped', () {
    for (final (what, source, follows) in [
      ('after a constant', 'Q ::= f(a ?).', 'a'),
      ('after a constant alternative', 'Q ::= a ? ; b.', 'a'),
      ('after a number', 'Q ::= f(5 ?).', '5'),
      ('after a string', 'Q ::= f("s" ?).', '"s"'),
      ('after a dual, joined', 'Q ::= f(R? ?).', 'R?'),
      ('after a dual, apart', 'Q ::= f(R ? ?).', 'R?'),
      ('after _?', 'Q ::= f(_? ?).', '_?'),
      ('after a parameterised dual', 'Q ::= f(Stream(R)? ?).', 'Stream(R)?'),
    ]) {
      test(what, () {
        final e = _refusal(source);
        expect(e.category, ErrorCategory.syntax, reason: '$e');
        expect(e.message, contains('marks nothing'));
        expect(e.message, contains('here it follows "$follows"'));
        // The refusal points at the "?" that marks nothing, the last one.
        expect(e.line, 1);
        expect(e.column, source.lastIndexOf('?') + 1, reason: '$e');
      });
    }
  });

  group('GLP\'s case', () {
    late GlpEngine engine;
    setUp(() => engine = GlpEngine(
        rootSelfGlpPath: File('../programs/self.glp').absolute.path));

    for (final dual in ['R ?', 'R?']) {
      test('Q ::= f($dual) loads and runs, its dual kept', () async {
        expect(engine.loadSource(_case(dual)), isTrue);
        final r = await engine.runGoal('p([f(Y)]).');
        expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
        expect(r.bindings['Y'].toString(), 'g(Const(1))');
      });
    }

    test('without the dual, Q ::= f(R), it does not load', () {
      expect(() => engine.loadSource(_case('R')), throwsA(anything));
    });
  });
}
