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
//
// A `?` after a structure, a list or a parenthesised term marks no type name
// and is refused (GLP, 2026-10-10 08:40 UTC): TGLP gives `?` a meaning on a
// type name only, GLP-Spec on a variable only (glp.tex, Definition "GLP
// Variables").  Until 2026-10-10 the parser dropped it, for an "explicit
// dual" written `Channel? ::= ch(Stream?, Stream)?.`.
//
// A `?` on the head of a type definition, `T? ::=`, `T ? ::=`, `T(X)? ::=`,
// starts no definition and is refused at the `?` (GLP #3 Cowork, 2026-10-10
// 11:37 UTC, "09:50"): a type is defined from its producer's perspective,
// "which implicitly defines its dual" (TGLP typed-glp.tex, "Type
// Declarations"), the dual's automaton being the type's with every mode
// complemented (appendix-type-automaton.tex, Definition "Dual Type
// Automaton"); a dual is implied, never defined.  Until 2026-10-10 `T? ::=
// alt` was read as the definition of a type named `T?`.

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

  group('a "?" after a structure, a list or a parenthesised term marks no '
      'type name, and is refused', () {
    for (final (what, source, follows) in [
      ('a structure alternative', 'Q ::= ch(R?, R)?.',
          'the structure "ch(...)"'),
      ('a structure, apart', 'Q ::= ch(R?, R) ?.', 'the structure "ch(...)"'),
      ('a structure argument', 'Q ::= f(g(R)?).', 'the structure "g(...)"'),
      ('an operator\'s structure', 'Q ::= +(R, R)?.',
          'the structure "+(...)"'),
      ('the empty list', 'Q ::= []? ; [R|Q].', 'the list "[]"'),
      ('a list with a tail', 'Q ::= [] ; [R|Q]?.', 'the list "[...]"'),
      ('a closed list', 'Q ::= [R, R]?.', 'the list "[...]"'),
      ('a list element', 'Q ::= [] ; [[R]?|Q].', 'the list "[...]"'),
      ('a parenthesised type name, not read as its dual', 'Q ::= f((R)?).',
          'the parenthesised term "(...)"'),
      ('a parenthesised term', r'Q ::= (R? \ R)?.',
          'the parenthesised term "(...)"'),
      ('a tuple', 'Q ::= f((R, R)?).', 'the parenthesised term "(...)"'),
    ]) {
      test('after $what', () {
        final e = _refusal(source);
        expect(e.category, ErrorCategory.syntax, reason: '$e');
        expect(e.message, contains('marks no type name'));
        expect(e.message, contains('here it follows $follows, which is not a '
            'type name'));
        // The refusal points at the "?", the last one.
        expect(e.line, 1);
        expect(e.column, source.lastIndexOf('?') + 1, reason: '$e');
      });
    }

    test('without the "?" the same forms parse, "(R)" as "R"', () {
      expect(_typeDefs('Q ::= f((R)).'), _typeDefs('Q ::= f(R).'));
      for (final source in [
        'Q ::= ch(R?, R).',
        'Q ::= f(g(R)).',
        'Q ::= +(R, R).',
        'Q ::= [] ; [R|Q].',
        'Q ::= [R, R].',
        'Q ::= [] ; [[R]|Q].',
        r'Q ::= (R? \ R).',
        'Q ::= f((R, R)).',
      ]) {
        expect(_typeDefs(source), hasLength(1), reason: source);
      }
    });
  });

  group('a "?" on the type a definition defines starts no definition, and is '
      'refused', () {
    /// Checks [e] is the refusal of a "?" on the head, after [follows], at
    /// [line] and [column].
    void expectHeadRefusal(
        CompileError e, String follows, int line, int column) {
      expect(e.category, ErrorCategory.syntax, reason: '$e');
      expect(e.message, contains('implies by the definition of T and never '
          'defines'));
      expect(e.message, contains('here it follows "$follows", the type being '
          'defined, and starts no definition'));
      expect(e.line, line, reason: '$e');
      expect(e.column, column, reason: '$e');
    }

    for (final (what, source, follows) in [
      ('a type name, joined', 'Q? ::= f(R).', 'Q'),
      ('a type name, apart', 'Q ? ::= f(R).', 'Q'),
      ('the alias of a type', 'Q? ::= R.', 'Q'),
      ('a channel', 'Channel? ::= ch(Stream?, Stream).', 'Channel'),
      ('a difference list', r'DiffList? ::= Stream? \ Stream.', 'DiffList'),
      ('a parameterised type, before its parameters', 'Q?(X) ::= f(X).', 'Q'),
      ('a parameterised type, after its parameters', 'Q(X)? ::= f(X).',
          'Q(X)'),
      ('a parameterised type, apart', 'Q(X, Y) ? ::= f(X, Y).', 'Q(X, Y)'),
      ('a type name marked twice', 'Q?? ::= f(R).', 'Q'),
      // Until 2026-10-10 refused at its last "?", after the structure, with
      // its head read as the type "Q?".
      ('the structure of an "explicit dual", at its head', 'Q? ::= ch(R?, R)?.',
          'Q'),
    ]) {
      test('on $what', () {
        // The refusal points at the "?" on the head, the first one.
        expectHeadRefusal(
            _refusal(source), follows, 1, source.indexOf('?') + 1);
      });
    }

    test('on a later line, at its "?"', () {
      expectHeadRefusal(
          _refusal('R ::= g(Integer).\n\nQ ? ::= f(R).'), 'Q', 3, 3);
    });

    test('in an interface section', () {
      try {
        Parser(Lexer('R ::= g(Integer).\nQ? ::= f(R).').tokenize())
            .parseInterface();
      } on CompileError catch (e) {
        expectHeadRefusal(e, 'Q', 2, 2);
        return;
      }
      fail('parsed');
    });

    test('without the "?" the same heads parse, each defining the type named',
        () {
      for (final (source, name) in [
        ('Q ::= f(R).', 'Q'),
        ('Q ::= R.', 'Q'),
        ('Channel ::= ch(Stream?, Stream).', 'Channel'),
        (r'DiffList ::= Stream? \ Stream.', 'DiffList'),
        ('Q(X) ::= f(X).', 'Q'),
        ('Q(X, Y) ::= f(X, Y).', 'Q'),
        ('Q ::= ch(R?, R).', 'Q'),
      ]) {
        final defs =
            Parser(Lexer(source).tokenize()).parseModule().typeDefs;
        expect([for (final d in defs) d.name], [name], reason: source);
      }
    });
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

    test('with the "?" after the structure, Q ::= f(R)?, it does not load, '
        'refused at the "?"', () {
      final source = _case('R').replaceFirst('Q ::= f(R).', 'Q ::= f(R)?.');
      expect(source, contains('Q ::= f(R)?.'));
      expect(
          () => engine.loadSource(source),
          throwsA(predicate((e) => '$e'.contains(
              'here it follows the structure "f(...)", which is not a type '
              'name, and marks no type name'))));
    });

    test('with its dual defined, Q? ::= f(R?), it does not load, refused at '
        'the "?"', () {
      final source =
          _case('R?').replaceFirst('Q ::= f(R?).', 'Q? ::= f(R?).');
      expect(source, contains('\nQ? ::= f(R?).'));
      expect(
          () => engine.loadSource(source),
          throwsA(predicate((e) => '$e'.contains(
              'here it follows "Q", the type being defined, and starts no '
              'definition'))));
    });
  });
}
