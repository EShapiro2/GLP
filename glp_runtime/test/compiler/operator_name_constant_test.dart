/// An operator name where a term is expected is a name.
///
/// GLP-Spec appendix-lp.tex, Definition "Logic Programs Syntax": a term is a
/// variable, a constant or a compound term f(T1, ..., Tn), in standard LP
/// notions, and GLP-Spec reserves no word.  So where a term is expected the
/// reader takes an operator name or keyword as the constant of that name, and
/// as the functor of a compound term where "(" follows it, as Prolog does (GLP
/// #3 Cowork, 2026-10-03 21:18 UTC, "11:58. 2": "`mod` and `procedure` are
/// constants ... compliance, fix it").  Until 2026-10-03 an unquoted `mod` in
/// a term was "[syntax] Expected term, got TokenType.MOD", `procedure` was
/// read as the declaration keyword there, and every other operator name was
/// refused alike.  The operators themselves read as before.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The head arguments of the single clause of [source].
List<Term> _headArgs(String source) =>
    Parser(Lexer(source).tokenize()).parse().procedures.single.clauses.single
        .head.args;

/// The body goals of the single clause of [source].
List<Goal> _body(String source) =>
    Parser(Lexer(source).tokenize()).parse().procedures.single.clauses.single
        .body!;

/// Every operator name and keyword the lexer makes a token of, punctuation
/// apart, as it is written.
const _names = [
  'mod', 'procedure', '+', '-', '*', '/', '//', '<', '>', '=<', '>=', '=',
  '=:=', r'=\=', '=?=', r'=?\=', '@<', '=..', '..=', ':-', ':=', '::=', ';',
  ':', '~', '#', r'\', '@',
];

void main() {
  group('an operator name where a term is expected', () {
    for (final name in _names) {
      test('$name is the constant of its name, as an argument', () {
        final args = _headArgs('p($name, a).');
        expect(args[0], isA<ConstTerm>());
        expect((args[0] as ConstTerm).value, name);
        expect((args[1] as ConstTerm).value, 'a');
      });

      test('$name is the constant of its name, last in a list', () {
        final list = _headArgs('p([a, $name]).').single as ListTerm;
        final second = (list.tail as ListTerm).head;
        expect(second, isA<ConstTerm>());
        expect((second as ConstTerm).value, name);
      });
    }

    test('mod and procedure on the right of =', () {
      final goals = _body('p(X?, Y?) :- X = mod, Y = procedure.');
      expect(((goals[0].args[1]) as ConstTerm).value, 'mod');
      expect(((goals[1].args[1]) as ConstTerm).value, 'procedure');
    });

    test('mod and procedure as a list tail', () {
      final list = _headArgs('p([mod|procedure]).').single as ListTerm;
      expect((list.head as ConstTerm).value, 'mod');
      expect((list.tail as ConstTerm).value, 'procedure');
    });

    test('procedure is a functor before "("', () {
      final t = _headArgs('p(procedure(a)).').single;
      expect(t, isA<StructTerm>());
      expect((t as StructTerm).functor, 'procedure');
      expect((t.args.single as ConstTerm).value, 'a');
    });

    test('= is a functor before "("', () {
      final t = _headArgs('p(=(a, b)).').single as StructTerm;
      expect(t.functor, '=');
      expect(t.args, hasLength(2));
    });

    test('a quoted name reads as it did', () {
      final args = _headArgs("p('mod', 'procedure', '+').");
      expect(args.map((a) => (a as ConstTerm).value),
          ['mod', 'procedure', '+']);
    });

    test('a constant alternative of a type definition', () {
      final m = Parser(Lexer('Op ::= mod ; procedure ; = ; +.\n'
              'procedure op(Op?).\nop(mod).')
          .tokenize()).parseModule();
      expect(m.typeDefs.single.alternatives, hasLength(4));
    });
  });

  group('the operators read as before', () {
    test('X mod Y is mod(X, Y)', () {
      final g = _body('p(Z?) :- Z := 7 mod 3.').single;
      final e = g.args[1] as StructTerm;
      expect(e.functor, 'mod');
      expect(e.args, hasLength(2));
    });

    test('unary minus is neg', () {
      final g = _body('p(Z?) :- Z := - 5.').single;
      expect((g.args[1] as StructTerm).functor, 'neg');
    });

    test('-(X, Y) is a structure, not neg', () {
      final t = _headArgs('p(-(a, b)).').single as StructTerm;
      expect(t.functor, '-');
      expect(t.args, hasLength(2));
    });

    test('+ and * by precedence', () {
      final g = _body('p(Z?) :- Z := 2 + 3 * 4.').single;
      final e = g.args[1] as StructTerm;
      expect(e.functor, '+');
      expect((e.args[1] as StructTerm).functor, '*');
    });

    test('the procedure keyword still begins a declaration', () {
      final m = Parser(Lexer('procedure(X) id(X?, X).\nid(A, A?).')
              .tokenize())
          .parseModule();
      expect(m.procDeclarations.single.name, 'id');
      expect(m.procDeclarations.single.typeParams, ['X']);
    });
  });

  group('the fixture loads and runs', () {
    late GlpEngine engine;
    setUp(() {
      engine = GlpEngine(
          rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      engine.loadFile(
          File('../programs/tests/typed/operator_names.glp').absolute.path);
    });

    test('names/1 gives every name as a constant', () async {
      final r = await engine.runGoal('names(N).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    });

    test('op/2 matches the constant alternatives', () async {
      for (final (arg, want) in [
        ('mod', 'is_mod'),
        ('procedure', 'is_procedure'),
        ('+', 'is_plus'),
        ('=', 'is_equals'),
      ]) {
        final r = await engine.runGoal('op($arg, R).');
        expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
        expect(r.bindings['R'].toString(), 'Const($want)');
      }
    });

    test('mod still computes', () async {
      final r = await engine.runGoal('arith(A, B, C).');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(r.bindings['A'].toString(), 'Const(1)');
      expect(r.bindings['B'].toString(), 'Const(-3)');
      expect(r.bindings['C'].toString(), 'Const(14)');
    });
  });
}
