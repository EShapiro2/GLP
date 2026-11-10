// glp_runtime/test/analysis/assignment_result_type_test.dart
//
// The type of the result of `:=` (TGLP typed-constant-readers.tex, "Type
// checking of :=", 1962166): "The root declares the body kernel :=(Number,
// Exp?) ... A goal X := E is checked with the type of X taken as Integer where
// E is an integer expression --- an integer literal, a reader of type Integer,
// one of +, -, *, //, mod or abs over integer expressions, or integer, round,
// floor or ceil over any expression ---, as Real where E is a real expression
// --- a real literal, a reader of type Real, one of /, real, sqrt, sin, cos,
// tan, asin, acos, atan, exp, ln or log over any expression, one of +, -, * or
// pow with an operand a real expression, or abs over a real expression --- and
// as Number otherwise; so N1 := N? + 1 with N an Integer types N1 as Integer,
// which the monitor of Section [typed-glp-examples] needs, and Z := X? + Y?
// with X and Y of type Real types Z as Real.  pow over integers is a Number, a
// negative exponent yielding a real."  Until 2026-11-10 (TGLP 6858789) / and
// the functions yielded a Number whatever their operands, and no typed
// procedure could give a computed real the type Real (Code, GLP_inbox
// 2026-11-07 20:37 UTC; GLP #14, Code_inbox 2026-11-10 12:03 UTC, item 1).
//
// Until 2026-10-02 a `:=` goal was not type-checked at all (root_scope.dart's
// builtinGoals); checked against the root's declaration alone, its writer was a
// Number and TGLP's own monitor was refused (round three, item 3, reverted).

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

void main() {
  final rootSelf = File('../programs/self.glp').absolute.path;

  GlpEngine engineWith(String source) {
    final engine = GlpEngine(rootSelfGlpPath: rootSelf);
    engine.loadSource(source, filename: 'probe');
    return engine;
  }

  bool load(String source) =>
      GlpEngine(rootSelfGlpPath: rootSelf).loadSource(source, filename: 'probe');

  void refused(String source, String why) {
    expect(
      () => load(source),
      throwsA(predicate(
          (e) => e.toString().contains('Type checking failed'), why)),
    );
  }

  group('X := E types X as Integer where E is an integer expression', () {
    test("TGLP's typed monitor loads: N1 := N? + 1 with N an Integer", () {
      expect(
          load('CounterCall ::= add ; clear ; read(Integer?).\n'
              'procedure monitor(Stream(CounterCall)?).\n'
              'monitor(In) :- monitor(0, In?).\n'
              'procedure monitor(Integer?, Stream(CounterCall)?).\n'
              'monitor(N, [add|In]) :- N1 := N? + 1, monitor(N1?, In?).\n'
              'monitor(_, [clear|In]) :- monitor(0, In?).\n'
              'monitor(N, [read(N?)|In]) :- integer(N?) | monitor(N?, In?).\n'
              'monitor(_, []).\n'),
          isTrue);
    });

    test('an integer literal', () {
      expect(load('procedure one(Integer).\none(X?) :- X := 1.\n'), isTrue);
    });

    test('+, -, *, // and mod over integer expressions, and 0 - A for the '
        'negation of A', () {
      expect(
          load('procedure f(Integer?, Integer?, Integer).\n'
              'f(A, B, X?) :- X := A? * 2 - B? // 3 + A? mod 5.\n'),
          isTrue);
      expect(
          load('procedure n(Integer?, Integer).\nn(A, X?) :- X := 0 - A?.\n'),
          isTrue);
    });

    test('unary minus is not in Exp: X := -A? does not parse (TGLP 6858789)',
        () {
      expect(
          () => load(
              'procedure n(Integer?, Integer).\nn(A, X?) :- X := -A?.\n'),
          throwsA(predicate((e) => e.toString().contains(
              'GLP has no unary minus: write 0 - X for the negation of X'))));
      // neg/1 is no functor of Exp: X := neg(A?) is ill-typed.
      refused('procedure n(Number?, Number).\nn(A, X?) :- X := neg(A?).\n',
          'neg(A?) is not an Exp');
    });

    test('a result typed Integer is a reader of type Integer for another :=, '
        'in either order', () {
      expect(
          load('procedure g(Integer?, Integer).\n'
              'g(N, Y?) :- X := N? * 2, Y := X? + 1.\n'),
          isTrue);
      expect(
          load('procedure g(Integer?, Integer).\n'
              'g(N, Y?) :- Y := X? + 1, X := N? * 2.\n'),
          isTrue);
    });

    test('a guard narrowing a Number to an Integer makes its reader one', () {
      expect(
          load('procedure h(Number?, Integer).\n'
              'h(N, X?) :- integer(N?) | X := N? + 1.\n'),
          isTrue);
    });

    test('abs over an integer expression', () {
      expect(
          load('procedure a(Integer?, Integer).\na(N, X?) :- X := abs(N? - 7).\n'),
          isTrue);
    });

    test('integer, round, floor and ceil over any expression', () {
      for (final f in ['integer', 'round', 'floor', 'ceil']) {
        expect(
            load('procedure c(Number?, Integer).\n'
                'c(N, X?) :- X := $f(N? / 3).\n'),
            isTrue,
            reason: '$f yields an Integer whatever its operand');
      }
    });
  });

  group('X := E types X as Real where E is a real expression', () {
    test("the smallest case of Code's report: Z := X? + Y? with X and Y of "
        'type Real, at f(Real?, Real?, Real)', () {
      expect(
          load('procedure f(Real?, Real?, Real).\n'
              'f(X, Y, Z?) :- Z := X? + Y?.\n'),
          isTrue);
    });

    test('a real literal', () {
      expect(load('procedure r(Real).\nr(X?) :- X := 1.5.\n'), isTrue);
      expect(load('procedure r(Real).\nr(X?) :- X := 1.5 + 1.\n'), isTrue);
    });

    test('a reader of type Real, under + - * and pow with any other operand',
        () {
      for (final e in ['X? + 1', '2 - X?']) {
        expect(
            load('procedure f(Real?, Real).\nf(X, Z?) :- Z := $e.\n'),
            isTrue,
            reason: '$e is a real expression');
      }
      for (final e in ['X? * N?', 'pow(N?, X?)']) {
        expect(
            load('procedure f(Real?, Integer?, Real).\n'
                'f(X, N, Z?) :- Z := $e.\n'),
            isTrue,
            reason: '$e is a real expression');
      }
    });

    test('/, real, sqrt, sin, cos, tan, asin, acos, atan, exp, ln and log '
        'over any expression', () {
      expect(
          load('procedure half(Integer?, Real).\n'
              'half(N, H?) :- H := N? / 2.\n'),
          isTrue);
      for (final f in [
        'real', 'sqrt', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'exp',
        'ln', 'log'
      ]) {
        expect(
            load('procedure c(Integer?, Real).\n'
                'c(N, X?) :- X := $f(N?).\n'),
            isTrue,
            reason: '$f yields a Real whatever its operand');
      }
    });

    test('abs over a real expression', () {
      expect(
          load('procedure a(Real?, Real).\na(X, Y?) :- Y := abs(X? - 7).\n'),
          isTrue);
    });

    test('a result typed Real is a reader of type Real for another :=, in '
        'either order', () {
      expect(
          load('procedure g(Integer?, Real).\n'
              'g(N, Y?) :- X := N? / 2, Y := X? * 2.\n'),
          isTrue);
      expect(
          load('procedure g(Integer?, Real).\n'
              'g(N, Y?) :- Y := X? * 2, X := N? / 2.\n'),
          isTrue);
    });

    test('a real expression is no integer expression: refused where an '
        'Integer is produced', () {
      refused(
          'procedure half(Integer?, Integer).\n'
              'half(N, H?) :- H := N? / 2.\n',
          'a Real is handed where an Integer is produced');
      refused('procedure f(Real?, Integer).\nf(X, Z?) :- Z := X? + 1.\n',
          'a Real is handed where an Integer is produced');
    });

    test('a Real is a Number: accepted where a Number is produced', () {
      expect(
          load('procedure half(Integer?, Number).\n'
              'half(N, H?) :- H := N? / 2.\n'),
          isTrue);
    });
  });

  group('and as Number otherwise', () {
    test('pow over integers is a Number: neither an Integer nor a Real', () {
      expect(
          load('procedure p(Integer?, Number).\np(N, X?) :- X := pow(2, N?).\n'),
          isTrue);
      refused('procedure p(Integer?, Integer).\np(N, X?) :- X := pow(2, N?).\n',
          'a Number is handed where an Integer is produced');
      refused('procedure p(Integer?, Real).\np(N, X?) :- X := pow(2, N?).\n',
          'a Number is handed where a Real is produced');
    });

    test('+ over a Number and an Integer is a Number', () {
      refused('procedure f(Number?, Real).\nf(N, X?) :- X := N? + 1.\n',
          'a Number is handed where a Real is produced');
    });

    test('a real literal is no integer expression', () {
      refused('procedure r(Integer).\nr(X?) :- X := 1.5 + 1.\n',
          'a Real is handed where an Integer is produced');
    });

    test('a reader of type Number', () {
      refused(
          'procedure f(Number?, Integer).\nf(N, X?) :- X := N? + 1.\n',
          'a Number is handed where an Integer is produced');
    });

    test('pow and the other functions of Exp, whatever their operands', () {
      for (final e in ['pow(N?, 2)', 'sqrt(N?)', 'sin(N?)', 'exp(N?)',
          'ln(N?)', 'log(N?)', 'real(N?)', 'atan(N?)']) {
        refused(
            'procedure p(Integer?, Integer).\np(N, X?) :- X := $e.\n',
            '$e yields a Number');
        expect(
            load('procedure p(Integer?, Number).\np(N, X?) :- X := $e.\n'),
            isTrue,
            reason: '$e is an Exp, its value a Number');
      }
    });
  });

  group(':= goals are checked against :=(Number, Exp?)', () {
    test('an operand outside Exp is refused', () {
      refused('procedure a(Number).\na(X?) :- X := foo + 1.\n',
          'foo is no Exp');
    });

    test('a writer operand is refused', () {
      refused('procedure w(Number).\nw(X?) :- X := Y + 1, Y = 2.\n',
          'a writer at a consumed position');
    });
  });

  // The rule is read over the whole clause, so it is read inside the reading
  // of a call's instantiation (TGLP appendix-implementation-notes.tex, "The
  // instantiation of a call": "the checker reads the sites of a call over the
  // whole clause, the body goals in no order"): a writer the rule types Integer
  // is an Integer where its reader stands at a call to a parameterised
  // procedure, and a reader a call types Integer makes an expression an
  // integer expression.  Until 2026-10-02 the instantiation was read with the
  // := goals skipped, and the clause check, which typed them, then found the
  // binding the reading took did not fit them.
  group('inside the instantiation of a call', () {
    const put = 'Lot ::= lot(Integer).\n'
        'procedure(X) put(X?, Stream(X)).\n'
        'put(X, [X?]).\n';

    // Integration #4 Code's count_plus.glp (2026-10-02 22:01 UTC), verbatim.
    test('the writer of N? + 1 is read at a parameterised call as an Integer',
        () {
      expect(
          load('${put}procedure r(Integer?, Stream(Lot)).\n'
              'r(N, S?) :- C := N? + 1, put(lot(C?), S).\n'),
          isTrue);
    });

    test('the writer of integer(A?) likewise, in either order', () {
      // Integration #4 Code's count_param.glp (2026-10-02 22:01 UTC), verbatim.
      expect(
          load('${put}procedure q(Constant?, Stream(Lot)).\n'
              'q(A, S?) :- number(A?) | C := integer(A?), put(lot(C?), S).\n'),
          isTrue);
      expect(
          load('${put}procedure q(Constant?, Stream(Lot)).\n'
              'q(A, S?) :- number(A?) | put(lot(C?), S), C := integer(A?).\n'),
          isTrue);
    });

    test('a reader a parameterised call types Integer makes an integer '
        'expression', () {
      expect(
          load('${put}procedure(X) id(X?, X).\n'
              'id(X, X?).\n'
              'procedure r(Integer?, Stream(Lot)).\n'
              'r(N, S?) :- C := M? + 1, put(lot(C?), S), id(N?, M).\n'),
          isTrue);
    });

    test('a Number is still refused there: / and a reader of type Number', () {
      refused(
          '${put}procedure r(Integer?, Stream(Lot)).\n'
              'r(N, S?) :- C := N? / 2, put(lot(C?), S).\n',
          '/ yields a Number');
      refused(
          '${put}procedure r(Number?, Stream(Lot)).\n'
              'r(N, S?) :- C := N? + 1, put(lot(C?), S).\n',
          'a reader of type Number');
    });

    test('a posted goal is read the same way', () async {
      final engine = engineWith('${put}procedure take(Stream(Lot)?).\n'
          'take(_).\n');
      final result =
          await engine.runGoal('C := 1 + 2, put(lot(C?), S), take(S?)');
      expect(result.status, ExecutionStatus.succeeded,
          reason: result.error ?? '');
    });
  });

  group('a REPL goal follows the same rule', () {
    const source = 'procedure f(Integer?, Integer).\nf(N, M?) :- M := N? + 1.\n';

    test('X := 1 + 2 hands an Integer on', () async {
      final engine = engineWith(source);
      final r = await engine.runGoal('X := 1 + 2, f(X?, Y)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    });

    test('X := 1 / 2 does not', () async {
      final engine = engineWith(source);
      final r = await engine.runGoal('X := 1 / 2, f(X?, Y)');
      expect(r.status, ExecutionStatus.failed);
      expect(r.error, contains('Goal is not well-typed'));
    });
  });
}
