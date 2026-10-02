// glp_runtime/test/analysis/assignment_result_type_test.dart
//
// The type of the result of `:=` (TGLP typed-glp.tex, "Type checking of :=",
// dbb09f4, restated in 2e39edb; Udi's ruling of 2026-10-02): "The root declares
// the body kernel :=(Number, Exp?) ... A goal X := E is checked with the type of
// X taken as Integer where E is an integer expression --- an integer literal, a
// reader of type Integer, one of +, -, *, //, mod, unary - or abs over integer
// expressions, or integer, round, floor or ceil over any expression --- and as
// Number otherwise, / and the remaining functions of Exp yielding a Number
// whatever their operands; so N1 := N? + 1 with N an Integer types N1 as
// Integer, which the monitor of Section [typed-glp-examples] needs."
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

    test('+, -, *, // and mod over integer expressions, and unary minus', () {
      expect(
          load('procedure f(Integer?, Integer?, Integer).\n'
              'f(A, B, X?) :- X := A? * 2 - B? // 3 + A? mod 5.\n'),
          isTrue);
      expect(
          load('procedure n(Integer?, Integer).\nn(A, X?) :- X := -A?.\n'),
          isTrue);
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

  group('and as Number otherwise', () {
    test('/ yields a Number whatever its operands', () {
      refused(
          'procedure half(Integer?, Integer).\n'
              'half(N, H?) :- H := N? / 2.\n',
          'a Number is handed where an Integer is produced');
      expect(
          load('procedure half(Integer?, Number).\n'
              'half(N, H?) :- H := N? / 2.\n'),
          isTrue);
    });

    test('a real literal', () {
      refused('procedure r(Integer).\nr(X?) :- X := 1.5 + 1.\n',
          'a Number is handed where an Integer is produced');
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
