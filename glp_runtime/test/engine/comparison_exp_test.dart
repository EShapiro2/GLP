/// An arithmetic comparison evaluates its arguments as expressions of type
/// Exp: numbers, the operators +, -, *, /, // and mod, pow, unary negation,
/// and the sixteen functions, each as its body kernel computes it; an argument
/// outside a function's domain has no value, and the guard fails.
///
/// GLP-Spec appendix-guards.tex (026515d): "Arithmetic comparison guards
/// evaluate their arguments as arithmetic expressions of type Exp and compare
/// the results; an argument with no value --- a zero divisor, a non-integer
/// under // or mod, an argument outside a function's domain --- fails the
/// guard."  Each comparison is declared over Exp? in the root self.glp, Exp
/// the union of Number with the catalogue's arithmetic operators and functions
/// (TGLP appendix-root-self.tex, 2e39edb).  The functions and their domains are
/// the kernels' (GLP #3 Cowork, 2026-10-02 20:58 UTC, "20:10" C), one
/// definition serving both ([expFunction]), so a comparison and := agree on
/// every function.  Until 2026-10-02 the runtime evaluated + - * / // mod and
/// neg alone and a comparison over a function failed: sqrt(X?) > 1 never
/// succeeded (Integration #4 Code, 2026-10-02 20:10 UTC, C).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

/// The sixteen unary functions of Exp, each with an argument in its domain, a
/// number below its value there and a number above it.
const _inDomain = <String, (String, String, String)>{
  'abs': ('-3', '2.5', '3.5'),
  'sqrt': ('9', '2.5', '3.5'),
  'sin': ('1', '0.8', '0.9'),
  'cos': ('0', '0.5', '1.5'),
  'tan': ('1', '1.5', '1.6'),
  'asin': ('1', '1.5', '1.6'),
  'acos': ('-1', '3.1', '3.2'),
  'atan': ('1', '0.7', '0.8'),
  'exp': ('1', '2.7', '2.8'),
  'ln': ('1', '-0.5', '0.5'),
  'log': ('100', '1.5', '2.5'),
  'integer': ('7.9', '6.5', '7.5'),
  'real': ('3', '2.5', '3.5'),
  'round': ('7.6', '7.5', '8.5'),
  'floor': ('-1.5', '-2.5', '-1.5'),
  'ceil': ('1.2', '1.5', '2.5'),
};

/// Arguments outside the domain of a function that has one, as its kernel
/// aborts on them: a negative square root, a logarithm of a number that is
/// not positive, an arc sine or cosine outside [-1, 1].
const _outOfDomain = <(String, String)>[
  ('sqrt', '-1'),
  ('sqrt', '-0.5'),
  ('ln', '0'),
  ('ln', '-1'),
  ('log', '0'),
  ('log', '-5'),
  ('asin', '2'),
  ('asin', '-2'),
  ('acos', '1.5'),
  ('acos', '-3'),
];

/// The procedures of the function [f]: gt_f(X, L, R), R = yes where
/// f(X?) > L?; ne_f(X, L, R), R = yes where f(X?) =\= L?; eq_f(X, L, R),
/// R = yes where f(X?) =:= L?; R = no otherwise.
String _of(String f) => '''
procedure gt_$f(_?, _?, Constant).
gt_$f(X, L, yes) :- $f(X?) > L? | true.
gt_$f(_, _, no) :- otherwise | true.

procedure ne_$f(_?, _?, Constant).
ne_$f(X, L, yes) :- $f(X?) =\\= L? | true.
ne_$f(_, _, no) :- otherwise | true.

procedure eq_$f(_?, _?, Constant).
eq_$f(X, L, yes) :- $f(X?) =:= L? | true.
eq_$f(_, _, no) :- otherwise | true.
''';

/// pow, unary negation, / and functions nested in expressions.
const _rest = r'''
procedure gt_pow(_?, _?, _?, Constant).
gt_pow(X, Y, L, yes) :- pow(X?, Y?) > L? | true.
gt_pow(_, _, _, no) :- otherwise | true.

procedure eq_pow(_?, _?, _?, Constant).
eq_pow(X, Y, L, yes) :- pow(X?, Y?) =:= L? | true.
eq_pow(_, _, _, no) :- otherwise | true.

procedure gt_neg(_?, _?, Constant).
gt_neg(X, L, yes) :- -X? > L? | true.
gt_neg(_, _, no) :- otherwise | true.

procedure gt_div(_?, _?, _?, Constant).
gt_div(X, Y, L, yes) :- X? / Y? > L? | true.
gt_div(_, _, _, no) :- otherwise | true.

procedure gt_nest(_?, _?, Constant).
gt_nest(X, L, yes) :- sqrt(abs(X?) + 1) > L? | true.
gt_nest(_, _, no) :- otherwise | true.

procedure eq_halves(_?, _?, Constant).
eq_halves(X, L, yes) :- floor(X? / 2) =:= L? | true.
eq_halves(_, _, no) :- otherwise | true.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

/// [goal], run in a fresh engine that has loaded [source], ends [status], and
/// where [r] is given, with R bound to it.
void _runs(String source, String goal, ExecutionStatus status, {String? r}) {
  test('$goal ${status.name}${r == null ? '' : ', R = $r'}', () async {
    final engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    expect(engine.loadSource(source, filename: 'comparison_exp.glp'), isTrue);
    final result = await engine.runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    if (r != null) expect(result.bindings['R'].toString(), 'Const($r)');
  });
}

void main() {
  group('each function in its domain: compared as its value', () {
    for (final MapEntry(key: f, value: (x, below, above))
        in _inDomain.entries) {
      _runs(_of(f), 'gt_$f($x, $below, R)', _ok, r: 'yes');
      _runs(_of(f), 'gt_$f($x, $above, R)', _ok, r: 'no');
    }
  });

  group('the comparison and := agree on every function', () {
    for (final MapEntry(key: f, value: (x, _, _)) in _inDomain.entries) {
      _runs(_of(f), 'Y := $f($x), eq_$f($x, Y?, R)', _ok, r: 'yes');
    }
  });

  group('a function of a reader waits until the reader is bound', () {
    for (final MapEntry(key: f, value: (x, below, _)) in _inDomain.entries) {
      _runs(_of(f), 'gt_$f(Q?, $below, R)', _waits);
      _runs(_of(f), 'gt_$f(Q?, $below, R), Q = $x', _ok, r: 'yes');
    }
  });

  group('outside its domain: no value, and the guard fails', () {
    for (final (f, x) in _outOfDomain) {
      // =\= would hold of a value that is no number at all, NaN.
      _runs(_of(f), 'ne_$f($x, 0, R)', _ok, r: 'no');
      // No instance succeeds, so it fails, whatever the reader beside it.
      _runs(_of(f), 'gt_$f($x, Q?, R)', _ok, r: 'no');
    }
    // An argument that is an expression, evaluated first.
    _runs(_of('sqrt'), 'gt_sqrt(2 - 6, Q?, R)', _ok, r: 'no');
  });

  // A NaN or infinite real, which '_exp' and '_pow' yield, has no integer.
  group('a NaN or infinite real is outside the conversions\' domain', () {
    for (final f in ['integer', 'round', 'floor', 'ceil']) {
      _runs(_of(f), 'X := exp(1000), ne_$f(X?, 0, R)', _ok, r: 'no');
      _runs(_of(f), 'X := exp(1000), gt_$f(X?, Q?, R)', _ok, r: 'no');
      _runs(_of(f), 'X := pow(-8, 0.5), ne_$f(X?, 0, R)', _ok, r: 'no');
    }
  });

  group('a bound argument that is no number: the guard fails', () {
    for (final f in _inDomain.keys) {
      _runs(_of(f), 'gt_$f(foo, Q?, R)', _ok, r: 'no');
    }
    _runs(_rest, 'gt_pow(foo, 2, Q?, R)', _ok, r: 'no');
    _runs(_rest, 'gt_neg(foo, Q?, R)', _ok, r: 'no');
    // A structure whose functor is no function of Exp has no value, the
    // reader inside it notwithstanding.
    _runs(_of('sqrt'), 'gt_sqrt(foo(4), Q?, R)', _ok, r: 'no');
    _runs(_of('sqrt'), 'gt_sqrt(foo(Z?), 1, R)', _ok, r: 'no');
  });

  group('pow', () {
    _runs(_rest, 'gt_pow(2, 3, 7, R)', _ok, r: 'yes');
    _runs(_rest, 'gt_pow(2, 3, 8, R)', _ok, r: 'no');
    _runs(_rest, 'eq_pow(3, 2, 9, R)', _ok, r: 'yes');
    _runs(_rest, 'eq_pow(2, -1, 0.5, R)', _ok, r: 'yes');
    _runs(_rest, 'eq_pow(4, 0.5, 2, R)', _ok, r: 'yes');
    _runs(_rest, 'gt_pow(Q?, 2, 3, R)', _waits);
    _runs(_rest, 'gt_pow(Q?, 2, 3, R), Q = 2', _ok, r: 'yes');
    _runs(_rest, 'gt_pow(2, Q?, 3, R), Q = 2', _ok, r: 'yes');
    _runs(_rest, 'Y := pow(2, 10), eq_pow(2, 10, Y?, R)', _ok, r: 'yes');
  });

  group('unary negation and /', () {
    _runs(_rest, 'gt_neg(-3, 1, R)', _ok, r: 'yes');
    _runs(_rest, 'gt_neg(3, 1, R)', _ok, r: 'no');
    _runs(_rest, 'gt_neg(Q?, 1, R)', _waits);
    _runs(_rest, 'gt_neg(Q?, 1, R), Q = -3', _ok, r: 'yes');
    _runs(_rest, 'gt_div(3, 2, 1, R)', _ok, r: 'yes');
    _runs(_rest, 'gt_div(2, 4, 1, R)', _ok, r: 'no');
    _runs(_rest, 'gt_div(3, 0, Q?, R)', _ok, r: 'no');
  });

  group('functions nested in expressions', () {
    _runs(_rest, 'gt_nest(-8, 2.9, R)', _ok, r: 'yes');
    _runs(_rest, 'gt_nest(-8, 3.1, R)', _ok, r: 'no');
    _runs(_rest, 'gt_nest(Q?, 2.9, R)', _waits);
    _runs(_rest, 'gt_nest(Q?, 2.9, R), Q = -8', _ok, r: 'yes');
    _runs(_rest, 'eq_halves(7, 3, R)', _ok, r: 'yes');
    _runs(_rest, 'eq_halves(7, 4, R)', _ok, r: 'no');
    _runs(_rest, 'eq_halves(Q?, 3, R), Q = 6', _ok, r: 'yes');
  });
}
