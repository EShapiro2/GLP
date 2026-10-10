/// An arithmetic comparison whose expression has no value under any readers
/// substitution fails, whatever readers stand elsewhere in it.
///
/// GLP-Spec glp.tex, Guards: "A guard suspends if it does not succeed but some
/// instance of it under a readers substitution would succeed. A guard fails if
/// no such instance exists."  A comparison succeeds only where both operands
/// evaluate to numbers (appendix-guards.tex, "Arithmetic comparison guards"),
/// and every arithmetic operator needs a value of each of its operands, so an
/// operand with none in any instance --- a quotient or remainder by zero, a
/// quotient or remainder of an operand that is no integer, `//` and `mod`
/// taking integers only (026515d), a bound term that is no number, an unbound
/// writer --- leaves no instance that succeeds.  Until 2026-10-02 the
/// comparison waited on the readers beside it: `cz(X, yes) :- X? / 0 > 1 |
/// true.`, `otherwise` beneath,
/// suspended `cz(Q?, R)`, and `X? > 1 / 0` waited on `X?` before the right
/// operand was evaluated (Integration, 2026-10-02 17:06 UTC, S3; GLP #3
/// Cowork, 17:12 UTC: "S1, S2, S3: yes, each a task, compliance").  A
/// comparison some instance of which succeeds still waits on the goal's
/// readers.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
procedure cz(Number?, Constant).
cz(X, yes) :- X? / 0 > 1 | true.
cz(_, no) :- otherwise | true.

procedure cr(Number?, Constant).
cr(X, yes) :- X? > 1 / 0 | true.
cr(_, no) :- otherwise | true.

procedure cm(Integer?, Constant).
cm(X, yes) :- X? mod 0 =:= 1 | true.
cm(_, no) :- otherwise | true.

procedure ci(Integer?, Constant).
ci(X, yes) :- X? // 0 < 1 | true.
ci(_, no) :- otherwise | true.

procedure cd(Number?, Number?, Constant).
cd(X, Y, yes) :- X? / Y? > 1 | true.
cd(_, _, no) :- otherwise | true.

procedure cw(Number?, Constant).
cw(X, yes) :- X? + 1 > 1 | true.
cw(_, no) :- otherwise | true.

procedure cn(Number?, Constant).
cn(X, yes) :- X? =\= 2 * 0 | true.
cn(_, no) :- otherwise | true.

procedure cs(_?, _?, Constant).
cs(X, Y, yes) :- X? + Y? > 1 | true.
cs(_, _, no) :- otherwise | true.

procedure ch(Number?, Constant).
ch(X, yes) :- X? mod 0.5 =:= 1 | true.
ch(_, no) :- otherwise | true.

procedure cq(Number?, Constant).
cq(X, yes) :- X? // 2.5 > 1 | true.
cq(_, no) :- otherwise | true.

procedure co(Number?, Constant).
co(X, yes) :- X? mod 2 =:= 1 | true.
co(_, no) :- otherwise | true.

procedure cv(Number?, Constant).
cv(X, yes) :- X? // 2 > 2 | true.
cv(_, no) :- otherwise | true.

procedure cx(Number?, Number?, Constant).
cx(X, Y, yes) :- X? // Y? > 1 | true.
cx(_, _, no) :- otherwise | true.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'comparison_undefined.glp'),
      isTrue);
  return engine;
}

/// [goal] ends [status], and where [r] is given, with R bound to it.
void _runs(String goal, ExecutionStatus status, {String? r}) {
  test('$goal ${status.name}${r == null ? '' : ', R = $r'}', () async {
    final result = await _engine().runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    if (r != null) expect(result.bindings['R'].toString(), 'Const($r)');
  });
}

void main() {
  group('a zero divisor: no instance succeeds, and the clause fails', () {
    _runs('cz(Q?, R)', _ok, r: 'no');
    _runs('cz(4, R)', _ok, r: 'no');
    // The undefined operand on the right, an unbound reader on the left.
    _runs('cr(Q?, R)', _ok, r: 'no');
    _runs('cr(4, R)', _ok, r: 'no');
    _runs('cm(Q?, R)', _ok, r: 'no');
    _runs('ci(Q?, R)', _ok, r: 'no');
    // The divisor a reader of the goal bound to zero.
    _runs('cd(Q?, 0, R)', _ok, r: 'no');
  });

  group('a bound operand that is no number: the clause fails', () {
    _runs('cs(Q?, foo, R)', _ok, r: 'no');
    _runs('cs(foo, Q?, R)', _ok, r: 'no');
    _runs('cs(1, 2, R)', _ok, r: 'yes');
    _runs('cs(Q?, 2, R)', _waits);
  });

  group('some instance succeeds: it waits on the goal reader', () {
    _runs('cd(4, Q?, R)', _waits);
    _runs('cd(Q?, 2, R)', _waits);
    _runs('cd(Q?, 2, R), Q = 4', _ok, r: 'yes');
    _runs('cw(Q?, R)', _waits);
    _runs('cw(Q?, R), Q = 3', _ok, r: 'yes');
    _runs('cn(Q?, R)', _waits);
  });

  group('every operand a number: decided as before', () {
    _runs('cd(4, 2, R)', _ok, r: 'yes');
    _runs('cd(1, 2, R)', _ok, r: 'no');
    _runs('cw(3, R)', _ok, r: 'yes');
    _runs('cw(0, R)', _ok, r: 'no');
    _runs('cn(3, R)', _ok, r: 'yes');
    _runs('cn(0, R)', _ok, r: 'no');
  });

  // `//` and `mod` take integers only, an operand that is no integer having
  // no value (GLP-Spec appendix-guards.tex, 026515d: "an argument with no
  // value --- a zero divisor, a non-integer under // or mod, an argument
  // outside a function's domain --- fails the guard"; GLP #3 Cowork,
  // 2026-10-02 20:58 UTC, "20:10" B, S3's rule).  Until 2026-10-02 `//`
  // divided reals and `mod` truncated its operands: ch(5, R) threw
  // IntegerDivisionByZeroException, 0.5 truncating to 0, cq(5, R), co(7.5, R)
  // and cv(7.5, R) gave R = yes, and ch(Q?, R) and cq(Q?, R) waited on Q?.
  group('an operand of // or mod that is no integer: the clause fails', () {
    _runs('ch(5, R)', _ok, r: 'no');
    _runs('ch(Q?, R)', _ok, r: 'no');
    _runs('cq(5, R)', _ok, r: 'no');
    _runs('cq(Q?, R)', _ok, r: 'no');
    _runs('co(7.5, R)', _ok, r: 'no');
    _runs('cv(7.5, R)', _ok, r: 'no');
    _runs('cv(8.0, R)', _ok, r: 'no');
    // The real a reader of the goal, bound after the clause waited on it.
    _runs('cx(Q?, 2, R), Q = 7.5', _ok, r: 'no');
    _runs('cx(7, Q?, R), Q = 2.0', _ok, r: 'no');
  });

  group('integer operands of // and mod: decided as before', () {
    _runs('co(7, R)', _ok, r: 'yes');
    _runs('co(8, R)', _ok, r: 'no');
    _runs('cv(7, R)', _ok, r: 'yes');
    _runs('cv(5, R)', _ok, r: 'no');
    _runs('co(Q?, R)', _waits);
    _runs('co(Q?, R), Q = 7', _ok, r: 'yes');
    _runs('cx(7, Q?, R)', _waits);
    _runs('cx(7, Q?, R), Q = 2', _ok, r: 'yes');
  });
}
