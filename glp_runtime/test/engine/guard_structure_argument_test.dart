/// A guard's argument that is a nested term is built as a body's is, into a
/// tentative structure, and the guard decides on the whole term.
///
/// GLP-Spec glp.tex, Guards: "Each guard predicate explicitly defines its
/// success condition"; appendix-guards.tex: "=?= succeeds if both arguments
/// are ground and equal", and the comparison guards "evaluate their arguments
/// as arithmetic expressions".  A guard's arguments are put into its argument
/// slots by the instructions a body goal's are, put_structure beginning a
/// structure and set_* and unify_* filling it, a structure nested in it
/// beginning with put_structure of its own; before commit the structure is a
/// term held for the guard call alone, nothing bound on the heap (IGLP
/// Implementation Notes, "Clause try").  Until 2026-10-02 the nested
/// put_structure overwrote the structure it was nested in, and set_* acted in
/// the body alone, so the guard was decided on a term never completed:
/// `t6(Z, Y?) :- Z? =?= g(f(c)) | Y = ok.` failed `t6(g(f(c)), Y)`,
/// `X? + Y? * 2 > 3` with X = 1 and Y = 2 fell to `otherwise`, and
/// `X? =?= [a, b]` failed `[a, b]` (Integration, 2026-10-02 17:06 UTC, S1;
/// GLP #3 Cowork, 17:12 UTC: "S1, S2, S3: yes, each a task, compliance").
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
procedure t6(_?, Constant).
t6(Z, Y?) :- Z? =?= g(f(c)) | Y = ok.
t6(_, Y?) :- otherwise | Y = no.

procedure t7(Integer?, Integer?, Constant).
t7(X, Y, R?) :- X? + Y? * 2 > 3 | R = yes.
t7(_, _, R?) :- otherwise | R = no.

procedure t8(_?, Constant).
t8(X, R?) :- X? =?= [a, b] | R = yes.
t8(_, R?) :- otherwise | R = no.

procedure t9(_?, _?, Constant).
t9(X, Y, R?) :- X? =?= f(g(Y?)) | R = yes.
t9(_, _, R?) :- otherwise | R = no.

procedure t10(_?, Constant).
t10(X, R?) :- X? =?= h(g(f(c)), [1, [2, 3]], k) | R = yes.
t10(_, R?) :- otherwise | R = no.

procedure t11(Integer?, Integer?, Constant).
t11(X, Y, R?) :- X? * (Y? + 1) =:= 6 | R = yes.
t11(_, _, R?) :- otherwise | R = no.

procedure t12(_?, Constant).
t12(X, R?) :- ground(f(g(X?), [1])) | R = yes.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'guard_structure_argument.glp'),
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
  group('the three cases of S1', () {
    _runs('t6(g(f(c)), R)', _ok, r: 'ok');
    _runs('t6(g(f(d)), R)', _ok, r: 'no');
    _runs('t7(1, 2, R)', _ok, r: 'yes');
    _runs('t7(0, 1, R)', _ok, r: 'no');
    _runs('t8([a, b], R)', _ok, r: 'yes');
    _runs('t8([a, c], R)', _ok, r: 'no');
    _runs('t8([a], R)', _ok, r: 'no');
  });

  group('a reader of the goal nested in the guard argument', () {
    _runs('t9(f(g(1)), 1, R)', _ok, r: 'yes');
    _runs('t9(f(g(1)), 2, R)', _ok, r: 'no');
    // Some readers substitution makes the two ground and equal: it waits.
    _runs('t9(f(g(1)), Q?, R)', _waits);
    _runs('t9(f(g(1)), Q?, R), Q = 1', _ok, r: 'yes');
  });

  group('deeper nesting, lists in structures and structures in lists', () {
    _runs('t10(h(g(f(c)), [1, [2, 3]], k), R)', _ok, r: 'yes');
    _runs('t10(h(g(f(c)), [1, [2, 4]], k), R)', _ok, r: 'no');
    _runs('t10(h(g(f(c)), [1, [2, 3]], j), R)', _ok, r: 'no');
    _runs('t11(2, 2, R)', _ok, r: 'yes');
    _runs('t11(2, 3, R)', _ok, r: 'no');
  });

  group('the generic call of ground/1 on a nested term', () {
    _runs('t12(1, R)', _ok, r: 'yes');
    _runs('t12(Q?, R)', _waits);
  });
}
