/// A guard over an unknown variable is decided by the same decision as any
/// guard, the unknown standing for any term.
///
/// A head variable whose writer occurrence lies under a goal reader the head
/// suspends on is unknown: its value is the goal's subterm there, not yet
/// given (GLP's task of 2026-10-01 23:58 UTC, item 4, 3(b);
/// unknown_variable_test.dart).  GLP-Spec glp.tex, Guards: "A guard suspends if
/// it does not succeed but some instance of it under a readers substitution
/// would succeed.  A guard fails if no such instance exists."  The instances
/// are those of the goal reader the head suspends on, under which the unknown
/// variable stands for any term; so where no term makes the guard succeed it
/// fails, and the clause with it, whatever the head waits on (GLP #3 Cowork,
/// 2026-10-02 15:31 UTC, B).  Until 2026-10-02 a guard over an unknown
/// variable was passed by, undecided: pu(P?, g(W), R) suspended on P?, where
/// g(W) holds a writer, X? =?= g(W) fails in every instance, and otherwise
/// gives R = no.
///
/// Each =?= case runs on ground_equal (0x45), both operands variables, and on
/// the generic guard call, an operand built in the guard; =?\= is the generic
/// call alone.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
exported procedure pu(_?, _?, Constant).
pu(f(X), Y, yes) :- X? =?= Y? | true.
pu(_, _, no) :- otherwise | true.

exported procedure pug(_?, _?, Constant).
pug(f(X), Y, yes) :- Y? =?= w(X?) | true.
pug(_, _, no) :- otherwise | true.

exported procedure pun(_?, _?, Constant).
pun(f(X), Y, yes) :- X? =?\= Y? | true.
pun(_, _, no) :- otherwise | true.

exported procedure puo(_?, Constant).
puo(f(X), yes) :- X? =?= g(X?) | true.
puo(_, no) :- otherwise | true.

exported procedure pcl(_?, _?, Constant).
pcl(f(X), Y, yes) :- Y? =?= w(X?, a) | true.
pcl(_, _, no) :- otherwise | true.

exported procedure pb(_?, _?, Constant).
pb(f(X), f(Y), yes) :- X? =?= Y? | true.
pb(_, _, no) :- otherwise | true.
''';

const _ok = ExecutionStatus.succeeded;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'guard_unknown_variable.glp'),
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
  group('no term for the unknown makes the guard succeed: the clause fails',
      () {
    // g(W) holds a writer, which no readers substitution grounds.
    _runs('pu(P?, g(W), R)', _ok, r: 'no'); // 0x45
    _runs('pug(P?, w(g(W)), R)', _ok, r: 'no'); // the generic guard call
    // X would stand for a term containing itself.
    _runs('puo(P?, R)', _ok, r: 'no');
    // A clash beside the unknown: a against b.
    _runs('pcl(P?, w(c, b), R)', _ok, r: 'no');
  });

  group('some term for the unknown makes the guard succeed: the clause waits',
      () {
    _runs('pu(P?, b, R)', _waits);
    _runs('pu(P?, b, R), P = f(b)', _ok, r: 'yes');
    _runs('pu(P?, b, R), P = f(c)', _ok, r: 'no');
    _runs('pug(P?, w(b), R)', _waits);
    _runs('pug(P?, w(b), R), P = f(b)', _ok, r: 'yes');
    _runs('pcl(P?, w(c, a), R)', _waits);
    _runs('pcl(P?, w(c, a), R), P = f(c)', _ok, r: 'yes');
    // Both operands unknown, under two goal readers.
    _runs('pb(P?, Q?, R)', _waits);
    _runs('pb(P?, Q?, R), P = f(a), Q = f(a)', _ok, r: 'yes');
    _runs('pb(P?, Q?, R), P = f(a), Q = f(b)', _ok, r: 'no');
    // =?\= succeeds whatever X stands for, g(W) never being ground: the clause
    // waits on P? alone, and reduces when it is bound.
    _runs('pun(P?, g(W), R)', _waits);
    _runs('pun(P?, g(W), R), P = f(a)', _ok, r: 'yes');
    _runs('pun(P?, b, R), P = f(b)', _ok, r: 'no');
  });
}
