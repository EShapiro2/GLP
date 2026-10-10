/// A guard over a variable the clause alone holds: the member is decided by
/// whose readers it waits on.
///
/// GLP-Spec glp.tex: "if a GLP goal A cannot be reduced now, but there is a
/// readers substitution σ such that Aσ can be reduced, such readers are
/// identified, the goal A suspends on these readers" --- a goal suspends on its
/// own readers --- and Guards: "A guard suspends if it does not succeed but
/// some instance of it under a readers substitution would succeed.  A guard
/// fails if no such instance exists."  No readers substitution binds a
/// variable the clause alone holds, so a guard member left undecided with no
/// reader of the goal to wait on fails the clause (GLP #3 Cowork, 2026-10-02
/// 15:31 UTC, A).  The variable is X, fresh to the clause, its one head
/// occurrence in the structure f(X) the head gives the goal's writer, or Y,
/// the clause's own output, a head reader matched against the goal's writer.
///
/// Every guard but =?\= succeeds only where each reader it waits on is bound,
/// so one the clause alone holds fails it, a reader of the goal beside it or
/// not; =?\= succeeds in an instance assigning a reader of the goal a term with
/// a writer in it, so it waits on the goal's readers and fails where it has
/// none.  Until 2026-10-02 a member waited on every reader it met: the goal
/// waited for ever on a variable no one could bind, or, where an instruction
/// read the fresh variable's writer, the guard answered as of a writer ---
/// =?= on ground_equal (0x45) failed where the generic guard call suspended,
/// and no_readers(X?) succeeded.  Each =?= case runs on both paths: 0x45, both
/// operands variables, and the generic guard call, one operand built in the
/// guard.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
exported procedure he(_, _?, Constant).
he(f(X), Y, yes) :- X? =?= Y? | true.

exported procedure hq(_, _?, Constant).
hq(f(X), Y, yes) :- X? =?= w(Y?) | true.

exported procedure hw(_, _?, Constant).
hw(f(X), Y, yes) :- X? =?\= Y? | true.

exported procedure he2(_, _?, Constant).
he2(f(X), Y, yes) :- X? =?= Y? | true.
he2(_?, _, no) :- otherwise | true.

exported procedure hq2(_, _?, Constant).
hq2(f(X), Y, yes) :- X? =?= w(Y?) | true.
hq2(_?, _, no) :- otherwise | true.

exported procedure hw2(_, _?, Constant).
hw2(f(X), Y, yes) :- X? =?\= Y? | true.
hw2(_?, _, no) :- otherwise | true.

exported procedure hg(_, Constant).
hg(f(X), yes) :- ground(X?) | true.

exported procedure hk(_, Constant).
hk(f(X), yes) :- known(X?) | true.

exported procedure hn(_, Constant).
hn(f(X), yes) :- no_readers(X?) | true.

exported procedure hi(_, Constant).
hi(f(X), yes) :- integer(X?) | true.

exported procedure hc(_, Constant).
hc(f(X), yes) :- X? > 3 | true.

exported procedure hcx(_, Constant).
hcx(f(X), yes) :- X? + 1 > 3 | true.

exported procedure hat(_, Constant).
hat(f(X), yes) :- X? @< b | true.

exported procedure hwt(_, Constant).
hwt(f(X), yes) :- wait(X?) | true.

exported procedure hu(_, Constant).
hu(f(X), yes) :- unknown(X?) | true.

exported procedure ho(_, Constant).
ho(Y?, yes) :- ground(Y?) | Y = a.

exported procedure hon(_, Constant).
hon(Y?, yes) :- no_readers(Y?) | Y = a.

exported procedure hm(_, _?, Constant).
hm(f(X), Z, yes) :- X? =?= Z? | true.

exported procedure hmq(_, _?, Constant).
hmq(f(X), Z, yes) :- X? =?= w(Z?) | true.

exported procedure hmn(_, _?, Constant).
hmn(f(X), Z, yes) :- X? =?\= Z? | true.

exported procedure hmc(_, _?, Constant).
hmc(f(X), Z, yes) :- X? > Z? | true.

exported procedure t3(_, _?, _).
t3(f(c), Z, Y?) :- ground(Z?) | Y = ok.

exported procedure t4(_, _?, _).
t4(f(c), Z, Y?) :- no_readers(Z?) | Y = ok.

exported procedure eq(_?, _?, Constant).
eq(X, Y, yes) :- X? =?= Y? | true.

exported procedure gt(_?, Constant).
gt(X, yes) :- X? > 3 | true.
''';

const _ok = ExecutionStatus.succeeded;
const _fails = ExecutionStatus.failed;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'guard_clause_variable.glp'),
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
  group('no reader of the goal to wait on: the member fails the clause', () {
    // X? =?= b, X fresh: on 0x45 it read X's writer and failed; it fails now
    // because no readers substitution binds X.
    _runs('he(W, b, R)', _fails);
    // The same on the generic guard call: it suspended on X? for ever.
    _runs('hq(W, b, R)', _fails);
    // X? =?\= b: it suspended on X? for ever.
    _runs('hw(W, b, R)', _fails);
    // With an otherwise clause, the failure gives no.
    _runs('he2(W, b, R)', _ok, r: 'no');
    _runs('hq2(W, b, R)', _ok, r: 'no');
    _runs('hw2(W, b, R)', _ok, r: 'no');
  });

  group('every guard over a variable the clause alone holds fails', () {
    _runs('hg(W, R)', _fails); // ground (0x41)
    _runs('hk(W, R)', _fails); // known (0x42)
    // no_readers (0x44): it read X's writer, which holds no reader, and
    // succeeded.
    _runs('hn(W, R)', _fails);
    _runs('hi(W, R)', _fails); // a type guard, the generic call
    _runs('hc(W, R)', _fails); // a comparison
    _runs('hcx(W, R)', _fails); // a comparison, X? inside an expression
    _runs('hat(W, R)', _fails); // @<
    _runs('hwt(W, R)', _fails); // wait
    // The clause's own output, Y?: the goal holds Y's writer.
    _runs('ho(W, R)', _fails);
    _runs('hon(W, R)', _fails); // it succeeded, W = a
    // unknown/1 succeeds on an unbound variable and waits on none.
    _runs('hu(W, R)', _ok, r: 'yes');
  });

  group('a reader of the goal beside one the clause alone holds', () {
    // =?= cannot succeed while X? is unbound, whatever Q? becomes: it fails
    // at once, on 0x45 and the generic call alike.
    _runs('hm(W, Q?, R)', _fails);
    _runs('hmq(W, Q?, R)', _fails);
    // So does a comparison.
    _runs('hmc(W, Q?, R)', _fails);
    // =?\= waits on Q?: assigned a term with a writer in it, the two can never
    // be made ground and equal; assigned c, X? := c would make them so, and
    // no reader of the goal is left to wait on.
    _runs('hmn(W, Q?, R)', _waits);
    _runs('hmn(W, Q?, R), Q = f(V)', _ok, r: 'yes');
    _runs('hmn(W, Q?, R), Q = c', _fails);
  });

  group('a reader whose writer the head binds sees the binding', () {
    // W? stands in the goal's g(W?), and the head assigns the goal's W f(c):
    // the guard sees g(f(c)), as the generic guard call and ground_equal do.
    // ground (0x41) and no_readers (0x44) looked the reader up in the
    // tentative substitution by its own address, found nothing, and waited
    // on W?, which only this reduction assigns.
    _runs('t3(W, g(W?), Y)', _ok);
    _runs('t4(W, g(W?), Y)', _ok);
  });

  group("a guard over the goal's readers still waits on them", () {
    _runs('eq(A?, b, R)', _waits);
    _runs('eq(A?, b, R), A = b', _ok, r: 'yes');
    _runs('gt(A?, R)', _waits);
    _runs('gt(A?, R), A = 5', _ok, r: 'yes');
  });
}
