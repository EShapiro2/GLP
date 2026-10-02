/// A guard conjunction fails if any member fails, whatever its order.
///
/// GLP-Spec sections/glp.tex, Guards: "A guard conjunction succeeds if all
/// members succeed; it suspends if any member suspends and none fail; it fails
/// if any member fails."  A member that suspends puts its readers in the
/// clause's suspension set Si and the next member is tried; a member that
/// fails fails the clause, Si discarded; a clause whose guard suspended and
/// none failed suspends at commit, Si merged into the goal's suspension set
/// (GLP #3 Cowork, 2026-10-02 08:40 UTC, G).  Until 2026-10-02 a suspending
/// member put its readers in the goal's set and left the clause, so a later
/// member that fails was never tried: c1(X?, 3) over N? > 5, M? > 5 suspended
/// where the same guards swapped failed, and q(A?, 0, R) over ground(X?),
/// Y? > 0, then otherwise, suspended where it gives R = no.
///
/// One member of each guard instruction suspends before a failing one:
/// ground (0x41), known (0x42), no_readers (0x44), =?= (0x45), and the generic
/// guard call (0x40) --- a comparison, a type test, =?\= --- and the time
/// guard wait/1, whose suspension the runtime records as the others'.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = '''
Answer ::= yes ; no ; ok.
T ::= f(Integer).

procedure c1(Integer?, Integer?).
c1(N, M) :- N? > 5, M? > 5 | true.
procedure c2(Integer?, Integer?).
c2(N, M) :- M? > 5, N? > 5 | true.

procedure q(Integer?, Integer?, Answer).
q(X, Y, yes) :- ground(X?), Y? > 0 | true.
q(_, _, no) :- otherwise | true.

procedure g1(T?, Integer?).
g1(X, M) :- ground(X?), M? > 5 | true.
procedure k1(Integer?, Integer?).
k1(X, M) :- known(X?), M? > 5 | true.
procedure n1(T?, Integer?).
n1(X, M) :- no_readers(X?), M? > 5 | true.
procedure e1(Integer?, Integer?, Integer?).
e1(X, Y, M) :- X? =?= Y?, M? > 5 | true.
procedure ne1(Integer?, Integer?, Integer?).
ne1(X, Y, M) :- X? =?\\= Y?, M? > 5 | true.
procedure i1(Integer?, Integer?).
i1(X, M) :- integer(X?), M? > 5 | true.
procedure w1(Integer?, Integer?).
w1(D, M) :- wait(D?), M? > 5 | true.

procedure c3(Integer?, Integer?, Answer).
c3(N, M, ok) :- N? > 5, M? > 5 | true.
procedure later(Integer).
later(9).
procedure resume(Answer).
resume(R?) :- c3(X?, 7, R), later(X).
''';

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'guard_conjunction.glp'), isTrue);
  return engine;
}

void main() {
  final cases = <String, ExecutionStatus>{
    // The finding: the first member suspends on X?, the second fails.
    'c1(X?, 3)': ExecutionStatus.failed,
    // The same guards swapped: the failing member first.
    'c2(X?, 3)': ExecutionStatus.failed,
    // Neither fails: the conjunction suspends.
    'c1(X?, 7)': ExecutionStatus.suspended,
    'c2(X?, 7)': ExecutionStatus.suspended,
    // Decided both ways once the readers are bound.
    'c1(7, 3)': ExecutionStatus.failed,
    'c1(7, 8)': ExecutionStatus.succeeded,
    // ground (0x41) suspends on Y?, then the comparison fails.
    'g1(f(Y?), 3)': ExecutionStatus.failed,
    'g1(f(Y?), 7)': ExecutionStatus.suspended,
    // known (0x42).
    'k1(Y?, 3)': ExecutionStatus.failed,
    'k1(Y?, 7)': ExecutionStatus.suspended,
    // no_readers (0x44).
    'n1(f(Y?), 3)': ExecutionStatus.failed,
    'n1(f(Y?), 7)': ExecutionStatus.suspended,
    // =?= (0x45).
    'e1(A?, 1, 3)': ExecutionStatus.failed,
    'e1(A?, 1, 7)': ExecutionStatus.suspended,
    // =?\= (the generic call, deciding its own readers).
    'ne1(A?, 1, 3)': ExecutionStatus.failed,
    'ne1(A?, 1, 7)': ExecutionStatus.suspended,
    // A type test (the generic call).
    'i1(A?, 3)': ExecutionStatus.failed,
    'i1(A?, 7)': ExecutionStatus.suspended,
    // wait/1 suspends on its timer's reader, then the comparison fails.
    'w1(50, 3)': ExecutionStatus.failed,
  };

  for (final MapEntry(key: goal, value: status) in cases.entries) {
    test('$goal ${status.name}', () async {
      final r = await _engine().runGoal(goal);
      expect(r.status, status, reason: '${r.error}');
    });
  }

  test('q(A?, 0, R): the first clause fails, and otherwise gives R = no',
      () async {
    final r = await _engine().runGoal('q(A?, 0, R)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['R'].toString(), 'Const(no)');
  });

  test('q(A?, 1, R): the first clause suspends, and otherwise waits on it',
      () async {
    final r = await _engine().runGoal('q(A?, 1, R)');
    expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
  });

  test('q(5, 1, R): the first clause reduces, R = yes', () async {
    final r = await _engine().runGoal('q(5, 1, R)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['R'].toString(), 'Const(yes)');
  });

  test('a suspended conjunction is retried when its reader is bound',
      () async {
    final r = await _engine().runGoal('resume(R)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['R'].toString(), 'Const(ok)');
  });

  test('wait/1 still suspends on its timer and then succeeds', () async {
    final r = await _engine().runGoal('w1(50, 7)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
  });
}
