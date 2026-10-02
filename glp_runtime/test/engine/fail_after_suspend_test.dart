/// A clause that meets a mismatch fails, whatever it suspended on before.
///
/// GLP-Spec appendix-term-matching.tex, Definition "Term Matching": a goal
/// reader against a head term is "suspend on X1?", and "the writer mgu is the
/// union of all writer assignments if no fail was encountered and the
/// suspension set is empty" --- so a fail anywhere is a fail, and a suspension
/// stands only where nothing fails.  The runtime skips the head pattern under
/// a suspended reader, leaving the variables first met in it unknown, counts a
/// guard over them undecided, and merges the clause's suspension set into the
/// goal's only when the clause suspended (GLP's task of 2026-10-01 23:58 UTC,
/// item 4).  Until 2026-10-02 the merge was made on every next clause, the
/// pattern under a suspended reader was read against the structure the
/// traversal last held, and unify_structure and get_value abandoned the clause
/// at an unbound reader, so a goal whose every clause failed suspended.
///
/// The goals are those of /Users/udi/Grassroots/tmp/glp-gap2-probe3b.glp, and
/// the last group is the open edge of the task, implemented as the table has
/// it: a reader suspended on is a suspension whatever the same head's
/// tentative substitution binds.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _probe = '''
T ::= f(Integer).
E ::= [].
L ::= [Integer | E].
F ::= f(String).
G ::= g(String).
V ::= h(F).

procedure p1(Integer?, Integer?).
p1(1, 2).
procedure p2(Integer?, Integer?).
p2(2, 1).
procedure p3(T?, Integer?).
p3(f(1), 2).
procedure p4(L?, Integer?).
p4([1], 2).
procedure p5(T?).
p5(f(Y)) :- integer(Y?) | true.
procedure p6(L?).
p6([X|_]) :- integer(X?) | true.
procedure p7(Integer?, Integer?).
p7(1, 3).
procedure p8(F?, G?).
p8(f("a"), g("b")).
procedure p9(F?, G?).
p9(f("a"), g("a")).
procedure p10(F?, G?, Integer?).
p10(f("a"), g("b"), 1).
procedure p11(V?, Integer?).
p11(h(f("a")), 1).
procedure p12(V?, Integer?).
p12(h(f("a")), 1).

procedure q1(T?, Integer?).
q1(f(Y), N) :- Y? > 5, N? > 5 | true.

procedure e1(T?, T).
e1(f(2), f(1)).
procedure e2(T?, T).
e2(f(1), f(1)).
procedure e3(Integer?, Integer).
e3(2, 1).
''';

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_probe, filename: 'fail_after_suspend.glp'), isTrue);
  return engine;
}

void main() {
  final cases = <String, ExecutionStatus>{
    // Argument 1 suspends on X?, argument 2 mismatches: the clause fails.
    'p1(X?, 3)': ExecutionStatus.failed,
    // The control: argument 1 mismatches first.
    'p2(1, X?)': ExecutionStatus.failed,
    // A structure, and a list, under the suspended reader, then a mismatch.
    'p3(X?, 3)': ExecutionStatus.failed,
    'p4(X?, 3)': ExecutionStatus.failed,
    // A guard on a variable met only under the suspended reader: undecided.
    'p5(X?)': ExecutionStatus.suspended,
    'p6(X?)': ExecutionStatus.suspended,
    // Suspends on X?, and the second argument matches.
    'p7(X?, 3)': ExecutionStatus.suspended,
    // The suspended second argument's pattern is not read against the
    // structure the first left behind.
    'p8(f("a"), X?)': ExecutionStatus.suspended,
    'p9(f("a"), X?)': ExecutionStatus.suspended,
    'p10(f("a"), X?, 2)': ExecutionStatus.failed,
    // A nested structure under the suspended reader.
    'p11(X?, 1)': ExecutionStatus.suspended,
    'p12(X?, 2)': ExecutionStatus.failed,
    // A guard over an unknown variable is passed by, and a later guard over a
    // known one that fails fails the clause.
    'q1(X?, 3)': ExecutionStatus.failed,
    'q1(X?, 7)': ExecutionStatus.suspended,
    // The open edge: a reader whose writer the same head binds is suspended on,
    // whether the skipped pattern disagrees (e1, e3) or agrees (e2).
    'e1(Y?, Y)': ExecutionStatus.suspended,
    'e2(Y?, Y)': ExecutionStatus.suspended,
    'e3(Y?, Y)': ExecutionStatus.suspended,
  };

  for (final MapEntry(key: goal, value: status) in cases.entries) {
    test('$goal ${status.name}', () async {
      final r = await _engine().runGoal(goal);
      expect(r.status, status, reason: '${r.error}');
    });
  }
}
