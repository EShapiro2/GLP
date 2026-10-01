/// A parameter the call binds to an input type and a parameter the callee's
/// clauses fix, in one instantiation.
///
/// Specification: TGLP, parameterized-types.tex, Definition "Instantiation"
/// (a map from parameters to types of the program, under which the caller's
/// clause and the callee's clauses are well-typed); typed-glp.tex, "Type
/// Declarations" (an input type is a type).  The merge of main's input
/// bindings (cd1bb142) with gap's callee-clause solving: the polarity the call
/// chooses for a bare parameter is the one the callee's clauses are probed
/// under, and a parameter the callee's clauses fix is probed at its output and
/// its input type.  Fixture: programs/tests/input_binding/callee_route.glp.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/input_binding/$name').absolute.path;

void main() {
  test('pass(C?, D) instantiates pass(X, M?) at X = Choice?, M = Choice?: '
      'the call fixes X, pass\'s clause fixes M at its input type, and the '
      'program loads and runs', () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadFile(_fixture('callee_route.glp'));
    final r = await engine.runGoal('go(yes, E)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['E'].toString(), contains('yes'));
  });

  ratedGoalGetsCalleeSolving();
}

// A RATED goal (sGLP, Goal @ Rate) is typed as its goal (svGLP,
// sections/sglp.tex), so it gets callee-clause solving as any goal does: the
// RatedGoal branch of _checkBodyAtomWithTerm passes `callee` on.  Until
// 2026-10-01 it did not, and the instantiation of a rated call was solved
// without the callee's clauses.  GLP's task of 2026-10-01 13:06 UTC, 3(c).
void ratedGoalGetsCalleeSolving() {
  test('a rated goal is solved with the callee\'s clauses, as the same goal '
      'unrated is', () {
    String verdict(String call) {
      try {
        GlpEngine(rootSelfGlpPath: _root).loadSource('''
Choice ::= yes ; no.

procedure(X, M) pass(X, M?).
pass(A, A?).

procedure use(Choice?, Choice).
use(C, C?).

procedure go(Choice?, Choice).
go(C, E?) :- $call, use(D?, E).
''');
        return 'loads';
      } catch (e) {
        return '$e';
      }
    }

    expect(verdict('pass(C?, D)'), 'loads');
    expect(verdict('pass(C?, D) @ 1/day'), 'loads');
  });
}
