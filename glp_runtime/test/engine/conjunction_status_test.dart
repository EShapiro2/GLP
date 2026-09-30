/// A posted conjunction's status is that of the whole run at quiescence.
///
/// Specification: the conjuncts are the run's initial resolvent (GLP-Spec
/// glp.tex; IGLP dglp.tex), reduced FIFO to quiescence as a madGLP agent
/// reduces its resolvent (IGLP madglp.tex), and the status is the run's
/// there.  GLP's task of 2026-09-27 19:48 UTC: p(X?), q(X) with q binding X
/// reports success.  Fixture: programs/tests/conjunction_status/wait_later.glp.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
final _fixture =
    File('../programs/tests/conjunction_status/wait_later.glp').absolute.path;

Future<ExecutionResult> _run(String goal) =>
    (GlpEngine(rootSelfGlpPath: _root)..loadFile(_fixture)).runGoal(goal);

void main() {
  test('p(X?), q(X) with q binding X reports success', () async {
    final r = await _run('p(X?), q(X)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
  });

  test('the order of the conjuncts does not change the status', () async {
    final r = await _run('q(X), p(X?)');
    expect(r.status, ExecutionStatus.succeeded);
    expect(r.bindings['X'].toString(), contains('yes'));
    expect((await _run('p(X?), q(X), p(no)')).status,
        ExecutionStatus.succeeded);
  });

  test('a conjunct still waiting at quiescence leaves the run suspended',
      () async {
    expect((await _run('p(X?), q(Y)')).status, ExecutionStatus.suspended);
  });
}
