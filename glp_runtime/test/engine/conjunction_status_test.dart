/// A posted conjunction's status is that of the whole run at quiescence.
///
/// Specification: the conjuncts are the run's initial resolvent (GLP-Spec
/// glp.tex; IGLP dglp.tex), reduced FIFO to quiescence as a madGLP agent
/// reduces its resolvent (IGLP madglp.tex), and the status is the run's
/// there.  GLP's task of 2026-09-27 19:48 UTC: p(X?), q(X) with q binding X
/// reports success.  Fixture: programs/tests/conjunction_status/wait_later.glp.
///
/// A goal variable is reported in the bindings whichever occurrence meets it
/// first, writer or reader: the outcome of the run gives every variable of the
/// initial goal its value (GLP-Spec glp.tex, Definition "cGLP Proper Run,
/// Outcome").  GLP's task of 2026-09-30 14:31 UTC: p(X?), q(X) prints X.
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

  test('p(X?), q(X) reports X, first met as a reader', () async {
    final r = await _run('p(X?), q(X)');
    expect(r.bindings.keys, ['X']);
    expect(r.bindings['X'].toString(), contains('yes'));
  });

  test('a variable first met as a reader in a list or a structure is reported',
      () async {
    for (final goal in ['r([X?]), q(X)', 's(pair(X?)), q(X)']) {
      final r = await _run(goal);
      expect(r.status, ExecutionStatus.succeeded, reason: goal);
      expect(r.bindings.keys, ['X'], reason: goal);
      expect(r.bindings['X'].toString(), contains('yes'), reason: goal);
    }
    final r = await _run('r([X?, Y?]), q(Y), q(X)');
    expect(r.bindings.keys, ['X', 'Y']);
    expect(r.bindings['X'].toString(), contains('yes'));
    expect(r.bindings['Y'].toString(), contains('yes'));
  });

  test('a variable met only as a reader is reported unbound', () async {
    for (final goal in ['p(X?)', 'p(X?), p(X?)', 's(pair(X?))']) {
      final r = await _run(goal);
      expect(r.status, ExecutionStatus.suspended, reason: goal);
      expect(r.bindings.containsKey('X'), isTrue, reason: goal);
      expect(r.bindings['X'], isNull, reason: goal);
    }
  });
}
