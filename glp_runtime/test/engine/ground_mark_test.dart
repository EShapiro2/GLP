/// The ground mark (IGLP appendix-implementation-notes.tex, "Ground mark", IGLP
/// 9c605b3): "when a ground test finds a list cell or a compound term ground,
/// the engine marks the term, and a later ground test of it succeeds at once.
/// The mark changes the result of no construct".
///
/// Each test counts the structures every ground test examined
/// ([groundTestObserver]): a ground test of a marked term examines one.
///
/// 1. A ground guard on a list of 10,000 elements, run three times, succeeds
///    each time; the first examines the 10,000 list cells and marks them, and
///    each later test examines one.
/// 2. A term not yet ground is not marked: a ground guard on [1, 2 | Xs?]
///    waits, examining two cells, and once Xs is assigned [3] it is tested
///    again from its head, examining three, and succeeds.
/// 3. A structure found ground through a binding the clause tried makes
///    tentatively (σ̂) is not marked: the clause fails, its binding is
///    abandoned, and the next clause's ground guard on the same structure
///    waits on its unassigned variable.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/runner.dart' show groundTestObserver;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const String _source = r'''
procedure gen(Integer?, Stream(Integer), Done).
gen(0, [], done).
gen(N, [N?|Xs?], D?) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs, D).

procedure g_ground(Stream(Integer)?, Integer).
g_ground(Xs, 1) :- ground(Xs?) | true.

exported procedure thrice(Integer?, Integer, Integer, Integer).
thrice(N, R1?, R2?, R3?) :- gen(N?, Xs, D), thrice_done(D?, Xs?, R1, R2, R3).
procedure thrice_done(Done?, Stream(Integer)?, Integer, Integer, Integer).
thrice_done(done, Xs, R1?, R2?, R3?) :- g3(Xs?, R1, R2, R3).
procedure g3(Stream(Integer)?, Integer, Integer, Integer).
g3(Xs, 1, R2?, R3?) :- ground(Xs?) | g_ground(Xs?, R2), g_ground(Xs?, R3).

procedure delay(Integer?, Stream(Integer)).
delay(0, [3]).
delay(N, Xs?) :- N? > 0 | N1 := N? - 1, delay(N1?, Xs).

exported procedure later(Integer).
later(R?) :- g_ground([1, 2 | Xs?], R), delay(5, Xs).

Box ::= f(Integer).

procedure q(Box?, Integer, Constant).
q(T, 5, yes) :- ground(T?), 1 > 2 | true.
q(T, W?, no) :- ground(T?) | five(W).

procedure five(Integer).
five(5).

exported procedure tq(Constant).
tq(R?) :- q(f(W?), W, R).
''';

GlpEngine _engine() {
  final e =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(e.loadSource(_source, filename: 'ground_mark.glp'), isTrue);
  e.maxCycles = 10000000;
  return e;
}

/// Runs [goal], returning its result and the structures each ground test
/// examined, in order.
Future<(ExecutionResult, List<int>)> _run(String goal) async {
  final e = _engine();
  final counts = <int>[];
  groundTestObserver = counts.add;
  try {
    return (await e.runGoal(goal), counts);
  } finally {
    groundTestObserver = null;
  }
}

void main() {
  test('a ground guard on a long list, run three times: one result, and a '
      'later test examines one structure', () async {
    final (r, counts) = await _run('thrice(10000, R1, R2, R3)');
    expect(r.error, isNull);
    expect(r.status, ExecutionStatus.succeeded);
    for (final v in ['R1', 'R2', 'R3']) {
      expect('${r.bindings[v]}', 'Const(1)', reason: v);
    }
    expect(counts, [10000, 1, 1]);
  });

  test('a term not yet ground is not marked, and is tested again once its '
      'variables are assigned', () async {
    final (r, counts) = await _run('later(R)');
    expect(r.error, isNull);
    expect(r.status, ExecutionStatus.succeeded);
    expect('${r.bindings['R']}', 'Const(1)');
    expect(counts, [2, 3]);
  });

  test('a structure ground through a tentative binding is not marked',
      () async {
    final (r, counts) = await _run('tq(R)');
    expect(r.status, ExecutionStatus.suspended);
    expect(counts, [1, 1]);
  });
}
