/// A goal's state in the runtime --- its argument registers, program, module,
/// tail budget and wait --- is held while the goal is in the queue or
/// suspended, and dropped when the goal ends: when it reduces with no goal of
/// its body continuing it, or fails ([GlpRuntime.goalEnded]; IGLP dglp.tex,
/// Definition "dGLP Transition System": a Reduce replaces the goal by its
/// body, a Fail moves it to F, which keeps its text).
///
/// Until 2026-10-02 the state of every goal ever spawned was held for the
/// rest of the run, some 500 bytes a goal: 3.1 million goals and 1.6 GB of the
/// 5 GB live at the end of a year of sGLP's 100-agent social graph.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// gen/2 writes the stream N, ..., 1; total/3 sums a stream; bad/1 holds of
/// 1 alone.
const String _source = r'''
procedure gen(Integer?, Stream(Integer)).
gen(0, []).
gen(N, [N?|Xs?]) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs).

procedure total(Stream(Integer)?, Integer?, Integer).
total([X|Xs], A, S?) :- A1 := A? + X?, total(Xs?, A1?, S).
total([], A, A?).

procedure run(Integer?, Integer).
run(N, S?) :- gen(N?, Xs), total(Xs?, 0, S).

procedure bad(Integer?).
bad(1).
''';

void main() {
  test(
      'a run of thousands of goals that all end leaves no goal state held, '
      'and computes as before', () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
    engine.maxCycles = 1000000;
    final goalsBefore = engine.runtime.nextGoalId;
    final r = await engine.runGoal('run(3000, S)');
    expect('${r.bindings['S']}', 'Const(4501500)');
    expect(r.status, ExecutionStatus.succeeded);
    // Some ten thousand goals were spawned, and every one of them ended.
    expect(engine.runtime.nextGoalId - goalsBefore, greaterThan(9000));
    expect(engine.runtime.goalsHeld, 0);
  });

  test('a goal suspended at the end of the run is the one goal held',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
    final r = await engine.runGoal('total(Xs?, 0, S)');
    expect(r.status, ExecutionStatus.suspended);
    expect(engine.runtime.goalsHeld, 1);
  });

  test('a goal that fails leaves no state held, and the failure is reported',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
    final r = await engine.runGoal('bad(2)');
    expect(r.status, ExecutionStatus.failed);
    expect(engine.runtime.failedGoals, isNotEmpty);
    expect(engine.runtime.goalsHeld, 0);
  });
}
