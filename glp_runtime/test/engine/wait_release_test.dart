// glp_runtime/test/engine/wait_release_test.dart
//
// A goal reduced with a clause that does not wait leaves no timer pending.
//
// GLP-Spec appendix-guards.tex, the time guards: "wait(D) suspends for D
// milliseconds then succeeds.  wait_until(T) suspends until the current time
// is at or past timestamp T (Unix milliseconds), then succeeds."  Each is a
// guard of one clause: once the goal is reduced with another clause, no goal
// waits on the timer, and a run with no goal left ends.
//
// Until 2026-11-12 the timer stayed pending after the goal had gone, and the
// run waited for it (runtime.dart, [Runtime.releaseWait]; GSA #5, Code_inbox
// 2026-11-12 09:46 UTC: play_sovereign did not return, a goal of
// wait_until/1 for the next day having been reduced by its clause for the
// closed stream).
//
// Fixture: programs/tests/timer_release.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;
final String _fixture = Directory('../programs/tests/timer_release').absolute.path;

Object? _value(GlpEngine engine, rt.Term? t) {
  if (t == null) return null;
  final d = engine.runtime.heap.dereference(t);
  return d is rt.ConstTerm ? d.value : null;
}

void main() {
  test('a goal reduced by another clause ends the run before its timer fires',
      () async {
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    expect(engine.loadProgram(_fixture), isTrue);
    final clock = Stopwatch()..start();
    final r = await engine.runGoal('go(R).');
    clock.stop();
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_value(engine, r.bindings['R']), 0);
    // tick/2's timer is twenty seconds; its stream closes after a quarter of
    // a second.
    expect(clock.elapsed, lessThan(const Duration(seconds: 10)));
    expect(engine.runtime.pendingTimers, 0);
  });

  test('the clause of the wait is taken when its timer fires', () async {
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    expect(engine.loadProgram(_fixture), isTrue);
    final r = await engine.runGoal('timed(R).');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_value(engine, r.bindings['R']), 1);
    expect(engine.runtime.pendingTimers, 0);
  });
}
