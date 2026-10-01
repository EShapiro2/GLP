/// when_idle (GLP-Spec appendix-guards.tex at e3a8d52, the time guards):
/// "when_idle suspends while the machine has a Reduce or a Communicate to
/// make, and succeeds when it has none."  The engine's idleness decides it:
/// its goal queue empty, the goal asking having been taken from it.  A goal
/// that suspends on it is re-tried whenever the queue empties, one at a
/// time, the one that has waited longest first.
///
/// Fixture: programs/tests/typed/when_idle.glp.
library;

import 'dart:async';
import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(
      engine.loadFile(
          File('../programs/tests/typed/when_idle.glp').absolute.path),
      isTrue);
  return engine;
}

/// The value a binding prints as: `idle` for `Const(idle)`.
String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

/// Run [goal] with the scheduler's trace on, and the reductions it prints,
/// one line each, in the order of the run.
Future<(ExecutionResult, List<String>)> _traced(
    GlpEngine engine, String goal) async {
  final lines = <String>[];
  engine.debugTrace = true;
  final result = await runZoned(() => engine.runGoal(goal),
      zoneSpecification: ZoneSpecification(
          print: (self, parent, zone, line) => lines.add(line)));
  return (result, lines.where((l) => l.contains(' :- ')).toList());
}

void main() {
  group('when_idle succeeds only when the machine has no Reduce to make', () {
    test('m/1 reduces after every other goal of its conjunction', () async {
      final r =
          await _engine().runGoal('m(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['Z']), '0');
      // probe/3, woken by X, finds count/2 at rest.
      expect(_v(r.bindings['O']), 'after');
    });

    test('the control: the same clause without the guard reduces at once',
        () async {
      final r =
          await _engine().runGoal('n(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'before');
    });

    test('in the order of the run, m/1 is reduced after the last count/2',
        () async {
      final (r, reductions) =
          await _traced(_engine(), 'm(X), count(1000, Z), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final counts = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('count(')) i
      ];
      final ms = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('m(')) i
      ];
      final probes = [
        for (var i = 0; i < reductions.length; i++)
          if (reductions[i].startsWith('probe(')) i
      ];
      expect(counts, hasLength(1001), reason: reductions.join('\n'));
      expect(ms, hasLength(1), reason: reductions.join('\n'));
      expect(probes, hasLength(1), reason: reductions.join('\n'));
      expect(ms.single, greaterThan(counts.last));
      expect(probes.single, greaterThan(ms.single));
    });

    test('alone in the machine, it succeeds at once', () async {
      final r = await _engine().runGoal('m(X)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
    });

    test('placed after the work, it still waits for the work to rest',
        () async {
      final r =
          await _engine().runGoal('probe(X?, Z?, O), count(1000, Z), m(X)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['O']), 'after');
    });

    test('a goal suspended on a reader has no Reduce to make', () async {
      // probe/3 waits on X, and Z is never assigned: the queue empties with
      // probe/3 suspended, m/1 is re-tried and assigns X, and probe/3 finds
      // Z unassigned.
      final r = await _engine().runGoal('m(X), probe(X?, Z?, O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['O']), 'before');
    });
  });

  group('two goals waiting on when_idle', () {
    test('are re-tried one at a time, the one that waited longest first',
        () async {
      // m/1 and later/2 both wait.  Re-tried first, m/1 assigns X; later/2 is
      // re-tried only when the queue empties again, and finds X assigned.
      // Were both put back at once, m/1 would find later/2 in the queue and
      // wait again, and later/2 would find X unassigned.
      final r = await _engine().runGoal('m(X), later(X?, O), count(10, W)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_v(r.bindings['X']), 'idle');
      expect(_v(r.bindings['W']), '0');
      expect(_v(r.bindings['O']), 'after');
    });
  });

  group('the declaration', () {
    test('when_idle takes no argument: when_idle/1 is not declared', () {
      final engine =
          GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      expect(
          () => engine.loadSource('''
procedure w(Integer).
w(X?) :- when_idle(1) | X = 1.
'''),
          throwsA(predicate(
              (e) => '$e'.contains('Undefined procedure: when_idle/1'))));
    });
  });
}
