/// no_readers/1, on the no_readers instruction (0x44), the argument a
/// variable, and on the generic guard call, the argument a term built in the
/// guard: one decision on both.
///
/// GLP-Spec appendix-guards.tex, "Monotonicity and implications":
/// "no_readers(X) succeeds but known(X) fails for an unassigned writer;
/// no_readers(f(X?)) suspends but known(f(X?)) succeeds."  The guard collects
/// the term's unbound readers: none, it succeeds; some, it waits on those of
/// the goal (glp.tex, Guards), and fails on one the clause alone holds, no
/// readers substitution binding it (GLP #3 Cowork, 2026-10-02 15:31 UTC, A).
/// Until 2026-10-02 the runtime evaluated no_readers/1 on a variable alone: a
/// term reached the generic guard call, which had no case for it and failed
/// the clause with a warning (GLP #3 Cowork, 2026-10-02 15:31 UTC, item 5,
/// B9).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
exported procedure p(_?, _).
p(X, Y?) :- no_readers(f(X?)) | Y = a.

exported procedure q(_?, _).
q(X, Y?) :- no_readers(X?) | Y = a.

exported procedure pn(_, _).
pn(f(X), Y?) :- no_readers(g(X?)) | Y = a.
''';

const _ok = ExecutionStatus.succeeded;
const _fails = ExecutionStatus.failed;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'no_readers_guard.glp'), isTrue);
  return engine;
}

/// [goal] ends [status], and where [y] is given, with Y bound to it.
void _runs(String goal, ExecutionStatus status, {String? y}) {
  test('$goal ${status.name}${y == null ? '' : ', Y = $y'}', () async {
    final result = await _engine().runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    if (y != null) expect(result.bindings['Y'].toString(), 'Const($y)');
  });
}

void main() {
  group('the term holds no reader: it succeeds', () {
    for (final call in const ['p', 'q']) {
      _runs('$call(c, Y)', _ok, y: 'a');
      _runs('$call(g(1, [b]), Y)', _ok, y: 'a');
      // A writer is no reader.
      _runs('$call(g(W), Y)', _ok, y: 'a');
    }
  });

  group("a reader of the goal in it: it waits, and resumes when it is bound",
      () {
    for (final call in const ['p', 'q']) {
      _runs('$call(Z?, Y)', _waits);
      _runs('$call(Z?, Y), Z = b', _ok, y: 'a');
      _runs('$call(g(Z?), Y)', _waits);
      _runs('$call(g(Z?), Y), Z = 1', _ok, y: 'a');
      _runs('$call(g(Z?), Y), Z = h(V?)', _waits);
      _runs('$call(g(Z?), Y), Z = h(V?), V = 2', _ok, y: 'a');
    }
  });

  group('a reader the clause alone holds: it fails', () {
    _runs('pn(W, Y)', _fails);
  });
}
