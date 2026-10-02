/// ground/1, on the ground instruction (0x41), the argument a variable, and on
/// the generic guard call, the argument a term built in the guard: one
/// decision on both.
///
/// GLP-Spec appendix-guards.tex: ground/1 succeeds where its argument is
/// ground, and glp.tex, Guards: "A guard suspends if it does not succeed but
/// some instance of it under a readers substitution would succeed.  A guard
/// fails if no such instance exists."  So it succeeds on a ground term, waits
/// on the unbound readers of the goal in it, and fails on an unbound writer,
/// which no readers substitution grounds, and on a mutual reference, which
/// "holds the writer of a stream tail, so it is neither ground nor a constant
/// type" (TGLP typed-glp.tex; GLP #3 Cowork, 2026-10-02 15:31 UTC, C) --- as
/// =?= has failed on one since 418eb92b.  Until 2026-10-02 the ground
/// instruction passed a mutual reference as ground, and the generic call
/// succeeded on any term, unbound readers and writers in it or not.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = r'''
procedure g(MutualRef?, Constant).
g(M, yes) :- ground(M?) | true.
g(_, no) :- otherwise | true.

procedure gg(MutualRef?, Constant).
gg(M, yes) :- ground(h(M?)) | true.
gg(_, no) :- otherwise | true.

procedure e(MutualRef?, Constant).
e(M, yes) :- M? =?= M? | true.
e(_, no) :- otherwise | true.

exported procedure mr(Stream(Constant), Constant, Constant, Constant).
mr(S?, G?, GG?, E?) :-
    allocate_mutual_reference(M, S),
    g(M?, G),
    gg(M?, GG),
    e(M?, E),
    close_mutual_reference(M?).

exported procedure gv(_?, Constant).
gv(X, yes) :- ground(X?) | true.
gv(_, no) :- otherwise | true.

exported procedure gt(_?, Constant).
gt(X, yes) :- ground(h(X?)) | true.
gt(_, no) :- otherwise | true.

exported procedure hgg(_, Constant).
hgg(f(X), yes) :- ground(g(X?)) | true.

exported procedure hmg(_, _?, Constant).
hmg(f(X), Z, yes) :- ground(g(X?, Z?)) | true.
''';

const _ok = ExecutionStatus.succeeded;
const _fails = ExecutionStatus.failed;
const _waits = ExecutionStatus.suspended;

GlpEngine _engine() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source, filename: 'ground_guard.glp'), isTrue);
  return engine;
}

/// [goal] ends [status], and each of [bound] is the constant given.
void _runs(String goal, ExecutionStatus status,
    {Map<String, String> bound = const {}}) {
  test('$goal ${status.name}${bound.isEmpty ? '' : ', $bound'}', () async {
    final result = await _engine().runGoal(goal);
    expect(result.status, status, reason: '${result.error}');
    for (final MapEntry(:key, :value) in bound.entries) {
      expect(result.bindings[key].toString(), 'Const($value)', reason: key);
    }
  });
}

void main() {
  group('a mutual reference is not ground', () {
    // 0x41, the generic call, and =?=, each falling to otherwise.
    _runs('mr(S, G, GG, E)', _ok, bound: {'G': 'no', 'GG': 'no', 'E': 'no'});
  });

  group('the same decision on both paths', () {
    for (final (term, status, r) in const [
      ('a', _ok, 'yes'),
      ('f(a, [1, 2])', _ok, 'yes'),
      // A writer in the term: no readers substitution grounds it.
      ('f(W)', _ok, 'no'),
      ('[1, 2 | W]', _ok, 'no'),
      // A reader of the goal in it: wait for it.
      ('f(Z?)', _waits, null),
    ]) {
      _runs('gv($term, R)', status, bound: {if (r != null) 'R': r});
      _runs('gt($term, R)', status, bound: {if (r != null) 'R': r});
    }
    _runs('gv(Z?, R)', _waits);
    _runs('gt(Z?, R)', _waits);
    _runs('gv(Z?, R), Z = f(a)', _ok, bound: {'R': 'yes'});
    _runs('gt(Z?, R), Z = f(a)', _ok, bound: {'R': 'yes'});
    _runs('gt(f(Z?), R), Z = g(W)', _ok, bound: {'R': 'no'});
  });

  group('a variable the clause alone holds, inside a term', () {
    _runs('hgg(W, R)', _fails);
    _runs('hmg(W, Q?, R)', _fails);
  });
}
