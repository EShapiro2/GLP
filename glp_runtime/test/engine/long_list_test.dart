/// Guards and kernels that walk a term, on a list of 50,000 elements: the
/// ground/1 and no_readers/1 guards succeed on it, as GLP-Spec has a guard
/// succeed where its success condition holds (glp.tex, "Guards": "A guard
/// suspends if it does not succeed but some instance of it under a readers
/// substitution would succeed. A guard fails if no such instance exists"),
/// and '_output' prints it, as send_to_user/1 emits each ground element of
/// its stream (appendix-guards.tex: "Its clause is guarded by ground on the
/// element and invokes the '_output' body kernel").
///
/// Until 2026-10-02 the three walked the term recursively, a Dart frame or two
/// to a list element, and on a list of 10,000 elements the goal failed with
/// "Stack Overflow": a ground list of that length failed ground/1.
///
/// Each list is walked once it is complete (gen/3 says so): a guard waiting
/// on a list still being written walks it again from its head each time an
/// element arrives, which on 50,000 elements takes minutes.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

const String _source = r'''
procedure gen(Integer?, Stream(Integer), Done).
gen(0, [], done).
gen(N, [N?|Xs?], D?) :- N? > 0 | N1 := N? - 1, gen(N1?, Xs, D).

procedure g_ground(Stream(Integer)?, Integer).
g_ground(Xs, 1) :- ground(Xs?) | true.
procedure g_noreaders(Stream(Integer)?, Integer).
g_noreaders(Xs, 1) :- no_readers(Xs?) | true.

procedure ground_(Integer?, Integer).
ground_(N, R?) :- gen(N?, Xs, D), ground_done(D?, Xs?, R).
procedure ground_done(Done?, Stream(Integer)?, Integer).
ground_done(done, Xs, R?) :- g_ground(Xs?, R).
procedure noreaders_(Integer?, Integer).
noreaders_(N, R?) :- gen(N?, Xs, D), noreaders_done(D?, Xs?, R).
procedure noreaders_done(Done?, Stream(Integer)?, Integer).
noreaders_done(done, Xs, R?) :- g_noreaders(Xs?, R).
procedure out_(Integer?).
out_(N) :- gen(N?, Xs, D), out_done(D?, Xs?).
procedure out_done(Done?, Stream(Integer)?).
out_done(done, Xs) :- send_to_user([Xs?]).
''';

GlpEngine _engine() {
  final e = GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);
  e.maxCycles = 10000000;
  return e;
}

void main() {
  test('ground/1 succeeds on a ground list of 50,000 elements', () async {
    final r = await _engine().runGoal('ground_(50000, R)');
    expect(r.error, isNull);
    expect(r.status, ExecutionStatus.succeeded);
    expect('${r.bindings['R']}', 'Const(1)');
  });

  test('no_readers/1 succeeds on a ground list of 50,000 elements', () async {
    final r = await _engine().runGoal('noreaders_(50000, R)');
    expect(r.error, isNull);
    expect(r.status, ExecutionStatus.succeeded);
    expect('${r.bindings['R']}', 'Const(1)');
  });

  test('ground/1 on a list with an unbound reader suspends, as before',
      () async {
    final r = await _engine().runGoal('g_ground([1, 2 | Xs?], R)');
    expect(r.status, ExecutionStatus.suspended);
  });

  test("'_output' prints a list of 50,000 elements, in order", () async {
    final e = _engine();
    final lines = <String>[];
    e.runtime.outputCallback = lines.add;
    final r = await e.runGoal('out_(50000)');
    expect(r.error, isNull);
    expect(lines, hasLength(1));
    final items = lines.single
        .substring(1, lines.single.length - 1)
        .split(', ');
    expect(items, hasLength(50000));
    expect(items.first, '50000');
    expect(items.last, '1');
  });
}
