/// The byte runner passes over a clause whose head matches a goal argument
/// against a structure the argument is bound and is not ([ByteRunner]'s clause
/// index): the clause would fail at that instruction, whatever else its head
/// holds, and a failed clause leaves nothing behind (GLP-Spec
/// appendix-term-matching.tex, Definition "Term Matching": a mismatch fails;
/// IGLP Implementation Notes, "Clause try").  Until 2026-10-02 every clause
/// was tried, and a goal of `:=/2`, whose some forty clauses each match its
/// second argument against an operator, tried them all.
///
/// These tests hold the index to what trying every clause gives: a goal bound
/// at the indexed argument reduces by the clause that matches it, an unbound
/// one is matched by every clause and suspends or is assigned as before, a
/// constant there fails every structure clause, a clause that suspended keeps
/// `otherwise` from holding, and `:=/2` computes each operator.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// op/3 has eight clauses, seven matching its second argument against a
/// structure and the last a catch-all; sel/2 has an `otherwise` clause after
/// structure clauses, one of which waits on its argument's subterm.
const String _source = r'''
Shape ::= a(Integer) ; b(Integer) ; c(Integer) ; d(Integer) ; e(Integer) ;
  f(Integer) ; g(Integer, Integer) ; h.

procedure op(Integer?, Shape?, Integer).
op(K, a(X), Y?) :- Y := K? + X?.
op(K, b(X), Y?) :- Y := K? - X?.
op(K, c(X), Y?) :- Y := K? * X?.
op(_, d(X), X?).
op(_, e(_), 5).
op(_, f(_), 6).
op(_, g(X, Z), Y?) :- Y := X? + Z?.
op(_, _, 0).

procedure sel(Shape?, Integer).
sel(a(1), 1).
sel(b(X), 2) :- X? > 0 | true.
sel(c(_), 3).
sel(d(_), 4).
sel(e(_), 5).
sel(f(_), 6).
sel(_, 7) :- otherwise | true.
''';

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root)..loadSource(_source);

void main() {
  test('a goal bound at the indexed argument reduces by its own clause',
      () async {
    final engine = _engine();
    final cases = {
      'op(10, a(3), Y)': 'Const(13)',
      'op(10, b(3), Y)': 'Const(7)',
      'op(10, c(3), Y)': 'Const(30)',
      'op(10, d(3), Y)': 'Const(3)',
      'op(10, e(3), Y)': 'Const(5)',
      'op(10, f(3), Y)': 'Const(6)',
      'op(10, g(3, 4), Y)': 'Const(7)',
      'op(10, h, Y)': 'Const(0)',
    };
    for (final MapEntry(key: goal, value: want) in cases.entries) {
      final r = await engine.runGoal(goal);
      expect(r.status, ExecutionStatus.succeeded, reason: goal);
      expect('${r.bindings['Y']}', want, reason: goal);
    }
  });

  test('an unbound reader at the indexed argument is met by every clause: the '
      'structure clauses suspend on it, and a clause matching anything there '
      'reduces, or the goal suspends', () async {
    final engine = _engine();
    // op/3's last clause, op(_, _, 0), is applicable whatever the argument.
    final r = await engine.runGoal('op(10, S?, Y)');
    expect(r.status, ExecutionStatus.succeeded);
    expect('${r.bindings['Y']}', 'Const(0)');
    // sel/2's last clause waits on otherwise, which the others' suspension
    // keeps from holding.
    final s = await engine.runGoal('sel(S?, R)');
    expect(s.status, ExecutionStatus.suspended);
  });

  test('a clause that suspended keeps otherwise from holding, a clause passed '
      'over does not', () async {
    final engine = _engine();
    // b(X) with X unbound suspends; otherwise does not hold after it.
    final w = await engine.runGoal('sel(b(X?), R)');
    expect(w.status, ExecutionStatus.suspended);
    // f(1): every clause before it but the last is passed over or fails.
    final f = await engine.runGoal('sel(f(1), R)');
    expect('${f.bindings['R']}', 'Const(6)');
    // h matches no structure clause: otherwise holds.
    final h = await engine.runGoal('sel(h, R)');
    expect('${h.bindings['R']}', 'Const(7)');
    // a(2) fails a(1), and the rest are passed over to otherwise.
    final a = await engine.runGoal('sel(a(2), R)');
    expect('${a.bindings['R']}', 'Const(7)');
  });

  test(':=/2 computes each of its operators', () async {
    final engine = _engine();
    final cases = {
      'X := 7 + 5': 12,
      'X := 7 - 5': 2,
      'X := 7 * 5': 35,
      'X := 7 // 5': 1,
      'X := 7 mod 5': 2,
      'X := abs(0 - 7)': 7,
      'X := (1 + 2) * (3 + 4)': 21,
    };
    for (final MapEntry(key: goal, value: want) in cases.entries) {
      final r = await engine.runGoal(goal);
      expect(r.status, ExecutionStatus.succeeded, reason: goal);
      expect('${r.bindings['X']}', 'Const($want)', reason: goal);
    }
  });
}
