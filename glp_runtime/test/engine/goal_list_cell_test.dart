/// The goal builder builds a list cell's head and tail as written.  GLP-Spec
/// appendix-lp.tex, Definition "Logic Programs Syntax": a term is a variable,
/// a constant or a compound term, and "[X|Xs] for a list cell" --- a compound
/// term, whose head and tail are terms like any other.
///
/// Until 2026-10-07 the engine's goal builder (glp_engine.dart,
/// `_buildListTerm` and `_buildListTermForConj`) built a tail that was neither
/// a list nor a variable as `ConstTerm(null)`, which the REPL displays as [],
/// so `X = [a | b].` posted `[a]`; and a `_` head threw "Unsupported list head
/// type".  Each case is run as a single goal (`_buildListTerm`) and as a
/// conjunct (`_buildListTermForConj`).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final _root = File('../programs/self.glp').absolute.path;

/// [t] with every bound variable followed: a constant as its value, a
/// structure as `functor(args)`, an unbound variable as `_`.
String _show(GlpEngine e, rt.Term? t) {
  if (t == null) return '<unbound>';
  final d = e.runtime.heap.dereference(t);
  if (d is rt.VarRef) return '_';
  if (d is rt.ConstTerm) return '${d.value}';
  if (d is rt.StructTerm) {
    return '${d.functor}(${d.args.map((a) => _show(e, a)).join(',')})';
  }
  return '$d';
}

/// Run [goal] on a fresh engine and return the binding of each of [vars].
Future<List<String>> _run(String goal, List<String> vars) async {
  final e = GlpEngine(rootSelfGlpPath: _root);
  final r = await e.runGoal(goal);
  expect(r.error, isNull, reason: goal);
  expect(r.status, ExecutionStatus.succeeded, reason: goal);
  return [for (final v in vars) _show(e, r.bindings[v])];
}

void main() {
  group('a list tail that is neither a list nor a variable is built', () {
    test('a constant: X = [a | b]', () async {
      expect(await _run('X = [a | b]', ['X']), ['.(a,b)']);
      expect(await _run('X = [a | b], Y = [c | d]', ['X', 'Y']),
          ['.(a,b)', '.(c,d)']);
    });

    test('a number: X = [a | 7]', () async {
      expect(await _run('X = [a | 7]', ['X']), ['.(a,7)']);
      expect(await _run('X = [a | 7], Y = z', ['X']), ['.(a,7)']);
    });

    test('a structure: X = [a, b | f(c)]', () async {
      expect(await _run('X = [a, b | f(c)]', ['X']), ['.(a,.(b,f(c)))']);
      expect(await _run('X = [a, b | f(c)], Y = z', ['X']),
          ['.(a,.(b,f(c)))']);
    });

    test('an anonymous variable: X = [a | _]', () async {
      expect(await _run('X = [a | _]', ['X']), ['.(a,_)']);
      expect(await _run('X = [a | _], Y = z', ['X']), ['.(a,_)']);
    });
  });

  group('an anonymous head is built, not refused', () {
    test('X = [_ | b]', () async {
      expect(await _run('X = [_ | b]', ['X']), ['.(_,b)']);
      expect(await _run('X = [_ | b], Y = z', ['X']), ['.(_,b)']);
    });

    test('X = [_, _ | Y?], the tail the goal variable Y', () async {
      expect(await _run('X = [_, _ | Y?], Y = c', ['X']), ['.(_,.(_,c))']);
    });
  });
}
