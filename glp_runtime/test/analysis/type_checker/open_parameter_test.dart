// glp_runtime/test/analysis/type_checker/open_parameter_test.dart
//
// A parameter for which no type is supplied or fixed is left open where the
// callee is parametrically well-typed, the other parameters bound at the types
// tried for them; otherwise the call is refused.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call":
//
//   "... where types are supplied or fixed and none serves, the bindings
//    conflict and the call is refused.  A parameter for which no type is
//    supplied or fixed is left open where the callee is parametrically
//    well-typed (Section~\ref{sec:abstract-parameters}), the call checked with
//    it open and every argument typed at its position; otherwise the call is
//    refused."
//
// GLP #3 Cowork's ruling of 2026-10-02 15:46 UTC, item 1.  Fixtures:
// programs/tests/call_instantiation/open_bound*.glp, unsupplied_*.glp.  Until
// 2026-10-02 a call some parameter of which no site supplied was checked with
// every parameter open, each relation alone, so a call whose sites supply
// types for the other parameters that conflict loaded.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/call_instantiation/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

/// [term] as the REPL prints it, each variable dereferenced through [engine]'s
/// heap: a list as `[a, b]`.
String _show(GlpEngine engine, rt.Term? term) {
  if (term == null) return '_';
  final t = engine.runtime.heap.dereference(term);
  if (t is rt.ConstTerm) {
    if (t.value == null || t.value == 'nil') return '[]';
    return '${t.value}';
  }
  if (t is rt.StructTerm && t.functor == '.' && t.args.length == 2) {
    final rest = _show(engine, t.args[1]);
    final head = _show(engine, t.args[0]);
    if (rest == '[]') return '[$head]';
    return rest.startsWith('[') ? '[$head, ${rest.substring(1)}' : '[$head | $rest]';
  }
  if (t is rt.StructTerm) {
    return '${t.functor}(${t.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '_';
}

String _refusal(String name) {
  try {
    _engine().loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

void main() {
  test('a parameter no type is supplied or fixed for, beside one the sites '
      'supply: left open, the call checked at the supplied one, and run',
      () async {
    final engine = _engine()..loadFile(_fixture('open_bound.glp'));
    final r = await engine.runGoal('go(5, Out)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['Out']), '[5]');
  });

  test('the types the sites supply for the bound parameter conflict: refused, '
      'the open parameter notwithstanding', () {
    final err = _refusal('open_bound_conflict.glp');
    expect(err, contains('The bindings tried for the call '
        'pair_up("foo", N?, Out) conflict'));
    expect(err, contains('Y: Integer, String'));
    expect(err, contains('with X open'));
  });

  test('a callee that is not parametrically well-typed: refused', () {
    final err = _refusal('unsupplied_refused.glp');
    expect(err, contains('No instantiation of copy/2 is found for the call'));
    expect(err, contains('copy/2 is not parametrically well-typed'));
  });
}
