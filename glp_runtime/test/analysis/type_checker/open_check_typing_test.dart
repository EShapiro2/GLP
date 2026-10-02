// glp_runtime/test/analysis/type_checker/open_check_typing_test.dart
//
// A call checked with a parameter open has every argument typed at its
// position, a constant or a constructed argument too.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call": "A parameter for which no type is supplied or
// fixed is left open where the callee is parametrically well-typed
// (Section~\ref{sec:abstract-parameters}), the call checked with it open and
// every argument typed at its position"; well-typing.tex, Definition
// "Well-Typed Clause", condition 2: "For each unit goal A in B, the produced
// moded term A' corresponding to A is well-typed by D".  GLP #3 Cowork's ruling
// of 2026-10-02 15:46 UTC, item 1, on Integration's finding of 14:48 UTC, item
// 4.  Fixtures: programs/tests/call_instantiation/open_typed*.glp.  Until
// 2026-10-02 the open check compared the variables that are arguments alone,
// and a constant or constructed argument went unchecked: `p(foo, _)` loaded
// and ran with `foo` where `Integer?` is read.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/call_instantiation/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

String _refusal(String name) {
  try {
    _engine().loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

void main() {
  test('a constant and a constructed argument of the types read, X open: '
      'loads and runs', () async {
    final engine = _engine()..loadFile(_fixture('open_typed.glp'));
    final r = await engine.runGoal('ok');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    final p = await engine.runGoal('ok_pair(3)');
    expect(p.status, ExecutionStatus.succeeded, reason: '${p.error}');
  });

  test('a constant where an Integer is read, X open: refused', () {
    final err = _refusal('open_typed_constant.glp');
    expect(err, contains('No instantiation of q/2 for the call q("foo", _)'));
    expect(err, contains('argument 1, "foo", has no well-typing at any '
        'expansion of Integer?'));
  });

  test('a variable inside a constructed argument, of a type the position '
      'does not accept, X open: refused', () {
    final err = _refusal('open_typed_inside.glp');
    expect(err,
        contains('No instantiation of r/2 for the call r(p(S?, 1), _)'));
    expect(err, contains('argument 1, p(S?, 1), at S? holds String, which no '
        'expansion of Pair? accepts'));
  });
}
