// glp_runtime/test/analysis/type_checker/goal_instantiation_test.dart
//
// A posted goal's calls are read over the whole goal, as a clause's are.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call": "the checker reads the sites of a call over
// the whole clause, the body goals in no order, and a posted goal, checked as
// a body (Section~\ref{sec:runtime-boundary}), the same way"; modules.tex,
// Section "Type-Compatible Attestation Between Agents": the initial goal "is
// type-checked before execution as a body goal".  GLP #3 Cowork's ruling of
// 2026-10-02 15:46 UTC, item 1.  Fixture:
// programs/tests/call_instantiation/goal_order.glp.  Until 2026-10-02 a goal's
// calls were instantiated in order, each from the occurrences typed before it,
// the callee's clauses never asked and every callee taken to be
// parametrically well-typed.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
final _fixture =
    File('../programs/tests/call_instantiation/goal_order.glp').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root)..loadFile(_fixture);

/// [term] as the REPL prints it, each variable dereferenced through [engine]'s
/// heap, a list as `[a | rest]`.
String _show(GlpEngine engine, rt.Term? term) {
  if (term == null) return '_';
  final t = engine.runtime.heap.dereference(term);
  if (t is rt.ConstTerm) {
    if (t.value == null || t.value == 'nil') return '[]';
    return '${t.value}';
  }
  if (t is rt.StructTerm && t.functor == '.' && t.args.length == 2) {
    return '[${_show(engine, t.args[0])} | ${_show(engine, t.args[1])}]';
  }
  if (t is rt.StructTerm) {
    return '${t.functor}(${t.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '_';
}

void main() {
  test('a stream of a subtype merged into a stream of its supertype, the '
      'subtype stream typed by the goal before the merge', () async {
    final engine = _engine();
    final r = await engine
        .runGoal('friends(F), merge(F?, A?, C), net(A), drain(C?, D)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    final d = _show(engine, r.bindings['D']);
    expect(d, contains('ping'));
    expect(d, contains('pending_link'));
  });

  test('the same goals in the other order', () async {
    final r = await _engine()
        .runGoal('net(A), drain(C?, D), merge(F?, A?, C), friends(F)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
  });

  test('a call no site supplies a type for whose callee is not parametrically '
      'well-typed: refused', () async {
    final r = await _engine().runGoal('copy([msg(a, b)], Ys)');
    expect(r.status, ExecutionStatus.failed);
    expect(r.error, contains('Goal is not well-typed'));
    expect(r.error, contains('No instantiation of copy/2 is found for the call'));
    expect(r.error, contains('copy/2 is not parametrically well-typed'));
  });

  test('a procedure the loaded module declares monomorphic, of a name the '
      'root declares parameterised: the goal is read by the module\'s '
      'declaration', () async {
    // book's merge_ordered.glp declares merge(NumList?, NumList?, NumList),
    // which shadows the root's procedure(X) merge(Stream(X)?, Stream(X)?,
    // Stream(X)) (modules.tex, "Scope construction"); until 2026-10-02 the
    // goal-check scope kept the root's template beside it, and this goal was
    // read as a call to the template and refused.
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadFile(File('../programs/book/recursive/list_processing/'
              'merge_ordered.glp')
          .absolute
          .path);
    final r = await engine.runGoal('merge([1,3,5], [2,4,6], Zop)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['Zop']),
        '[1 | [2 | [3 | [4 | [5 | [6 | []]]]]]]');
  });

  test('the same callee instantiated by the goal\'s sites, the consumer '
      'after it: runs', () async {
    final r = await _engine().runGoal('copy(Xs?, Ys), src(Xs), sink(Ys?)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
  });
}
