/// A call whose argument at a parameter is a constructed term, which names no
/// type.
///
/// TGLP cc4a891, appendix-implementation-notes.tex, "The instantiation of a
/// call": "For each parameter it tries the types the sites supply and the types
/// the callee's clauses fix for it---a head occurrence of the parameter paired
/// by condition~3 with a body occurrence of a concrete type---and takes one
/// under which the clause and the callee's clauses are well-typed with
/// subtyping; where types are supplied or fixed and none serves, the bindings
/// conflict and the call is refused.  A parameter for which no type is supplied
/// or fixed is left open where the callee is parametrically well-typed
/// (Section~\ref{sec:abstract-parameters}), the call checked with it open and
/// every argument typed at its position; otherwise the call is refused."
///
/// A constructed term constrains a parameter by containment and supplies no
/// type for it.  In the first three programs the callee's clauses fix the
/// parameter the constructed term stands at, by a head pair: the first two
/// load and run, and in the third the type fixed does not admit the term and
/// the call is refused.  In the last two nothing fixes the parameter --- the
/// callee's heads match a constructor there, which is no occurrence of the
/// parameter --- and the callee inspects it, so it is not parametrically
/// well-typed: the call is refused, naming the parameter.  A constructed
/// argument the binding its sites supply does not admit is in
/// test/analysis/type_checker/call_instantiation_test.dart.
///
/// From 2026-09-20 to 2026-10-02 the checker read a parameter the call left
/// open off the callee's own head pair (TGLP e56c303), or built it from the
/// constructors the callee's heads match, coverage selecting it (TGLP
/// dddf684), and the first, second and fourth programs loaded.  On 2026-10-02
/// (TGLP 8a58729) a type no site supplied was not tried, and all five were
/// refused.  Before 2026-09-20 only a variable argument bound a parameter, and
/// they were rejected as carrying a parameter-inspecting procedure nothing
/// instantiates.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

/// [term] as the REPL prints it, each variable dereferenced through [engine]'s
/// heap, an unbound one as `_`.
String _show(GlpEngine engine, rt.Term? term) {
  if (term == null) return '_';
  final t = engine.runtime.heap.dereference(term);
  if (t is rt.ConstTerm) {
    if (t.value == null || t.value == rt.nil) return '[]';
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
  late GlpEngine engine;

  setUp(() {
    engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  });

  Matcher refusal(String callee, String call, String param) => throwsA(
          predicate((e) {
        final s = e.toString();
        return s.contains('No instantiation of $callee is found for the call '
                '$call: no site of the call supplies a type for $param and '
                'the clauses of $callee fix none') &&
            s.contains('$callee is not parametrically well-typed, so the call '
                'is refused');
      }, 'refuses the call, naming the parameter no type is supplied or fixed '
          'for'));

  test('a constructed argument at a parameter the callee\'s head pair fixes '
      'loads and runs', () async {
    final dir =
        Directory('../programs/tests/param_constructed_arg').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
    final r = await engine.runGoal('run(x, [out(S)], O)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['S']),
        startsWith('[msg(agent, person, bye(x)) | '));
  });

  test('a term inside a term at a parameter position loads and runs', () async {
    final dir =
        Directory('../programs/tests/param_constructed_nested').absolute.path;
    expect(engine.loadProgram(dir), isTrue);
    final r = await engine.runGoal('run(x, [out(S)], O)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['S']), startsWith('[w(i(x)) | '));
  });

  test('two constructed arguments at one parameter, the type the callee\'s '
      'clauses fix admitting one of them: refused, naming the other', () {
    final dir =
        Directory('../programs/tests/param_constructed_conflict').absolute.path;
    expect(
        () => engine.loadProgram(dir),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('The bindings tried for the call '
                  'send2(m(N?), bad(N?), Outs?, Outs1) conflict') &&
              s.contains('No transition for bad');
        }, 'refuses the call for bad(N?)')));
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('a parameter the callee inspects, reached by a constructed argument '
      'alone, is refused', () {
    final dir =
        Directory('../programs/tests/param_theta_covered').absolute.path;
    expect(() => engine.loadProgram(dir),
        refusal('pick/3', 'pick(a("one"), Xs?, N)', 'R'));
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('and so is the same call where the callee\'s clauses cover less', () {
    final dir =
        Directory('../programs/tests/param_theta_uncovered').absolute.path;
    expect(() => engine.loadProgram(dir),
        refusal('pick/3', 'pick(a("one"), Xs?, N)', 'R'));
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });
}
