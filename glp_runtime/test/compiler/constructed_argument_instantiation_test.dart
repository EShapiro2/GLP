/// A call whose argument at a parameter is a constructed term, which names no
/// type.
///
/// TGLP 8a58729, appendix-implementation-notes.tex, "The instantiation of a
/// call": "the checker reads the sites of a call over the whole clause, ... and
/// tries for each parameter the types those sites supply, taking one under
/// which every site is well-typed with subtyping.  A type no site supplies is
/// not tried, so a call whose instantiations all lie strictly between the types
/// its sites supply is refused, and a site is to name the type.  A call for
/// which no instantiation is found is refused unless its callee is
/// parametrically well-typed (Section~\ref{sec:abstract-parameters}), in which
/// case the call is checked with the callee's parameters open."
///
/// A constructed term constrains a parameter by containment and supplies no
/// type for it.  In each program below a parameter is reached by constructed
/// terms alone and the callee inspects a parameter, so it is not parametrically
/// well-typed: the call is refused, naming the parameter no site supplies.  A
/// constructed argument the binding its sites supply does not admit is in
/// test/analysis/type_checker/call_instantiation_test.dart.
///
/// From 2026-09-20 to 2026-10-02 the checker tried types no site supplies: a
/// parameter the call left open was fixed by the callee's own head pair (TGLP
/// e56c303), or built from the constructors the callee's heads match, coverage
/// selecting it (TGLP dddf684), and the first, second and fourth programs
/// loaded.  Before 2026-09-20 only a variable argument bound a parameter, and
/// they were rejected as carrying a parameter-inspecting procedure nothing
/// instantiates.
library;

import 'dart:io';
import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

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
                '$call: no site of the call supplies a type for $param') &&
            s.contains('$callee is not parametrically well-typed, so the call '
                'is refused');
      }, 'refuses the call, naming the parameter no site supplies'));

  test('a constructed argument at a parameter only the callee\'s clauses '
      'relate to a supplied one is refused', () {
    final dir =
        Directory('../programs/tests/param_constructed_arg').absolute.path;
    expect(
        () => engine.loadProgram(dir),
        refusal('send_user/3',
            'send_user(msg("agent", "person", bye(N?)), Outs?, Outs1)', 'M'));
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('a term inside a term at a parameter position is refused', () {
    final dir =
        Directory('../programs/tests/param_constructed_nested').absolute.path;
    expect(() => engine.loadProgram(dir),
        refusal('send_user/3', 'send_user(w(i(N?)), Outs?, Outs1)', 'M'));
    expect(engine.loadedPrograms.containsKey('__program__'), isFalse);
  });

  test('two constructed arguments at one parameter are refused', () {
    final dir =
        Directory('../programs/tests/param_constructed_conflict').absolute.path;
    expect(() => engine.loadProgram(dir),
        refusal('send2/4', 'send2(m(N?), bad(N?), Outs?, Outs1)', 'M'));
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
