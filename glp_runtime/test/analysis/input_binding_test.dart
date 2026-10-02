/// A type parameter binds to an input type where the argument is a reader.
///
/// Specification: TGLP, parameterized-types.tex, Definition "Instantiation"
/// (a map from parameters to types of the program, under which the caller's
/// clause and the callee's clauses are well-typed and every input path is
/// accepted); typed-glp.tex, "Type Declarations" (an input type is a type);
/// appendix-type-automaton.tex, Definition "Dual Type Automaton" (complement-
/// ation is an involution, (T?)? = T); Lemma "Parametricity" (sigma replaces
/// a parameter by a type of the same mode).  GLP's task of 2026-09-27 19:48 UTC.
/// Fixtures: programs/tests/input_binding/.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/input_binding/$name').absolute.path;

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
  test('a writer at a bare parameter binds it to the output type, as before',
      () async {
    final engine = _engine()..loadFile(_fixture('echo_out.glp'));
    final r = await engine.runGoal('go(D, no)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['D'].toString(), contains('no'));
  });

  test('a reader at a bare parameter binds it to the input type, and the '
      'callee\'s clauses are checked at the binding, where echo(X?, X) is not '
      'well-typed: the call is refused, and echo\'s head named', () {
    final err = _refusal('echo_in.glp');
    expect(err, isNotEmpty, reason: 'echo_in.glp loaded');
    // X = Choice? makes the call's own sites well-typed and not echo's
    // clauses, which Lemma "Parametricity" does not certify at it, the binding
    // not being of the parameter's mode, so no binding serves and the call is
    // refused (TGLP appendix-implementation-notes.tex, "The instantiation of a
    // call", cc4a891: the checker "takes one under which the clause and the
    // callee's clauses are well-typed with subtyping") ...
    expect(err,
        contains('The bindings tried for the call echo(C?, D) conflict'));
    expect(err,
        contains('under X = Choice? the clauses of echo/2 are not well-typed'));
    // ... and echo's head is named at echo(Choice?, Choice).
    expect(err, contains('Head of echo is not well-typed'));
  });

  test('a template instantiated at an input type complements by the '
      'involution: Swap(Choice?) with Swap(X) ::= sw(X?) is sw(Choice)',
      () async {
    final engine = _engine()..loadFile(_fixture('involution.glp'));
    final r = await engine.runGoal('f(S)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['S'].toString(), contains('sw'));
  });
}
