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

  test('a reader at a bare parameter binds it to the input type: the call is '
      'no longer refused, and the callee\'s clauses are checked at the '
      'binding, where echo(X?, X) is not well-typed', () {
    final err = _refusal('echo_in.glp');
    expect(err, isNotEmpty, reason: 'echo_in.glp loaded');
    // The refusal is echo's head at echo(Choice?, Choice), which Lemma
    // "Parametricity" does not certify, the binding not being of the
    // parameter's mode ...
    expect(err, contains('Head of echo is not well-typed'));
    // ... and not the call, which X = Choice? makes well-typed.
    expect(err, isNot(contains('Body atom 0 (echo)')));
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
