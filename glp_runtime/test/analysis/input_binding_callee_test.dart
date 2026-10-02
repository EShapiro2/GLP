/// A parameter the call binds to an input type and a parameter the callee's
/// clauses fix, in one instantiation.
///
/// Specification: TGLP, parameterized-types.tex, Definition "Instantiation"
/// (a map from parameters to types of the program, under which the caller's
/// clause and the callee's clauses are well-typed); typed-glp.tex, "Type
/// Declarations" (an input type is a type).  The merge of main's input
/// bindings (cd1bb142) with gap's callee-clause solving: the polarity the call
/// chooses for a bare parameter is the one the callee's clauses are probed
/// under, and a parameter the callee's clauses fix is probed at its output and
/// its input type.  Fixture: programs/tests/input_binding/callee_route.glp.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/input_binding/$name').absolute.path;

void main() {
  test('pass(C?, D) instantiates pass(X, M?) at X = Choice?, M = Choice?: '
      'the call fixes X, pass\'s clause fixes M at its input type, and the '
      'program loads and runs', () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadFile(_fixture('callee_route.glp'));
    final r = await engine.runGoal('go(yes, E)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['E'].toString(), contains('yes'));
  });
}
