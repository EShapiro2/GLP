/// `otherwise` succeeds if all previous clauses for this procedure fail
/// (GLP-Spec appendix-guards.tex, 5971d5b).  A clause that suspends has not
/// failed, so `otherwise` waits while any earlier clause suspends, and fires
/// once every one of them has failed.
///
/// The compiler emits 0x46 for a bare `otherwise`; a generic guard call of
/// otherwise/0 (hand-assembled bytecode, a foreign artefact) takes the same
/// rule, and the second group runs the same goals with every 0x46 of the
/// loaded program replaced by that generic call.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

const _source = '''
procedure go(Constant).
go(R?) :- w(X?, R), later(X).

procedure go_fail(Constant).
go_fail(R?) :- w(X?, R), later_b(X).

procedure w(Constant?, Constant).
w(a, one).
w(_, other) :- otherwise | true.

procedure later(Constant).
later(a).

procedure later_b(Constant).
later_b(b).
''';

GlpEngine _fresh({bool generic = false}) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source), isTrue);
  if (generic) {
    final ops = engine.loadedPrograms['_source_']!.ops;
    var replaced = 0;
    for (var i = 0; i < ops.length; i++) {
      if (ops[i] is Otherwise) {
        ops[i] = Guard('otherwise', 0);
        replaced++;
      }
    }
    expect(replaced, greaterThan(0),
        reason: 'w/2 has an otherwise clause, and its 0x46 is replaced');
  }
  return engine;
}

void _outcomes(bool generic) {
  test('go(R): w/2 waits on X? and does not take otherwise; R = one',
      () async {
    final result = await _fresh(generic: generic).runGoal('go(R)');
    expect(result.status, ExecutionStatus.succeeded);
    expect(result.bindings['R'].toString(), 'Const(one)');
  });

  test('otherwise fires where every earlier clause fails', () async {
    final result = await _fresh(generic: generic).runGoal('w(b, R)');
    expect(result.status, ExecutionStatus.succeeded);
    expect(result.bindings['R'].toString(), 'Const(other)');
  });

  test('an earlier clause that commits keeps otherwise from firing',
      () async {
    final result = await _fresh(generic: generic).runGoal('w(a, R)');
    expect(result.status, ExecutionStatus.succeeded);
    expect(result.bindings['R'].toString(), 'Const(one)');
  });

  test('otherwise does not fire while an earlier clause suspends', () async {
    final result = await _fresh(generic: generic).runGoal('w(X?, R)');
    expect(result.status, ExecutionStatus.suspended);
    expect(result.bindings['R'], isNull,
        reason: 'w(a, one) suspends on X?, so otherwise waits with it');
  });

  test('and fires once that clause fails', () async {
    final result = await _fresh(generic: generic).runGoal('go_fail(R)');
    expect(result.status, ExecutionStatus.succeeded);
    expect(result.bindings['R'].toString(), 'Const(other)',
        reason: 'X becomes b, w(a, one) fails, and otherwise fires');
  });
}

void main() {
  group('otherwise compiled to 0x46', () => _outcomes(false));
  group('otherwise as a generic guard call takes the same rule',
      () => _outcomes(true));
}
