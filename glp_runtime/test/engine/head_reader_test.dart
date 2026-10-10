/// A head reader against a goal subterm.
///
/// GLP-Spec appendix-term-matching.tex (5971d5b), Definition "Term Matching",
/// column "Reader X2?": a goal writer is assigned the head's reader
/// (X1 := X2?); a goal reader fails; a goal term fails.  glp.tex Definition
/// "Writer MGU" agrees: the mgu is a writers substitution, which leaves a
/// reader as it is, so for q(X, X?) against q(1, 2) the only candidate,
/// {X := 1}, leaves q(1, X?), which nothing makes equal to q(1, 2).  A
/// constant the goal passes arrives as a reader of a bound writer and stands
/// for its value.
///
/// q/2 and t/2 reach the head reader at an argument (get_value), n/2 inside a
/// structure (unify_variable).  The second group runs n/2 again with each of
/// its compiled reader unify_variable instructions replaced by head_variable,
/// the instruction a foreign artefact may carry for the same position.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as bc;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _source = '''
procedure q(Integer?, Integer).
q(X, X?).

T ::= f(Integer).
procedure t(T?, T).
t(X, X?).

procedure n(T?, T).
n(f(X), f(X?)).
''';

GlpEngine _fresh({bool headVariable = false}) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source), isTrue);
  if (headVariable) {
    final ops = engine.loadedPrograms['_source_']!.ops;
    var replaced = 0;
    for (var i = 0; i < ops.length; i++) {
      final op = ops[i];
      if (op is bc.UnifyVariable && op.isReader) {
        ops[i] = bc.HeadVariable(op.varIndex, isReader: true);
        replaced++;
      }
    }
    expect(replaced, greaterThan(0),
        reason: 'n/2 has a reader inside a head structure');
  }
  return engine;
}

/// The value [t] is bound to, written out: constants by their value, a
/// structure by its functor and arguments.
String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return '${d.value}';
  if (d is StructTerm) {
    return '${d.functor}(${d.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '$d';
}

void main() {
  group('a head reader (appendix-term-matching.tex, column Reader X2?)', () {
    test('q(1, 2) fails: a constant at the head reader is "fail"', () async {
      final result = await _fresh().runGoal('q(1, 2)');
      expect(result.status, ExecutionStatus.failed,
          reason: 'X := 1 leaves the head reader X? as it is');
    });

    test('q(1, Y) gives Y = 1: a goal writer is assigned the head reader',
        () async {
      final engine = _fresh();
      final result = await engine.runGoal('q(1, Y)');
      expect(result.status, ExecutionStatus.succeeded);
      expect(_show(engine, result.bindings['Y']), '1');
    });

    test('t(f(1), U) gives U = f(1): the same for a compound term', () async {
      final engine = _fresh();
      final result = await engine.runGoal('t(f(1), U)');
      expect(result.status, ExecutionStatus.succeeded);
      expect(_show(engine, result.bindings['U']), 'f(1)');
    });

    test('n(f(1), f(2)) fails: a constant at a nested head reader', () async {
      final result = await _fresh().runGoal('n(f(1), f(2))');
      expect(result.status, ExecutionStatus.failed);
    });

    test('n(f(1), f(V)) gives V = 1: a nested goal writer is assigned it',
        () async {
      final engine = _fresh();
      final result = await engine.runGoal('n(f(1), f(V))');
      expect(result.status, ExecutionStatus.succeeded);
      expect(_show(engine, result.bindings['V']), '1');
    });
  });

  group('the same nested head reader as head_variable', () {
    test('n(f(1), f(2)) fails', () async {
      final result = await _fresh(headVariable: true).runGoal('n(f(1), f(2))');
      expect(result.status, ExecutionStatus.failed);
    });

    test('n(f(1), f(V)) gives V = 1', () async {
      final engine = _fresh(headVariable: true);
      final result = await engine.runGoal('n(f(1), f(V))');
      expect(result.status, ExecutionStatus.succeeded);
      expect(_show(engine, result.bindings['V']), '1');
    });
  });
}
