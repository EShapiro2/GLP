/// A goal posted to a loaded unit is checked against the procedures the load
/// certified: its callee's parametricity is decided on the callee's clauses as
/// the partial evaluator unfolds their defined guards, "at compile time,
/// before type checking" (TGLP appendix-root-self.tex), as the load decides
/// it.  One procedure, one verdict.
///
/// TGLP parameterized-types.tex, Definition "Parametrically Well-Typed" and
/// Lemma "Parametricity"; "Where a program contains a parameterised procedure
/// that no call in it instantiates and that is not parametrically well-typed,
/// compilation rejects the program"; appendix-implementation-notes.tex, "The
/// instantiation of a call": a parameter for which no type is supplied or
/// fixed "is left open where the callee is parametrically well-typed ...;
/// otherwise the call is refused".
///
/// Until 2026-10-12 the goal's scope held the unit's clauses as written, so
/// `p([a,b], O)` posted to goal_cert/unfolded_guard.glp, whose p/2 the load
/// certified, was refused as "p/2 is not parametrically well-typed".
///
/// Fixtures: programs/tests/goal_cert/, and the book's
/// streams/buffered_communication/switch2x2.glp.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;
const _dir = '../programs/tests/goal_cert';

/// [t] as a list of constants, or null where it is not a closed list of them.
List<String>? _list(GlpEngine engine, rt.Term? t) {
  final out = <String>[];
  var d = t == null ? null : engine.runtime.heap.dereference(t);
  while (d is rt.StructTerm && d.functor == '.' && d.args.length == 2) {
    final h = engine.runtime.heap.dereference(d.args[0]);
    if (h is! rt.ConstTerm) return null;
    out.add('${h.value}');
    d = engine.runtime.heap.dereference(d.args[1]);
  }
  if (d is rt.ConstTerm && d.value == rt.nil) return out;
  return null;
}

void main() {
  group('a goal is checked against the procedures as the load certified them',
      () {
    test(
        'unfolded_guard.glp: p/2, parametrically well-typed as unfolded, '
        'admits p([a,b], O), which runs, O = [a,b]', () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(engine.loadFile('$_dir/unfolded_guard.glp'), isTrue);
      final r = await engine.runGoal('p([a,b], O)');
      expect(r.error, isNull);
      expect(r.status, ExecutionStatus.succeeded);
      expect(_list(engine, r.bindings['O']), ['a', 'b']);
    });

    test(
        'inspect_neg.glp: pskip/1, inspecting its parameter once unfolded '
        'and instantiated by no call, is refused at load', () {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(
          () => engine.loadFile('$_dir/inspect_neg.glp'),
          throwsA(predicate((e) =>
              e.toString().contains('pskip/1') &&
              e.toString().contains('not parametrically well-typed'))));
    });

    test(
        'inspect_instantiated.glp: pskip/1 loads, instantiated by run/1, and '
        'a goal posted to it supplying no type for its parameter is refused; '
        'run([done]) runs', () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(engine.loadFile('$_dir/inspect_instantiated.glp'), isTrue);
      final refused = await engine.runGoal('pskip([done])');
      expect(refused.error, isNotNull);
      expect(refused.error,
          contains('pskip/1 is not parametrically well-typed'));
      final ran = await engine.runGoal('run([done])');
      expect(ran.error, isNull);
      expect(ran.status, ExecutionStatus.succeeded);
    });

    test(
        "the book's switch2x2.glp: switch2x2/4, matching its inputs through "
        'defined guards, admits a goal, which routes a, b and c', () async {
      final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
      expect(
          engine.loadFile(
              '../programs/book/streams/buffered_communication/switch2x2.glp'),
          isTrue);
      final r = await engine.runGoal(
          'switch2x2([a,b|I1?], [c|I2?], [A1, A2, A3|T1?], [B1|T2?])');
      expect(r.error, isNull);
      expect(r.status, ExecutionStatus.suspended);
      final routed = <String>[
        for (final v in ['A1', 'A2', 'A3', 'B1'])
          if (r.bindings[v] case final rt.ConstTerm c) '${c.value}',
      ]..sort();
      expect(routed, ['a', 'b', 'c']);
    });
  });
}
