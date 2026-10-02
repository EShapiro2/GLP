// glp_runtime/test/analysis/type_checker/slash_functor_test.dart
//
// A term whose functor contains '/' --- `/` itself, as in a rate `1/week`, or
// `//` --- is typed by its functor and arity like any other.  The checker
// names a path step `<functor>/<arity>`, so `/` is "//2" and `//` is "///2";
// until 2026-10-02 `_buildTransitionLabel` (well_typed_term.dart) split that
// name at every '/', got three parts, and took the step for a leaf, refusing
// the head `delay(N/U, K, D?)` with "No transition for //2 from state Rate?".
// Found by sGLP's monitor (programs/sglp/monitor.glp:81) on gap 7db9c0a5.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

void main() {
  test('a head matching a term of functor / loads and runs', () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
TimeUnit ::= second ; minute ; hour.
Rate ::= /(Number, TimeUnit).
procedure unit_of(Rate?, TimeUnit).
unit_of(_/U, U?).
''', filename: 'slash_head.glp'),
        isTrue);
    final r = await engine.runGoal('unit_of(1/second, U)');
    expect(r.succeeded, isTrue, reason: '${r.error}');
    expect('${r.bindings['U']}', contains('second'));
  });

  test('a body argument of functor / is typed against its declared type',
      () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
TimeUnit ::= second ; minute ; hour.
Rate ::= /(Number, TimeUnit).
procedure unit_of(Rate?, TimeUnit).
unit_of(_/U, U?).
procedure go(TimeUnit).
go(U?) :- unit_of(3/hour, U).
''', filename: 'slash_body.glp'),
        isTrue);
    final r = await engine.runGoal('go(U)');
    expect(r.succeeded, isTrue, reason: '${r.error}');
    expect('${r.bindings['U']}', contains('hour'));
  });

  test('a term of functor / that the declared type does not admit is refused',
      () {
    final engine = _engine();
    expect(
        () => engine.loadSource('''
TimeUnit ::= second ; minute ; hour.
Rate ::= /(Number, TimeUnit).
procedure unit_of(Rate?, TimeUnit).
unit_of(_/U, U?).
procedure go(TimeUnit).
go(U?) :- unit_of(3/fortnight, U).
''', filename: 'slash_bad.glp'),
        throwsA(anything));
  });
}
