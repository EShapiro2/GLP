// glp_runtime/test/analysis/type_checker/head_copy_subtyping_test.dart
//
// A variable copied within a head.
//
// Specification: TGLP b927b41, well-typing.tex, Definition "Well-Typed Clause
// with Subtyping", clause (a): "if both occur in the head, the dual of the
// type of X is a subtype of the type of X?" --- the input's type within the
// output's, where it was duality.  GLP, 2026-10-15 11:00 UTC, item 2.
// Fixtures: programs/tests/head_copy/.
//
// Until 2026-10-15 a head pair had to be exactly dual, and a head could not
// place an Ask taken from its input into an output stream of a union
// containing Ask; the write had to go to the body (Code #8, 2026-10-14 23:54
// UTC, point 2).
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/head_copy/$name').absolute.path;

void main() {
  test('an Ask from the input placed in an output stream of a union '
      'containing Ask loads and runs', () async {
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadFile(_fixture('subtype_copy.glp'));
    final r = await engine.runGoal('relay([ask(1, "a")], E)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    final heap = engine.runtime.heap;
    final list = heap.dereference(r.bindings['E']!) as rt.StructTerm;
    final first = heap.dereference(list.args[0]) as rt.StructTerm;
    expect(first.functor, 'ask');
  });

  test('a union element from the input placed in an Ask position is refused',
      () {
    var err = '';
    try {
      GlpEngine(rootSelfGlpPath: _root)
          .loadFile(_fixture('subtype_copy_refused.glp'));
    } catch (e) {
      err = e.toString();
    }
    expect(err, contains('Variable pair (A, A?)'));
    expect(err, contains('Event<String,Integer> is not a subtype of '
        'Ask<String>'));
  });
}
