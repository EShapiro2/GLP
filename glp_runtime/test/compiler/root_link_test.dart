// glp_runtime/test/compiler/root_link_test.dart
//
// The root self.glp is the first link of every module's chain, d_1 (GLP's
// weeding round six, item 1; GLP #3 Cowork, 2026-10-03 21:18 UTC, "16:11").
//
// Specification: TGLP modules.tex, Definition "Root, Scope" and "The root
// self.glp is in the scope of every module compiled on the device, wherever
// that module's program sits below the root: it is d_1"; Compilation: the
// first step collects "the self.glp of each directory from the root down to
// the program", the second checks "each module ... against its scope", the
// third renames "every procedure p/n and every type T in every .glp file
// (including self.glp files) ... to M:p/n and M:T, where M is the module's path
// from the root", and the fourth resolves "a call matching a procedure in an
// ancestor self.glp ... to its renamed form by walking the scope".
//
// The two cases measured on 2026-10-03 (Integration, round six): a type error
// in the root self.glp loaded and ran where the same clause in an application
// module was refused; and an application module defining a root helper's
// name, mwm1/4, took over the root's mwm/2, which gave O = [].
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;

/// [term] as the REPL prints it, each variable dereferenced through [engine]'s
/// heap.
String _show(GlpEngine engine, rt.Term? term) {
  if (term == null) return '_';
  final t = engine.runtime.heap.dereference(term);
  if (t is rt.ConstTerm) {
    if (t.value == null || t.value == 'nil') return '[]';
    return '${t.value}';
  }
  if (t is rt.StructTerm && t.functor == '.' && t.args.length == 2) {
    final items = <String>[];
    rt.Term cur = t;
    while (true) {
      final c = engine.runtime.heap.dereference(cur);
      if (c is rt.StructTerm && c.functor == '.' && c.args.length == 2) {
        items.add(_show(engine, c.args[0]));
        cur = c.args[1];
        continue;
      }
      if (c is rt.ConstTerm && (c.value == null || c.value == 'nil')) break;
      items.add('| ${_show(engine, c)}');
      break;
    }
    return '[${items.join(', ')}]';
  }
  if (t is rt.StructTerm) {
    return '${t.functor}(${t.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '_';
}

void main() {
  group('a module defining a root helper\'s name does not take it over', () {
    // programs/tests/root_link/hijack_mwm.glp defines mwm1/4, the helper the
    // root's mwm/2 reaches through mwm_main/2.  Its own mwm1/4 is its own; the
    // root's mwm/2 reaches the root's.
    final fixture =
        File('../programs/tests/root_link/hijack_mwm.glp').absolute.path;

    test('through the module: t(O) gives O = [1, 2]', () async {
      final engine = GlpEngine(rootSelfGlpPath: _root)..loadFile(fixture);
      final r = await engine.runGoal('t(O)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['O']), '[1, 2]');
    });
  });

  test('a call to a root procedure resolves to its renamed form: find_type/2, '
      'whose argument is resolved too', () async {
    // programs/tests/find_type_scope: m.glp's probe/1 calls the root's
    // find_type/2 on its own local/1.  Both the call and its P/N argument are
    // resolved in m.glp's scope (TGLP modules.tex, Compilation, fourth step).
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('../programs/tests/find_type_scope').absolute.path);
    final r = await engine.runGoal('probe(T)');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings['T']), matches(RegExp(r'^[0-9a-f]{64}$')));
  });

  group('the root self.glp is checked in step 2 with every module', () {
    // programs/tests/root_check/univ_typing_root.glp is the root self.glp with
    // '_univ_compose'/4's second argument declared Integer?: its =.. clause
    // and its own clauses are not well-typed, as the same clauses in an
    // application module are not.
    final mutant =
        File('../programs/tests/root_check/univ_typing_root.glp').absolute.path;
    final program =
        File('../programs/tests/root_check/compose.glp').absolute.path;

    test('a program under a root that does not check is refused, naming the '
        'root', () {
      expect(
          () => GlpEngine(rootSelfGlpPath: mutant).loadFile(program),
          throwsA(predicate((e) =>
              e.toString().contains('univ_typing_root.glp') &&
              e.toString().contains('_univ_compose'))));
    });

    test('under the root self.glp the same program loads and runs', () async {
      final engine = GlpEngine(rootSelfGlpPath: _root)..loadFile(program);
      final r = await engine.runGoal('compose(T)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['T']), 'f(a)');
    });
  });
}
