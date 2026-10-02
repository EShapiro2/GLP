// glp_runtime/test/analysis/type_checker/callee_fixed_types_test.dart
//
// The types a call's callee's clauses fix for a parameter are tried beside the
// types its sites supply, and the binding taken is one under which the clause
// and the callee's clauses are well-typed.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call":
//
//   "For each parameter it tries the types the sites supply and the types the
//    callee's clauses fix for it---a head occurrence of the parameter paired
//    by condition~3 with a body occurrence of a concrete type---and takes one
//    under which the clause and the callee's clauses are well-typed with
//    subtyping; where types are supplied or fixed and none serves, the
//    bindings conflict and the call is refused."
//
// GLP #3 Cowork's ruling of 2026-10-02 15:46 UTC, item 1.  Fixtures:
// programs/tests/call_instantiation/callee_fixed_*.glp.  Until 2026-10-02 (TGLP
// 8a58729) a type no site supplied was not tried, and a call some parameter of
// which only the callee's clauses fixed was refused unless the callee was
// parametrically well-typed.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/call_instantiation/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

String _refusal(String name) {
  try {
    _engine().loadFile(_fixture(name));
  } catch (e) {
    return e.toString();
  }
  return '';
}

/// [term] as the REPL prints it, each variable dereferenced through [engine]'s
/// heap: a list as `[a, b]`, a structure as `f(a, b)`.
String _show(GlpEngine engine, rt.Term? term) {
  if (term == null) return '[]';
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
      } else {
        final rest = _show(engine, c);
        return rest == '[]'
            ? '[${items.join(', ')}]'
            : '[${items.join(', ')} | $rest]';
      }
    }
  }
  if (t is rt.StructTerm) {
    return '${t.functor}(${t.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '_';
}

Future<String> _answer(String fixture, String goal, String variable) async {
  final engine = _engine()..loadFile(_fixture(fixture));
  final r = await engine.runGoal(goal);
  expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
  return _show(engine, r.bindings[variable]);
}

void main() {
  group('a type the callee\'s clauses fix is tried', () {
    test('by a body pair: the head occurrence at M? paired with a body '
        'occurrence at Msg?', () async {
      expect(await _answer('callee_fixed_body.glp', 'go(c, a, Out)', 'Out'),
          '[m(c)]');
    });

    test('by a head pair, the shape of send_user/3: a constructed message no '
        'site names a type for', () async {
      expect(await _answer('callee_fixed_head.glp', 'go(c, b, Out)', 'Out'),
          '[m(c)]');
    });

    test('beside the type a site supplies, where only the fixed one makes the '
        'callee\'s clauses well-typed', () async {
      expect(
          await _answer('callee_fixed_head.glp', 'go_sub(m(d), a, Out)', 'Out'),
          '[m(d)]');
    });
  });

  group('the bindings conflict', () {
    test('neither the supplied type nor the fixed one serves: refused, naming '
        'the call and the types tried', () {
      final err = _refusal('callee_fixed_conflict.glp');
      expect(err, contains('The bindings tried for the call put(N?, K?, Out) '
          'conflict'));
      expect(err, contains('M: Constant, Msg; K: Kind'));
      expect(err, contains('under M = Constant, K = Kind the clauses of '
          'put/3 are not well-typed'));
      expect(err, contains('"The instantiation of a call"'));
    });
  });
}
