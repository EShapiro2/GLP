// glp_runtime/test/analysis/type_checker/call_instantiation_test.dart
//
// The instantiation of a call, read over the whole clause.
//
// Specification: TGLP (Moded-Types) cc4a891, appendix-implementation-notes.tex,
// "The instantiation of a call":
//
//   "Definition~\ref{def:instantiation} asks that an instantiation exist and
//    orders nothing; the checker reads the sites of a call over the whole
//    clause, the body goals in no order, and a posted goal, checked as a body
//    (Section~\ref{sec:runtime-boundary}), the same way.  For each parameter
//    it tries the types the sites supply and the types the callee's clauses
//    fix for it---a head occurrence of the parameter paired by condition~3
//    with a body occurrence of a concrete type---and takes one under which the
//    clause and the callee's clauses are well-typed with subtyping; where
//    types are supplied or fixed and none serves, the bindings conflict and
//    the call is refused.  A parameter for which no type is supplied or fixed
//    is left open where the callee is parametrically well-typed
//    (Section~\ref{sec:abstract-parameters}), the call checked with it open
//    and every argument typed at its position; otherwise the call is
//    refused."
//
// with Definition "Instantiation" (parameterized-types.tex), its paragraph "The
// bindings the sites of a call supply conflict exactly when no theta makes the
// clause well-typed with subtyping; so a stream of a subtype merges into a
// stream of its supertype ..., with X bound to the supertype", and Definition
// "Well-Typed Clause with Subtyping", condition 3 (well-typing.tex).  GLP's task
// of 2026-10-02 13:38 UTC (TGLP 8a58729) and its ruling of 15:46 UTC (TGLP
// cc4a891).  Fixtures: programs/tests/call_instantiation/; the types the
// callee's clauses fix are callee_fixed_types_test.dart's.
//
// Until 2026-10-02 the checker inferred a call's instantiation from the
// occurrences typed before the call, binding each parameter to the first type
// met: copy_first.glp was refused for its goal order alone, and a merge whose
// subtype stream came first was checked at the subtype and refused.
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
  group('the body goals in no order', () {
    test('copy before the merge that types its output loads and runs',
        () async {
      expect(await _answer('copy_first.glp', 'go([msg(a, b)], [], C)', 'C'),
          '[msg(a, b)]');
    });

    test('the merge before copy loads and runs', () async {
      expect(await _answer('merge_first.glp', 'go([msg(a, b)], [], C)', 'C'),
          '[msg(a, b)]');
    });
  });

  group('a stream of a subtype merges into a stream of its supertype', () {
    test('the network input first', () async {
      final v = await _answer('subtype_merge.glp',
          'join([pending_link(a, b)], [msg("k", c, ping)], X)', 'X');
      expect(v, contains('pending_link(a, b)'));
      expect(v, contains('msg("k", c, ping)'));
    });

    test('the friend stream first', () async {
      final v = await _answer('subtype_merge.glp',
          'join_first([pending_link(a, b)], [msg("k", c, ping)], Y)', 'Y');
      expect(v, contains('pending_link(a, b)'));
      expect(v, contains('msg("k", c, ping)'));
    });

    test('the friend stream written by an earlier goal', () async {
      final v = await _answer(
          'subtype_merge.glp', 'join_hub([pending_link(a, b)], Z)', 'Z');
      expect(v, contains('pending_link(a, b)'));
      expect(v, contains('msg("k", c, ping)'));
    });

    test('inside an instantiated parameterised procedure', () async {
      final v = await _answer(
          'subtype_merge.glp',
          'join_setup([friend_stream([msg("k", c, text("hi"))])], '
              '[pending_link(a, b)], W)',
          'W');
      expect(v, contains('pending_link(a, b)'));
      expect(v, contains('msg("k", c, text("hi"))'));
    });

    test('where an alternative of the one is not an alternative of the other, '
        'refused by the site the supertype does not fit', () {
      final err = _refusal('subtype_merge_refused.glp');
      expect(err, isNotEmpty, reason: 'subtype_merge_refused.glp loaded');
      expect(err, contains('Variable pair (FIn, FIn?)'));
      expect(err, contains('Stream<FriendMsg>'));
      expect(err, contains('Stream<NetInMsg>'));
      expect(err, isNot(contains('Variable pair (NetIn, NetIn?)')));
    });

    test('the sites supply Integer and String: refused by the one site the '
        'better binding does not fit', () {
      final err = _refusal('conflict_refused.glp');
      expect(err, contains('Variable pair (B, B?)'));
      expect(err, contains('Stream<String>'));
      expect(err, isNot(contains('Variable pair (A, A?)')));
      expect(err, isNot(contains('Variable pair (C, C?)')));
    });

    test('a constructed argument the supplied binding does not admit is '
        'refused, naming it', () {
      final err = _refusal('constructed_not_admitted.glp');
      expect(err, contains('No transition for bad(1,1)'));
      expect(err, contains('NetInMsg?'));
    });
  });

  group('no type supplied or fixed', () {
    test('no site supplies a parameter of a callee that is not parametrically '
        'well-typed, and its clauses fix none: the call is refused', () {
      final err = _refusal('unsupplied_refused.glp');
      expect(err, contains('No instantiation of copy/2 is found for the call'));
      expect(err, contains('no site of the call supplies a type for FM'));
      expect(err, contains('copy/2 is not parametrically well-typed'));
      expect(err, contains('"The instantiation of a call"'));
    });

    test('no site supplies a parameter of a parametrically well-typed callee: '
        'the call is checked with the parameter open, and runs', () async {
      expect(await _answer('unsupplied_parametric.glp', 'go(N)', 'N'), '2');
    });

    test('a declaration with Stream(_) beside a parameter is instantiated',
        () async {
      expect(await _answer('wildcard_arg.glp', 'go([msg(a, b)], Y)', 'Y'),
          '[msg(a, b)]');
    });
  });
}
