/// The anonymous head reader `_?` is refused at every position.
///
/// GLP-Spec glp.tex, Remark "Anonymous Variables" (297db75), and TGLP
/// "Anonymous variables" (2424f4c): "Anonymous readers (`_?`, `_Name?`) are
/// not permitted in a clause, in the head or the body: a reader whose writer
/// is never written is a goal that can never be satisfied, so a clause writes
/// every writer it holds, closing a stream it does not extend."
///
/// From 2026-10-02 until 2026-10-14 a head `_?` at a produced position was
/// admitted as "an output the clause never produces" and compiled as a head
/// reader of a variable of its own (`get_variable` and `unify_variable` in
/// reader mode), and this file held the term matching that gave it: a goal
/// writer assigned it, a goal reader or term failing.  It now holds the
/// refusal at each position it held then --- an argument, a consumed
/// structure, a produced structure, beside a `_`, and the Out of an unfolded
/// receive (programs/tests/probe13_strict, programs/tests/recv2x2_neg.glp) ---
/// on the checked load path and in codegen, and the same clauses written to
/// the rule, each writing the writer it holds, as the controls (GLP, 2026-10-14
/// 09:35 UTC, item 2).
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes.dart' as op;
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _types = '''
Ch ::= ch(Integer, Integer?).
Out2 ::= out(Integer).
Pair ::= pair(Integer?, Integer).
Item ::= it(Constant).
Items ::= [] ; [Item | Items].
Closed ::= closed.
''';

/// Each refused clause, with its declaration: the positions `_?` held.
const _refused = <String, String>{
  'at a produced argument': 'procedure p(Integer?, Integer).\np(_, _?).\n',
  'at a consumed argument': 'procedure q(Integer?).\nq(_?).\n',
  'in a consumed structure': 'procedure c(Ch?).\nc(ch(_, _?)).\n',
  'in a produced structure': 'procedure o(Out2).\no(out(_?)).\n',
  'beside a `_`': 'procedure w(Pair).\nw(pair(_, _?)).\n',
  'at the Out of an unfolded receive':
      'procedure cons(Channel(Items, Closed)?, Constant).\n'
          'cons(ch([it(_)|_], _?), matched).\n'
          'cons(_, other) :- otherwise | true.\n',
  'named, `_Out?`, at a produced argument':
      'procedure p(Integer?, Integer).\np(_, _Out?).\n',
};

/// The same procedures, each clause writing the writer it holds: a constant
/// of the position's type.
const _written = '''
procedure p(Integer?, Integer).
p(_, 0).

procedure c(Ch?).
c(ch(_, 0)).

procedure o(Out2).
o(out(0)).

procedure w(Pair).
w(pair(_, 0)).

procedure cons(Channel(Items, Closed)?, Constant).
cons(ch([it(_)|_], closed), matched).
cons(_, other) :- otherwise | true.
''';

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

GlpEngine _load(String source) {
  final engine = _engine();
  expect(engine.loadSource(source), isTrue);
  return engine;
}

Matcher _refusal(String spelling) => throwsA(predicate((Object e) => '$e'
    .contains('$spelling (anonymous reader) is not permitted in program '
        'clauses')));

String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return '${d.value}';
  return '$d';
}

void main() {
  group('a head `_?` is refused on load', () {
    for (final MapEntry(key: where, value: clause) in _refused.entries) {
      test(where, () {
        final spelling = clause.contains('_Out?') ? '_Out?' : '_?';
        expect(() => _engine().loadSource('$_types$clause'),
            _refusal(spelling));
      });
    }
  });

  group('a head `_?` is refused by codegen, compiled without the checker', () {
    test('at an argument', () {
      expect(() => GlpCompiler().compile('p(_, _?).\n'), _refusal('_?'));
    });

    test('in a structure', () {
      expect(() => GlpCompiler().compile('c(ch(_, _?)).\n'), _refusal('_?'));
    });
  });

  group('the clauses written to the rule (controls)', () {
    test('compile with no reader instruction: `_` is the writer alone', () {
      final prog = _load('$_types$_written').loadedPrograms['_source_']!;
      List<Object?> code(String sig) =>
          prog.ops.sublist(prog.labels[sig]!, prog.labels['${sig}_end']!);
      final ops = [
        for (final sig in ['p/2', 'c/1', 'o/1', 'w/1', 'cons/2']) ...code(sig),
      ];
      // The source names no variable, so a reader instruction could only be
      // an anonymous reader.
      expect(ops.whereType<op.GetVariable>().where((o) => o.isReader), isEmpty);
      expect(
          ops.whereType<op.UnifyVariable>().where((o) => o.isReader), isEmpty);
      // c/1's, w/1's and cons/2's two `_`: unify_void is the writer's.
      expect(ops.whereType<op.UnifyVoid>().length, 4);
    });

    test('p(1, Y) gives Y = 0: the output is produced', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('p(1, Y)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['Y']), '0');
    });

    test('p(1, 2) fails: a goal constant other than the one written', () async {
      final r = await _load('$_types$_written').runGoal('p(1, 2)');
      expect(r.status, ExecutionStatus.failed, reason: '${r.error}');
    });

    test('c(ch(1, Y)) gives Y = 0', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('c(ch(1, Y))');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['Y']), '0');
    });

    test('o(out(Z)) gives Z = 0', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('o(out(Z))');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['Z']), '0');
    });

    test('w(W) gives W = pair(_, 0), `_` a fresh writer', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('w(W)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      final heap = engine.runtime.heap;
      final pair = heap.dereference(r.bindings['W']!);
      expect(pair, isA<StructTerm>());
      pair as StructTerm;
      expect(pair.functor, 'pair');
      final first = pair.args[0] as VarRef;
      expect(heap.isWriter(first.addr), isTrue, reason: '`_` is a writer');
      expect(heap.isWriterBound(first.addr), isFalse);
      expect(_show(engine, pair.args[1]), '0');
    });

    test('cons(ch([it(a)], closed), A): the receive meeting the closed Out it '
        'writes matches', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('cons(ch([it(a)], closed), A)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['A']), 'matched');
    });

    test('cons(ch([it(a)], Out), A): an open Out is written closed', () async {
      final engine = _load('$_types$_written');
      final r = await engine.runGoal('cons(ch([it(a)], Out), A)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['A']), 'matched');
      expect(_show(engine, r.bindings['Out']), 'closed');
    });
  });
}
