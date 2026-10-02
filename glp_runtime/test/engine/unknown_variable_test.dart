/// A variable whose writer occurrence lies under a suspended reader is
/// unknown, and stays unknown for the rest of the clause attempt.
///
/// GLP-Spec appendix-term-matching.tex, Definition "Term Matching": a goal
/// reader against a head term is "suspend on X1?", and "the writer mgu is the
/// union of all writer assignments if no fail was encountered and the
/// suspension set is empty"; glp.tex, Guards: "A guard suspends if it does not
/// succeed but some instance of it under a readers substitution would succeed.
/// A guard fails if no such instance exists."  The head pattern under the
/// suspended reader is skipped, and a head variable whose writer occurrence
/// lies in it has no value yet --- its value is the goal's subterm there, not
/// yet given --- so a guard over it is undecided, and the clause suspends on
/// the reader if nothing else fails (GLP's task of 2026-10-01 23:58 UTC, item
/// 4, 3(b)).  At 73816f1c a later occurrence of the variable gave it a value as
/// at a first occurrence --- a fresh variable in a structure built for the
/// goal's writer (f1, f8), the goal's writer itself (f6, f7) --- and the guard
/// then failed on an unbound writer, so the clause failed and the goal no
/// longer waited on the reader: sGLP's agent, graph.glp:51, missed its menu.
///
/// A reader occurrence gives a variable no value, so a variable met there only
/// as a reader is not unknown, and takes its value from its writer occurrence
/// elsewhere in the head (g1); and a variable that an earlier reader occurrence
/// gave only a representation, a fresh writer or the goal's, is unknown when
/// its writer occurrence is skipped (r2, r3, r4).
///
/// Each "then" case binds the reader after the goal has suspended and checks
/// that the goal reduces.  The last group runs f1 and f6 with their compiled
/// unify_variable instructions replaced by head_variable, the instruction a
/// foreign artefact may carry for the same positions.
library;

import 'dart:io';

import 'package:glp_runtime/bytecode/opcodes_v2.dart' as opv2;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _source = '''
Peer    ::= Constant.
Choice  ::= same(Peer).
Out     ::= out(Peer).
Outs    ::= [] ; [Peer | Outs].
In      ::= in(Peer).
R       ::= r(Integer?).

procedure f1(Choice?, Out).
f1(same(To), out(To?)) :- ground(To?) | true.

procedure f6(Choice?, Out).
f6(same(To), out(To?)) :- ground(To?) | true.

procedure f7(Choice?, Peer).
f7(same(To), To?) :- ground(To?) | true.

procedure f8(Choice?, Outs).
f8(same(To), [To?]) :- ground(To?) | true.

procedure g1(R?, Integer?).
g1(r(X?), X) :- X? > 5 | true.

procedure r2(Out, In?).
r2(out(X?), in(X)) :- ground(X?) | true.

procedure r3(Out, In?).
r3(out(X?), in(X)) :- ground(X?) | true.

procedure r4(Peer, In?).
r4(X?, in(X)) :- ground(X?) | true.
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
      if (op is opv2.UnifyVariable) {
        ops[i] = opv2.HeadVariable(op.varIndex, isReader: op.isReader);
        replaced++;
      }
    }
    expect(replaced, greaterThan(0),
        reason: 'the heads hold variables inside structures');
  }
  return engine;
}

/// The value [t] is bound to, written out: constants by their value, a list in
/// brackets, a structure by its functor and arguments.
String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return d.value == 'nil' ? '[]' : '${d.value}';
  if (d is StructTerm) {
    if ((d.functor == '[|]' || d.functor == '.') && d.args.length == 2) {
      final items = <String>[];
      Object? cur = d;
      while (cur is StructTerm &&
          (cur.functor == '[|]' || cur.functor == '.') &&
          cur.args.length == 2) {
        items.add(_show(engine, cur.args[0]));
        cur = engine.runtime.heap.dereference(cur.args[1]);
      }
      final tail = _show(engine, cur as Term?);
      return tail == '[]' ? '[${items.join(', ')}]' : '[${items.join(', ')}|$tail]';
    }
    return '${d.functor}(${d.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '$d';
}

/// [goal] suspends; with [binding] after it, it reduces and [variable] is
/// [value].
void _suspendsThenReduces(String goal, String binding, String variable,
    String value, {bool headVariable = false}) {
  test('$goal suspends', () async {
    final r = await _fresh(headVariable: headVariable).runGoal(goal);
    expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
  });
  test('$goal, $binding gives $variable = $value', () async {
    final engine = _fresh(headVariable: headVariable);
    final r = await engine.runGoal('$goal, $binding');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(_show(engine, r.bindings[variable]), value);
  });
}

void main() {
  group('a later occurrence gives an unknown variable no value', () {
    // To? into a structure built for the goal's writer (unify_variable, WRITE
    // mode): graph.glp:51's case.
    _suspendsThenReduces('f1(P?, O)', 'P = same(3)', 'O', 'out(3)');
    // To? against the goal's writer inside a goal structure (unify_variable,
    // READ mode).
    _suspendsThenReduces('f6(P?, out(W))', 'P = same(3)', 'W', '3');
    // To? against the goal's writer at the top level (get_variable, reader).
    _suspendsThenReduces('f7(P?, W)', 'P = same(3)', 'W', '3');
    // To? into a list built for the goal's writer.
    _suspendsThenReduces('f8(P?, O)', 'P = same(3)', 'O', '[3]');
  });

  group('a reader occurrence under the suspended reader gives no value', () {
    // X? lies only under the suspended reader; X's writer occurrence, at the
    // top level, gives it 3, and 3 > 5 fails whatever P becomes: the clause
    // fails.  Marked unknown at its reader occurrence, X kept no value from
    // its writer occurrence, the guard was passed by, and the goal suspended.
    test('g1(P?, 3) fails', () async {
      final r = await _fresh().runGoal('g1(P?, 3)');
      expect(r.status, ExecutionStatus.failed, reason: '${r.error}');
    });
    _suspendsThenReduces('g1(P?, 7)', 'P = r(W)', 'W', '7');
  });

  group('an earlier reader occurrence gives a representation, not a value', () {
    // X? first, in a structure built for the goal's writer: a fresh writer.
    _suspendsThenReduces('r2(O, I?)', 'I = in(5)', 'O', 'out(5)');
    // X? first, against the goal's writer inside a goal structure.
    _suspendsThenReduces('r3(out(W), I?)', 'I = in(5)', 'W', '5');
    // X? first, against the goal's writer at the top level.
    _suspendsThenReduces('r4(W, I?)', 'I = in(5)', 'W', '5');
  });

  group('the same heads with head_variable for unify_variable', () {
    _suspendsThenReduces('f1(P?, O)', 'P = same(3)', 'O', 'out(3)',
        headVariable: true);
    _suspendsThenReduces('f6(P?, out(W))', 'P = same(3)', 'W', '3',
        headVariable: true);
  });
}
