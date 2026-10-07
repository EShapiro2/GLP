/// A goal suspends on a reader met after the head has built a structure for a
/// goal writer.
///
/// GLP-Spec appendix-term-matching.tex, Definition "Term Matching": a goal
/// reader against a head term is "suspend on X1?"; glp.tex, "Monotonicity": a
/// goal that cannot be reduced now but can be under a readers substitution
/// suspends on those readers, and is reduced once one of them is assigned.
/// `d2([T?], f(T))` on `d2(O, X?)` builds `[T?]` for the goal writer `O`, then
/// meets `f(T)` at the unbound `X?`.  Before 73816f1c the head instruction that
/// met the reader added it to the suspension set and left the traversal on the
/// structure the first argument had built, full and in WRITE mode, so the `T`
/// of `f(T)` was written into a third slot of the two-slot list cell:
/// `RangeError (index): Invalid value: Not in inclusive range 0..1: 2` from
/// `execUnifyVariable`, where the goal must suspend (vGLP's report; GLP
/// Cowork's task of 2026-10-02 08:40 UTC, F).  73816f1c skips the pattern under
/// the reader.  These goals pin it at the two head instructions that meet a
/// reader after such a structure: `head_structure` on the clause register a
/// consumed structure argument is loaded into (`d2`; and `agent`, vGLP's
/// compiled shape, whose interactive term follows its output stream), and
/// `head_structure` on an argument a list pattern is matched at directly
/// (`d4`).
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

const _source = '''
R ::= f(Integer) ; g.

procedure d2(Stream(Integer), R?).
d2([T?], f(T)).
d2([], g).

procedure d4(Stream(Integer), Stream(Integer)?).
d4([T?], [T | _]).
d4([], []).

Request ::= post(String) ; quit.

procedure agent(String?, Stream(String), Request?).
agent(Id, [Text?|Outs?], post(Text)) :- ground(Id?) | close(Outs).
agent(_, [], quit).

procedure close(Stream(String)).
close([]).
''';

GlpEngine _fresh() {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(_source), isTrue);
  return engine;
}

/// The value [t] is bound to, written out: constants by their value, a list in
/// brackets, a structure by its functor and arguments.
String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return d.value == nil ? '[]' : '${d.value}';
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
      return tail == '[]'
          ? '[${items.join(', ')}]'
          : '[${items.join(', ')}|$tail]';
    }
    return '${d.functor}(${d.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '$d';
}

/// [goal] suspends; with each binding of [then] after it, it reduces and `O`
/// is the value the binding maps to.
void _suspendsThenReduces(String goal, Map<String, String> then) {
  test('$goal suspends', () async {
    final r = await _fresh().runGoal(goal);
    expect(r.status, ExecutionStatus.suspended, reason: '${r.error}');
  });
  for (final MapEntry(key: binding, value: value) in then.entries) {
    test('$goal, $binding gives O = $value', () async {
      final engine = _fresh();
      final r = await engine.runGoal('$goal, $binding');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_show(engine, r.bindings['O']), value);
    });
  }
}

void main() {
  group('head_structure at a clause register holding the unbound reader', () {
    // The reported goal: f(T) after [T?] was built for O.
    _suspendsThenReduces('d2(O, X?)', {'X = f(3)': '[3]', 'X = g': '[]'});
    // vGLP's compiled reader-mode clause while its question is open.
    _suspendsThenReduces('agent("alice", O, Q?)',
        {'Q = post("hi")': '["hi"]', 'Q = quit': '[]'});
  });

  group('head_structure at an argument holding the unbound reader', () {
    // [T | _] after [T?] was built for O.
    _suspendsThenReduces('d4(O, X?)', {'X = [5]': '[5]', 'X = []': '[]'});
  });
}
