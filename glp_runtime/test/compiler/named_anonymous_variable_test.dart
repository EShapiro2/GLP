/// A named anonymous variable is compiled and typed as `_` is.
///
/// TGLP typed-glp.tex, "Anonymous variables": "An anonymous variable is any
/// variable whose name begins with `_` (e.g., `_`, `_In`, `_Out`).  Anonymous
/// variables may appear anywhere a writer variable may appear.  Each
/// occurrence denotes a fresh writer with no paired reader".  The analyzer
/// keeps no register for a name beginning with `_`, SRSW not counting it, and
/// until 2026-10-02 codegen then refused every such name, "Undefined variable:
/// _X", where it compiles `_`: at a head argument (codegen.dart:262), in a head
/// structure, at a body argument, and in a structure built in the body (GLP
/// Cowork's task of 2026-10-02 08:40 UTC, F; vGLP's compiled reader-mode
/// `(_Answer)` clause did not load for it).  And the type checker took two
/// occurrences of one such name for one variable, so `h(_A, _A, 1)` at an
/// Integer and a String position was refused, "Variable _A? has inconsistent
/// types".  Each program below loads and runs as it would with `_`, each
/// occurrence a variable of its own.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// Each case: its program, and goals with the variable each binds and the
/// value it binds it to.
final _cases = <String, (String, Map<String, (String, String)>)>{
  // At a head argument: vGLP's report.
  'a head argument': (
    '''
procedure f(Integer?, Integer).
f(_X, 0).
''',
    {
      'f(5, Y)': ('Y', '0'),
      // As `_` does, the head's fresh writer takes the goal's reader without
      // waiting on it.
      'f(Z?, Y)': ('Y', '0'),
    }
  ),
  'a head structure': (
    '''
procedure g(Stream(Integer)?, Integer).
g([_First | _Rest], 1).
g([], 0).
''',
    {
      'g([7, 8], N)': ('N', '1'),
      'g([], N)': ('N', '0'),
    }
  ),
  // TGLP's foo(X) :- bar(_Result, X?).
  'a body argument': (
    '''
procedure two(Integer, Integer).
two(1, 2).

procedure k(Integer).
k(N?) :- two(_Discard, N).
''',
    {'k(N)': ('N', '2')}
  ),
  'a structure built in the body, and one nested in it': (
    '''
P ::= p(Integer, Integer).
Q ::= q(P).

procedure pair(P).
pair(p(1, 2)).

procedure m(Integer).
m(N?) :- pair(p(_Skip, N)).

procedure nest(Q).
nest(q(p(1, 2))).

procedure m2(Integer).
m2(N?) :- nest(q(p(_Skip, N))).
''',
    {
      'm(N)': ('N', '2'),
      'm2(N)': ('N', '2'),
    }
  ),
  // Compiled as one variable, _A would take 1 and then fail on 2.
  'two occurrences of one name, compiled': (
    '''
procedure h2(Integer?, Integer?, Integer).
h2(_A, _A, 1).
''',
    {'h2(1, 2, N)': ('N', '1')}
  ),
  // Typed as one variable, _A was refused at two types.
  'two occurrences of one name, typed': (
    '''
procedure h(Integer?, String?, Integer).
h(_A, _A, 1).
''',
    {'h(1, "s", N)': ('N', '1')}
  ),
  // vGLP's compiled reader-mode clause, its interactive term _Answer.
  "vGLP's (_Answer) clause": (
    '''
YesNo ::= yes ; no.
Handle ::= withdraw.

procedure ask1(Integer?, Integer, YesNo?, Handle, Stream(Integer)).
ask1(N, N?, yes, _?, []).
ask1(_, 0, _Answer, withdraw, []).
''',
    {'ask1(1, N, no, W, D)': ('N', '0')}
  ),
};

GlpEngine _load(String source) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(source), isTrue);
  return engine;
}

/// The value [t] is bound to: a constant by its value.
String _show(GlpEngine engine, Term? t) {
  final d = t == null ? null : engine.runtime.heap.dereference(t);
  if (d is ConstTerm) return '${d.value}';
  return '$d';
}

void main() {
  for (final MapEntry(key: name, value: (source, goals)) in _cases.entries) {
    group(name, () {
      test('loads', () => _load(source));
      for (final MapEntry(key: goal, value: (variable, value))
          in goals.entries) {
        test('$goal gives $variable = $value', () async {
          final engine = _load(source);
          final r = await engine.runGoal(goal);
          expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
          expect(_show(engine, r.bindings[variable]), value);
        });
      }
    });
  }
}
