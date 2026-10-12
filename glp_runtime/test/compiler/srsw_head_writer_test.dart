// glp_runtime/test/compiler/srsw_head_writer_test.dart
//
// A writer occurring more than once in a clause, in the head as in the body:
// licensed by a guard whose success implies that its reader is bound to a
// ground term, and refused without one.
//
// Spec: GLP-Spec sections/glp.tex, Definition "GLP Program": a clause
// satisfies SRSW "if it satisfies SO", and Definition "Single-Occurrence (SO)
// Invariant": "every variable occurs in it at most once".  Remark "Guards and
// SRSW" (831f315, Udi's sentence restored on 2026-10-12; TGLP glp.tex, the
// Remark of the same name): "if X? occurs in a guard, its paired writer X
// must occur in the head ... Furthermore, if the success of a guard implies
// that X? is bound to a ground term, then both X and X? may occur multiple
// times in the clause."  The groundness-implying guards are the catalogue's
// "Ground: yes" (appendix-guards.tex) and the arithmetic comparisons, whose
// "Success implies both arguments are ground" there and which TGLP's Remark
// names.  A goal reduces with such a clause if the goal and the head have a
// writer mgu (Definitions "Writer MGU", "GLP Goal/Clause Reduction"): the
// goal subterms a repeated head writer meets must be one term.
//
// From 2026-10-02 (b4cd4191, then 905a9ed8 on GLP-Spec bbff21d, undone by
// 831f315) a writer twice was refused whatever the guards.  Before, the guard
// licensed it and the engine ran it wrong: the second head occurrence's
// get_variable overwrote the first, so h1(same(To), To) :- ground(To?) | true
// reduced h1(same(4), 3); get_value compared by identity, so two(R, R) :-
// ground(R?) | true failed two(3, 3); and put_variable refused a writer of the
// body that the head had given a value.

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;

/// The compiler's diagnostic for [source], or '' if it compiles.
String _compileError(String source) {
  try {
    GlpCompiler().compile(source);
  } catch (e) {
    return e.toString();
  }
  return '';
}

/// An engine over the root self.glp with [source] loaded as [filename].
GlpEngine _engineWith(String source, String filename) {
  final engine =
      GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
  expect(engine.loadSource(source, filename: filename), isTrue);
  return engine;
}

/// [term], dereferenced, printed.
String _show(GlpEngine engine, rt.Term? term) {
  final heap = engine.runtime.heap;
  final t = term == null ? null : heap.dereference(term);
  if (t is rt.ConstTerm) return '${t.value}';
  if (t is rt.StructTerm) {
    return '${t.functor}(${t.args.map((a) => _show(engine, a)).join(', ')})';
  }
  return '$t';
}

void main() {
  group('a writer twice in the head, under a groundness-implying guard, loads',
      () {
    final licensed = [
      // The case of b4cd4191, in a structure and at top level, under ground/1.
      'h1(same(To), To) :- ground(To?) | true.\n',
      // GLP's test (GLP #3 Cowork, 2026-10-12 07:30 UTC).
      'two(R, R) :- ground(R?) | true.\n',
      // Twice nested, under integer/1, "Ground: yes".
      'p(f(X), g(X)) :- integer(X?) | true.\n',
      // In a list, under an arithmetic comparison.
      'p([X, X]) :- X? > 0 | true.\n',
      // Under =?=, "Ground: yes (both)".
      'p(X, X, Y) :- X? =?= Y? | true.\n',
      // Three times.
      'p(X, f(X), [X]) :- ground(X?) | true.\n',
    ];
    for (final clause in licensed) {
      test(clause.trim(), () => expect(_compileError(clause), isEmpty));
    }
  });

  group('a writer twice in the head without such a guard is refused', () {
    final refused = {
      // The case of b4cd4191 without its guard.
      'h1(same(To), To) :- true.\n':
          'Writer variable "To" occurs 2 times in the head of the clause '
              'h1(same(To), To) without a groundness-implying guard',
      'h1(same(To), To).\n':
          'Writer variable "To" occurs 2 times in the head of the clause '
              'h1(same(To), To) without a groundness-implying guard',
      // GLP's test, without the guard.
      'two(R, R) :- true.\n':
          'Writer variable "R" occurs 2 times in the head of the clause '
              'two(R, R) without a groundness-implying guard',
      // Under known/1, "Ground: no".
      'two(R, R) :- known(R?) | true.\n':
          'Writer variable "R" occurs 2 times in the head of the clause '
              'two(R, R) without a groundness-implying guard',
      // Under =?\=, "Ground: no".
      'p(X, X, Y) :- X? =?\\= Y? | true.\n':
          'Writer variable "X" occurs 2 times in the head',
      'p(X, X).\n':
          'Writer variable "X" occurs 2 times in the head of the clause p(X, X)',
      'p(X, f(X), [X]).\n': 'Writer variable "X" occurs 3 times in the head',
    };

    for (final MapEntry(key: clause, value: diagnostic) in refused.entries) {
      test(clause.trim(), () {
        final e = _compileError(clause);
        expect(e, contains('SRSW violation'), reason: e);
        expect(e, contains(diagnostic), reason: e);
      });
    }

    test('the diagnostic names the procedure and the line', () {
      final e = _compileError('q(a).\n\nh1(same(To), To) :- true.\n');
      expect(e, contains('h1/2: Line 3: Writer variable "To"'), reason: e);
    });

    test('the engine refuses to load the case of b4cd4191 without its guard',
        () {
      final engine =
          GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
      const source = '''
Same ::= same(Integer).
procedure h1(Same?, Integer?).
h1(same(To), To) :- true.
''';
      Object? error;
      bool? loaded;
      try {
        loaded = engine.loadSource(source, filename: 'h1.glp');
      } catch (e) {
        error = e;
      }
      expect(loaded, isNot(isTrue));
      expect('$error', contains('occurs 2 times in the head'), reason: '$error');
    });
  });

  group('a writer in the head and again in the body', () {
    final licensed = [
      // Under integer/1, "Ground: yes".
      'p(X, Y?) :- integer(X?) | Y = s(X).\n',
      // Under ground/1, at a produced position of a body goal.
      'p(X, Y?) :- ground(X?) | q(X, Y).\nq(A?, b) :- A = c.\n',
      // Under =?=, which grounds both of its arguments.
      'p(X, Z, Y?) :- X? =?= Z? | Y = s(X).\n',
      // Under an arithmetic comparison.
      'p(X, Y?) :- X? > 0 | Y = s(X).\n',
      // Twice in the body.
      'p(X, Y?, Z?) :- ground(X?) | Y = s(X), Z = s(X).\n',
    ];
    for (final clause in licensed) {
      test('loads: ${clause.trim().replaceAll('\n', ' ')}',
          () => expect(_compileError(clause), isEmpty));
    }

    final refused = {
      // Without a guard.
      'p(X, Y?) :- Y = s(X).\n': ' without a groundness-implying guard',
      'p(X, Y?, Z?) :- Y = s(X), Z = s(X).\n':
          ' without a groundness-implying guard',
      // Under known/1, "Ground: no".
      'p(X, Y?) :- known(X?) | Y = s(X).\n':
          ' without a groundness-implying guard',
      // Twice in the body under a guard on its reader, the head holding the
      // reader: "if X? occurs in a guard, its paired writer X must occur in
      // the head" (Remark "Guards and SRSW").
      'p(X?) :- ground(X?) | X = a, X = b.\n':
          'licenses a repeated writer X only through its reader X?, with X in '
              'the head',
      // In the head and again as a guard's argument, which is a reader's
      // place: "Guard arguments are readers paired to head writers" (glp.tex,
      // Definition "Guarded Clause").
      'p(X, Y?) :- ground(X) | Y = s(X?).\n':
          'licenses a repeated writer X only through its reader X?, with X in '
              'the head',
    };
    for (final MapEntry(key: clause, value: why) in refused.entries) {
      test('refused: ${clause.trim().replaceAll('\n', ' ')}', () {
        final e = _compileError(clause);
        expect(e, contains('SRSW violation'), reason: e);
        expect(e, contains('Writer variable "X" occurs '), reason: e);
        expect(e, contains(why), reason: e);
      });
    }

    test('a writer once in the head and once in a guard-free body is refused '
        'by the general count, not the head\'s', () {
      final e = _compileError('p(X) :- q(X).\n');
      expect(e, contains('without a groundness-implying guard'), reason: e);
      expect(e, isNot(contains('in the head')), reason: e);
    });
  });

  group('what the rule leaves as it was', () {
    test('a reader twice under a groundness-implying guard, its writer once',
        () {
      expect(_compileError('p(X, Y?) :- integer(X?) | Y = s(X?, X?).\n'),
          isEmpty);
    });

    test('a writer and its reader in the head', () {
      expect(_compileError('p(X, X?).\n'), isEmpty);
    });

    test('anonymous variables, each a writer of its own', () {
      expect(_compileError('p(_, _, f(_)).\n'), isEmpty);
    });
  });

  // The goal reduces with a clause whose head repeats a writer only if the
  // goal and the head have a writer mgu: the subterms the occurrences meet are
  // one term, or the goal suspends on its readers until they are, or fails.
  group('a repeated head writer runs as the writer mgu says', () {
    late GlpEngine two;
    setUpAll(() {
      two = _engineWith('''
procedure two(_?, _?).
two(R, R) :- ground(R?) | true.
''', 'two.glp');
    });

    final outcomes = {
      'two(3, 3)': ExecutionStatus.succeeded,
      'two(3, 4)': ExecutionStatus.failed,
      'two(f(1), f(1))': ExecutionStatus.succeeded,
      'two(f(1), f(2))': ExecutionStatus.failed,
      'two(f(1, 2), f(1, 3))': ExecutionStatus.failed,
      'two([1, 2], [1, 2])': ExecutionStatus.succeeded,
      'two([1, 2], [1, 3])': ExecutionStatus.failed,
      'two(3, f(3))': ExecutionStatus.failed,
      // An unbound reader: no writer mgu yet, one under a readers
      // substitution, so the goal suspends.
      'two(X?, 3)': ExecutionStatus.suspended,
      'two(3, X?)': ExecutionStatus.suspended,
      'two(f(Z?), f(1))': ExecutionStatus.suspended,
      // And resumes when the reader is assigned.
      'two(X?, 3), X = 3': ExecutionStatus.succeeded,
      'two(X?, 3), X = 4': ExecutionStatus.failed,
      'two(3, X?), X = 3': ExecutionStatus.succeeded,
      'two(3, X?), X = 4': ExecutionStatus.failed,
      'two(f(Z?), f(1)), Z = 1': ExecutionStatus.succeeded,
      'two(f(Z?), f(1)), Z = 2': ExecutionStatus.failed,
      'two(X?, Y?), X = a, Y = a': ExecutionStatus.succeeded,
      'two(X?, Y?), X = a, Y = b': ExecutionStatus.failed,
    };
    for (final MapEntry(key: goal, value: status) in outcomes.entries) {
      test('$goal ${status.name}', () async {
        final r = await two.runGoal(goal);
        expect(r.status, status, reason: '${r.error}');
      });
    }

    test('the case of b4cd4191: h1(same(4), 3) fails, h1(same(3), 3) succeeds',
        () async {
      final h1 = _engineWith('''
Same ::= same(Integer).
procedure h1(Same?, Integer?).
h1(same(To), To) :- ground(To?) | true.
''', 'h1.glp');
      expect((await h1.runGoal('h1(same(4), 3)')).status,
          ExecutionStatus.failed);
      expect((await h1.runGoal('h1(same(3), 3)')).status,
          ExecutionStatus.succeeded);
      expect((await h1.runGoal('h1(same(X?), 3), X = 4')).status,
          ExecutionStatus.failed);
      expect((await h1.runGoal('h1(same(X?), 3), X = 3')).status,
          ExecutionStatus.succeeded);
    });

    test('nested twice, and in a list', () async {
      final e = _engineWith('''
procedure p(_?, _?).
p(f(X), g(X)) :- integer(X?) | true.
procedure q(_?).
q([X, X]) :- X? > 0 | true.
''', 'nested.glp');
      expect((await e.runGoal('p(f(1), g(1))')).status,
          ExecutionStatus.succeeded);
      expect(
          (await e.runGoal('p(f(1), g(2))')).status, ExecutionStatus.failed);
      expect((await e.runGoal('q([3, 3])')).status, ExecutionStatus.succeeded);
      expect((await e.runGoal('q([3, 4])')).status, ExecutionStatus.failed);
    });
  });

  // The body under the writer mgu: each body occurrence of the writer is the
  // term the head gave it.
  group('a writer twice in the body runs, given the head\'s term', () {
    late GlpEngine e;
    setUpAll(() {
      e = _engineWith('''
procedure dups(_?, _, _).
dups(X, Y?, Z?) :- ground(X?) | Y = s(X), Z = s(X).
procedure dupt(_?, _, _).
dupt(X, Y?, Z?) :- ground(X?) | Y = X, Z = X.
''', 'dup.glp');
    });

    test('in a structure', () async {
      final r = await e.runGoal('dups(f(1, [a]), Y, Z)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_show(e, r.bindings['Y']), 's(f(1, .(a, [])))');
      expect(_show(e, r.bindings['Z']), 's(f(1, .(a, [])))');
    });

    test('at a goal\'s argument', () async {
      final r = await e.runGoal('dupt(g(h), Y, Z)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_show(e, r.bindings['Y']), 'g(h)');
      expect(_show(e, r.bindings['Z']), 'g(h)');
    });

    test('after the goal\'s reader is assigned', () async {
      final r = await e.runGoal('dupt(X?, Y, Z), X = 7');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_show(e, r.bindings['Y']), '7');
      expect(_show(e, r.bindings['Z']), '7');
    });
  });
}
