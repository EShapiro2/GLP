/// A list of any length is built by the runtime and posted in a goal.
///
/// TGLP well-typing.tex, Definition "Well-Typed Clause", puts no bound on a
/// term's size, and a program that loads runs: the runtime builds a clause's
/// list of 100000 elements in its head, and a goal posted as the REPL posts it
/// ([GlpEngine.runGoal]) carries one.  Until 2026-10-20 both overflowed the
/// stack: g(X) on a clause of 30000 integers recursed down the list's tail
/// building the structure at commit (bytecode/runner.dart,
/// _convertTentativeToStruct), and a goal holding a long list recursed on its
/// tail in the walk of its inputs and in building its terms (glp_engine.dart,
/// _requireInputs, _buildListTerm, _buildListTermForConj) (Code #11,
/// 2026-10-19 17:32 UTC; GLP #9 Cowork, 2026-10-20 09:22 UTC, item 3: "the
/// runtime and the REPL loop along a list's spine as the checker now does,
/// each with a 100000-element test").  The checker's walks loop along the
/// spine since 2026-10-19 (analysis/type_checker/long_list_test.dart).
///
/// Each program is written to a directory under the root, programs/ (TGLP
/// modules.tex, "Scope construction"), and loaded as the REPL loads a file.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

const int _n = 100000;

/// The integers 0 to [n] - 1, written as a list.
String _list(int n) => '[${List.generate(n, (i) => '$i').join(', ')}]';

/// The elements of the list [t], read along its spine, each dereferenced in
/// [engine]'s heap; null where [t] is not a proper list of integers.
List<int>? _ints(GlpEngine engine, rt.Term? t) {
  final heap = engine.runtime.heap;
  final out = <int>[];
  rt.Term? cur = t;
  while (true) {
    if (cur is rt.VarRef) cur = heap.dereference(cur);
    if (cur is rt.ConstTerm && (cur.value == rt.nil || cur.value == null)) {
      return out;
    }
    if (cur is! rt.StructTerm || cur.functor != '.' || cur.args.length != 2) {
      return null;
    }
    var h = cur.args[0];
    if (h is rt.VarRef) h = heap.dereference(h);
    if (h is! rt.ConstTerm || h.value is! int) return null;
    out.add(h.value as int);
    cur = cur.args[1];
  }
}

void main() {
  late Directory dir;

  setUp(() {
    dir = Directory('../programs/tests').createTempSync('glp_long_list_run_');
  });

  tearDown(() => dir.deleteSync(recursive: true));

  /// An engine with [source] loaded as the file [name] in [dir].
  GlpEngine load(String name, String source) {
    final file = File('${dir.path}/$name')..writeAsStringSync(source);
    // A list of $_n elements counted takes two reductions an element.
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf)..maxCycles = 1000000;
    expect(engine.loadFile(file.absolute.path), isTrue);
    return engine;
  }

  test('the runtime builds a clause\'s list of $_n integers in its head: '
      'g(X) binds X to it', () async {
    final engine = load('g.glp',
        'exported procedure g(Stream(Integer)).\ng(${_list(_n)}).\n');
    final result = await engine.runGoal('g(X)');
    expect(result.error, isNull);
    expect(result.status, ExecutionStatus.succeeded);
    final xs = _ints(engine, result.bindings['X']);
    expect(xs, isNotNull);
    expect(xs!.length, _n);
    expect(xs.first, 0);
    expect(xs.last, _n - 1);
  });

  test('a goal posted as the REPL posts it carries a list of $_n integers: '
      'len counts it', () async {
    final engine = load(
        'len.glp',
        'exported procedure len(Stream(Integer)?, Integer).\n'
            'len(Xs, N?) :- count(Xs?, 0, N).\n\n'
            'procedure count(Stream(Integer)?, Integer?, Integer).\n'
            'count([_|Xs], N, M?) :- N1 := N? + 1, count(Xs?, N1?, M).\n'
            'count([], N, N?).\n');
    final result = await engine.runGoal('len(${_list(_n)}, N)');
    expect(result.error, isNull);
    expect(result.status, ExecutionStatus.succeeded);
    final n = result.bindings['N'];
    expect(n, isA<rt.ConstTerm>());
    expect((n as rt.ConstTerm).value, _n);
  });

  test('a goal of a conjunction carries a list of $_n integers in each of its '
      'goals', () async {
    final engine = load(
        'pair.glp',
        'exported procedure len(Stream(Integer)?, Integer).\n'
            'len(Xs, N?) :- count(Xs?, 0, N).\n\n'
            'procedure count(Stream(Integer)?, Integer?, Integer).\n'
            'count([_|Xs], N, M?) :- N1 := N? + 1, count(Xs?, N1?, M).\n'
            'count([], N, N?).\n');
    final result = await engine
        .runGoal('len(${_list(_n)}, N), len(${_list(_n)}, M)');
    expect(result.error, isNull);
    expect(result.status, ExecutionStatus.succeeded);
    for (final v in ['N', 'M']) {
      final n = result.bindings[v];
      expect(n, isA<rt.ConstTerm>(), reason: v);
      expect((n as rt.ConstTerm).value, _n, reason: v);
    }
  });

  test('a goal carrying a list of $_n ill-typed elements is refused, its '
      'error printing the first failing path and the number of the rest',
      () async {
    final engine = load(
        'bad_goal.glp',
        'exported procedure len(Stream(Integer)?, Integer).\n'
            'len(Xs, N?) :- count(Xs?, 0, N).\n\n'
            'procedure count(Stream(Integer)?, Integer?, Integer).\n'
            'count([_|Xs], N, M?) :- N1 := N? + 1, count(Xs?, N1?, M).\n'
            'count([], N, N?).\n');
    final strings = List.generate(_n, (i) => "'s$i'").join(', ');
    final result = await engine.runGoal('len([$strings], N)');
    expect(result.status, ExecutionStatus.failed);
    final error = result.error!;
    expect(error, contains('Goal is not well-typed'));
    expect(error,
        contains('and ${_n - 1} more inconsistent paths of the term'));
    expect('Inconsistent path:'.allMatches(error).length, 1);
    expect(error.length, lessThan(2000));
  });

  test('a goal\'s list of $_n elements holding an input reader is walked to '
      'its last element: the input named is found there', () {
    final engine = load(
        'tail.glp',
        'exported procedure len(Stream(Integer)?, Integer).\n'
            'len(Xs, N?) :- count(Xs?, 0, N).\n\n'
            'procedure count(Stream(Integer)?, Integer?, Integer).\n'
            'count([_|Xs], N, M?) :- N1 := N? + 1, count(Xs?, N1?, M).\n'
            'count([], N, N?).\n');
    final ints = List.generate(_n - 1, (i) => '$i').join(', ');
    final posted = engine.postGoal('len([$ints | T?], N)', inputs: ['T']);
    expect(posted.inputs.keys, ['T']);
    var r = posted.scheduler.drainToQuiescence();
    expect(r.status, ExecutionStatus.suspended);
    posted.inputs['T']!.close().forEach(engine.runtime.gq.enqueue);
    r = posted.scheduler.drainToQuiescence();
    expect(r.status, ExecutionStatus.succeeded);
    final heap = engine.runtime.heap;
    final n = heap.dereference(rt.VarRef(posted.variables['N']!));
    expect(n, isA<rt.ConstTerm>());
    expect((n as rt.ConstTerm).value, _n - 1);
  });
}
