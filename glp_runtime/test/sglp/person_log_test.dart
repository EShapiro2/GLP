/// sGLP: agents, person/2 and the log.
///
/// Specification: svGLP, sections/sglp.tex at 52ca276 --- Definitions
/// "Simulation Program" (the compilation of M runs at each agent a; the
/// asking clause spawns p(., S), S derived from the run's seed, a and the
/// asked goal's identifier) and "Interface Variable, Log".  With GLP's answers
/// of 2026-09-27: the run declaration creates the agents, each with its
/// initial goal placed at it, and every goal a Reduce spawns inherits its
/// parent's agent; the asking clause calls person(T, X), declared in
/// programs/system/sglp.glp.  Fixtures: programs/tests/sglp/person_*.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/sglp/simulation.dart';
import 'package:glp_runtime/runtime/terms.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/sglp/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

const _agents3 = 'agent(1, 2, A), agent(2, 2, B), agent(3, 2, C)';

/// One entry of the log: t, a, the variable and the term.
class _Entry {
  final double t;
  final int agent;
  final String variable;
  final String term;
  _Entry(this.t, this.agent, this.variable, this.term);

  static _Entry parse(String line) {
    final f = line.split('\t');
    expect(f, hasLength(3), reason: line);
    final a = f[2].indexOf(' := ');
    expect(a, greaterThan(0), reason: line);
    return _Entry(double.parse(f[0]), int.parse(f[1]), f[2].substring(0, a),
        f[2].substring(a + 4));
  }

  @override
  String toString() => '$t $agent $variable := $term';
}

/// The constant [t] is bound to, through the heap.
Object? _value(GlpEngine engine, Term? t) {
  if (t == null) return null;
  final d = engine.runtime.heap.dereference(t);
  return d is ConstTerm ? d.value : d;
}

/// The elements of the list [t] is bound to, each a constant.
List<Object?> _list(GlpEngine engine, Term? t) {
  final out = <Object?>[];
  var cur = t == null ? null : engine.runtime.heap.dereference(t);
  while (cur is StructTerm && cur.functor == '.') {
    out.add(_value(engine, cur.args[0]));
    cur = engine.runtime.heap.dereference(cur.args[1]);
  }
  return out;
}

/// Run [goal] placed by [agents] on [engine], and return the log's lines.
Future<(ExecutionResult, List<String>)> _run(
    GlpEngine engine, String goal, List<int?> agents,
    {void Function(SimState)? onStart}) async {
  final lines = <String>[];
  engine.onSimulationLog = lines.add;
  engine.onSimulationStart = onStart;
  final r = await engine.runGoal(goal, agents: agents);
  return (r, lines);
}

void main() {
  group('placement', () {
    test('parsePlacement: agents, ranges and -', () {
      expect(GlpEngine.parsePlacement('1..3,-'), [1, 2, 3, null]);
      expect(GlpEngine.parsePlacement('2,-,1'), [2, null, 1]);
      expect(GlpEngine.parsePlacement('5..5'), [5]);
      expect(() => GlpEngine.parsePlacement('3..1'), throwsFormatException);
      expect(() => GlpEngine.parsePlacement('a'), throwsFormatException);
      expect(() => GlpEngine.parsePlacement('1,,2'), throwsFormatException);
    });

    test('each agent\'s initial goal is at it, and every goal a Reduce '
        'spawns is at its parent\'s agent, through a Release', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      final reduced = <(String, int?)>[];
      final (r, lines) = await _run(engine, _agents3, [1, 2, 3],
          onStart: (s) => s.onReduce =
              (sig, id, _) => reduced.add((sig, s.agentOf(id))));
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      // Every goal of the run descends from a placed initial goal.
      expect(reduced, isNotEmpty);
      for (final (sig, a) in reduced) {
        expect(a, inInclusiveRange(1, 3), reason: sig);
      }
      // lefty_do/righty_do are rated goals, reduced after their Release, at
      // the agent of the person goal that spawned them; ask1/3 at the agent
      // whose number it acknowledges.
      expect(reduced.where((e) => e.$1.endsWith('_do/2')), hasLength(6));
      final entries = lines.map(_Entry.parse).toList();
      for (final e in entries.where((e) => e.term.startsWith('ok('))) {
        expect(e.term, 'ok(${e.agent})', reason: '$e');
      }
    });

    test('a conjunct at no agent is no agent\'s: its person/2 fails and '
        'nothing of it is logged', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      final (r, lines) = await _run(
          engine, '$_agents3, ask(9, D)', [1, 2, 3, null]);
      expect(r.status, ExecutionStatus.failed);
      expect(engine.runtime.failedGoals,
          contains(startsWith('person(Pick?')));
      final entries = lines.map(_Entry.parse).toList();
      expect(entries, hasLength(12));
      expect(entries.where((e) => e.term.contains('9')), isEmpty);
    });

    test('a placement is refused where it does not place the run\'s agents',
        () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      Future<String?> refusal(String goal, List<int?> agents) async =>
          (await engine.runGoal(goal, agents: agents)).error;
      expect(await refusal(_agents3, [1, 2]),
          contains('names 2 agents for 3 conjuncts'));
      expect(await refusal(_agents3, [1, 2, 4]),
          contains('the run declares agents 1 to 3'));
      expect(await refusal(_agents3, [1, 2, 2]),
          contains('places none at 3'));
      expect(await refusal(_agents3, [1, 2, null]),
          contains('places none at 3'));

      final plain = _engine()..loadFile(_fixture('release_after.glp'));
      final r = await plain.runGoal('run(Y)', agents: [1]);
      expect(r.error, contains('only in a program with a run declaration'));
    });
  });

  group('person/2', () {
    test('spawns the agent\'s kind\'s person procedure for the type, with the '
        'seed of the run, the agent and the asked goal', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      final pop = engine.population!;
      final spawns = <PersonSpawn>[];
      final asked = <int>{};
      final (r, _) = await _run(engine, _agents3, [1, 2, 3], onStart: (s) {
        s.onPerson = spawns.add;
        s.onReduce = (sig, id, _) {
          if (sig == 'ask/2') asked.add(s.lineageOf(id));
        };
      });
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(spawns, hasLength(6));
      for (final p in spawns) {
        final want = pop.personProcedure(p.agent, 'Pick?')!;
        expect(p.type, 'Pick?');
        expect(p.kind, pop.kindsOf(p.agent).single);
        expect(p.procedure, want.procedure);
        expect(p.procedure, '${p.kind}_pick');
        expect(p.seed, pop.personSeed(p.agent, p.askedLineage));
      }
      // The asked goals are the goals of ask/2 the asking clause reduced.
      expect(spawns.map((p) => p.askedLineage).toSet(), asked);
      // Two per agent, each seed its own.
      for (var a = 1; a <= 3; a++) {
        expect(spawns.where((p) => p.agent == a), hasLength(2));
      }
      expect(spawns.map((p) => p.seed).toSet(), hasLength(6));
      // The person's choice reached the program through X.
      for (final v in ['A', 'B', 'C']) {
        expect(_list(engine, r.bindings[v]),
            [anyOf('left', 'right'), anyOf('left', 'right')]);
      }
    });

    test('registers X as an interactive variable of the asker\'s agent: the '
        'person\'s assignment to it is logged at that agent', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      final spawns = <PersonSpawn>[];
      final (r, lines) = await _run(engine, _agents3, [1, 2, 3],
          onStart: (s) => s.onPerson = spawns.add);
      expect(r.succeeded, isTrue, reason: '${r.error}');
      final picks =
          lines.map(_Entry.parse).where((e) => e.term.startsWith('pick('));
      expect(picks, hasLength(6));
      for (var a = 1; a <= 3; a++) {
        expect(picks.where((e) => e.agent == a), hasLength(2));
      }
    });

    test('fails with a diagnostic for a type the agent\'s person does not '
        'declare', () async {
      final source = File(_fixture('person_asks.glp'))
          .readAsStringSync()
          .replaceFirst("ask(N, C?) :- person('Pick?', X)",
              "ask(N, C?) :- person('Menu', X)");
      expect(source, contains("person('Menu', X)"));
      final engine = _engine()..loadSource(source);
      final (r, lines) = await _run(engine, _agents3, [1, 2, 3]);
      expect(r.status, ExecutionStatus.failed);
      expect(engine.runtime.failedGoals, contains(startsWith('person(Menu')));
      expect(lines, isEmpty);
    });

    test('a program that declares no population cannot call it', () {
      expect(
          () => _engine().loadFile(_fixture('person_not_imported.glp')),
          throwsA(predicate(
              (e) => '$e'.contains('Undefined procedure: person/2'))));
    });

    test('through the linker: person/2 a bare call, the person procedures '
        'kept though nothing in the program calls them', () async {
      final engine = _engine();
      expect(engine.loadProgram(_fixture('person_linked')), isTrue);
      final (r, lines) =
          await _run(engine, 'start(1, A), start(2, B)', [1, 2]);
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_value(engine, r.bindings['A']), 'left');
      expect(_value(engine, r.bindings['B']), 'left');
      final entries = lines.map(_Entry.parse).toList();
      expect(entries, hasLength(4));
      expect(entries.where((e) => e.term == 'ok(1)').single.agent, 1);
      expect(entries.where((e) => e.term == 'ok(2)').single.agent, 2);
    });
  });

  group('the log', () {
    test('one entry per assignment a Reduce at a makes to an interface '
        'variable of a, in the order of the run, with the clock', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      final (r, lines) = await _run(engine, _agents3, [1, 2, 3]);
      expect(r.succeeded, isTrue, reason: '${r.error}');
      final entries = lines.map(_Entry.parse).toList();
      // Each ask: the person's pick, and the program's acknowledgement.
      expect(entries, hasLength(12), reason: lines.join('\n'));
      for (var i = 1; i < entries.length; i++) {
        expect(entries[i].t, greaterThanOrEqualTo(entries[i - 1].t));
      }
      expect(entries.first.t, greaterThan(0));
      expect(entries.last.t, engine.simulatedTime);
      for (var a = 1; a <= 3; a++) {
        final mine = entries.where((e) => e.agent == a).toList();
        expect(mine, hasLength(4));
        for (var k = 0; k < 4; k += 2) {
          final pick = mine[k];
          final ack = mine[k + 1];
          // pick(Choice, V): the person writes the question's variable X,
          // whose term holds the writer the program then acknowledges.
          final m = RegExp(r'^pick\((left|right), (V\d+)\)$')
              .firstMatch(pick.term);
          expect(m, isNotNull, reason: '$pick');
          expect(ack.variable, m!.group(2));
          expect(ack.term, 'ok($a)');
          expect(ack.t, pick.t);
        }
      }
      // Every assigned variable is assigned once.
      final assigned = entries.map((e) => e.variable).toList();
      expect(assigned.toSet(), hasLength(assigned.length));
    });

    test('two runs with one seed write the same bytes, and two seeds do not',
        () async {
      Future<String> logOf(String source) async {
        final engine = _engine()..loadSource(source);
        final (r, lines) = await _run(engine, _agents3, [1, 2, 3]);
        expect(r.succeeded, isTrue, reason: '${r.error}');
        return lines.join('\n');
      }

      final file = _fixture('person_asks.glp');
      final a = _engine()..loadFile(file);
      final (_, l1) = await _run(a, _agents3, [1, 2, 3]);
      final (_, l2) = await _run(a, _agents3, [1, 2, 3]); // one engine, again
      final b = _engine()..loadFile(file);
      final (_, l3) = await _run(b, _agents3, [1, 2, 3]); // another engine
      expect(l1, isNotEmpty);
      expect(l2.join('\n'), l1.join('\n'));
      expect(l3.join('\n'), l1.join('\n'));

      final source = File(file).readAsStringSync();
      expect(source, contains('seed 11.'));
      final s11 = await logOf(source);
      final s12 = await logOf(source.replaceFirst('seed 11.', 'seed 12.'));
      expect(s11, l1.join('\n'));
      expect(s12, isNot(s11));
    });

    test('no log is kept, and no variable tracked, where there is nowhere to '
        'write it', () async {
      final engine = _engine()..loadFile(_fixture('person_asks.glp'));
      SimState? sim;
      engine.onSimulationStart = (s) => sim = s;
      final r = await engine.runGoal(_agents3, agents: [1, 2, 3]);
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(sim!.log, isNull);
      expect(engine.runtime.heap.onAssign, isNull);
    });
  });
}
