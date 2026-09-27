/// sGLP: the rated goal, the simulated clock, Release at quiescence, the seed,
/// and the population syntax with its checks.
///
/// Specification: svGLP, sections/sglp.tex at 52ca276 --- Definitions "Rated
/// Goal", "Configuration, Pending, Quiescent", "sGLP Transition System",
/// "Dual, Person Procedure, Person Declaration, Kind, Dimension, Population"
/// and Proposition "Time to the Next Release".  Tests (i), (ii), (iv) and (v)
/// of svGLP's work item of 2026-09-27 16:58 UTC are the groups so named.
/// Fixtures: programs/tests/sglp/.
library;

import 'dart:io';
import 'dart:math' as math;

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:glp_runtime/compiler/ast.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/glp_printer.dart';
import 'package:glp_runtime/bytecode/opcodes.dart' show SpawnRated;
import 'package:glp_runtime/wire/instruction_codec.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/sglp/simulation.dart';
import 'package:glp_runtime/sglp/draws.dart' as draws;

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/sglp/$name').absolute.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _root);

Module _parse(String source) => Parser(Lexer(source).tokenize()).parseModule();

/// One event of a run, in the order of the run: a Release or a reduction,
/// with the clock.
class _Event {
  final String kind; // 'release' | 'reduce'
  final String signature;
  final double clock;
  _Event(this.kind, this.signature, this.clock);
  @override
  String toString() => '$kind $signature @ $clock';
}

/// Record every Release and every reduction of the run [engine] starts next.
List<_Event> _record(GlpEngine engine, {int? maxReleases}) {
  final events = <_Event>[];
  engine.onSimulationStart = (SimState s) {
    s.maxReleases = maxReleases;
    s.onRelease = (r) => events.add(_Event('release', r.signature, r.time));
    s.onReduce = (sig, _, clock) => events.add(_Event('reduce', sig, clock));
  };
  return events;
}

String _int(Object? term) => '$term'.replaceAll(RegExp(r'[^0-9]'), '');

void main() {
  group('rated goal syntax', () {
    test('Goal @ Rate parses as a rated goal, the rate per simulated second', () {
      final m = _parse('''
procedure p(Integer?).
p(X) :- q(X?) @ 1/week, r(X?) @ 2.5/day, s(X?) @ 3/hour.
''');
      final body = m.procedures.single.clauses.single.body!;
      expect(body, everyElement(isA<RatedGoal>()));
      final rated = body.cast<RatedGoal>();
      expect(rated[0].innerGoal.functor, 'q');
      expect(rated[0].rateText, '1/week');
      expect(rated[0].ratePerSecond, closeTo(1 / 604800, 1e-18));
      expect(rated[1].ratePerSecond, closeTo(2.5 / 86400, 1e-15));
      expect(rated[2].ratePerSecond, closeTo(3 / 3600, 1e-15));
    });

    test('every time unit of the grammar', () {
      const units = {
        'second': 1.0,
        'minute': 60.0,
        'hour': 3600.0,
        'day': 86400.0,
        'week': 604800.0,
        'year': 31557600.0,
      };
      for (final u in units.entries) {
        final m = _parse('p :- q @ 1/${u.key}.\n');
        final g = m.procedures.single.clauses.single.body!.single as RatedGoal;
        expect(g.ratePerSecond, closeTo(1 / u.value, 1e-18), reason: u.key);
      }
    });

    test('a rate that is not a positive real per unit is rejected', () {
      expect(() => _parse('p :- q @ 0/day.\n'), throwsA(anything));
      expect(() => _parse('p :- q @ -1/day.\n'), throwsA(anything));
      expect(() => _parse('p :- q @ 1/fortnight.\n'), throwsA(anything));
      expect(() => _parse('p :- q @ 1/weeks.\n'), throwsA(anything));
      expect(() => _parse('p :- q @ 1.\n'), throwsA(anything));
    });

    test('a rated goal cannot stand before the guard bar', () {
      expect(() => _parse('p(X) :- q(X?) @ 1/day | true.\n'), throwsA(anything));
    });

    test('Goal@AgentId is still the spawn annotation', () {
      final m = _parse('p :- q@alice.\n');
      expect(m.procedures.single.clauses.single.body!.single, isA<SpawnGoal>());
    });

    test('M # p(...) @ r is the rated goal of the remote goal', () {
      final m = _parse('p :- m # q(1) @ 1/day.\n');
      final g = m.procedures.single.clauses.single.body!.single;
      expect(g, isA<RatedGoal>());
      expect((g as RatedGoal).innerGoal, isA<RemoteGoal>());
    });

    test('the printer writes the rate back', () {
      final m = _parse('p(X) :- q(X?) @ 1/week.\n');
      final g = m.procedures.single.clauses.single.body!.single;
      expect(GlpPrinter().printGoal(g), 'q(X?) @ 1/week');
    });
  });

  group('rated goal compilation', () {
    test('a rated goal compiles to spawn_rated, which the wire carries', () {
      final prog = GlpCompiler().compile('''
procedure p(Integer?).
p(X) :- q(X?) @ 1/week.
procedure q(Integer?).
q(_).
''');
      final rated = prog.ops.whereType<SpawnRated>().toList();
      expect(rated, hasLength(1));
      expect(rated.single.procedureLabel, 'q/1');
      expect(rated.single.ratePerSecond, closeTo(1 / 604800, 1e-18));

      final w = WireWriter();
      encodeInstruction(w, rated.single,
          procIndexOf: (_) => 3, ctargetOf: (_) => 0);
      final back = decodeInstruction(WireReader(w.toBytes()),
          procNameOf: (i) => i == 3 ? 'q/1' : '?', ctargetLabelOf: (i) => '#$i');
      expect(back, isA<SpawnRated>());
      expect((back as SpawnRated).procedureLabel, 'q/1');
      expect(back.arity, 1);
      expect(back.ratePerSecond, rated.single.ratePerSecond);
    });

    test('a rated goal is typed as its goal', () {
      // The same verdict, and the same diagnostic, rated and unrated.
      String verdict(String body) {
        try {
          _engine().loadSource('procedure p.\np :- $body.\n'
              'procedure q(Integer?).\nq(_).\n');
          return 'loads';
        } catch (e) {
          return '$e';
        }
      }

      for (final g in ['q(1)', 'q(abc)', 'q(1, 2)']) {
        final unrated = verdict(g);
        expect(verdict('$g @ 1/day'), unrated, reason: g);
      }
      expect(verdict('q(abc) @ 1/day'), contains('Body atom 0 (q)'));
      expect(verdict('q(1) @ 1/day'), 'loads');
    });

    test('a program with no rated goal and no run starts no sGLP run', () async {
      final engine = _engine();
      engine.loadSource('''
procedure p(Integer).
p(1).
''');
      final r = await engine.runGoal('p(X)');
      expect(r.succeeded, isTrue);
      expect(engine.isSimulation, isFalse);
      expect(engine.simulation, isNull);
    });
  });

  group('(i) a rated goal is not reduced before its release', () {
    test('the rated goal reduces after its Release, at its activation time',
        () async {
      final engine = _engine()..loadFile(_fixture('release_after.glp'));
      final events = _record(engine);
      final r = await engine.runGoal('run(Y)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_int(r.bindings['Y']), '1');

      final release = events.indexWhere(
          (e) => e.kind == 'release' && e.signature == 'finish/1');
      final reduce = events.indexWhere(
          (e) => e.kind == 'reduce' && e.signature == 'finish/1');
      expect(release, isNonNegative, reason: '$events');
      expect(reduce, greaterThan(release), reason: '$events');
      // Only one finish/1 reduction, and none before the Release.
      expect(events.where((e) => e.signature == 'finish/1' && e.kind == 'reduce'),
          hasLength(1));
      expect(events[release].clock, greaterThan(0));
      expect(events[reduce].clock, events[release].clock);
      // The goal that spawned it reduced at time 0: a machine step takes no
      // simulated time.
      expect(events.firstWhere((e) => e.signature == 'run/1').clock, 0);
      expect(engine.simulatedTime, events[release].clock);
    });

    test('with its Release withheld, the rated goal stays pending, unreduced',
        () async {
      final engine = _engine()..loadFile(_fixture('release_after.glp'));
      engine.onSimulationStart = (s) => s.releaseEnabled = false;
      final r = await engine.runGoal('run(Y)');
      expect(r.bindings['Y'], isNull, reason: 'finish/1 reduced unreleased');
      expect(engine.simulation!.pendingCount, 1);
      expect(engine.simulation!.releases, 0);
      expect(engine.simulatedTime, 0);
      // A pending goal is a goal of the configuration that is not runnable
      // now: the run did not succeed.
      expect(r.status, ExecutionStatus.suspended);
    });

    test('a released goal whose input is not there reduces when it arrives',
        () async {
      final engine = _engine()..loadFile(_fixture('input_arrives.glp'));
      final events = _record(engine);
      final r = await engine.runGoal('run(Y)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_int(r.bindings['Y']), '2');

      final relWait = events.indexWhere(
          (e) => e.kind == 'release' && e.signature == 'wait_for/2');
      final relSupply = events.indexWhere(
          (e) => e.kind == 'release' && e.signature == 'supply/1');
      final redSupply = events.indexWhere(
          (e) => e.kind == 'reduce' && e.signature == 'supply/1');
      final redWait = events.indexWhere(
          (e) => e.kind == 'reduce' && e.signature == 'wait_for/2');
      // wait_for/2 is released first (1000/second against 1/second), with its
      // input absent: it suspends, and does not reduce.
      expect(relWait, lessThan(relSupply), reason: '$events');
      expect(redWait, greaterThan(relSupply), reason: '$events');
      // It reduces once supply/1 has bound X, at supply's Release time: no
      // further delay, since the clock moves only at a Release.
      expect(redSupply, greaterThan(relSupply));
      expect(redWait, greaterThan(redSupply));
      expect(events[redWait].clock, events[relSupply].clock);
      expect(events[relSupply].clock, greaterThan(events[relWait].clock));
    });
  });

  group('(ii) release happens only at quiescence', () {
    test('an unrated busy goal runs to rest before the rated goal is released',
        () async {
      final engine = _engine()..loadFile(_fixture('quiescence_first.glp'));
      engine.maxCycles = 1000000;
      final events = _record(engine);
      final r = await engine.runGoal('run(Y, Z)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_int(r.bindings['Y']), '1');
      expect(_int(r.bindings['Z']), '0');

      final counts = [
        for (var i = 0; i < events.length; i++)
          if (events[i].kind == 'reduce' && events[i].signature == 'count/2') i
      ];
      expect(counts, hasLength(10001));
      final release = events.indexWhere((e) => e.kind == 'release');
      // The rate is 1000/second, so its activation time is about a
      // millisecond away; still, no Release comes before the countdown is at
      // rest, and every countdown step is at simulated time 0.
      expect(release, greaterThan(counts.last), reason: 'released too early');
      expect(counts.every((i) => events[i].clock == 0), isTrue);
      expect(events.where((e) => e.kind == 'release'), hasLength(1));
      expect(events.last.signature, 'finish/1');
      expect(events.last.clock, greaterThan(0));
    });
  });

  group('(ii) a posted conjunction is one initial configuration', () {
    test('no Release is taken until every conjunct is in the machine',
        () async {
      final engine = _engine()..loadFile(_fixture('release_after.glp'));
      final events = _record(engine);
      final r = await engine.runGoal('run(Y), run(Z)');
      expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
      expect(_int(r.bindings['Y']), '1');
      expect(_int(r.bindings['Z']), '1');
      final runs = [
        for (var i = 0; i < events.length; i++)
          if (events[i].signature == 'run/1') i
      ];
      final firstRelease = events.indexWhere((e) => e.kind == 'release');
      expect(runs, hasLength(2));
      expect(firstRelease, greaterThan(runs.last), reason: '$events');
      expect(events.where((e) => e.kind == 'release'), hasLength(2));
    });
  });

  group('(iv) the law of the time to the next release', () {
    const rates = {'a/1': 1.0, 'b/1': 2.0, 'c/1': 5.0};
    const total = 8.0;

    /// The inter-release times and the released procedures of a run of
    /// [n] Releases of release_law.glp under [seed].
    Future<(List<double>, List<String>)> run(int seed, int n) async {
      final engine = _engine()..loadFile(_fixture('release_law.glp'));
      engine.maxCycles = 100000000;
      engine.simulationSeed = seed;
      final times = <double>[];
      final procs = <String>[];
      engine.onSimulationStart = (s) {
        s.maxReleases = n;
        s.onRelease = (r) {
          times.add(r.time);
          procs.add(r.signature);
        };
      };
      await engine.runGoal('run');
      final gaps = <double>[
        for (var i = 0; i < times.length; i++)
          times[i] - (i == 0 ? 0 : times[i - 1])
      ];
      return (gaps, procs);
    }

    test('m = 3 pending goals of rates 1, 2, 5: a thousand releases', () async {
      final (gaps, procs) = await run(0, 1000);
      expect(gaps, hasLength(1000));
      final mean = gaps.reduce((a, b) => a + b) / gaps.length;
      final variance =
          gaps.map((g) => (g - mean) * (g - mean)).reduce((a, b) => a + b) /
              (gaps.length - 1);
      // Exponential with rate R = 8: mean 1/R, variance 1/R^2, within a tenth.
      print('(iv) seed 0, 1000 releases: mean $mean (1/R = ${1 / total}), '
          'variance $variance (1/R^2 = ${1 / (total * total)})');
      expect((mean - 1 / total).abs(), lessThanOrEqualTo(0.1 / total));
      expect((variance - 1 / (total * total)).abs(),
          lessThanOrEqualTo(0.1 / (total * total)));
      // Each Release is of goal i with probability r_i / R.
      for (final e in rates.entries) {
        final share = procs.where((p) => p == e.key).length / procs.length;
        print('(iv) share of ${e.key}: $share (r/R = ${e.value / total})');
        expect((share - e.value / total).abs(), lessThan(0.05));
      }
    });

    test('two runs with one seed release the same goals at the same times',
        () async {
      final (g1, p1) = await run(20260927, 200);
      final (g2, p2) = await run(20260927, 200);
      final (g3, _) = await run(1, 200);
      expect(g1, g2);
      expect(p1, p2);
      expect(g1, isNot(g3));
    });
  });

  group('(v) the population syntax and its checks', () {
    test('the kinds and the run declaration of the social graph load', () {
      final engine = _engine();
      expect(engine.loadFile(_fixture('population_social_graph.glp')), isTrue);
      final pop = engine.population!;
      expect(pop.agents, 1000);
      expect(pop.seed, 20260927);
      expect(pop.untilText, '5 years');
      expect(pop.untilSeconds, 5 * 31557600.0);
      expect([for (final d in pop.dimensions) d.name], ['approach', 'response']);
      expect(pop.dimensions[0].kinds, ['homophile', 'indifferent']);
      expect(pop.dimensions[1].probabilities, [0.7, 0.3]);
      expect(pop.kinds['wary']!.personProcedures, {'Card': 'wary_card'});
      expect(engine.isSimulation, isTrue);
    });

    test('each agent draws one kind per dimension from the seed', () {
      final engine = _engine()..loadFile(_fixture('population_social_graph.glp'));
      final pop = engine.population!;
      final homophile = [
        for (var a = 1; a <= pop.agents; a++) pop.kindOf(a, 0)
      ].where((k) => k == 'homophile').length;
      final wary = [
        for (var a = 1; a <= pop.agents; a++) pop.kindOf(a, 1)
      ].where((k) => k == 'wary').length;
      // 0.6 and 0.7 of a thousand, within four standard deviations.
      expect((homophile - 600).abs(), lessThan(4 * math.sqrt(1000 * .6 * .4)));
      expect((wary - 700).abs(), lessThan(4 * math.sqrt(1000 * .7 * .3)));
      // The draw is the seed's: a second engine draws the same.
      final again = _engine()..loadFile(_fixture('population_social_graph.glp'));
      for (var a = 1; a <= pop.agents; a++) {
        expect(again.population!.kindsOf(a), pop.kindsOf(a));
      }
      // Each agent's stochastic person declares Menu and Card once each.
      final k1 = pop.kindsOf(1);
      expect(pop.personProcedure(1, 'Menu')!.procedure, '${k1[0]}_menu');
      expect(pop.personProcedure(1, 'Card')!.procedure, '${k1[1]}_card');
      expect(pop.personProcedure(1, 'Offer'), isNull);
    });

    test('the seed handed to a person goal is the seed, agent and goal\'s', () {
      final engine = _engine()..loadFile(_fixture('population_social_graph.glp'));
      final pop = engine.population!;
      final s = pop.personSeed(17, 12345);
      expect(s, inInclusiveRange(1, 2147483646));
      expect(pop.personSeed(17, 12345), s);
      expect(pop.personSeed(18, 12345), isNot(s));
      expect(pop.personSeed(17, 12346), isNot(s));
      expect(draws.personSeed(1, 17, 12345), isNot(s));
      expect(() => pop.personSeed(1001, 1), throwsA(isA<RangeError>()));
    });

    test('a directory program: rated goals through the linker, and a run '
        'naming kinds of another module', () async {
      final engine = _engine();
      expect(engine.loadProgram(_fixture('linked')), isTrue);
      final pop = engine.population!;
      expect(pop.agents, 4);
      expect(pop.kinds.keys, containsAll(['quick', 'slow']));
      expect(pop.kinds['quick']!.module, 'kinds');
      final events = _record(engine);
      final r = await engine.runGoal('run(X, Y)');
      expect(r.succeeded, isTrue, reason: '${r.error}');
      expect(_int(r.bindings['X']), '1');
      expect(_int(r.bindings['Y']), '2');
      expect(events.where((e) => e.kind == 'release'), hasLength(2));
      expect(engine.simulation!.horizon, 3600);
      expect(engine.simulation!.seed, 7);
    });

    void rejects(String fixture, Pattern message) {
      final engine = _engine();
      expect(
          () => engine.loadFile(_fixture(fixture)),
          throwsA(predicate((e) => '$e'.contains('sGLP population check') &&
              '$e'.contains(message))),
          reason: fixture);
    }

    test('rejects a person procedure whose first argument is not the dual',
        () => rejects('population_wrong_dual.glp', 'the dual of Menu'));

    test('rejects a person procedure whose second argument is not Integer?',
        () => rejects('population_wrong_seed.glp',
            'is declared procedure pick_menu(Menu?, Integer)'));

    test('rejects a dimension whose kinds declare different types',
        () => rejects('population_dimension_types.glp',
            'declare different interactive types'));

    test('rejects a dimension whose probabilities do not sum to one',
        () => rejects('population_probabilities.glp', 'not to one'));

    test('0.7 + 0.3 sums to one', () {
      final engine = _engine();
      engine.loadSource('''
Menu ::= menu(Choice?).
Choice ::= left ; right.
person lefty.
Menu =::= lefty_menu.
procedure lefty_menu(Menu?, Integer?).
lefty_menu(menu(left), _).
person righty.
Menu =::= righty_menu.
procedure righty_menu(Menu?, Integer?).
righty_menu(menu(right), _).
run 3 agents [ d ~ (lefty : 0.7 ; righty : 0.3) ] until 2 weeks seed 5.
''');
      expect(engine.population!.untilSeconds, 2 * 604800.0);
    });

    test('rejects the malformed: a kind the run does not declare, a person '
        'declaration outside a kind, one naming no procedure of its kind', () {
      const types = 'Menu ::= menu(Choice?).\nChoice ::= left ; right.\n';
      expect(
          () => _engine().loadSource('${types}person a.\nMenu =::= am.\n'
              'procedure am(Menu?, Integer?).\nam(menu(left), _).\n'
              'run 2 agents [ d ~ (b : 1.0) ] until 1 day seed 1.\n'),
          throwsA(predicate((e) => '$e'.contains('no "person b."'))));
      expect(() => _parse('${types}Menu =::= am.\n'),
          throwsA(predicate((e) => '$e'.contains('outside a kind'))));
      expect(
          () => _engine().loadSource('${types}procedure am(Menu?, Integer?).\n'
              'am(menu(left), _).\nperson a.\nMenu =::= am.\n'
              'run 2 agents [ d ~ (a : 1.0) ] until 1 day seed 1.\n'),
          throwsA(predicate(
              (e) => '$e'.contains('names no binary procedure of the kind'))));
    });
  });
}
