/// Communicate keeps the assignment: every occurrence of a ground-guarded
/// reader receives the writer's value, at the agent that assigned it and at
/// any other.
///
/// Normative source: the GLP language paper, GLP-Spec 555552c, maglp.tex,
/// Definition of the maGLP transition system, Communicate: "$\sigma'_p =
/// \sigma_p$" --- "It keeps the assignment in $\sigma_p$, as cGLP does, so
/// that every occurrence of $X?$ receives it, at $q$ or at any other agent";
/// glp.tex, Remark "Guards and SRSW": "$X?$ may then occur several times in a
/// goal, and each occurrence receives the ground term, since Communicate keeps
/// the assignment"; IGLP 84b36fe, maglp.tex, Communicate.  GLP, 2026-10-15
/// 11:00 UTC, item 3, on vGLP #13's case:
///
///   p(X, f(X?)) :- ground(X?) | q(X?).
///
/// with the goal p(a, Y), r(Y?): q receives a and r receives f(a), q and r at
/// one agent, and at two.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/multiagent/boot_loader.dart';
import 'package:glp_runtime/multiagent/isolate_manager.dart';
import 'package:test/test.dart';

String get _rootSelf => File('../programs/self.glp').absolute.path;

/// vGLP's p/2, and q/1 and r/1 that show their person what they receive:
/// programs/tests/communicate/ground_reader.glp, its comments dropped.
final String _procsPath =
    File('../programs/tests/communicate/ground_reader.glp').absolute.path;
final String _procs = File(_procsPath)
    .readAsLinesSync()
    .where((l) => !l.startsWith('%'))
    .join('\n');

/// q and r at one agent, alice.
final String _oneAgent = '''
procedure boot.
boot :- alice_init(alice, _)@alice.

procedure alice_init(_?, _?).
alice_init(_, _) :- p(a, Y), r(Y?).
$_procs''';

/// q at alice, r at bob: alice sends bob Y? before p is reduced, so bob's r
/// holds the reader of the variable p assigns f(X?) to.
final String _twoAgents = '''
procedure boot.
boot :- alice_init(alice, _)@alice, bob_init(bob, _)@bob.

procedure alice_init(_?, _?).
alice_init(_, _) :- send_to_net([msg(bob, Y?)]), p(a, Y).

procedure bob_init(_?, _?).
bob_init(_, [msg(_, Y) | _]) :- r(Y?).
$_procs''';

/// As [_twoAgents], with a itself communicated to alice: bob sends alice the
/// reader A? and then assigns A := a, so the ground-guarded reader alice's p
/// reads is assigned at bob, and its occurrences in q at alice and in f(X?),
/// sent back to bob, each receive it.
final String _fromBob = '''
procedure boot.
boot :- alice_init(alice, _)@alice, bob_init(bob, _)@bob.

procedure alice_init(_?, _?).
alice_init(_, [msg(_, A) | _]) :- send_to_net([msg(bob, Y?)]), p(A?, Y).

procedure bob_init(_?, _?).
bob_init(_, In) :- send_to_net([msg(alice, A?)]), give(A), take(In?).

procedure give(Constant).
give(a).

procedure take(_?).
take([msg(_, Y) | _]) :- r(Y?).
$_procs''';

void main() {
  test('one agent, one heap: q receives a and r receives f(a)', () async {
    final engine = GlpEngine(rootSelfGlpPath: _rootSelf);
    final lines = <String>[];
    engine.runtime.outputCallback = lines.add;
    expect(engine.loadFile(_procsPath), isTrue);
    final result = await engine.runGoal('p(a, Y), r(Y?)');
    expect(result.succeeded, isTrue, reason: '${result.error}');
    expect(lines.toSet(), {'q(a)', 'r(f(a))'});
  });

  group('agents in isolates', () {
    late IsolateManager manager;
    setUp(() => manager = IsolateManager());
    tearDown(() async => manager.shutdown());

    test('q and r at one agent: q receives a and r receives f(a)', () async {
      final config = BootLoader().load(_oneAgent);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice').toSet(), {'q(a)', 'r(f(a))'});
    }, timeout: const Timeout(Duration(seconds: 60)));

    test('q at alice and r at bob: q receives a and r receives f(a)',
        () async {
      final config = BootLoader().load(_twoAgents);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice'), ['q(a)']);
      expect(manager.outputOf('bob'), ['r(f(a))']);
    }, timeout: const Timeout(Duration(seconds: 60)));

    test('a assigned at bob, q at alice and r at bob: q receives a and r '
        'receives f(a)', () async {
      final config = BootLoader().load(_fromBob);
      config.rootSelfGlpPath = _rootSelf;
      await manager.boot(config);
      manager.start();
      await manager.settle(timeout: const Duration(seconds: 30));
      expect(manager.faults, isEmpty);
      expect(manager.outputOf('alice'), ['q(a)']);
      expect(manager.outputOf('bob'), ['r(f(a))']);
    }, timeout: const Timeout(Duration(seconds: 60)));
  });
}
