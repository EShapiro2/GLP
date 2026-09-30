/// sGLP person/2 with interactive types in writer mode.
///
/// Specification: svGLP, sections/sglp.tex at 115ed79, Sections "Example: The
/// Kinds of the Social Graph" and "Example: Coins Among Friends", with
/// sections/example.tex's program: Menu, Card and Offer are in writer mode,
/// so the asking clause passes person/2 the reader of the interactive
/// variable, person('Menu', X?), before the call that writes it (vGLP,
/// sections/elicitation.tex, Definition "Canonical Compilation": X and X?
/// exchanged in writer mode).  person/2, procedure(X) person(Constant?, X),
/// then binds X to the input type Menu? (TGLP, parameterized-types.tex,
/// Definition "Instantiation").  Fixtures: programs/tests/sglp/
/// person_asks_writer.glp and person_asks_coins.glp, compiled by hand.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';

final _root = File('../programs/self.glp').absolute.path;
String _fixture(String name) =>
    File('../programs/tests/sglp/$name').absolute.path;

/// Run [goal] placed by [agents] on a fresh engine with [fixture] loaded;
/// return the result and the log's lines, each (t, a, V := s).
Future<(ExecutionResult, List<List<String>>)> _run(
    String fixture, String goal, List<int?> agents) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)..loadFile(_fixture(fixture));
  final lines = <String>[];
  engine.onSimulationLog = lines.add;
  final r = await engine.runGoal(goal, agents: agents);
  return (r, [for (final l in lines) l.split('\t')]);
}

void main() {
  test('the social graph: Menu and Card in writer mode load, the program '
      'writes the menu and the card and the person answers them', () async {
    final (r, log) = await _run(
        'person_asks_writer.glp',
        'start(a, [c], [b, d], [msg(b, offer(b))], O1), '
            'start(b, [d], [a, c], [], O2)',
        [1, 2]);
    expect(r.error, isNull);
    // The run is at its horizon with pending goals, not failed.
    expect(r.status, isNot(ExecutionStatus.failed));
    String terms(int a) =>
        log.where((e) => e[1] == '$a').map((e) => e[2]).join(' ');
    // Each agent's program writes its first menu; agent 1 writes the card
    // for b's offer, and its person answers it yes or no.
    expect(terms(1), contains('menu([c], [b, d],'));
    expect(terms(2), contains('menu([d], [a, c],'));
    expect(terms(1), contains('card(b,'));
    expect(terms(1), matches(RegExp(r':= (yes|no)\b')));
    // A person answers a menu with a choice.
    expect(log.map((e) => e[2]).join(' '),
        matches(RegExp(r':= (same|other)\(')));
    expect(r.bindings['O1'].toString(), contains('reply'));
  });

  test('coins among friends: Menu and Offer in writer mode load, and a '
      'proposal is put to the person as an offer and answered', () async {
    final (r, log) = await _run('person_asks_coins.glp',
        'start(a, [b], O2?, O1), start(b, [a], O1?, O2)', [1, 2]);
    expect(r.error, isNull);
    expect(r.status, isNot(ExecutionStatus.failed));
    final all = log.map((e) => e[2]).join(' ');
    expect(all, contains('menu([b], [],'));
    expect(all, contains('menu([a], [],'));
    // An empty wallet proposes a swap of ten; the friend is offered it.
    expect(all, matches(RegExp(r':= swap\(V\d+\?, 10\)')));
    expect(all, matches(RegExp(r':= offer\([ab], 10, \d+, V\d+\)')));
    expect(all, matches(RegExp(r':= (yes|no)\b')));
  });
}
