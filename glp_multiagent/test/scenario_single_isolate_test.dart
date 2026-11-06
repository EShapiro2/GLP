/// Single-isolate scenario: ONE heap, real agent/4 + ui_mediator/5 for bob,
/// alice, charlie; in-heap crossbar (no MAD). Bob is the live-UI agent (UserIn
/// injected, notifies observed); alice/charlie are scenario-driven. Validates
/// the substrate the app will use.
@Timeout.none
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'isolate_agents.dart';
import 'programs_dir.dart';

final _programs = programsDir();

/// The one program bob's agent runs: the scenario, with agent/4 and
/// ui_mediator/5 reached through agent_roundtrip/self.glp.
final _program = '$_programs/tests/agent_roundtrip/play_scenario';

void main() {
  test('scenario auto-drives bob inbox; accept -> connected (single isolate)',
      () async {
    final agents = IsolateAgents();
    addTearDown(agents.dispose);
    await agents.spawn('bob', _program, '$_programs/self.glp',
        deferStart: false);
    await agents.until('bob initialised', () => agents.started.contains('bob'));
    // The person's first act tells the agent its name, the agent started from
    // agent(ch(_?,_),ch(_?,_)) (GLP-Spec maglp.tex, Definition "Multiagent
    // GLP"; IGLP, Implementation Notes, "Boot").
    agents.input('bob', const GAtom('bob'));
    final out = agents.out['bob']!;

    // alice and charlie both cold-call bob -> two befriend cards.
    await agents.until('two befriend cards',
        () => agents.has('bob', 'befriend(alice, ') &&
            agents.has('bob', 'befriend(charlie, '));
    final gotAlice = agents.has('bob', 'befriend(alice, ');
    final gotCharlie = agents.has('bob', 'befriend(charlie, ');

    // Accept each card with its actual req id (assigned by bob's mediator in
    // arrival order, so parse it rather than assume).
    final reqOf = RegExp(r'befriend\((\w+), req\((\d+)\)\)');
    final accepted = <String>{};
    for (final l in out.where((l) => l.contains('befriend(')).toList()) {
      final m = reqOf.firstMatch(l);
      if (m != null && accepted.add(m.group(1)!)) {
        agents.input('bob', GStruct('decision', [
          const GAtom('yes'),
          GAtom(m.group(1)!),
          GStruct('req', [GInt(int.parse(m.group(2)!))]),
        ]));
      }
    }
    await agents.until('both connected',
        () => agents.has('bob', 'connected(alice)') &&
            agents.has('bob', 'connected(charlie)'));
    final connAlice = agents.has('bob', 'connected(alice)');
    final connCharlie = agents.has('bob', 'connected(charlie)');

    await agents.settle();
    File('/private/tmp/scen-log.txt')
        .writeAsStringSync(agents.logs['bob']!.join('\n'));
    // ignore: avoid_print
    print('BOB OUTPUT:\n${out.where((l) => l.startsWith('< ')).join('\n')}');

    expect(gotAlice && gotCharlie, isTrue, reason: 'two befriend cards');
    expect(connAlice, isTrue, reason: 'accept alice -> connected(alice)');
    expect(connCharlie, isTrue, reason: 'accept charlie -> connected(charlie)');
  });
}
