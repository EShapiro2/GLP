/// Headless end-to-end round-trip over the REAL two-isolate path.
///
/// Spawns alice + bob as agent isolates (exactly like the live app), routes
/// every AgentSendMad as DeliverMad to the target, and drives:
///   alice: connect(bob)  →  bob shows befriend(alice, req(1))
///   bob:   decision(yes, alice, req(1))  →  BOTH see connected(...)
///
/// This exercises the cross-isolate response variable (a `_w` mutual-ref
/// reader) — the exact case the mediator polarity fix had to handle. A
/// single-heap test would not, since there the response slot is a plain
/// writer.
@Timeout.none
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'isolate_agents.dart';
import 'programs_dir.dart';

final _programs = programsDir();

/// The one program each agent runs: agent/4, ui_mediator/5 and the
/// interactive agent_init/3, reached through agent_roundtrip/self.glp.
final _program = '$_programs/tests/agent_roundtrip/play_ui';

void main() {
  test('connect → accept → connected round-trips on both sides', () async {
    final rootSelf = '$_programs/self.glp';

    final agents = IsolateAgents();
    addTearDown(agents.dispose);
    await agents.spawn('alice', _program, rootSelf);
    await agents.spawn('bob', _program, rootSelf);

    // All ports registered (deferred-start), then release GLP init.
    await agents.ready();
    agents.start('alice');
    agents.start('bob');
    // Each person's first act tells the agent its name, the agent started
    // from agent(ch(_?,_),ch(_?,_)) (GLP-Spec maglp.tex, Definition
    // "Multiagent GLP"; IGLP, Implementation Notes, "Boot").
    agents.input('alice', const GAtom('alice'));
    agents.input('bob', const GAtom('bob'));
    await agents.until('both initialized',
        () => agents.started.containsAll(['alice', 'bob']));

    // Alice cold-calls Bob.
    agents.input('alice', const GStruct('connect', [GAtom('bob')]));
    await agents.until('bob received the befriend notify',
        () => agents.has('bob', 'befriend(alice, req(1))'));

    // Bob accepts.
    agents.input('bob', const GStruct('decision',
        [GAtom('yes'), GAtom('alice'), GStruct('req', [GInt(1)])]));
    await agents.until('both connected',
        () => agents.has('bob', 'connected(alice)') &&
            agents.has('alice', 'connected(bob)'));

    File('/private/tmp/rt-alice-log.txt')
        .writeAsStringSync(agents.logs['alice']!.join('\n'));
    File('/private/tmp/rt-bob-log.txt')
        .writeAsStringSync(agents.logs['bob']!.join('\n'));
    // ignore: avoid_print
    print('ALICE OUT:\n${agents.out['alice']!.join('\n')}');
    // ignore: avoid_print
    print('BOB OUT:\n${agents.out['bob']!.join('\n')}');

    expect(agents.has('bob', 'connected(alice)'), isTrue,
        reason: 'bob emits connected(alice)');
    expect(agents.has('alice', 'connected(bob)'), isTrue,
        reason: 'alice emits connected(bob)');
  });
}
