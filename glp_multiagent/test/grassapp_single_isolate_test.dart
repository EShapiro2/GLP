/// Live GrassApp scenario, single isolate: alice/charlie cold-call Bob; on
/// accept they message him. Checks the messaging path (text over the friend
/// channel → `received`) actually reaches Bob's UI output.
@Timeout.none
library;


import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'isolate_agents.dart';
import 'programs_dir.dart';

final _programs = programsDir();

/// The one program Bob's agent runs: the scenario, with agent/4 and
/// ui_mediator/5 reached through agent_roundtrip/self.glp.
final _program = '$_programs/tests/agent_roundtrip/play_grassapp';

void main() {
  test('accept a friend → actor messages Bob → received reaches Bob', () async {
    final agents = IsolateAgents();
    addTearDown(agents.dispose);
    await agents.spawn('Bob', _program, '$_programs/self.glp',
        deferStart: false);
    await agents.until('Bob initialised', () => agents.started.contains('Bob'));
    // The person's first act tells the agent its name, the agent started from
    // agent(ch(_?,_),ch(_?,_)) (GLP-Spec maglp.tex, Definition "Multiagent
    // GLP"; IGLP, Implementation Notes, "Boot").
    agents.input('Bob', const GAtom('bob'));

    await agents.until('the befriend(alice) card',
        () => agents.has('Bob', 'befriend(alice, req(1))'));
    final card = agents.has('Bob', 'befriend(alice, req(1))');
    agents.input('Bob', const GStruct('decision',
        [GAtom('yes'), GAtom('alice'), GStruct('req', [GInt(1)])]));
    // Alice's actor sends a message once connected; it should reach Bob.
    await agents.until('connected(alice) and received(alice, ...)',
        () => agents.has('Bob', 'connected(alice)') &&
            agents.has('Bob', 'received(alice'));
    final connected = agents.has('Bob', 'connected(alice)');
    final got = agents.has('Bob', 'received(alice');

    await agents.settle();
    // ignore: avoid_print
    print('BOB OUT:\n${agents.out['Bob']!.where((l) => l.startsWith('< ')).join('\n')}');

    expect(card, isTrue, reason: 'befriend(alice) card');
    expect(connected, isTrue, reason: 'connected(alice)');
    expect(got, isTrue, reason: 'received(alice, ...) — messaging over friend channel');
  });
}
