/// The demonstration started as main_superapp_ui.dart starts it: the agent
/// from agent(ch(_?,_),ch(_?,_)), every agent's initial goal, which
/// programs/social/graph/core's agent/2 answers, and, at the agent's first
/// start, the application's act on the person channel that starts the
/// super-app, `superapp_ui(Me, [])`, Me the agent's own key and no peer yet
/// (IGLP b014a9e, Implementation Notes, "Boot": "At the agent's first start
/// the application sends on the person channel the act that starts its
/// program, with the agent's own key and the keys of the peers the person
/// knows, none at a first start; later peers come by acquaintance";
/// lib/isolate_protocol.dart, InitAgent.entry).  On that act the super-app
/// starts and draws home: the request form of home.vglp, a menu of the four
/// acts, on the person channel.  The waits are on events, with no clock
/// (isolate_agents.dart).
library;

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/term.dart' show formatTerm;

import 'isolate_agents.dart';
import 'programs_dir.dart';

void main() {
  test('the application\'s first act superapp_ui(Me, []) starts the super-app, '
      'which draws home', () async {
    final programs = programsDir();
    final agents = IsolateAgents();
    addTearDown(agents.dispose);
    await agents.spawn('alice', '$programs/social/graph/core',
        '$programs/self.glp',
        deferStart: false, entry: 'superapp_ui');

    // The act the application sent: superapp_ui of the agent's key, 64 hex
    // characters, and no peer.
    await agents.until('the first act',
        () => agents.logs['alice']!.any((l) => l.contains('USER_INPUT: superapp_ui(')));
    final act = agents.logs['alice']!
        .firstWhere((l) => l.contains('USER_INPUT: superapp_ui('));
    expect(act, matches(RegExp(r"superapp_ui\('?[0-9a-f]{64}'?, \[\]\)")));

    // Home's request form, drawn on the person channel.
    bool drawnHome() => agents.notes['alice']!.any((t) {
          final s = formatTerm(t);
          return s.contains('stream_request_r') && s.contains('connect');
        });
    await agents.until('home drawn', drawnHome);
    expect(drawnHome(), isTrue);
    expect(agents.faults, isEmpty);
  });
}
