/// Live-UI probe for the social-graph scenario: drives the exact path the app
/// uses (AgentRuntime, the program statically linked, agent_init/3, the
/// person's UserCmds injected as ground terms) with the canonical compiled
/// pair programs/social/graph (agent.glp + ui/mediator.glp) booted by
/// play_ui_boot.glp.
///
/// The probe is the person: it reads Bob's UserNotify lines (the inbox cards
/// and the screen) and answers with UserCmds (the taps): accept both friend
/// offers, receive the actors' messages and Charlie's unfriend, then end the
/// friendship with Alice.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() {
  test('social-graph live-UI scenario over the canonical compiled pair',
      () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'Bob',
      rootSelfGlpPath: '$repo/self.glp',
      program: '$repo/social/graph',
    );
    agent.onOutput = lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};

    await agent.initialize();
    expect(agent.initialized, isTrue);

    // The person's act, written as the term the screen grants and read by the
    // screen's own ground-term reader (ui_runtime/term.dart): the agent is
    // handed a term, never text.
    Future<void> act(String term) =>
        agent.injectUserInput(runtimeTermOf(tryParseTerm(term)!));

    // Both actors cold-called Bob: two friend-offer cards.
    String reqOf(String who) {
      final m = RegExp('befriend\\($who, (req\\(\\d+\\))\\)')
          .firstMatch(lines.join('\n'));
      expect(m, isNotNull, reason: 'no befriend card from $who in: $lines');
      return m!.group(1)!;
    }

    final aliceReq = reqOf('alice');
    final charlieReq = reqOf('charlie');

    // Accept Alice: friendship forms, and Alice messages Bob.
    await act('decision(yes, alice, $aliceReq)');
    expect(lines.join('\n'), contains('connected(alice)'));
    expect(lines.join('\n'), contains('received(alice, hello)'));

    // Bob messages Alice; she answers.
    await act('send(alice, thanks)');
    expect(lines.join('\n'),
        contains('received(alice, so_nice_to_hear_from_you_bob)'));

    // Accept Charlie: friendship forms, Charlie messages Bob and then
    // unfriends him — the Integrate-unfriend path reaches the screen.
    await act('decision(yes, charlie, $charlieReq)');
    expect(lines.join('\n'), contains('connected(charlie)'));
    expect(lines.join('\n'), contains('received(charlie, hi)'));
    expect(lines.join('\n'), contains('unfriended(charlie)'));

    // End friend from the compose form: the mediator passes unfriend/1
    // through, and the initiator's own screen drops the friend.
    await act('unfriend(alice)');
    expect(lines.join('\n'), contains('unfriended(alice)'));
    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
