/// Live-UI probe for the GrassApp scenario: drives the exact path the app
/// uses (AgentRuntime, source-list load, agent_init/3, injected UserCmd text)
/// with programs/book/grassapp (grassapp_agent + grassapp_mediator) booted by
/// play_grassapp_boot.glp — four actors: Alice (swap), Charlie (unfriend),
/// Dana (swap then redeem), Eve (chat greeting).
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() {
  test('GrassApp scenario: four actors, chat replies, swap-then-redeem',
      () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';
    const files = [
      'self.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_grassapp_boot.glp',
    ];
    final paths = [for (final f in files) '$repo/book/grassapp/$f'];

    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'Bob',
      glpSources: [for (final p in paths) File(p).readAsStringSync()],
      glpSourcePaths: paths,
      rootSelfGlpPath: '$repo/self.glp',
      friends: const ['alice', 'charlie', 'dana', 'eve'],
    );
    agent.onOutput = lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};

    await agent.initialize();
    expect(agent.initialized, isTrue);

    String all() => lines.join('\n');
    String reqOf(String who) {
      final m = RegExp('befriend\\($who, (req\\(\\d+\\))\\)').firstMatch(all());
      expect(m, isNotNull, reason: 'no befriend card from $who in: $lines');
      return m!.group(1)!;
    }

    // All four actors cold-called Bob.
    final reqs = {for (final w in ['alice', 'charlie', 'dana', 'eve']) w: reqOf(w)};

    // Eve: greeting lands in chat; Bob answers and she replies.
    // NOTE: eve is the deepest pending (req(1) of four) — this exercises the
    // escrow lookup at depth 4, the acceptance case for
    // IGLP/docs/bug-forwarded-writer-reactivation-2026-07-04.md.
    await agent.injectUserInput('decision(yes, eve, ${reqs['eve']})');
    expect(all(), contains('received(eve, welcome_to_grassroots_bob)'));
    await agent.injectUserInput('send(eve, thanks_eve)');
    expect(all(), contains('received(eve, lovely_day_isnt_it)'));

    // Dana: pay arrives, swap offer arrives; Bob accepts; Dana then redeems a
    // bob-coin and Bob's agent fills back — his dana-holding drops to 3.
    await agent.injectUserInput('decision(yes, dana, ${reqs['dana']})');
    final swap = RegExp('swap_offer\\(dana, dana, 2, bob, 1, (req\\(\\d+\\))\\)')
        .firstMatch(all());
    expect(swap, isNotNull, reason: 'no swap offer from dana in: $lines');
    await agent.injectUserInput('accept_swap(dana, ${swap!.group(1)})');
    expect(all(), contains('swap_done(dana)'));
    expect(all(), contains('balance_report(bob, dana, 3)'),
        reason: 'dana redeemed one bob-coin; the fill-back leaves 3 dana-coins');

    // Alice and Charlie as before: message + swap card; pay + unfriend.
    await agent.injectUserInput('decision(yes, alice, ${reqs['alice']})');
    expect(all(), contains('swap_offer(alice, alice, 1, bob, 1'));
    await agent.injectUserInput('decision(yes, charlie, ${reqs['charlie']})');
    expect(all(), contains('unfriended(charlie)'));

    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
