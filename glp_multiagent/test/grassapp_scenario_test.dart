/// Live-UI probe for the GrassApp scenario: drives the exact path the app
/// uses (AgentRuntime, the program loaded as one, scenario_init/3, the
/// person's UserCmds injected as ground terms) with programs/grassapp
/// (grassapp_agent + grassapp_mediator) booted by play_grassapp_boot.glp —
/// four actors: Alice (swap), Charlie (unfriend), Dana (swap then redeem), Eve
/// (chat greeting).
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

void main() {
  test('GrassApp scenario: four actors, chat replies, swap-then-redeem',
      () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'Bob',
      // programs/grassapp is a program (SGSG, d27e4d6a): loaded as one, its
      // self.glp exports agent_init/3 as an entry point.
      program: '$repo/grassapp',
      // The boot play's entry point, exported by grassapp/self.glp under its
      // own name (SGSG, 8412aae7); agent_init/3 is the duo play's.
      goalLabel: 'scenario_init/3',
      rootSelfGlpPath: '$repo/self.glp',
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
    await act('decision(yes, eve, ${reqs['eve']})');
    expect(all(), contains('received(eve, welcome_to_grassroots_bob)'));
    await act('send(eve, thanks_eve)');
    expect(all(), contains('received(eve, lovely_day_isnt_it)'));

    // Dana: her pay of dana-coins is returned — a pay is q-coins to q (Def 3.2),
    // and Bob is not their issuer, so it is not absorbed.  Her trade offer then
    // arrives, Bob accepts (+2 dana-coins), and Dana redeems a bob-coin, which
    // Bob's agent honours by setting off one dana-coin — leaving Bob 1.
    // Balances are keyed by (owner, issuer, maturity); coins are maturity 0.
    await act('decision(yes, dana, ${reqs['dana']})');
    final swap = RegExp('trade_proposed\\(dana, .*?(req\\(\\d+\\))\\)')
        .firstMatch(all());
    expect(swap, isNotNull, reason: 'no trade offer from dana in: $lines');
    await act('accept_trade(dana, ${swap!.group(1)})');
    expect(all(), contains('trade_completed(dana)'));
    expect(all(), contains('balance_report(bob, dana, 0, 1)'),
        reason: 'the swap gave Bob 2 dana-coins and the redemption set off 1, '
            "leaving 1; Dana's pay of foreign coins was returned (a pay is q-coins to q)");

    // Alice and Charlie as before: message + trade card; pay + unfriend.
    await act('decision(yes, alice, ${reqs['alice']})');
    expect(all(), contains('trade_proposed(alice, bob, 0, 1'));
    await act('decision(yes, charlie, ${reqs['charlie']})');
    expect(all(), contains('unfriended(charlie)'));

    expect(lines.where((l) => l.contains('[ERROR]')), isEmpty);
  });
}
