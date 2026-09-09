/// The path the app itself takes: `coins_ui/3` booted in an agent isolate the
/// way `lib/main_coins.dart` boots it, over `programs/coins` resolved from the
/// repo on disc, with the person's grants injected and her screen read back.
///
/// The widget test beside this one drives the surface; this one holds the
/// wiring the surface never sees — the resolved program directory, the entry
/// point and its three arguments, and the isolate boundary.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  test('alice mints and swaps across the isolate boundary', () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final reply = ReceivePort();
    SendPort? commands;
    final sends = <String>[];
    final r = UiRuntime(manifest: coinsManifest, onSend: sends.add);

    reply.listen((m) {
      if (m is AgentReady) {
        commands = m.commandPort;
      } else if (m is AgentOutput) {
        final line = m.line.startsWith('< ') ? m.line.substring(2) : m.line;
        r.handleLine(line);
      } else if (m is AgentError) {
        fail('agent error: ${m.error}');
      }
    });

    final isolate = await Isolate.spawn(
      agentIsolateEntry,
      InitAgent(
        agentId: 'alice',
        glpSources: const [],
        programDir: '$repo/coins',
        goalLabel: 'coins_ui/3',
        rootSelfGlpPath: '$repo/self.glp',
        friends: const ['bob'],
        replyPort: reply.sendPort,
        deferStart: false,
      ),
    );
    addTearDown(() {
      isolate.kill(priority: Isolate.immediate);
      reply.close();
    });

    Future<bool> until(bool Function() done,
        {Duration t = const Duration(seconds: 20)}) async {
      final end = DateTime.now().add(t);
      while (DateTime.now().isBefore(end)) {
        if (done()) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return done();
    }

    /// The person's grant: what the surface sends when she submits a form or
    /// taps a button.
    Future<void> grant() async {
      while (sends.isNotEmpty) {
        commands!.send(UserInput(sends.removeAt(0)));
        await Future<void>.delayed(const Duration(milliseconds: 200));
      }
    }

    // The agent poses its four request clauses as soon as it runs.
    expect(await until(() => r.standing.length == 4), isTrue,
        reason: 'four standing cards, one per request clause');

    // Mint 2.
    r.submitCommand(r.manifest.standingForm('agent_1')!.$2, {'Amount': GInt(2)});
    await grant();
    expect(
        await until(() =>
            r.store.balances['balances']?.containsKey('alice') ?? false),
        isTrue);
    expect(formatTerm(r.store.balances['balances']!['alice']!), '2');

    // Swap her 2 for bob's 2; bob's script accepts.
    await until(() => r.standing.containsKey('agent_2'));
    r.submitCommand(r.manifest.standingForm('agent_2')!.$2, {
      'Friend': GAtom('bob'),
      'GiveCoin': GAtom('alice'),
      'GiveAmount': GInt(2),
      'WantCoin': GAtom('bob'),
      'WantAmount': GInt(2),
    });
    await grant();
    expect(
        await until(() => r.store.lists['screen']!
            .any((t) => formatTerm(t) == 'swap_done(bob)')),
        isTrue);
    expect(r.store.balances['balances']!.keys, ['bob']);

    // Bob proposes the reverse swap: one card, both sibling asks.
    expect(await until(() => r.inbox.length == 1), isTrue);
    final card = r.inbox.single;
    expect(card.asks.keys.toSet(), {'respond_swap_1', 'respond_swap_2'});
    r.answerCard(card, card.liveAnswers.firstWhere((a) => a.label == 'Accept'));
    await grant();
    expect(
        await until(() =>
            r.store.balances['balances']?.containsKey('alice') ?? false),
        isTrue);
    expect(r.store.balances['balances']!.keys, ['alice']);
  }, timeout: const Timeout(Duration(minutes: 2)));
}
