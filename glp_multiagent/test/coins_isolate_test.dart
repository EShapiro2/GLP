/// The play `coins_ui/3` run in an isolate of its own, over
/// `programs/currencies/coins` resolved from the repo on disc, with the
/// person's grants injected and her screen read back across the isolate
/// boundary.
///
/// The play is run directly, as test code, with no host: the host posts
/// `superapp/3` and nothing of Currencies' (GSG Section 5.1; Currencies #7
/// Cowork, 2026-10-04 09:15 UTC, item 2).  In the play's isolate the engine
/// loads the coins program and posts `coins_ui(alice, Answers?, [])` through
/// its posting call, which checks the goal as every posted goal is checked
/// (TGLP modules.tex, "the initial goal posted to the runtime ... is
/// type-checked before execution as a body goal"; GlpEngine.postGoal); each
/// grant the test sends is injected into `Answers` through the injector the
/// call returned, and every line the play sends alice comes back.  Until
/// 2026-10-04 the play was booted under the host, AgentRuntime, the way
/// `lib/main_coins.dart` boots it, which posted it unchecked.
///
/// The widget test beside this one drives the surface; this one holds the
/// wiring the surface never sees — the resolved program directory, the goal
/// and its three arguments, and the isolate boundary.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show runtimeTermOf;
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

/// What the play's isolate reports that is not a line of alice's screen.
class _PlayFault {
  final String error;
  _PlayFault(this.error);
}

/// The play's isolate: [init] is the test's port and the repo's programs/.
/// It sends back its own port, for alice's grants, once the play is posted;
/// then each line the play sends alice, and a [_PlayFault] where the load,
/// the posting or a run fails.
void _playIsolate((SendPort, String) init) {
  final (reply, repo) = init;
  final grants = ReceivePort();
  final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp');
  engine.runtime.outputCallback = reply.send;
  try {
    engine.loadProgram('$repo/currencies/coins');
    final play = engine.postGoal('coins_ui(alice, Answers?, [])',
        inputs: ['Answers']);
    final answers = play.inputs['Answers']!;

    /// The play run until quiescent; a run stopped at the net is reported.
    void drain() {
      final result = play.scheduler.drainToQuiescence(maxCycles: 200000);
      if (result.status == ExecutionStatus.capped) {
        reply.send(_PlayFault('the play did not quiesce'));
      }
    }

    reply.send(grants.sendPort);
    drain();
    grants.listen((grant) {
      answers
          .inject(runtimeTermOf(grant as GTerm))
          .forEach(engine.runtime.gq.enqueue);
      drain();
    });
  } catch (e) {
    reply.send(_PlayFault('$e'));
    grants.close();
  }
}

void main() {
  test('alice mints and swaps across the isolate boundary', () async {
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final reply = ReceivePort();
    SendPort? commands;
    final sends = <GTerm>[];
    final r = UiRuntime(manifest: coinsManifest, onSend: sends.add);

    reply.listen((m) {
      if (m is SendPort) {
        commands = m;
      } else if (m is String) {
        r.handleLine(m);
      } else if (m is _PlayFault) {
        fail('the play: ${m.error}');
      }
    });

    final isolate = await Isolate.spawn(_playIsolate, (reply.sendPort, repo));
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

    /// The balances panel as owner-to-amount, sorted — the state the
    /// assertions below are on. A tally message replaces the panel whole, so
    /// during a swap the panel is empty between her coins leaving and bob's
    /// arriving: a wait on "alice is named" sees the panel mid-swap.
    String balances() {
      final b = r.store.balances['balances'] ?? const <String, GTerm>{};
      final keys = b.keys.toList()..sort();
      return [for (final k in keys) '$k=${formatTerm(b[k]!)}'].join(' ');
    }

    /// The person's grant: what the surface sends when she submits a form or
    /// taps a button.
    Future<void> grant() async {
      while (sends.isNotEmpty) {
        commands!.send(sends.removeAt(0));
        await Future<void>.delayed(const Duration(milliseconds: 200));
      }
    }

    // The agent poses its four request clauses as soon as it runs.
    expect(await until(() => r.standing.length == 4), isTrue,
        reason: 'four standing cards, one per request clause');

    // Mint 2.
    r.submitCommand(r.manifest.standingForm('agent_1')!.$2, {'Amount': GInt(2)});
    await grant();
    await until(() => balances() == 'alice=2');
    expect(balances(), 'alice=2');

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
    await until(() => balances() == 'bob=2');
    expect(balances(), 'bob=2');

    // Bob proposes the reverse swap: one card, the responder's one ask.
    await until(() => r.inbox.length == 1);
    expect(r.inbox.length, 1);
    final card = r.inbox.single;
    expect(card.asks.keys.toSet(), {'respond_swap_1'});
    r.answerCard(card, card.liveAnswers.firstWhere((a) => a.label == 'Accept'));
    await grant();
    await until(() => balances() == 'alice=2');
    expect(balances(), 'alice=2');
  }, timeout: const Timeout(Duration(minutes: 2)));
}
