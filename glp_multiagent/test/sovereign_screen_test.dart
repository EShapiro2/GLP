/// The sovereign mini-app in use: the central bank mints and opens the
/// mutual credit line with Diana, willed through the constructs the Dart
/// bridge draws; the play runs on to the chain redemption and Frank's payment
/// to the central bank; and Diana's reverse swap is accepted on its card.
///
/// The whole path runs: `programs/currencies/sovereign` from its agent/2, at
/// the person's first act `sovereign_ui(cb, [diana, frank])` --- the
/// mini-app's live-person entry (`programs/currencies/sovereign/play_ui.glp`),
/// the central bank's execution of the mini-app on the person channel
/// `ch(Grants?, Draws)` of vGLP's Definition "Person Channel, Person Writer,
/// GLP with Persons, Grant", and Diana's and Frank's, scripted, over the
/// three conversations of the sovereign market cut to them.  The compiled
/// program's dispatcher draws each construct, `draw(Id, W, V)`, and withdraws
/// it, `withdraw(Id)`; the bridge draws each as the default widget of its
/// interactive type (Definition "Widget Declaration, Default Widget"), and
/// the central bank's taps and submissions on them go back as its grants,
/// `input(Id, P, R)` (vGLP, sections/elicitation.tex, the paragraph "The
/// implementation").  Nothing is simulated and no term is hand-fed; the
/// screen's own mechanics over hand-fed draws are `sovereign_ui_test.dart`'s.
///
/// The play is run directly, as test code, with no host.  The engine loads
/// the sovereign program and posts the agent's initial goal as AgentRuntime
/// posts it, agent(ch(UserIn?, UserOut), ch(NetIn?, NetOut)) with
/// send_to_user/1 and send_to_net/1 (GLP-Spec maglp.tex, Definition
/// "Multiagent GLP"; IGLP, Implementation Notes, "Boot"), through its posting
/// call, which checks the goal as every posted goal is checked (TGLP
/// modules.tex, "the initial goal posted to the runtime ... is type-checked
/// before execution as a body goal"; GlpEngine.postGoal).  The test is the
/// person: its first act on `UserIn` is `sovereign_ui(cb, [diana, frank])`,
/// the agent's key and the keys of the peers the person knows, each a
/// readable name standing for a key (IGLP, "Boot" and Remark "Agent Names");
/// its grants follow on `UserIn` through the injector the call returns, and
/// what `send_to_user/1` sends is read off the runtime's output.  The suite's
/// own reading of the scripted play is the REPL's (`test/run_all_tests.sh`,
/// Section N4, `play_sovereign_ui`).
library;

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/sovereign_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_multiagent/isolate_protocol.dart'
    show runtimeTermOf, screenTermOf;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

void main() {
  testWidgets(
      'the sovereign mini-app: the central bank mints and opens the credit '
      'line through its screen, the play runs on to the chain redemption and '
      'Frank\'s payment, and it accepts Diana\'s reverse swap on its card',
      (tester) async {
    tester.view.physicalSize = const Size(600, 1200);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';
    // What send_to_user/1 sends, each term whole: the central bank's draws
    // and withdrawals, and the peers' screens, tagged.
    final notifies = <GTerm>[];
    final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp');
    engine.runtime.outputTermCallback = (t) => notifies.add(screenTermOf(t));
    // programs/currencies/sovereign is a program: denominated/ is the
    // certified mini-app and this directory adds the harness that runs it
    // for a live person, play_ui.glp's sovereign_ui/4, which agent/2 starts
    // on the person's first act and which reads its grants after it.
    expect(engine.loadProgram('$repo/currencies/sovereign'), isTrue);
    final play = engine.postGoal(
        'agent(ch(UserIn?, UserOut), ch(NetIn?, NetOut)), '
        'send_to_user(UserOut?), send_to_net(NetOut?)',
        inputs: ['UserIn', 'NetIn']);
    final grants = play.inputs['UserIn']!;
    // The first act: the agent's key and the keys of the peers the person
    // knows.
    grants
        .inject(runtimeTermOf(const GStruct('sovereign_ui', [
          GAtom('cb'),
          GList([GAtom('diana'), GAtom('frank')])
        ])))
        .forEach(engine.runtime.gq.enqueue);

    /// The play run until quiescent, as one event of the agent's is.
    void drain() {
      final result = play.scheduler.drainToQuiescence(maxCycles: 200000);
      expect(result.status, isNot(ExecutionStatus.capped),
          reason: 'the play did not quiesce');
    }

    final sends = <GTerm>[];
    final r = UiRuntime(manifest: sovereignManifest, onSend: sends.add);

    var fed = 0;
    void replay() {
      for (; fed < notifies.length; fed++) {
        r.handle(notifies[fed]);
      }
    }

    // Each of the central bank's grants, a ground term, onto its person
    // channel.
    Future<void> settle() async {
      while (sends.isNotEmpty) {
        grants
            .inject(runtimeTermOf(sends.removeAt(0)))
            .forEach(engine.runtime.gq.enqueue);
        drain();
      }
      replay();
      await tester.pumpAndSettle();
    }

    drain();
    replay();

    /// What the agent has written to the central bank, each message's
    /// content: the thread of construct 0 (vGLP 169b008, Definition
    /// "Canonical Compilation").
    List<String> screen() {
      final v = r.bridge.construct(0)!.view;
      return [
        if (v is GList)
          for (final m in v.items) formatTerm(m)
      ];
    }

    /// Its latest holdings.
    String holdings() =>
        screen().lastWhere((m) => m.startsWith('holdings('), orElse: () => '');

    /// What the peers' screens showed, tagged by the harness.
    List<String> tagged() => [
          for (final n in notifies)
            if (ctorArgs(n).$1 == 'tagged') formatTerm(n)
        ];

    /// The questions' constructs: all but the output thread.
    List<OpenConstruct> questions() =>
        [for (final c in r.bridge.constructs) if (c.id != 0) c];

    // --- The start.  The agent asks its question as soon as it runs: the
    // stream of its requests, one construct, an input box of a menu of the
    // twelve forms of Request, one per request clause.  Its two
    // conversations, with Diana and with Frank, are opened on its screen; the
    // peers are scripted inside the harness, and nothing of theirs reaches
    // the central bank's screen: their mints are theirs alone.
    expect(formatTerm(r.bridge.construct(0)!.widget),
        startsWith('thread(menu([form(holdings, '));
    final form = questions().single;
    expect(form.id, 1);
    expect(formatTerm(form.widget), startsWith('input_box(menu([form(mint, '));
    expect(formatTerm(form.view), 'input([])');
    expect(screen(), ['opened(diana)', 'opened(frank)']);
    expect(tagged(), contains('tagged(diana, minted(10, now))'));
    expect(tagged(), contains('tagged(frank, minted(18, 28))'));

    await tester.pumpWidget(MaterialApp(
        home: AgentSurface(agentId: 'cb', runtime: r, muteNotices: true)));
    await tester.pumpAndSettle();

    /// Bring [f] on screen.
    Future<void> show(Finder f) async {
      if (f.evaluate().isEmpty) {
        await tester.scrollUntilVisible(f, 200,
            scrollable: find.byType(Scrollable).first);
      }
      await tester.ensureVisible(f);
      await tester.pumpAndSettle();
    }

    Finder inForm(Finder f) =>
        find.descendant(of: find.byKey(constructKey(form.id)), matching: f);

    // The form offers the twelve requests, each by its functor, and by its
    // arity where another of its forms has that functor (Remark "Construct
    // Families"): the alternatives of Request, and only those.
    expect(inForm(find.byType(ChoiceChip)), findsNWidgets(12));
    for (final label in const [
      'mint/2',
      'mint/3',
      'swap',
      'accept',
      'pay',
      'payout',
      'spend',
      'redeem',
      'mature',
      'deposit',
      'release',
      'return',
    ]) {
      expect(inForm(find.widgetWithText(ChoiceChip, label)), findsOneWidget,
          reason: label);
    }

    /// Choose the form [alternative], fill its fields, and send it: one
    /// request on the request form, one element of the stream.
    Future<void> request(
        String alternative, Map<List<int>, (String, String)> fields) async {
      final chip = inForm(find.widgetWithText(ChoiceChip, alternative));
      await show(chip);
      await tester.tap(chip);
      await tester.pumpAndSettle();
      expect(inForm(find.byType(TextField)), findsNWidgets(fields.length));
      for (final e in fields.entries) {
        final (kind, text) = e.value;
        await tester.enterText(
            find.descendant(
                of: find.byKey(nodeKey(form.id, kind, e.key)),
                matching: find.byType(TextField)),
            text);
      }
      await tester.pumpAndSettle();
      await show(find.byKey(submitKey(form.id, const [])));
      await tester.tap(find.byKey(submitKey(form.id, const [])));
      await tester.pumpAndSettle();
      await settle();
    }

    // --- Act 1: the central bank mints 30 coins of its own, on the mint form
    // of two fields, mint(Denomination, Amount).  Its date stands at now.
    await request('mint/2', {
      [1]: ('text', 'usd'),
      [2]: ('number', '30'),
    });
    expect(sends, isEmpty);
    expect(screen(), contains('minted(30, now)'));
    expect(holdings(), 'holdings([lot(cb, usd, now, 30)])');
    // The question stands: a stream question takes one element per
    // submission and stays open while its stream does (vGLP, Remark
    // "Persistence").
    expect(r.bridge.construct(form.id), isNotNull);

    // --- Act 2: the mutual credit line, proposed to Diana on the swap form
    // --- ten of its coins for ten of hers, at par --- its eight fields the
    // arguments of swap(Friend, Denomination, GiveIssuer, GiveDue,
    // GiveAmount, WantIssuer, WantDue, WantAmount), in order.  Diana's script
    // accepts, and the swap is done on both screens.
    await request('swap', {
      [1]: ('peer', 'diana'),
      [2]: ('text', 'usd'),
      [3]: ('peer', 'cb'),
      [4]: ('text', 'now'),
      [5]: ('number', '10'),
      [6]: ('peer', 'diana'),
      [7]: ('text', 'now'),
      [8]: ('number', '10'),
    });
    expect(sends, isEmpty);
    expect(screen(), contains('swap_done(diana)'));
    expect(tagged(), contains('tagged(diana, swap_done(cb))'));

    // --- Acts 3 and 4 are the harness's scripted play, and they run on the
    // central bank's own answer to act 2: each event reduces until
    // quiescent, so the play is not left standing mid-run.  Act 3 is the
    // chain redemption frank -> diana -> cb, after which Frank holds a
    // central-bank coin.
    expect(tagged(), contains('tagged(diana, presented(frank, cb, now))'));
    expect(tagged(), contains('tagged(frank, redeemed(diana, cb, now))'));
    // Act 4 is on the central bank's own screen: Frank pays that coin to its
    // issuer, which its screen shows received; the unit of the fiat currency
    // owed for it is handed over by the person, outside the contract.
    expect(tagged(), contains('tagged(frank, paid(cb, 1))'));
    expect(screen(), contains('received(frank, 1)'));
    // The coin is back with its issuer: twenty-one of its own and the ten of
    // Diana's the credit line gave it.
    expect(holdings(),
        'holdings([lot(cb, usd, now, 21), lot(diana, usd, now, 10)])');

    // --- The card: Diana proposes the reverse swap, the nine central-bank
    // coins she holds for nine of hers, and the swap responder's question,
    // SwapCard ::= card(SwapOffer, YesNo?) in writer mode, is one construct:
    // the offer shown, and the buttons yes and no at its answer.
    final card = questions().singleWhere((c) => c.id != form.id);
    expect(formatTerm(card.widget), startsWith("form(card, [field('SwapOffer', "));
    final (_, cardArgs) = ctorArgs(card.view);
    final (_, offer) = ctorArgs(cardArgs[0]);
    expect(formatTerm(offer[0]), 'diana');
    expect(formatTerm(offer[1]), 'lot(diana, usd, now, 9)');
    expect((offer[2] as GList).items, hasLength(9));
    expect(formatTerm(cardArgs[1]), 'input([2])');
    Finder inCard(Finder f) =>
        find.descendant(of: find.byKey(constructKey(card.id)), matching: f);
    await show(find.byKey(constructKey(card.id)));
    // The card shows the offer by its fields, never as its term, and takes
    // no text.
    expect(inCard(find.textContaining('lot(diana')), findsNothing);
    expect(inCard(find.textContaining('offer(')), findsNothing);
    expect(inCard(find.byType(TextField)), findsNothing);
    expect(inCard(find.widgetWithText(ElevatedButton, 'yes')), findsOneWidget);
    expect(inCard(find.widgetWithText(ElevatedButton, 'no')), findsOneWidget);

    await show(inCard(find.widgetWithText(ElevatedButton, 'yes')));
    await tester.tap(inCard(find.widgetWithText(ElevatedButton, 'yes')));
    await tester.pumpAndSettle();
    await settle();
    expect(sends, isEmpty);
    // Answered, the card is withdrawn; the request form stands.
    expect(r.bridge.construct(card.id), isNull);
    expect(find.byKey(constructKey(card.id)), findsNothing);
    expect(questions().map((c) => c.id), [form.id]);
    expect(screen(), contains('swap_done(diana)'));
    expect(holdings(),
        'holdings([lot(cb, usd, now, 30), lot(diana, usd, now, 1)])');
  }, timeout: const Timeout(Duration(minutes: 3)));
}
