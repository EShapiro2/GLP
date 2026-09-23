/// The sovereign mini-app on the central bank's screen:
/// `programs/currencies/sovereign` at `sovereign_ui/3`, booted as
/// `lib/main_sovereign.dart` boots it — the central bank the live person,
/// `diana` and `frank` its scripted counterparties — over the harness
/// Currencies wrote, `programs/currencies/sovereign/play_ui.glp`, and read
/// back through the screen derived from `sovereign_agent.vglp`'s display
/// declarations.
///
/// What is proved is the boot and what the person then does on it. The
/// harness's one clause is for `cb`, and booting `cb` stands the seven forms
/// of the `sovereign` panel — Mint to Return, each bound to the standing card
/// of its own request clause — with the two conversations opened on the
/// screen and nothing of the peers' on it. Then the central bank's own two of
/// the harness's four acts go through the derived forms: the mint of 30 on
/// the Mint form, and the mutual credit line on the Swap form, which Diana's
/// script accepts. The peers' two acts and Diana's reverse swap are the
/// harness's scripted play, which the suite reads off the log
/// (`test/run_all_tests.sh`, Section N4, `play_sovereign_ui`). Nothing is
/// simulated and no term is hand-fed; the screen's own mechanics over
/// hand-fed lines are `sovereign_ui_test.dart`'s.
///
/// The values booted are those of `main_sovereign.dart`, whose `VglpProgram`
/// is built inside its `main`: `person: 'cb'`,
/// `directory: (glp) => glp.sovereignDir`, `goalLabel: 'sovereign_ui/3'`,
/// `manifest: sovereignManifest`, `friends: ['diana', 'frank']`. A person the
/// harness has no clause for boots and compiles as well as this one and
/// stands no form, which is what this test tells apart.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/glp_sources.dart';
import 'package:glp_multiagent/manifests/sovereign_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

import 'programs_dir.dart';

void main() {
  testWidgets(
      'the sovereign mini-app: the central bank boots, its seven forms stand, '
      'and it mints and opens the credit line through its screen',
      (tester) async {
    tester.view.physicalSize = const Size(600, 1200);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    // The tree the app reads, as `resolveGlpPaths` resolves it on a desktop
    // checkout — this checkout's `programs/` and no other — so that the
    // program directory is the app's own `sovereignDir`.
    final repo = programsDir();
    final glp = GlpPaths(
        '$repo/grassapp', '$repo/social/graph', '$repo/cssn', '$repo/self.glp');

    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'cb',
      glpSources: const [],
      // programs/currencies/sovereign is a program: denominated/ is the
      // certified mini-app and this directory adds the harness that runs it
      // for a live person.
      programDir: glp.sovereignDir,
      goalLabel: 'sovereign_ui/3',
      rootSelfGlpPath: glp.rootSelfGlp,
      friends: const ['diana', 'frank'],
    );
    agent.onOutput = lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};

    final sends = <String>[];
    final r = UiRuntime(manifest: sovereignManifest, onSend: sends.add);

    var fed = 0;
    void replay() {
      for (; fed < lines.length; fed++) {
        final l = lines[fed];
        if (l.startsWith('< ')) r.handleLine(l.substring(2));
      }
    }

    Future<void> settle() async {
      while (sends.isNotEmpty) {
        await tester.runAsync(() => agent.injectUserInput(sends.removeAt(0)));
      }
      replay();
      await tester.pumpAndSettle();
    }

    /// The central bank's default display: every screen message but the
    /// holdings, which go to the balances view.
    Iterable<String> screen() => r.store.lists['screen']!.map(formatTerm);

    await tester.runAsync(() => agent.initialize());
    replay();

    // --- The boot. The entry is found and its clause is for cb: the compiled
    // agent poses its seven request clauses as soon as it runs, so the seven
    // forms stand before the person has done anything, each in the sovereign
    // panel under its clause's label, and none opens a card.
    expect(lines, isNot(anyElement(startsWith('[ERROR]'))));
    expect(r.standing.keys.toSet(), {
      'agent_1',
      'agent_2',
      'agent_3',
      'agent_4',
      'agent_5',
      'agent_6',
      'agent_7',
    });
    for (final e in const {
      'agent_1': 'Mint',
      'agent_2': 'Swap',
      'agent_3': 'Pay',
      'agent_4': 'Redeem',
      'agent_5': 'Deposit',
      'agent_6': 'Release',
      'agent_7': 'Return',
    }.entries) {
      final (panel, form) = r.manifest.standingForm(e.key)!;
      expect(panel.id, 'sovereign', reason: e.key);
      expect(form.label, e.value, reason: e.key);
    }
    expect(r.inbox, isEmpty);

    // Its two conversations — with Diana the community bank and Frank the
    // household — are opened on its screen. The peers are scripted inside
    // the harness, their screens tagged in the trace, and nothing of theirs
    // reaches the central bank's screen: their mints are theirs alone.
    expect(screen(), containsAll(['opened(diana)', 'opened(frank)']));
    expect(lines, anyElement(contains('tagged(diana, minted(10, 0))')));
    expect(lines, anyElement(contains('tagged(frank, minted(18, 28))')));
    expect(screen(), isNot(contains('minted(10, 0)')));
    expect(screen(), isNot(contains('minted(18, 28)')));

    await tester.pumpWidget(MaterialApp(
        home: AgentSurface(agentId: 'cb', runtime: r, muteNotices: true)));
    await tester.pumpAndSettle();

    // The panel's "+" offers the seven persistent clauses, each by its label,
    // and only those the agent has posed: a volition is offered iff pending.
    await tester.tap(find.byType(FloatingActionButton));
    await tester.pumpAndSettle();
    for (final label in const [
      'Mint',
      'Swap',
      'Pay',
      'Redeem',
      'Deposit',
      'Release',
      'Return',
    ]) {
      expect(find.text(label), findsOneWidget, reason: label);
    }
    await tester.tapAt(const Offset(300, 100)); // dismiss the sheet
    await tester.pumpAndSettle();

    /// Fill and submit one of the panel's compose forms: its fields are the
    /// writers of its clause's question, in the question's order.
    Future<void> compose(String label, List<String> values) async {
      await tester.tap(find.byType(FloatingActionButton));
      await tester.pumpAndSettle();
      await tester.tap(find.text(label));
      await tester.pumpAndSettle();
      final fields = find.byType(TextField);
      expect(fields, findsNWidgets(values.length), reason: '$label fields');
      for (var i = 0; i < values.length; i++) {
        await tester.enterText(fields.at(i), values[i]);
      }
      await tester.tap(find.widgetWithText(ElevatedButton, 'Send'));
      await tester.pumpAndSettle();
      await settle();
    }

    // --- Act 1: the central bank mints 30 coins of its own on its Mint form,
    // whose three fields are its clause's three writers, F, K and D. Its
    // date stands at 0, so a bond of maturity 0 is a coin.
    await compose('Mint', ['usd', '30', '0']);
    expect(sends, isEmpty);
    expect(screen(), contains('minted(30, 0)'));
    expect(
        lines,
        anyElement(
            contains('msg(agent, person, holdings([lot(cb, usd, 0, 30)]))')));
    // And that holdings message is a row of the balances view the manifest
    // declares: the bond is named by every argument of the lot but the last —
    // its issuer, its denomination and its maturity — and the last is how many
    // of it are held.
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'lot(cb, usd, 0)': '30'});
    // The person reads that row on the screen: the bond by its default
    // display, the arguments in order, and the count beside it. The lot is
    // never shown as its term (vGLP, Definition "Display Declaration").
    expect(find.text('BALANCES'), findsOneWidget);
    final row = find.widgetWithText(ListTile, 'cb usd 0');
    expect(row, findsOneWidget);
    expect(find.descendant(of: row, matching: find.text('30')), findsOneWidget);
    expect(find.text('lot(cb, usd, 0, 30)'), findsNothing);
    // Answering consumed one ask and the goal posed the next: the form stands
    // again, with a fresh ReqId.
    expect(r.standing.containsKey('agent_1'), isTrue);

    // --- Act 2: the mutual credit line, proposed to Diana on the Swap form —
    // ten of its coins for ten of hers, at par — its eight fields the eight
    // writers Q, F, U, D, K, V, D1, K1. Diana's script accepts, and the swap
    // is done on both screens.
    await compose(
        'Swap', ['diana', 'usd', 'cb', '0', '10', 'diana', '0', '10']);
    expect(sends, isEmpty);
    expect(screen(), contains('swap_done(diana)'));
    expect(lines, anyElement(contains('tagged(diana, swap_done(cb))')));
    expect(r.inbox, isEmpty);
  }, timeout: const Timeout(Duration(minutes: 3)));
}
