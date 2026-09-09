/// The currency mini-app in use: a mint and a swap, willed through the screen
/// derived from `coins_agent.vglp`'s display declarations and carried by the
/// person channel of the compiled program.
///
/// The whole path runs: `programs/coins` at `coins_ui/3` — alice's execution of
/// the mini-app with its mediator, and bob's, scripted, over one conversation —
/// the cards arriving as ground terms, the person's taps on the derived
/// constructs going back as `answer(req(N), xs_C(...))`, and the agent's screen
/// coming back to the balances view and the default display. Nothing is
/// simulated and no term is hand-fed.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

Future<void> _loadFonts() async {
  Future<void> add(FontLoader l, String p) async =>
      l.addFont(Future.value(ByteData.view(File(p).readAsBytesSync().buffer)));
  final f = FontLoader('AppFont');
  await add(f, '/System/Library/Fonts/Supplemental/Arial.ttf');
  await add(f, '/System/Library/Fonts/Supplemental/Arial Bold.ttf');
  await f.load();
  final mi = FontLoader('MaterialIcons');
  await add(mi,
      '/opt/homebrew/share/flutter/bin/cache/artifacts/material_fonts/MaterialIcons-Regular.otf');
  await mi.load();
}

Widget _phoneApp(Widget surface) => Directionality(
      textDirection: TextDirection.ltr,
      child: Container(
        color: const Color(0xFF2B2B33),
        child: Center(
          child: RepaintBoundary(
            child: Container(
              width: 360,
              height: 740,
              padding: const EdgeInsets.all(10),
              decoration: BoxDecoration(
                  color: Colors.black, borderRadius: BorderRadius.circular(40)),
              child: ClipRRect(
                borderRadius: BorderRadius.circular(30),
                child: Container(
                  color: Colors.white,
                  child: MaterialApp(
                    debugShowCheckedModeBanner: false,
                    theme: ThemeData(
                        fontFamily: 'AppFont',
                        useMaterial3: true,
                        colorScheme:
                            ColorScheme.fromSeed(seedColor: Colors.green),
                        elevatedButtonTheme: ElevatedButtonThemeData(
                            style: ElevatedButton.styleFrom(
                                backgroundColor: Colors.green,
                                foregroundColor: Colors.white))),
                    home: surface,
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );

Future<void> _shot(WidgetTester tester, String path) async {
  final boundary = tester.renderObject<RenderRepaintBoundary>(
      find.byType(RepaintBoundary).first);
  final bytes = await tester.runAsync(() async {
    final img = await boundary.toImage(pixelRatio: 3.0);
    final data = await img.toByteData(format: ui.ImageByteFormat.png);
    return data!.buffer.asUint8List();
  });
  File(path).writeAsBytesSync(bytes!);
}

void main() {
  testWidgets('the currency mini-app: alice mints 2 and swaps them with bob',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';
    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'alice',
      glpSources: const [],
      // programs/coins is a program: currency/ is the certified mini-app and
      // this directory adds the harness that runs it for a live person.
      programDir: '$repo/coins',
      goalLabel: 'coins_ui/3',
      rootSelfGlpPath: '$repo/self.glp',
      friends: const ['bob'],
    );
    agent.onOutput = lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};

    final sends = <String>[];
    final r = UiRuntime(manifest: coinsManifest, onSend: sends.add);

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

    await tester.runAsync(() => agent.initialize());
    replay();

    // The compiled agent poses its four request clauses as soon as it runs, so
    // all four forms stand before the person has done anything.
    expect(r.standing.keys.toSet(),
        {'agent_1', 'agent_2', 'agent_3', 'agent_4'});
    expect(r.store.lists['screen']!.map(formatTerm), contains('opened(bob)'));

    await tester.pumpWidget(_phoneApp(AgentSurface(agentId: 'alice', runtime: r)));
    await tester.pumpAndSettle();

    /// Fill and submit one of the panel's compose forms.
    Future<void> compose(String label, List<String> values,
        {String? formShot, String? sheetShot}) async {
      await tester.tap(find.byType(FloatingActionButton));
      await tester.pumpAndSettle();
      if (sheetShot != null) await _shot(tester, sheetShot);
      await tester.tap(find.text(label));
      await tester.pumpAndSettle();
      final fields = find.byType(TextField);
      expect(fields, findsNWidgets(values.length));
      for (var i = 0; i < values.length; i++) {
        await tester.enterText(fields.at(i), values[i]);
      }
      if (formShot != null) {
        await tester.pumpAndSettle();
        await _shot(tester, formShot);
      }
      await tester.tap(find.widgetWithText(ElevatedButton, 'Send'));
      await tester.pumpAndSettle();
      await settle();
    }

    // The panel's "+" offers the four persistent clauses, and only those the
    // agent has posed: a volition is offered iff it is pending.
    await tester.tap(find.byType(FloatingActionButton));
    await tester.pumpAndSettle();
    for (final label in ['Mint', 'Swap', 'Pay', 'Redeem']) {
      expect(find.text(label), findsOneWidget);
    }
    await _shot(tester, '/private/tmp/coins-forms.png');
    await tester.tapAt(const Offset(180, 120)); // dismiss the sheet
    await tester.pumpAndSettle();

    // --- The mint: the Mint form's one field is its clause's one writer. ----
    await compose('Mint', ['2']);
    expect(r.store.lists['screen']!.map(formatTerm), contains('minted(2)'));
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'alice': '2'});
    expect(find.text('MINT'), findsNothing); // the form is not the panel
    expect(find.text('BALANCES'), findsOneWidget);
    await _shot(tester, '/private/tmp/coins-minted.png');

    // The question stands again: answering consumed one ask and the goal posed
    // the next, so the form is offered with a fresh ReqId.
    expect(r.standing.containsKey('agent_1'), isTrue);

    // --- The swap: alice gives her 2 for bob's 2; bob's script accepts. -----
    // The form's five fields are the five writers of its clause's question,
    // in the question's order.
    await compose('Swap', ['bob', 'alice', '2', 'bob', '2'],
        formShot: '/private/tmp/coins-swap-form.png');
    expect(r.store.lists['screen']!.map(formatTerm), contains('swap_done(bob)'));
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'bob': '2'});

    // --- The card: bob proposes the reverse swap, and both sibling clauses
    // are one card with a button each, each answering its own ReqId. ---------
    final card = r.inbox.single;
    expect(card.asks.keys.toSet(), {'respond_swap_1', 'respond_swap_2'});
    expect(card.itemKey, 'bob');
    expect(formatTerm(card.fields['Want']!), 'lot(bob, 2)');
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);
    await _shot(tester, '/private/tmp/coins-card.png');

    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pumpAndSettle();
    await settle();
    expect(sends, isEmpty);
    expect(r.inbox, isEmpty);
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'alice': '2'});
    await _shot(tester, '/private/tmp/coins-accepted.png');
  });
}
