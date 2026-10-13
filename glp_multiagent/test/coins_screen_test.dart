/// The currency mini-app in use: a mint and a swap, willed through the
/// constructs the Dart bridge draws, and a swap of bob's accepted on its
/// card.
///
/// The whole path runs: `programs/currencies/coins` at `coins_ui/3` — the
/// mini-app's bridge entry (`programs/currencies/coins/play_ui.glp`), alice's
/// execution of the mini-app on the person channel `ch(Grants?, Draws)` of
/// vGLP's Definition "Person Channel, Person Writer, GLP with Persons,
/// Grant", and bob's, scripted, over one conversation.  The compiled
/// program's dispatcher draws each construct, `draw(Id, W, V)`, and withdraws
/// it, `withdraw(Id)`; the bridge draws each as the default widget of its
/// interactive type (Definition "Widget Declaration, Default Widget"), and
/// alice's taps and submissions on them go back as her grants, `input(Id, P,
/// R)` (vGLP, sections/elicitation.tex, the paragraph "The implementation");
/// the agent's screen comes back to the balances view and the default
/// display.  Nothing is simulated and no term is hand-fed.
///
/// The play is run directly, as test code, with no host: the host posts
/// `superapp/3` and nothing of Currencies' (GSG Section 5.1; Currencies #7
/// Cowork, 2026-10-04 09:15 UTC, item 2).  The engine loads the coins program
/// and posts `coins_ui(alice, Grants?, [])` through its posting call, which
/// checks the goal as every posted goal is checked (TGLP modules.tex, "the
/// initial goal posted to the runtime ... is type-checked before execution as
/// a body goal"; GlpEngine.postGoal); alice's grants go into `Grants` through
/// the injector the call returns, and what `send_to_user/1` sends her is read
/// off the runtime's output.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_multiagent/isolate_protocol.dart'
    show runtimeTermOf, screenTermOf;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

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
    // What send_to_user/1 sends alice, each term whole.
    final notifies = <GTerm>[];
    final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp');
    engine.runtime.outputTermCallback = (t) => notifies.add(screenTermOf(t));
    // programs/currencies/coins is a program: currency/ is the certified
    // mini-app and this directory adds the harness that runs it for a live
    // person, play_ui.glp's coins_ui/3, the bridge entry, which reads alice's
    // grants on its second argument and not its third.
    expect(engine.loadProgram('$repo/currencies/coins'), isTrue);
    final play = engine.postGoal('coins_ui(alice, Grants?, [])',
        inputs: ['Grants']);
    final grants = play.inputs['Grants']!;

    /// The play run until quiescent, as one event of the agent's is: a run
    /// stopped at the net is half a run, and nothing after it means what it
    /// says.
    void drain() {
      final result = play.scheduler.drainToQuiescence(maxCycles: 200000);
      expect(result.status, isNot(ExecutionStatus.capped),
          reason: 'the play did not quiesce');
    }

    final sends = <GTerm>[];
    final r = UiRuntime(manifest: coinsManifest, onSend: sends.add);

    var fed = 0;
    void replay() {
      for (; fed < notifies.length; fed++) {
        r.handle(notifies[fed]);
      }
    }

    // Each of alice's grants, a ground term, onto her person channel.
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

    /// The open constructs whose widget is [w].
    List<OpenConstruct> drawn(String w) => [
          for (final c in r.bridge.constructs)
            if (formatTerm(c.widget) == w) c,
        ];

    // The agent asks its question as soon as it runs, before the person has
    // done anything: the stream of her requests, one construct, drawn as the
    // default widget of Stream(Request)? --- an input box of a menu of the
    // four forms of Request ::= mint(Amount) ; swap(Friend, GiveCoin,
    // GiveAmount, WantCoin, WantAmount) ; pay(Friend, Amount) ;
    // redeem(Friend, WantCoin), each field labelled by the name of its
    // declared type, a Key a peer field.
    const requestForm = "input_box(menu([form(mint, [field('Amount', number)]), "
        "form(swap, [field('Friend', peer), field('GiveCoin', peer), field('GiveAmount', number), "
        "field('WantCoin', peer), field('WantAmount', number)]), "
        "form(pay, [field('Friend', peer), field('Amount', number)]), "
        "form(redeem, [field('Friend', peer), field('WantCoin', peer)])]))";
    expect(r.bridge.constructs.map((c) => formatTerm(c.widget)), [requestForm]);
    final form = r.bridge.constructs.single;
    expect(formatTerm(form.view), 'input([])');
    expect(r.store.lists['screen']!.map(formatTerm), contains('opened(bob)'));

    await tester.pumpWidget(_phoneApp(AgentSurface(agentId: 'alice', runtime: r)));
    await tester.pumpAndSettle();

    Finder inForm(Finder f) =>
        find.descendant(of: find.byKey(constructKey(form.id)), matching: f);

    /// Choose one of the menu's forms, fill its fields, and send it: one
    /// request on the request form, one element of the stream.
    Future<void> request(String alternative, Map<List<int>, (String, String)> fields,
        {String? formShot}) async {
      await tester.tap(inForm(find.widgetWithText(ChoiceChip, alternative)));
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
      if (formShot != null) await _shot(tester, formShot);
      await tester.tap(find.byKey(submitKey(form.id, const [])));
      await tester.pumpAndSettle();
      await settle();
    }

    // The form offers the four requests, a menu of their forms, and only
    // those: the alternatives of Request.
    for (final label in ['mint', 'swap', 'pay', 'redeem']) {
      expect(inForm(find.widgetWithText(ChoiceChip, label)), findsOneWidget);
    }
    expect(inForm(find.byType(ChoiceChip)), findsNWidgets(4));
    await _shot(tester, '/private/tmp/coins-forms.png');

    // --- The mint: the mint form's one field is its one argument. ------------
    await request('mint', {
      [1]: ('number', '2')
    });
    expect(r.store.lists['screen']!.map(formatTerm), contains('minted(2)'));
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'alice': '2'});
    expect(find.text('BALANCES'), findsOneWidget);
    // The default display shows the message as its scalars, never as a term
    // (vGLP Section 6.3).
    expect(find.text('SCREEN'), findsOneWidget);
    expect(find.text('minted(2)'), findsNothing);
    await _shot(tester, '/private/tmp/coins-minted.png');

    // The question stands: a stream question takes one element per
    // submission and stays open while its stream does (vGLP, Remark
    // "Persistence"), so the same construct takes the next request.
    expect(r.bridge.construct(form.id), isNotNull);
    expect(find.byKey(constructKey(form.id)), findsOneWidget);

    // --- The swap: alice gives her 2 for bob's 2; bob's script accepts. -----
    // The swap form's five fields are the arguments of swap(Friend, GiveCoin,
    // GiveAmount, WantCoin, WantAmount), in order: the counterparty, and each
    // lot's issuer, a Key and so a peer field, and number.
    await request(
        'swap',
        {
          [1]: ('peer', 'bob'),
          [2]: ('peer', 'alice'),
          [3]: ('number', '2'),
          [4]: ('peer', 'bob'),
          [5]: ('number', '2'),
        },
        formShot: '/private/tmp/coins-swap-form.png');
    expect(r.store.lists['screen']!.map(formatTerm), contains('swap_done(bob)'));
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'bob': '2'});

    // --- The card: bob proposes the reverse swap, and the swap responder's
    // question, Card ::= card(Offer, YesNo?) in writer mode, is one construct:
    // the offer shown, and the buttons yes and no at its answer. -------------
    const cardWidget =
        "form(card, [field('Offer', shown), field('YesNo', buttons([yes, no]))])";
    final cards = drawn(cardWidget);
    expect(cards, hasLength(1));
    final card = cards.single;
    final (_, cardArgs) = ctorArgs(card.view);
    final (_, offer) = ctorArgs(cardArgs[0]);
    expect(formatTerm(offer[0]), 'bob');
    expect(formatTerm(offer[1]), 'lot(bob, 2)');
    expect(formatTerm(cardArgs[1]), 'input([2])');
    Finder inCard(Finder f) =>
        find.descendant(of: find.byKey(constructKey(card.id)), matching: f);
    // The card shows the offer as its scalars, never as its term.
    expect(inCard(find.textContaining('lot(bob, 2)')), findsNothing);
    expect(inCard(find.textContaining('offer(')), findsNothing);
    expect(find.byKey(nodeKey(card.id, 'shown', const [1])), findsOneWidget);
    expect(find.text('swap_done(bob)'), findsNothing);
    expect(inCard(find.widgetWithText(ElevatedButton, 'yes')), findsOneWidget);
    expect(inCard(find.widgetWithText(ElevatedButton, 'no')), findsOneWidget);
    await _shot(tester, '/private/tmp/coins-card.png');

    await tester.tap(inCard(find.widgetWithText(ElevatedButton, 'yes')));
    await tester.pumpAndSettle();
    await settle();
    expect(sends, isEmpty);
    // Answered, the card is withdrawn; the request form stands.
    expect(r.bridge.construct(card.id), isNull);
    expect(find.byKey(constructKey(card.id)), findsNothing);
    expect(r.bridge.constructs.map((c) => c.id), [form.id]);
    expect(
        r.store.balances['balances']!
            .map((k, v) => MapEntry(k, formatTerm(v))),
        {'alice': '2'});
    await _shot(tester, '/private/tmp/coins-accepted.png');
  });
}
