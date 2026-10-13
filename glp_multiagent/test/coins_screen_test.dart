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
    // The agent's output to its person, msg(agent, person, C), reaches her
    // only as the thread of construct 0, drawn at dispatch start, each
    // message by the widget of the reserved type Output ::= ScreenMsg, a
    // form headed msg whose ScreenContent is a menu of the screen's forms,
    // each headed by its functor and its fields labelled by their declared
    // types; the questions are numbered from 1 (vGLP 169b008, Definition
    // "Canonical Compilation"; vGLP, 2026-10-13 19:25 UTC, items 2 and 3).
    final out = r.bridge.construct(0)!;
    expect(formatTerm(out.widget), startsWith('thread(form(msg, '));
    /// What the agent has written to her, each message's content.
    List<String> screen() {
      final v = r.bridge.construct(0)!.view;
      return [
        if (v is GList)
          for (final m in v.items) formatTerm(ctorArgs(m).$2[2])
      ];
    }

    /// Her latest holdings.
    String holdings() =>
        screen().lastWhere((m) => m.startsWith('holdings('), orElse: () => '');

    /// The questions' constructs: all but the output thread.
    List<OpenConstruct> questions() =>
        [for (final c in r.bridge.constructs) if (c.id != 0) c];

    expect(questions().map((c) => formatTerm(c.widget)), [requestForm]);
    final form = questions().single;
    expect(form.id, 1);
    expect(formatTerm(form.view), 'input([])');
    expect(screen(), contains('opened(bob)'));

    await tester.pumpWidget(_phoneApp(AgentSurface(agentId: 'alice', runtime: r)));
    await tester.pumpAndSettle();

    /// Bring [f] on screen: the surface scrolls, the output thread above the
    /// questions growing with every message.
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
    Finder inOutput(Finder f) =>
        find.descendant(of: find.byKey(constructKey(0)), matching: f);

    /// The message [k] of the output thread, from 0: headed msg, its content
    /// a form headed [heading] whose fields are [fields], label and value.
    void message(int k, String heading, List<(String, String)> fields) {
      final at = elementAt(const [], k);
      Finder headed(List<int> at, String h) => find.descendant(
          of: find.byKey(nodeKey(0, 'heading', at)), matching: find.text(h));
      expect(headed(at, 'msg'), findsOneWidget, reason: 'msg at $at');
      final content = [...at, 3];
      expect(headed(content, heading), findsOneWidget, reason: heading);
      final form = find.byKey(nodeKey(0, 'form', content));
      for (final (label, value) in fields) {
        expect(find.descendant(of: form, matching: find.text(label)),
            findsWidgets, reason: '$heading: $label');
        expect(find.descendant(of: form, matching: find.text(value)),
            findsWidgets, reason: '$heading: $value');
      }
    }

    /// Choose one of the menu's forms, fill its fields, and send it: one
    /// request on the request form, one element of the stream.
    Future<void> request(String alternative, Map<List<int>, (String, String)> fields,
        {String? formShot}) async {
      await show(inForm(find.widgetWithText(ChoiceChip, alternative)));
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
      // The person's form headed by its functor, as the program's are.
      expect(
          find.descendant(
              of: find.byKey(nodeKey(form.id, 'heading', const [])),
              matching: find.text(alternative)),
          findsOneWidget);
      await show(find.byKey(constructKey(form.id)));
      if (formShot != null) await _shot(tester, formShot);
      await show(find.byKey(submitKey(form.id, const [])));
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
    expect(screen(), contains('minted(2)'));
    expect(holdings(), 'holdings([lot(alice, 2)])');
    // The messages read with their functors and labelled fields, and never
    // as terms: opened(bob), its Key bob; minted(2), its Integer 2.
    expect(screen().take(2), ['opened(bob)', 'minted(2)']);
    message(0, 'opened', [('Key', 'bob')]);
    message(1, 'minted', [('Integer', '2')]);
    expect(find.textContaining('opened(bob)'), findsNothing);
    expect(find.textContaining('minted(2)'), findsNothing);
    expect(find.text('BALANCES'), findsNothing);
    expect(find.text('SCREEN'), findsNothing);
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
    expect(screen(), contains('swap_done(bob)'));
    expect(holdings(), 'holdings([lot(bob, 2)])');
    message(screen().indexOf('swap_done(bob)'), 'swap_done', [('Key', 'bob')]);
    expect(find.textContaining('swap_done(bob)'), findsNothing);

    // --- The card: bob proposes the reverse swap, and the swap responder's
    // question, Card ::= card(Offer, YesNo?) in writer mode, is one construct:
    // the offer shown, and the buttons yes and no at its answer.  The offer
    // is a tuple the program writes, Offer ::= offer(From, Want, Offered):
    // "a form with one field per argument, each labelled by the name of its
    // declared type", shown and not edited, its lot, Want ::= Lot and Lot ::=
    // lot(Issuer, Amount), a form of its own, and its coins, Offered ::=
    // Stream(Coin) and Coin ::= coin(Issuer, Serial), a thread of forms, each
    // field's type named by the variable its clause gives it (Definition
    // "Widget Declaration, Default Widget"; vGLP #8 Cowork, 2026-10-13 08:15
    // UTC, item b; Currencies, 2026-10-13 10:33 UTC). --------------------
    const cardWidget = "form(card, [field('Offer', form(offer, "
        "[field('From', shown), field('Want', form(lot, [field('Issuer', shown), "
        "field('Amount', shown)])), field('Offered', thread(form(coin, "
        "[field('Issuer', shown), field('Serial', shown)])))])), "
        "field('YesNo', buttons([yes, no]))])";
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
    // The card below the output thread and the request form: scrolled to.
    await show(find.byKey(constructKey(card.id)));
    // The card shows the offer as its scalars, never as its term.
    expect(inCard(find.textContaining('lot(bob, 2)')), findsNothing);
    expect(inCard(find.textContaining('offer(')), findsNothing);
    expect(find.byKey(nodeKey(card.id, 'form', const [1])), findsOneWidget);
    // Each field of the offer labelled by its declared type's name: the
    // offer's From, Want and Offered; the lot's Issuer and Amount; and each
    // coin's Issuer and Serial.
    for (final (label, n) in [
      ('Offer', 1),
      ('From', 1),
      ('Want', 1),
      ('Offered', 1),
      ('Issuer', 3),
      ('Amount', 1),
      ('Serial', 2),
    ]) {
      expect(inCard(find.text(label)), findsNWidgets(n), reason: label);
    }
    expect(find.byKey(nodeKey(card.id, 'form', const [1, 2])), findsOneWidget);
    expect(find.byKey(nodeKey(card.id, 'shown', const [1, 1])), findsOneWidget);
    expect(find.byKey(nodeKey(card.id, 'thread', const [1, 3])), findsOneWidget);
    expect(inCard(find.text('bob')), findsNWidgets(2));
    expect(inCard(find.text('alice')), findsNWidgets(2));
    expect(inCard(find.text('2')), findsNWidgets(2));
    expect(inCard(find.text('1')), findsOneWidget);
    // Each form headed by its functor (vGLP f9b63d6, Definition "Widget
    // Declaration, Default Widget": "a tuple: a form headed by its
    // functor"): the card, its offer, the offer's lot and each coin.
    for (final (heading, at) in [
      ('card', const <int>[]),
      ('offer', const [1]),
      ('lot', const [1, 2]),
      ('coin', const [1, 3, 1]),
      ('coin', const [1, 3, 2, 1]),
    ]) {
      expect(
          find.descendant(
              of: find.byKey(nodeKey(card.id, 'heading', at)),
              matching: find.text(heading)),
          findsOneWidget,
          reason: '$heading at $at');
    }
    expect(inCard(find.text('coin')), findsNWidgets(2));
    // Shown and not edited: no field of the card takes text.
    expect(inCard(find.byType(TextField)), findsNothing);
    expect(find.text('swap_done(bob)'), findsNothing);
    expect(inCard(find.widgetWithText(ElevatedButton, 'yes')), findsOneWidget);
    expect(inCard(find.widgetWithText(ElevatedButton, 'no')), findsOneWidget);
    await show(find.byKey(constructKey(card.id)));
    await _shot(tester, '/private/tmp/coins-card.png');

    await show(inCard(find.widgetWithText(ElevatedButton, 'yes')));
    await tester.tap(inCard(find.widgetWithText(ElevatedButton, 'yes')));
    await tester.pumpAndSettle();
    await settle();
    expect(sends, isEmpty);
    // Answered, the card is withdrawn; the request form stands.
    expect(r.bridge.construct(card.id), isNull);
    expect(find.byKey(constructKey(card.id)), findsNothing);
    expect(questions().map((c) => c.id), [form.id]);
    expect(holdings(), 'holdings([lot(alice, 2)])');
    await _shot(tester, '/private/tmp/coins-accepted.png');
  });
}
