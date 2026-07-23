// Paper figure fig:constructs — the manifest's constructs in the running app,
// shot from the LIVE scenario (same staging as the panel shots): a
// friend-offer card open with its Accept and Decline buttons, the pay form
// with its fields, and an open conversation with the persistent chat input.
// The whole MaterialApp sits inside the phone shell so sheets and dialogs
// render within the bezel. Outputs are named for the shot list in
// sections/ui-primitives.tex.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassapp_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
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

/// The phone shell with the MaterialApp inside it, so overlays (the card's
/// sheet, the form's dialog) render within the phone screen.
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
                  child: Column(children: [
                    Container(
                      height: 26,
                      color: Colors.green,
                      padding: const EdgeInsets.symmetric(horizontal: 16),
                      child: const Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Text('9:41',
                              style: TextStyle(
                                  fontFamily: 'AppFont',
                                  color: Colors.white,
                                  fontSize: 12,
                                  fontWeight: FontWeight.w700)),
                          Row(children: [
                            Icon(Icons.signal_cellular_alt,
                                color: Colors.white, size: 13),
                            SizedBox(width: 5),
                            Icon(Icons.wifi, color: Colors.white, size: 13),
                            SizedBox(width: 5),
                            Icon(Icons.battery_full,
                                color: Colors.white, size: 14),
                          ]),
                        ],
                      ),
                    ),
                    Expanded(
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
                  ]),
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
  testWidgets('fig:constructs — card, form, and chat input in the running app',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    // The live scenario, staged as for the panel shots.
    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';
    const files = [
      'self.glp',
      'currency_txn.glp',
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

    final sends = <String>[];
    final r = UiRuntime(manifest: grassrootsManifest, onSend: sends.add);
    final chatView =
        grassrootsManifest.panels.firstWhere((p) => p.id == 'chats').chat!;

    var fed = 0;
    void replay() {
      for (; fed < lines.length; fed++) {
        final l = lines[fed];
        if (l.startsWith('< ')) r.handleLine(l.substring(2));
      }
    }

    Future<void> flushSends() async {
      while (sends.isNotEmpty) {
        await tester.runAsync(() => agent.injectUserInput(sends.removeAt(0)));
      }
      replay();
    }

    await tester.runAsync(() => agent.initialize());
    replay();

    Future<void> tapAnswer(String notifyCtor, String itemKey, String label) async {
      final card = r.inbox.firstWhere(
          (c) => c.desc.notifyCtor == notifyCtor && c.itemKey == itemKey,
          orElse: () => fail('no $notifyCtor card for $itemKey'));
      final answer = card.desc.answers.firstWhere((a) => a.label == label);
      r.answerCard(card, answer);
      await flushSends();
    }

    await tapAnswer('befriend', 'eve', 'Accept');
    await tapAnswer('befriend', 'dana', 'Accept');
    await tapAnswer('trade_proposed', 'dana', 'Accept');
    await tapAnswer('befriend', 'alice', 'Accept');
    r.sendChat(chatView, 'eve', 'thanks eve');
    await flushSends();

    await tester
        .pumpWidget(_phoneApp(AgentSurface(agentId: 'Bob', runtime: r)));

    // Shot 1 — the friend-offer card: tapping Charlie's alerting row opens the
    // sheet whose buttons are the sibling clauses.
    await tester.tap(find.text('charlie wants to connect'));
    await tester.pumpAndSettle();
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);
    await _shot(tester, '/private/tmp/sim-card-offer.png');
    await tester.tapAt(const Offset(210, 200)); // dismiss the sheet
    await tester.pumpAndSettle();

    // Shot 2 — the pay form: Currencies → Dana's wallet → Pay. The friend is
    // fixed by the open wallet, so the form's fields are the coin and amount.
    await tester.tap(find.text('Currencies').last);
    await tester.pumpAndSettle();
    await tester.tap(find.text('Dana'));
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(ElevatedButton, 'Pay'));
    await tester.pumpAndSettle();
    expect(find.text('Coin (issuer)'), findsOneWidget);
    expect(find.text('Amount'), findsOneWidget);
    await _shot(tester, '/private/tmp/sim-form-pay.png');
    await tester.tap(find.widgetWithText(TextButton, 'Cancel'));
    await tester.pumpAndSettle();
    await tester.tap(find.byType(BackButton));
    await tester.pumpAndSettle();

    // Shot 3 — the persistent chat input: Chats → Eve's conversation, with the
    // greeting, Bob's answer, and her reply above the input.
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    await tester.tap(find.text('Eve'));
    await tester.pumpAndSettle();
    expect(find.text('welcome to grassroots bob'), findsOneWidget);
    expect(find.text('thanks eve'), findsOneWidget);
    expect(find.text('lovely day isnt it'), findsOneWidget);
    await _shot(tester, '/private/tmp/sim-chat-input.png');
  });
}
