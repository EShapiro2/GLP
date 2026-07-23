// Paper figure fig:grassapp — the one GrassApp build, its three panels shot
// from the LIVE scenario: the real agent + mediator + actors run headlessly
// (AgentRuntime, the same path the app uses), the person's taps are injected
// UserCmds, and the surface renders the notify stream the run actually
// produces. Friends (the social graph), Coins (the wallet, organised by
// friend), and Chats (the social network) — one interpreter, one runtime; the
// panels differ only by manifest. Phone-framed with the green chrome and 9:41
// status bar to match the live app. Outputs are named for the
// \includegraphics in sections/ui-primitives.tex.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassroots.dart';
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

Widget _phone(Widget surface) => Container(
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
                          color: Colors.white,
                          fontSize: 12,
                          fontWeight: FontWeight.w700)),
                  Row(children: [
                    Icon(Icons.signal_cellular_alt,
                        color: Colors.white, size: 13),
                    SizedBox(width: 5),
                    Icon(Icons.wifi, color: Colors.white, size: 13),
                    SizedBox(width: 5),
                    Icon(Icons.battery_full, color: Colors.white, size: 14),
                  ]),
                ],
              ),
            ),
            Expanded(child: surface),
          ]),
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
  testWidgets('fig:grassapp — Friends, Coins, Chats panels of one GrassApp',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    // The live scenario, headless: the same sources the app loads.
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

    // The surface over the run's own notify stream; the person's commands go
    // back into the run (as main.dart wires them, minus the isolate).
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

    // The person's tap: answer a card on the surface (consuming it) and let
    // the granted command flow into the run.
    Future<void> tapAnswer(String notifyCtor, String itemKey, String label) async {
      final card = r.inbox.firstWhere(
          (c) => c.desc.notifyCtor == notifyCtor && c.itemKey == itemKey,
          orElse: () => fail('no $notifyCtor card for $itemKey'));
      final answer = card.desc.answers.firstWhere((a) => a.label == label);
      r.answerCard(card, answer);
      await flushSends();
    }

    // The person accepts Eve, Dana, and Alice; Charlie's offer stays pending.
    // Eve greets Bob in chat and pays him; Dana pays and proposes a swap,
    // which Bob accepts, and Dana then redeems a bob-coin; Alice pays and
    // proposes a swap that stays pending on her Coins row.
    await tapAnswer('befriend', 'eve', 'Accept');
    await tapAnswer('befriend', 'dana', 'Accept');
    await tapAnswer('swap_offer', 'dana', 'Accept');
    await tapAnswer('befriend', 'alice', 'Accept');

    // Bob answers Eve's greeting; she replies.
    r.sendChat(chatView, 'eve', 'thanks eve');
    await flushSends();

    Future<void> pump() => tester.pumpWidget(MaterialApp(
          debugShowCheckedModeBanner: false,
          theme: ThemeData(
              fontFamily: 'AppFont',
              useMaterial3: true,
              colorScheme: ColorScheme.fromSeed(seedColor: Colors.green)),
          home: Scaffold(
            backgroundColor: const Color(0xFF2B2B33),
            body: Center(
              child: RepaintBoundary(
                child: _phone(AgentSurface(agentId: 'Bob', runtime: r)),
              ),
            ),
          ),
        ));
    await pump();

    // Panel 1 — Friends: Eve, Dana, Alice (friends) and Charlie (the pending
    // friend offer, alerting his row).
    expect(find.text('Eve'), findsOneWidget);
    expect(find.text('Dana'), findsOneWidget);
    expect(find.text('Alice'), findsOneWidget);
    expect(find.text('charlie wants to connect'), findsOneWidget);
    await _shot(tester, '/private/tmp/gsg-app-friends.png');

    // Panel 2 — Coins: the wallet, organised by friend; Alice's row alerts her
    // proposed swap; Dana's redeem has settled.
    await tester.tap(find.text('Coins').last);
    await tester.pumpAndSettle();
    expect(find.text('You'), findsOneWidget);
    expect(find.text('alice proposes a swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/coins-wallet.png');

    // Panel 3 — Chats: the conversations friendship opened; Eve's carries the
    // greeting, Bob's answer, and her reply.
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    expect(find.text('Eve'), findsWidgets);
    await _shot(tester, '/private/tmp/grassapp-chats.png');
  });
}
