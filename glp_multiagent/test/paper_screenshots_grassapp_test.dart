// Paper figure fig:grassapp — the one GrassApp build, its three panels shot
// from the unified manifest: Friends (the social graph), Coins (the wallet,
// organised by friend), and Chats (the social network). One interpreter, one
// runtime; the panels differ only by manifest. Phone-framed with the green
// chrome and 9:41 status bar to match the live app. Outputs are named for the
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

    final r = UiRuntime(manifest: grassrootsManifest, onSend: (_) {});
    final chatView =
        grassrootsManifest.panels.firstWhere((p) => p.id == 'chats').chat!;

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

    // Bob's opening coins; Alice a friend who messaged and paid him; Charlie a
    // pending friend offer (alerts the Friends panel); Alice has proposed a swap
    // (alerts the Coins panel).
    r.handleLine('balance_report(bob, bob, 9)');
    r.handleLine('balance_report(bob, alice, 2)');
    r.handleLine('connected(alice)');
    r.handleLine("received(alice, 'Thanks_for_the_coins')");
    r.sendChat(chatView, 'alice', 'Anytime');
    r.handleLine('balance_report(alice, alice, 3)');
    r.handleLine('befriend(charlie, req(2))');
    r.handleLine('swap_offer(alice, alice, 1, bob, 1, req(3))');
    await pump();

    // Panel 1 — Friends: Alice (a friend) and Charlie (a friend offer) — the
    // only inbox alert grassapp's Friends panel produces. (No introduction: the
    // grassapp mediator emits no befriend_intro, so that alert can't occur here.)
    expect(find.text('Alice'), findsOneWidget);
    expect(find.text('charlie wants to connect'), findsOneWidget);
    await _shot(tester, '/private/tmp/gsg-app-friends.png');

    // Panel 2 — Coins: the wallet, organised by friend; Alice's row alerts the
    // proposed swap.
    await tester.tap(find.text('Coins').last);
    await tester.pumpAndSettle();
    expect(find.text('You'), findsOneWidget);
    expect(find.text('alice proposes a swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/coins-wallet.png');

    // Panel 3 — Chats: the conversation Alice's friendship opened.
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    expect(find.text('Alice'), findsWidgets);
    await _shot(tester, '/private/tmp/grassapp-chats.png');
  });
}
