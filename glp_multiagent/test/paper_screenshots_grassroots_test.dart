// Paper figure of the running GrassApp — the one interpreter rendering the
// unified manifest as four screens, one per app surface: messaging (Chats),
// the coins wallet, a friend's coins (the transactions), and the shared inbox
// (Requests). Matches the live app: green chrome, phone status bar.
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

// Phone frame matching the live app: green status bar with a clock and signal
// / wifi / battery icons.
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
  final b = tester.renderObject<RenderRepaintBoundary>(
      find.byType(RepaintBoundary).first);
  final bytes = await tester.runAsync(() async {
    final img = await b.toImage(pixelRatio: 3.0);
    final d = await img.toByteData(format: ui.ImageByteFormat.png);
    return d!.buffer.asUint8List();
  });
  File(path).writeAsBytesSync(bytes!);
}

void main() {
  testWidgets('GrassApp: Chats, Wallet, a friend\'s coins, Requests',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: grassrootsManifest, onSend: (_) {});

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
    // pending friend offer; Alice has proposed a swap.
    r.handleLine('balance_report(bob, bob, 9)');
    r.handleLine('balance_report(bob, alice, 2)');
    r.handleLine('connected(alice)');
    r.handleLine("received(alice, 'Thanks_for_the_coins')");
    r.sendChat(grassrootsManifest.chat!, 'alice', 'Anytime');
    r.handleLine('balance_report(alice, alice, 3)');
    r.handleLine('befriend(charlie, req(2))');
    r.handleLine('swap_offer(alice, alice, 1, bob, 1, req(3))');
    await pump();

    // Screen 1 — Chats (the messaging app).
    expect(find.text('Alice'), findsWidgets);
    await _shot(tester, '/private/tmp/fig_gr_chats.png');

    // Screen 2 — Wallet (the coins app).
    await tester.tap(find.text('Wallet').last);
    await tester.pumpAndSettle();
    expect(find.text('You'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_gr_wallet.png');

    // Screen 3 — Requests (the shared inbox: friend offer + swap offer).
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('charlie wants to connect'), findsOneWidget);
    expect(find.text('alice proposes a swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_gr_requests.png');

    // Screen 4 — a friend's coins, with the transactions.
    await tester.tap(find.text('Wallet').last);
    await tester.pumpAndSettle();
    await tester.tap(find.text('Alice'));
    await tester.pumpAndSettle();
    expect(find.text('Propose swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_gr_friend.png');
  });
}
