// Screenshots of the unified 3-tab Grassroots app (Chats | Wallet | Requests)
// rendered from grassrootsManifest with the live coins notify vocabulary.
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
              color: Colors.orange,
              padding: const EdgeInsets.symmetric(horizontal: 14),
              child: const Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text('grassroots',
                      style: TextStyle(
                          color: Colors.white,
                          fontSize: 11,
                          fontWeight: FontWeight.w600)),
                  Row(children: [
                    Icon(Icons.wifi, color: Colors.white, size: 13),
                    SizedBox(width: 5),
                    Icon(Icons.battery_full, color: Colors.white, size: 13),
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
  testWidgets('unified app: Chats, Wallet, Requests', (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: grassrootsManifest, onSend: (_) {});

    Future<void> pump() => tester.pumpWidget(MaterialApp(
          debugShowCheckedModeBanner: false,
          theme: ThemeData(fontFamily: 'AppFont', useMaterial3: true),
          home: Scaffold(
            backgroundColor: const Color(0xFF2B2B33),
            body: Center(
              child: RepaintBoundary(
                child: _phone(AgentSurface(agentId: 'Bob', runtime: r)),
              ),
            ),
          ),
        ));

    // Charlie still pending in Requests; Alice already a friend who transacted.
    r.handleLine('befriend(charlie, req(2))');
    r.handleLine('connected(alice)');
    r.handleLine("received(alice, 'Thanks_for_the_coins')");
    r.sendChat(grassrootsManifest.chat!, 'alice', 'Anytime');
    // Bob's own minted coins, and the 2 Alice paid him.
    r.handleLine('balance_report(bob, bob, 5)');
    r.handleLine('balance_report(bob, alice, 2)');
    // Alice's holdings (she kept 3 of her own).
    r.handleLine('balance_report(alice, alice, 3)');
    // Alice proposes a swap: 1 of hers for 1 of Bob's.
    r.handleLine('swap_offer(alice, alice, 1, bob, 1, req(3))');
    await pump();

    // Tab 2 — Wallet.
    await tester.tap(find.text('Wallet').last);
    await tester.pumpAndSettle();
    expect(find.text('You'), findsOneWidget);
    expect(find.text('Alice'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_gr_wallet.png');

    // Tab 3 — Requests (friend offer + swap offer share one inbox).
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('charlie wants to connect'), findsOneWidget);
    expect(find.text('alice proposes a swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_gr_requests.png');

    // Tab 1 — Chats.
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    expect(find.text('Alice'), findsWidgets);
    await _shot(tester, '/private/tmp/fig_gr_chats.png');
  });
}
