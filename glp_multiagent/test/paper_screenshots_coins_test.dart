// Paper-quality screenshots of the coins-among-friends app (sec:ui-coins,
// app:coins), rendered from coinsManifest with real text: the wallet (people +
// holdings), a friend's coins with actions, and a swap proposal in Requests.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins.dart';
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
  testWidgets('coins app: Wallet, a friend\'s coins, swap request',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 840);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: coinsManifest, onSend: (_) {});

    Future<void> pump() => tester.pumpWidget(MaterialApp(
          debugShowCheckedModeBanner: false,
          theme: ThemeData(fontFamily: 'AppFont', useMaterial3: true, colorScheme: ColorScheme.fromSeed(seedColor: Colors.green)),
          home: Scaffold(
            backgroundColor: const Color(0xFF2B2B33),
            body: Center(
              child: RepaintBoundary(
                child: _phone(AgentSurface(agentId: 'You', runtime: r)),
              ),
            ),
          ),
        ));

    // Two friends in the social graph.
    r.handleLine('connected(alice)');
    r.handleLine('connected(bob)');
    // Your holdings: your own coins and some of each friend's.
    r.handleLine('balance_report(you, you, 12)');
    r.handleLine('balance_report(you, alice, 5)');
    r.handleLine('balance_report(you, bob, 3)');
    // Alice's holdings: her own coins and some of yours.
    r.handleLine('balance_report(alice, alice, 20)');
    r.handleLine('balance_report(alice, you, 4)');
    // Bob's holdings.
    r.handleLine('balance_report(bob, bob, 15)');
    // A swap Alice proposes: 2 of her coins for 3 of Bob's that you hold.
    r.handleLine('swap_offer(alice, alice, 2, bob, 3, req(1))');
    await pump();

    // Screen 1 — Wallet (you + friends, with holdings).
    expect(find.text('You'), findsOneWidget);
    expect(find.text('Alice'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_coins_wallet.png');

    // Screen 2 — Alice's coins, with actions.
    await tester.tap(find.text('Alice'));
    await tester.pumpAndSettle();
    expect(find.text("Alice's coins"), findsWidgets);
    expect(find.text('Redeem'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_coins_friend.png');
    // Back to the wallet list.
    await tester.tap(find.byType(BackButton));
    await tester.pumpAndSettle();

    // Screen 3 — Requests (the swap proposal).
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('alice proposes a swap'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_coins_request.png');
  });
}
