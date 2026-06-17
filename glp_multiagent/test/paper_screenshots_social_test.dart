// Paper-quality screenshots of the running streamlined app (one connection,
// Requests + Chats + Conversation), rendered from socialManifest with real text.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/social.dart';
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
  testWidgets('streamlined app: Requests, Chats, Conversation', (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 840);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: socialManifest, onSend: (_) {});

    Future<void> pump() => tester.pumpWidget(MaterialApp(
          debugShowCheckedModeBanner: false,
          theme: ThemeData(fontFamily: 'AppFont', useMaterial3: true),
          home: Scaffold(
            backgroundColor: const Color(0xFF2B2B33),
            body: Center(
              child: RepaintBoundary(
                child: _phone(AgentSurface(agentId: 'You', runtime: r)),
              ),
            ),
          ),
        ));

    // Two friend offers in the inbox.
    r.handleLine("befriend(alice, req(1))");
    r.handleLine("befriend(carol, req(2))");
    // A few established friends, with messages → conversations.
    r.handleLine("connected(bob)");
    r.handleLine("received(bob, 'Hi - this is Bob')");
    r.sendChat(socialManifest.chat!, 'bob', 'See you at six');
    r.handleLine("connected(dave)");
    r.handleLine("received(dave, 'Lunch tomorrow?')");
    r.handleLine("received(mom, 'Call me back')");
    await pump();

    // Screen 1 — Requests.
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('alice wants to connect'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_app_requests.png');

    // Screen 2 — Chats.
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    expect(find.text('Bob'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_app_chats.png');

    // Screen 3 — Conversation (Bob).
    await tester.tap(find.text('Bob'));
    await tester.pumpAndSettle();
    expect(find.text('Hi - this is Bob'), findsOneWidget);
    expect(find.text('See you at six'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_app_conversation.png');
  });
}
