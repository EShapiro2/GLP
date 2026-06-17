// Generates paper-quality screenshots of the GrassApp three surfaces with real
// text, phone-framed — for Figure fig:grassapp in the UIVE paper.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassapp.dart';
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
  testWidgets('GrassApp screenshots: Requests, Chats, Conversation',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 840);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: grassappManifest, onSend: (_) {});

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

    // Inbox: a friend offer and a group invitation.
    r.handleLine("befriend(carol, req(1))");
    r.handleLine("group_invite('Grassroots Devs', bob, req(2))");
    // Conversations (chat list + Bob's thread).
    r.handleLine("received(carol, 'You are connected')");
    r.handleLine("received(bob, 'Hi - this is Bob')");
    r.handleLine("sent(bob, 'Hey Bob!', delivered)");
    r.handleLine("sent(bob, 'See you at six', sent)");
    r.handleLine("received('Grassroots Devs', 'Alice: pushed the fix')");
    r.handleLine("received(mom, 'Call me back')");
    await pump();

    // Screen 1 — Requests (inbox).
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('carol wants to connect'), findsOneWidget);
    expect(find.text('Grassroots Devs'), findsWidgets);
    await _shot(tester, '/private/tmp/fig_grassapp_requests.png');

    // Screen 2 — Chats (chat list).
    await tester.tap(find.text('Chats').last);
    await tester.pumpAndSettle();
    expect(find.text('Bob'), findsOneWidget);
    expect(find.text('See you at six'), findsWidgets);
    await _shot(tester, '/private/tmp/fig_grassapp_chats.png');

    // Screen 3 — Conversation (open Bob).
    await tester.tap(find.text('Bob'));
    await tester.pumpAndSettle();
    expect(find.text('Hi - this is Bob'), findsOneWidget);
    expect(find.text('Hey Bob!'), findsOneWidget);
    await _shot(tester, '/private/tmp/fig_grassapp_conversation.png');
  });
}
