// Generates clean, paper-quality screenshots of the GSG two-surface UI with
// real text (Arial), phone-framed — for Figure fig:gsg in the UIVE paper.
import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

Future<void> _loadFont() async {
  Future<void> add(FontLoader l, String path) async {
    final b = File(path).readAsBytesSync();
    l.addFont(Future.value(ByteData.view(b.buffer)));
  }

  final l = FontLoader('AppFont');
  await add(l, '/System/Library/Fonts/Supplemental/Arial.ttf');
  await add(l, '/System/Library/Fonts/Supplemental/Arial Bold.ttf');
  await l.load();

  // Material icons (nav bar, status-bar glyphs) so they aren't placeholder boxes.
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
        color: Colors.black,
        borderRadius: BorderRadius.circular(40),
      ),
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
  testWidgets('GSG screenshots: Requests and Friends', (tester) async {
    await _loadFont();
    tester.view.physicalSize = const Size(420, 840);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final r = UiRuntime(manifest: gsgManifest, onSend: (_) {});

    Future<void> pumpSurface() => tester.pumpWidget(MaterialApp(
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

    // Two cold-call requests + an introduction -> the inbox screen.
    r.handleLine('befriend(alice, req(1))');
    r.handleLine('befriend_intro(charlie, dave, req(2))');
    await pumpSurface();
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    await _shot(tester, '/private/tmp/fig_gsg_requests.png');

    // Accept both, plus an earlier friend -> the friends screen.
    r.handleLine('connected(bob)');
    r.handleLine('connected(carol)');
    r.handleLine('connected(dave)');
    await pumpSurface();
    await tester.tap(find.text('Friends').last);
    await tester.pumpAndSettle();
    await _shot(tester, '/private/tmp/fig_gsg_friends.png');
  });
}
