import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

Future<void> _dump(WidgetTester tester, String path) async {
  final boundary = tester.renderObject<RenderRepaintBoundary>(
      find.byType(RepaintBoundary).first);
  final bytes = await tester.runAsync(() async {
    final img = await boundary.toImage(pixelRatio: 2.0);
    final data = await img.toByteData(format: ui.ImageByteFormat.png);
    return data!.buffer.asUint8List();
  });
  File(path).writeAsBytesSync(bytes!);
}

void main() {
  testWidgets('two screens: Requests (inbox) and Friends; no Activity',
      (tester) async {
    tester.view.physicalSize = const Size(820, 1500);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final sent = <String>[];
    final bob = UiRuntime(manifest: gsgManifest, onSend: sent.add);

    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      home: RepaintBoundary(
        child: AgentSurface(agentId: 'Bob', runtime: bob),
      ),
    ));

    // Scenario emits two cold-call cards.
    bob.handleLine('befriend(alice, req(1))');
    bob.handleLine('befriend(charlie, req(2))');
    await tester.pump();

    // Default screen is Friends (empty) with a badge of 2 on Requests.
    expect(find.text('No friends yet'), findsOneWidget);
    expect(find.text('Requests'), findsWidgets);
    await _dump(tester, '/private/tmp/sg_friends_empty.png');

    // Switch to the Requests (inbox) screen — two cards with Accept/Decline.
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();
    expect(find.text('alice wants to connect'), findsOneWidget);
    expect(find.text('charlie wants to connect'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsNWidgets(2));
    await _dump(tester, '/private/tmp/sg_requests.png');

    // Accept alice -> decision sent, card clears; connected adds her to Friends.
    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept').first);
    await tester.pump();
    bob.handleLine('connected(alice)');
    await tester.pump();
    expect(sent, contains('decision(yes, alice, req(1))'));

    // Back to Friends — alice is now listed (no Activity screen anywhere).
    await tester.tap(find.text('Friends').last);
    await tester.pumpAndSettle();
    expect(find.text('Alice'), findsOneWidget);
    expect(find.textContaining('Activity'), findsNothing);
    await _dump(tester, '/private/tmp/sg_friends_one.png');
  });
}
