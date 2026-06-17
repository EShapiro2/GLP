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
  testWidgets('render bob scenario panel: two cards, then one accepted',
      (tester) async {
    tester.view.physicalSize = const Size(900, 1400);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final sent = <String>[];
    final bob = UiRuntime(manifest: gsgManifest, onSend: sent.add);

    late StateSetter rebuild;
    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      home: Scaffold(
        body: RepaintBoundary(
          child: StatefulBuilder(builder: (c, setState) {
            rebuild = setState;
            bob.onChange = () => setState(() {});
            return AgentSurface(agentId: 'Bob', runtime: bob);
          }),
        ),
      ),
    ));

    // Exactly what the scenario boot emits on launch.
    bob.handleLine('befriend(alice, req(1))');
    bob.handleLine('befriend(charlie, req(2))');
    rebuild(() {});
    await tester.pump();
    expect(bob.inbox.length, 2);
    expect(find.text('alice wants to connect'), findsOneWidget);
    expect(find.text('charlie wants to connect'), findsOneWidget);
    await _dump(tester, '/private/tmp/scenario_two_cards.png');

    // Accept alice's card -> decision sent, card clears, friend added.
    final card = bob.inbox.firstWhere((c) => c.desc.notifyCtor == 'befriend');
    final accept = card.desc.answers.firstWhere((a) => a.label == 'Accept');
    bob.answerCard(card, accept);
    bob.handleLine('connected(alice)'); // agent's reply after accept
    rebuild(() {});
    await tester.pump();
    expect(find.widgetWithText(Chip, 'alice'), findsOneWidget);
    await _dump(tester, '/private/tmp/scenario_after_accept.png');

    // ignore: avoid_print
    print('SENT: $sent');
  });
}
