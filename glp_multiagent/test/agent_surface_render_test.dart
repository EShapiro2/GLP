import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

// Replicates main.dart _buildAgentPanel for a ui-backed agent: an AgentSurface
// in an Expanded inside a bordered Column. Three of these go in a Row, exactly
// like the live trio layout.
Widget _panel(String id, UiRuntime r) => Container(
      decoration: BoxDecoration(
        border: Border(right: BorderSide(color: Colors.grey.shade300)),
      ),
      child: Column(
        children: [
          Expanded(child: AgentSurface(agentId: id, runtime: r)),
          Container(height: 24, color: Colors.grey.shade200),
        ],
      ),
    );

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
  testWidgets('render live-trio layout to PNG (empty + befriend card)',
      (tester) async {
    tester.view.physicalSize = const Size(1200, 800);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final alice = UiRuntime(manifest: gsgManifest, onSend: (_) {});
    final bob = UiRuntime(manifest: gsgManifest, onSend: (_) {});
    final charlie = UiRuntime(manifest: gsgManifest, onSend: (_) {});

    late StateSetter rebuild;
    await tester.pumpWidget(MaterialApp(
      debugShowCheckedModeBanner: false,
      home: Scaffold(
        body: RepaintBoundary(
          child: StatefulBuilder(builder: (context, setState) {
            rebuild = setState;
            for (final r in [alice, bob, charlie]) {
              r.onChange = () => setState(() {});
            }
            return Row(children: [
              Expanded(child: _panel('Alice', alice)),
              Expanded(child: _panel('Bob', bob)),
              Expanded(child: _panel('Charlie', charlie)),
            ]);
          }),
        ),
      ),
    ));

    await _dump(tester, '/private/tmp/trio_empty.png');

    // Alice cold-calls Bob → Bob's mediator emits this notify.
    bob.handleLine('befriend(alice, req(1))');
    rebuild(() {});
    await tester.pump();

    expect(bob.inbox.length, 1);
    expect(find.text('alice wants to connect'), findsOneWidget);

    await _dump(tester, '/private/tmp/trio_card.png');
  });
}
