import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

// Mirror the app: onChange triggers a setState that rebuilds AgentSurface.
Widget _wrap(UiRuntime r) => MaterialApp(
      home: Scaffold(
        body: SizedBox(
          width: 360,
          height: 720,
          child: StatefulBuilder(
            builder: (context, setState) {
              r.onChange = () => setState(() {});
              return AgentSurface(agentId: 'Bob', runtime: r);
            },
          ),
        ),
      ),
    );

void main() {
  testWidgets('befriend notify renders an inbox card with Accept/Decline',
      (tester) async {
    final sent = <String>[];
    final r = UiRuntime(manifest: gsgManifest, onSend: sent.add);

    await tester.pumpWidget(_wrap(r));
    // No request yet.
    expect(find.text('No requests'), findsOneWidget);
    expect(find.textContaining('wants to connect'), findsNothing);

    // Deliver the boundary notify, as main.dart would after stripping '< '.
    r.handleLine('befriend(alice, req(1))');
    await tester.pump();

    expect(r.inbox.length, 1);
    expect(find.text('alice wants to connect'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'Decline'), findsOneWidget);

    // Tapping Accept sends decision(yes, alice, req(1)) and clears the card.
    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pump();
    expect(sent, ['decision(yes, alice, req(1))']);
    expect(find.text('alice wants to connect'), findsNothing);
  });

  testWidgets('connected notify renders the friend in the Friends section',
      (tester) async {
    final r = UiRuntime(manifest: gsgManifest, onSend: (_) {});
    await tester.pumpWidget(_wrap(r));

    r.handleLine('connected(bob)');
    await tester.pump();

    expect(find.widgetWithText(Chip, 'bob'), findsOneWidget);
  });
}
