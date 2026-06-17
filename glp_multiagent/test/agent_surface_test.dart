import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';

// Mirror the app: onChange triggers a setState that rebuilds AgentSurface.
Widget _wrap(UiRuntime r) => MaterialApp(
      home: SizedBox(
        width: 380,
        height: 760,
        child: StatefulBuilder(
          builder: (context, setState) {
            r.onChange = () => setState(() {});
            return AgentSurface(agentId: 'Bob', runtime: r);
          },
        ),
      ),
    );

void main() {
  testWidgets('befriend notify becomes an inbox card on the Requests screen',
      (tester) async {
    final sent = <String>[];
    final r = UiRuntime(manifest: gsgManifest, onSend: sent.add);

    await tester.pumpWidget(_wrap(r));
    // Default screen is Friends (the state surface), empty.
    expect(find.text('No friends yet'), findsOneWidget);

    // Deliver the boundary notify (main.dart strips '< ' first).
    r.handleLine('befriend(alice, req(1))');
    await tester.pump();

    // The card lives on the Requests (inbox) screen — switch to it.
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();

    expect(r.inbox.length, 1);
    expect(find.text('alice wants to connect'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);

    // Accept sends decision(yes, alice, req(1)) and clears the card.
    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pump();
    expect(sent, ['decision(yes, alice, req(1))']);
    expect(find.text('alice wants to connect'), findsNothing);
  });

  testWidgets('befriend_intro Decline sends reject_intro WITHOUT the req id',
      (tester) async {
    final sent = <String>[];
    final r = UiRuntime(manifest: gsgManifest, onSend: sent.add);
    await tester.pumpWidget(_wrap(r));

    r.handleLine('befriend_intro(alice, bob, req(2))');
    await tester.pump();
    await tester.tap(find.text('Requests').last);
    await tester.pumpAndSettle();

    await tester.tap(find.widgetWithText(OutlinedButton, 'Decline'));
    await tester.pump();
    expect(sent, ['reject_intro(bob)']);
  });

  testWidgets('connected adds the friend to the Friends screen', (tester) async {
    final r = UiRuntime(manifest: gsgManifest, onSend: (_) {});
    await tester.pumpWidget(_wrap(r));

    r.handleLine('connected(bob)');
    await tester.pump();

    // Friends is the default screen; the friend is listed (capitalised).
    expect(find.text('Bob'), findsOneWidget);
  });
}
