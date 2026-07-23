import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassapp_ui.dart';
import 'package:glp_multiagent/manifests/social_ui.dart';
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
  testWidgets('a friend offer alerts a Friends row; tapping it accepts',
      (tester) async {
    final sent = <String>[];
    final r = UiRuntime(manifest: grassrootsManifest, onSend: sent.add);

    await tester.pumpWidget(_wrap(r));
    // Friends is the default panel, empty.
    expect(find.text('No friends yet'), findsOneWidget);

    // Deliver the boundary notify (main.dart strips '< ' first).
    r.handleLine('befriend(alice, req(1))');
    await tester.pump();

    // The offer is a per-item alert on the Friends panel — a row for the
    // offering person, no separate Requests screen.
    expect(find.text('Alice'), findsOneWidget);
    expect(find.text('alice wants to connect'), findsOneWidget);

    // Tapping the row opens the accept/decline sheet (a confirmed gesture).
    await tester.tap(find.text('alice wants to connect'));
    await tester.pumpAndSettle();
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);

    // Accept sends decision(yes, alice, req(1)) and clears the card (settle so
    // the closing sheet, which also shows the title, leaves the tree).
    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pumpAndSettle();
    expect(sent, ['decision(yes, alice, req(1))']);
    expect(find.text('alice wants to connect'), findsNothing);
  });

  testWidgets('an introduction alerts a Friends row; tapping it accepts',
      (tester) async {
    final sent = <String>[];
    // The introduction ask is the social-graph platform's (its mediator emits
    // befriend_intro and takes accept_intro/reject_intro), so it renders from
    // the social manifest; the grassapp mediator carries no such ask.
    final r = UiRuntime(manifest: socialManifest, onSend: sent.add);
    await tester.pumpWidget(_wrap(r));

    r.handleLine('befriend_intro(alice, charlie, req(5))');
    await tester.pump();

    expect(find.text('Charlie'), findsOneWidget);
    expect(find.text('alice introduces you to charlie'), findsOneWidget);

    await tester.tap(find.text('alice introduces you to charlie'));
    await tester.pumpAndSettle();
    expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
    expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);

    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pumpAndSettle();
    expect(sent, ['accept_intro(charlie, req(5))']);
  });

  testWidgets('connected lists the friend on the Friends panel',
      (tester) async {
    final r = UiRuntime(manifest: grassrootsManifest, onSend: (_) {});
    await tester.pumpWidget(_wrap(r));

    r.handleLine('connected(bob)');
    await tester.pump();

    // Friends is the default panel; the friend is listed (capitalised).
    expect(find.text('Bob'), findsOneWidget);
  });

  testWidgets('a swap offer alerts a Currencies row; tapping it accepts',
      (tester) async {
    final sent = <String>[];
    final r = UiRuntime(manifest: grassrootsManifest, onSend: sent.add);
    await tester.pumpWidget(_wrap(r));

    r.handleLine('connected(alice)');
    r.handleLine('trade_proposed(alice, bob, 0, 1, req(3))');
    await tester.pump();

    // Switch to the Currencies panel; the swap alerts Alice's wallet row.
    await tester.tap(find.text('Currencies').last);
    await tester.pumpAndSettle();
    expect(find.text('alice proposes a swap'), findsOneWidget);

    await tester.tap(find.text('alice proposes a swap'));
    await tester.pumpAndSettle();
    await tester.tap(find.widgetWithText(ElevatedButton, 'Accept'));
    await tester.pump();
    expect(sent, ['accept_trade(alice, req(3))']);
  });
}
