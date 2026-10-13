import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/social_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

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
  testWidgets('an introduction alerts a Friends row; tapping it accepts',
      (tester) async {
    final sent = <String>[];
    // The introduction ask is the social-graph platform's (its mediator emits
    // befriend_intro and takes accept_intro/reject_intro), so it renders from
    // the social manifest.
    final r = UiRuntime(manifest: socialManifest, onSend: (t) => sent.add(formatTerm(t)));
    await tester.pumpWidget(_wrap(r));

    r.handle(fixtureTerm('befriend_intro(alice, charlie, req(5))'));
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

  // GSG Section 5.1: "A message the person addresses to someone who is not
  // their friend is not sent, and the agent tells the person so."  Each
  // mediator passes the agent's not_a_friend(Target) to the person; the screen
  // shows it, and it adds no friend.  Before its manifest named it, the runtime
  // dropped it as a notify it did not know.
  for (final (name, manifest) in [
    ('the social graph', socialManifest),
  ]) {
    testWidgets('$name: not_a_friend(Who) is shown to the person',
        (tester) async {
      final r = UiRuntime(manifest: manifest, onSend: (_) {});
      await tester.pumpWidget(_wrap(r));

      r.handle(fixtureTerm('not_a_friend(carol)'));
      await tester.pump();

      expect(find.widgetWithText(SnackBar, 'carol is not your friend — not sent'),
          findsOneWidget);
      expect(r.store.lists['friends'], isEmpty);
      expect(find.text('No friends yet'), findsOneWidget);
    });
  }
}
