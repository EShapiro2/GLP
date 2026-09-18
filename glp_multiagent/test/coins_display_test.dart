/// The default display of a compound value, asserted where the rule is
/// shared: the currency mini-app's manifest over the generic surface, the
/// mediator's terms hand-fed as it sends them.
///
/// vGLP (`shapiro2026volition`, Section 6.3, the ruling of 2026-09-18): "A
/// value of a compound type has no widget of its own: under default display
/// it renders as its arguments, in order, each by the widget of its type, and
/// a list as one such group per element, a term never being shown to the
/// person."  A card's context and the panel's list are the two places the
/// currency mini-app shows a compound; both are asserted here on the one
/// implementation, `scalarGroups` in `term.dart`, together with the one-line
/// forms every other place goes through.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  late UiRuntime r;
  late List<String> sent;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: coinsManifest, onSend: sent.add);
  });

  group('a compound under default display shows its scalars', () {
    testWidgets('a context value of a card and a message of the panel\'s list',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      // The swap card: `Want` a lot, `Offered` a list of coins.
      r.handleLine('card(respond_swap_1, ctx_respond_swap_1(bob, lot(bob, 2), '
          '[coin(alice, 3)]), req(7))');
      // Two messages of the default display: a menu, whose second argument is
      // a list, and a message of one scalar.
      r.handleLine('msg(agent, person, menu(carol, [dave, erin]))');
      r.handleLine('msg(agent, person, opened(frank))');

      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      // The nearest row of scalars a text sits in.
      Finder rowOf(String text) =>
          find.ancestor(of: find.text(text), matching: find.byType(Wrap)).first;

      // The card: no term, and the scalars each once — `bob` twice, as `From`
      // and as the lot's issuer.
      expect(find.text('lot(bob, 2)'), findsNothing);
      expect(find.text('[coin(alice, 3)]'), findsNothing);
      expect(find.text('coin(alice, 3)'), findsNothing);
      expect(find.text('bob'), findsNWidgets(2));
      expect(find.text('2'), findsOneWidget);
      expect(find.text('alice'), findsOneWidget);
      expect(find.text('3'), findsOneWidget);
      // The lot's two scalars are one row; the coin's two are one row.
      expect(find.descendant(of: rowOf('2'), matching: find.text('bob')),
          findsOneWidget);
      expect(find.descendant(of: rowOf('alice'), matching: find.text('3')),
          findsOneWidget);

      // The list: no term, and the scalars each once.
      expect(find.text('SCREEN'), findsOneWidget);
      expect(find.text('menu(carol, [dave, erin])'), findsNothing);
      expect(find.text('opened(frank)'), findsNothing);
      expect(find.text('carol'), findsOneWidget);
      expect(find.text('dave'), findsOneWidget);
      expect(find.text('erin'), findsOneWidget);
      expect(find.text('frank'), findsOneWidget);
      // A list is one group per element: `dave` and `erin` are two rows, and
      // neither shares the row of `carol`, the argument before the list.
      expect(find.descendant(of: rowOf('carol'), matching: find.text('dave')),
          findsNothing);
      expect(find.descendant(of: rowOf('dave'), matching: find.text('erin')),
          findsNothing);

      // The card's buttons are the Accept of its clause and the decline its
      // else-branch earns, whose wording is the construct family's.
      expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
      expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);
    });
  });

  group('the one implementation', () {
    test('the rows of a compound: its arguments in order, a list one row per element',
        () {
      expect(
          scalarGroups(tryParseTerm(
              'menu(bob, [lot(alice, usd, 0, 3), lot(carol, usd, 30, 2)])')!),
          [
            ['bob'],
            ['alice', 'usd', '0', '3'],
            ['carol', 'usd', '30', '2'],
          ]);
      expect(
          scalarGroups(tryParseTerm('received_transfer(release, bob, '
              '[lot(bob, usd, 30, 7), lot(bob, usd, 30, 8)])')!),
          [
            ['release', 'bob'],
            ['bob', 'usd', '30', '7'],
            ['bob', 'usd', '30', '8'],
          ]);
      expect(scalarGroups(tryParseTerm('minted(2, 30)')!), [
        ['2', '30']
      ]);
      expect(scalarGroups(tryParseTerm('holdings([])')!), isEmpty);
      expect(scalarGroups(const GString('hello')), [
        ['hello']
      ]);
    });

    test('one line of text: the rows a comma apart, a String its text', () {
      expect(
          displayText(tryParseTerm(
              'received_transfer(release, bob, [lot(bob, usd, 30, 7)])')!),
          'release bob, bob usd 30 7');
      expect(displayText(const GString('hello')), 'hello');
      expect(displayText(const GAtom('bob')), 'bob');
      expect(displayKey('lot(bob, 2)'), 'bob 2');
      expect(displayKey('bob'), 'bob');
    });

    test('a template substitutes the default display, never the term', () {
      expect(
          renderTemplate('{who} offers {what}', {
            'who': const GAtom('bob'),
            'what': tryParseTerm('lot(bob, 2)')!,
          }),
          'bob offers bob 2');
    });
  });
}
