/// The default display of a compound value, asserted where the rule is
/// shared: the currency mini-app's screen over the generic surface, the
/// compiled program's draws and messages hand-fed as it sends them.
///
/// A compound the program writes for its person is shown and not edited
/// (vGLP, Definition "Widget Declaration, Default Widget": "a value the
/// program writes: shown and not edited"), and it is shown by its content and
/// never as a term: the swap card shows the offer, its content (Definition
/// "Informed Offer"), as the scalars the offer is made of, and the panel's
/// list shows each message of the agent's as its scalars.  How a family lays
/// that content out is its own (Remark "Construct Families"): the grassroots
/// app's draws a shown value as one line --- its arguments in order, a list
/// one group per element --- and the panel's list as a row of scalars per
/// group.  Both are asserted here on the one implementation, `scalarGroups`
/// in `term.dart`, together with the one-line forms every other place goes
/// through.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

void main() {
  late UiRuntime r;
  late List<String> sent;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: coinsManifest, onSend: (t) => sent.add(formatTerm(t)));
  });

  group('a compound under default display shows its scalars', () {
    testWidgets('the offer a card shows and a message of the panel\'s list',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      // The swap card, as the compiled agent draws it: the offer of bob, who
      // wants a lot of his own and offers a list of coins.
      r.handle(fixtureTerm("draw(7, form(card, [field('Offer', shown), "
          "field('YesNo', buttons([yes, no]))]), "
          'card(offer(bob, lot(bob, 2), [coin(alice, 3)]), input([2])))'));
      // Two messages of the default display: a menu, whose second argument is
      // a list, and a message of one scalar.
      r.handle(fixtureTerm('msg(agent, person, menu(carol, [dave, erin]))'));
      r.handle(fixtureTerm('msg(agent, person, opened(frank))'));

      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      // The nearest row of scalars a text sits in.
      Finder rowOf(String text) =>
          find.ancestor(of: find.text(text), matching: find.byType(Wrap)).first;
      Finder inCard(Finder f) =>
          find.descendant(of: find.byKey(constructKey(7)), matching: f);

      // The card: the offer's scalars, its arguments in order, the list of
      // coins a group of its own --- and no term, of the offer or of any part
      // of it.
      expect(inCard(find.text('bob bob 2, alice 3')), findsOneWidget);
      for (final term in [
        'offer(bob, lot(bob, 2), [coin(alice, 3)])',
        'lot(bob, 2)',
        '[coin(alice, 3)]',
        'coin(alice, 3)',
      ]) {
        expect(find.textContaining(term), findsNothing, reason: term);
      }

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

      // The card's buttons are its answer's two constants, yes and no, the
      // default widget of YesNo ::= yes ; no, a row of buttons.
      expect(inCard(find.widgetWithText(ElevatedButton, 'yes')), findsOneWidget);
      expect(inCard(find.widgetWithText(ElevatedButton, 'no')), findsOneWidget);
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
      // A stored key: the default display of the term it names, which the
      // store holds (until 2026-10-11 the key's text was parsed back); a key
      // of a name alone, the phone owner's, is that name.
      expect(r.store.displayKey(r.store.keyOf(fixtureTerm('lot(bob, 2)'))),
          'bob 2');
      expect(r.store.displayKey('bob'), 'bob');
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
