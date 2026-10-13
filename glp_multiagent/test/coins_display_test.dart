/// The default display of a compound value, asserted where the rule is
/// shared: the currency mini-app's screen over the generic surface, the
/// compiled program's draws hand-fed as it sends them.
///
/// A compound the program writes for its person is shown and not edited
/// (vGLP, Definition "Widget Declaration, Default Widget": "a value the
/// program writes: shown and not edited"), and it is shown by its content and
/// never as a term.  The card's offer here is drawn by a hand-fed widget,
/// `shown`, as one line of scalars --- its arguments in order, a list one
/// group per element (`scalarGroups` in `term.dart`, the one implementation
/// every one-line form goes through); the agent's messages reach the person
/// only as the thread of construct 0, each drawn by the widget of the
/// reserved type Output, a form headed by its functor with its fields
/// labelled (vGLP 169b008, Definition "Canonical Compilation").
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

/// The widget of the agent's output, as `coins_agent.glp` draws it.
const _output =
    "thread(form(msg, [field('Constant', shown), field('Constant', shown), "
    "field('ScreenContent', menu([form(holdings, [field('Stream(Lot)', thre"
    "ad(form(lot, [field('Issuer', shown), field('Amount', shown)])))]), fo"
    "rm(opened, [field('Key', shown)]), form(minted, [field('Integer', show"
    "n)]), form(paid, [field('Key', shown), field('Integer', shown)]), form"
    "(received, [field('Key', shown), field('Integer', shown)]), form(retur"
    "ned, [field('Key', shown), field('Integer', shown)]), form(swap_done, "
    "[field('Key', shown)]), form(swap_declined, [field('Key', shown)]), fo"
    "rm(swap_failed, [field('Key', shown)]), form(declined, [field('Key', s"
    "hown)]), form(redeemed, [field('Key', shown), field('Key', shown)]), f"
    "orm(menu, [field('Key', shown), field('Stream(Key)', thread)]), form(p"
    "resented, [field('Key', shown), field('Key', shown)]), form(unfulfille"
    "d, [field('Key', shown), field('Key', shown)]), form(refused, [field('"
    "Key', shown)]), form(lacking, [field('Key', shown), field('Integer', s"
    "hown)]), form(no_conversation, [field('Key', shown)])]))]))";

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
      // Two messages of the agent's output, drawn as the thread of construct
      // 0 by the widget of Output ::= ScreenMsg (vGLP 169b008, Definition
      // "Canonical Compilation"): a menu, whose second argument is a list,
      // and a message of one scalar.
      r.handle(fixtureTerm('draw(0, $_output, [msg(agent, person, '
          'menu(carol, [dave, erin])), msg(agent, person, opened(frank))])'));

      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

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

      // The output: each message a form headed by its functor, its fields
      // labelled by their declared types, its list a thread of its own; no
      // term, and the scalars each once.
      Finder inOutput(Finder f) =>
          find.descendant(of: find.byKey(constructKey(0)), matching: f);
      expect(find.text('SCREEN'), findsNothing);
      expect(find.textContaining('menu(carol'), findsNothing);
      expect(find.textContaining('opened(frank)'), findsNothing);
      for (final (heading, at) in [
        ('msg', const [1]),
        ('menu', const [1, 3]),
        ('msg', const [2, 1]),
        ('opened', const [2, 1, 3]),
      ]) {
        expect(
            find.descendant(
                of: find.byKey(nodeKey(0, 'heading', at)),
                matching: find.text(heading)),
            findsOneWidget,
            reason: '$heading at $at');
      }
      expect(inOutput(find.text('Stream(Key)')), findsOneWidget);
      expect(inOutput(find.text('Key')), findsNWidgets(2));
      for (final v in ['carol', 'dave', 'erin', 'frank']) {
        expect(inOutput(find.text(v)), findsOneWidget, reason: v);
      }

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
