/// The sovereign mini-app's screen, over the person channel of the compiled
/// sovereign agent (vGLP, Definition "Person Channel, Person Writer, GLP with
/// Persons, Grant"), through the Dart bridge (sections/elicitation.tex, the
/// paragraph "The implementation").
///
/// The draws are fed as the dispatcher of
/// `programs/currencies/sovereign/denominated/sovereign_agent.glp` draws them
/// (its `construct/4`): the agent's question, `Stream(Request)?`, an input
/// box of a menu of the twelve forms of `Request`, one per request clause of
/// SGC's Definition 2.2; and the two responders' questions, `SwapCard ::=
/// card(SwapOffer, YesNo?)` and `TransferCard ::= card(TransferOffer,
/// YesNo?)`, each in writer mode, a card showing the offer with the buttons
/// yes and no.  What is exercised here is what that screen adds to the coins
/// one: twelve forms in the one request form rather than four, every request
/// granted on the one question; two kinds of card, each answered at the
/// position its view marks; and an offer that is not a scalar --- a lot, and
/// a list of bonds --- which the card shows by its scalars.
///
/// The live run of this screen is `sovereign_screen_test.dart`'s; the
/// vocabulary between the two is the canonical compilation's and is the same
/// for every compiled vGLP program.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/sovereign_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

/// The widget of the agent's question as `sovereign_agent.glp` draws it,
/// each field `field(L, W)`, labelled by the name of its declared type, a
/// `Key` a peer field (vGLP, Definition "Widget Declaration, Default
/// Widget").
const _c = "field('Constant', text)";
const _i = "field('Integer', number)";
const _k = "field('Key', peer)";
const _requestForm = 'input_box(menu(['
    'form(mint, [$_c, $_i]), '
    'form(mint, [$_c, $_i, $_i]), '
    'form(swap, [$_k, $_c, $_k, $_c, $_i, $_k, $_c, $_i]), '
    'form(accept, [$_c, $_k, $_i]), '
    'form(pay, [$_k, $_c, $_i]), '
    'form(payout, [$_k, $_c, $_i]), '
    'form(spend, [$_k, $_k, $_c, $_i]), '
    'form(redeem, [$_k, $_c, $_k, $_c]), '
    'form(mature, [$_k, $_c, $_i]), '
    'form(deposit, [$_k, $_c, $_k, $_c, $_i]), '
    'form(release, [$_k, $_c, $_k, $_c, $_i]), '
    'form(return, [$_k, $_c, $_k, $_c, $_i])]))';

/// The widget of each responder's question: the offer shown, labelled by its
/// type, and the buttons yes and no --- the offer, a tuple the program
/// writes, a form of shown fields each labelled by its declared type's name,
/// its lot a form of its own and its instruments a thread of their forms, as
/// the compiler emits it (programs/currencies/sovereign/denominated/
/// sovereign_agent.glp; vGLP #8 Cowork, 2026-10-13 08:15 UTC, item b).
const _swapCard =
    "form(card, [field('SwapOffer', form(offer, [field('Key', shown), f"
    "ield('Lot', form(lot, [field('Key', shown), field('Constant', show"
    "n), field('Constant', shown), field('Integer', shown)])), field('S"
    "tream(Instrument)', thread(menu([form(coin, [field('Key', shown), "
    "field('Constant', shown), field('Integer', shown)]), form(bond, [f"
    "ield('Key', shown), field('Constant', shown), field('Integer', sho"
    "wn), field('Integer', shown)])])))])), field('YesNo', buttons([yes"
    ", no]))])";
const _transferCard =
    "form(card, [field('TransferOffer', form(offer_transfer, [field('Ki"
    "nd', shown), field('Key', shown), field('Stream(Instrument)', thre"
    "ad(menu([form(coin, [field('Key', shown), field('Constant', shown)"
    ", field('Integer', shown)]), form(bond, [field('Key', shown), fiel"
    "d('Constant', shown), field('Integer', shown), field('Integer', sh"
    "own)])])))])), field('YesNo', buttons([yes, no]))])";

void main() {
  late List<String> sent;
  late UiRuntime r;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: sovereignManifest, onSend: (t) => sent.add(formatTerm(t)));
  });

  /// The agent's question, drawn as construct [id] over the view `input([])`.
  void drawRequestForm({int id = 0}) =>
      r.handle(fixtureTerm('draw($id, $_requestForm, input([]))'));

  /// The swap card: bob offers two of his own bonds for two alice-bonds.
  void drawSwapCard({int id = 20}) => r.handle(fixtureTerm('draw($id, $_swapCard, '
      'card(offer(bob, lot(alice, usd, 0, 2), '
      '[bond(bob, usd, 30, 7), bond(bob, usd, 30, 8)]), input([2])))'));

  /// The transfer card: the escrow agent releases one bond to the beneficiary.
  void drawTransferCard({int id = 30}) =>
      r.handle(fixtureTerm('draw($id, $_transferCard, '
      'card(offer_transfer(release, escrow, [bond(charlie, usd, 0, 1)]), '
      'input([2])))'));

  /// The position of the question construct [id] holds at [at] of its view.
  GList position(int id, List<int> at) {
    var v = r.bridge.construct(id)!.view;
    for (final i in at) {
      v = ctorArgs(v).$2[i - 1];
    }
    return questionAt(v)!;
  }

  /// The forms of the request form's menu, in order.
  List<GTerm> forms() {
    final (_, box) = ctorArgs(r.bridge.construct(0)!.widget);
    final (_, menu) = ctorArgs(box.single);
    return (menu.single as GList).items;
  }

  group('the twelve forms', () {
    test('the request form is one construct, and no card', () {
      drawRequestForm();
      final c = r.bridge.constructs.single;
      expect(formatTerm(c.widget), _requestForm);
      expect(formatTerm(c.view), 'input([])');
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });

    test('the menu offers each request by its functor, in the order of '
        'Request', () {
      drawRequestForm();
      expect(forms().map((f) => formatTerm(ctorArgs(f).$2[0])), [
        'mint',
        'mint',
        'swap',
        'accept',
        'pay',
        'payout',
        'spend',
        'redeem',
        'mature',
        'deposit',
        'release',
        'return',
      ]);
    });

    test('the fields of each form are its arguments, in order, each the '
        'widget of its type', () {
      drawRequestForm();
      final fields = {
        for (final f in forms())
          '${formatTerm(ctorArgs(f).$2[0])}/${(ctorArgs(f).$2[1] as GList).items.length}':
              (ctorArgs(f).$2[1] as GList).items.map(formatTerm).toList(),
      };
      expect(fields, {
        'mint/2': [_c, _i],
        'mint/3': [_c, _i, _i],
        'swap/8': [_k, _c, _k, _c, _i, _k, _c, _i],
        'accept/3': [_c, _k, _i],
        'pay/3': [_k, _c, _i],
        'payout/3': [_k, _c, _i],
        'spend/4': [_k, _k, _c, _i],
        'redeem/4': [_k, _c, _k, _c],
        'mature/3': [_k, _c, _i],
        'deposit/5': [_k, _c, _k, _c, _i],
        'release/5': [_k, _c, _k, _c, _i],
        'return/5': [_k, _c, _k, _c, _i],
      });
    });
  });

  group('every request is granted on the one question', () {
    test('a mint is input(Id, [], mint(F, K))', () {
      drawRequestForm();
      r.bridge.grant(0, position(0, []), tryParseTerm('mint(usd, 5)')!);
      expect(sent, ['input(0, [], mint(usd, 5))']);
    });

    test('a swap carries its eight arguments in the order of Request', () {
      drawRequestForm();
      r.bridge.grant(0, position(0, []),
          tryParseTerm('swap(bob, usd, alice, now, 15, bob, now, 15)')!);
      expect(sent, ['input(0, [], swap(bob, usd, alice, now, 15, bob, now, 15))']);
    });

    test('a request leaves the form open, and the next goes to it', () {
      drawRequestForm();
      r.bridge.grant(0, position(0, []), tryParseTerm('mint(usd, 1)')!);
      expect(r.bridge.construct(0), isNotNull);
      r.bridge.grant(0, position(0, []), tryParseTerm('mint(usd, 2)')!);
      expect(sent, ['input(0, [], mint(usd, 1))', 'input(0, [], mint(usd, 2))']);
    });

    test('two requests of different forms go to the same construct, in '
        'order, and the form stays open', () {
      drawRequestForm(id: 41);
      r.bridge.grant(
          41, position(41, []), tryParseTerm('return(charlie, usd, frank, now, 5)')!);
      r.bridge.grant(41, position(41, []), tryParseTerm('pay(alice, usd, 5)')!);
      expect(sent, [
        'input(41, [], return(charlie, usd, frank, now, 5))',
        'input(41, [], pay(alice, usd, 5))',
      ]);
      expect(r.bridge.construct(41), isNotNull);
    });
  });

  group('the two cards', () {
    test('the swap card is a construct of its own, its answer at [2]', () {
      drawSwapCard();
      final card = r.bridge.construct(20)!;
      expect(formatTerm(card.widget), _swapCard);
      final (_, args) = ctorArgs(card.view);
      expect(formatTerm(args[0]),
          'offer(bob, lot(alice, usd, 0, 2), [bond(bob, usd, 30, 7), bond(bob, usd, 30, 8)])');
      expect(formatTerm(position(20, [2])), '[2]');
      expect(r.inbox, isEmpty);
    });

    test('yes is granted on the card\'s own construct', () {
      drawSwapCard(id: 21);
      r.bridge.grant(21, position(21, [2]), const GAtom('yes'));
      expect(sent, ['input(21, [2], yes)']);
    });

    test('a refusal is an answer, no, granted as yes is', () {
      drawSwapCard(id: 22);
      r.bridge.grant(22, position(22, [2]), const GAtom('no'));
      expect(sent, ['input(22, [2], no)']);
    });

    test('the transfer card answers on its own construct', () {
      drawTransferCard(id: 31);
      final (_, args) = ctorArgs(r.bridge.construct(31)!.view);
      expect(formatTerm(ctorArgs(args[0]).$2[0]), 'release');
      r.bridge.grant(31, position(31, [2]), const GAtom('yes'));
      r.handle(fixtureTerm('withdraw(31)'));
      drawTransferCard(id: 32);
      r.bridge.grant(32, position(32, [2]), const GAtom('no'));
      expect(sent, ['input(31, [2], yes)', 'input(32, [2], no)']);
    });

    test('the two cards stand together, are distinct, and go on their own '
        'withdrawals', () {
      drawSwapCard(id: 23);
      drawTransferCard(id: 33);
      expect(r.bridge.constructs.map((c) => c.id), [23, 33]);
      r.handle(fixtureTerm('withdraw(23)'));
      expect(r.bridge.constructs.map((c) => c.id), [33]);
    });

    test('the deployed mediator\'s card vocabulary opens nothing', () {
      r.handle(fixtureTerm('card(agent_1, ctx_agent_1, req(1))'));
      r.handle(fixtureTerm('card(respond_swap_1, ctx_respond_swap_1(bob, '
          'lot(alice, usd, 0, 2), []), req(9))'));
      expect(r.bridge.constructs, isEmpty);
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });
  });

  group('the views of the agent\'s output', () {
    test('every screen message but holdings lands in the default display', () {
      r.handle(fixtureTerm('msg(agent, person, minted(2, 30))'));
      r.handle(fixtureTerm('msg(agent, person, transferred(release, frank))'));
      expect(r.store.lists['screen']!.map(formatTerm),
          ['minted(2, 30)', 'transferred(release, frank)']);
    });
  });

  group('a compound offer is shown as a form, each field labelled by its type', () {
    testWidgets('a lot and a list of bonds show their fields, each labelled, not their terms',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      drawRequestForm();
      drawSwapCard(id: 24);
      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(
              agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      Finder inCard(Finder f) =>
          find.descendant(of: find.byKey(constructKey(24)), matching: f);

      // The offer, a form, each field labelled by its declared type's name:
      // the offerer, a Key; the lot wanted, Lot ::= lot(Key, Constant,
      // Constant, Integer), a form of its own; and the two bonds offered,
      // each a form of its own in the thread of Stream(Instrument).
      expect(find.byKey(nodeKey(24, 'form', const [1])), findsOneWidget);
      expect(find.byKey(nodeKey(24, 'form', const [1, 2])), findsOneWidget);
      expect(find.byKey(nodeKey(24, 'thread', const [1, 3])), findsOneWidget);
      expect(find.byKey(nodeKey(24, 'form', const [1, 3, 1])), findsOneWidget);
      expect(find.byKey(nodeKey(24, 'form', const [1, 3, 2, 1])), findsOneWidget);
      for (final (label, n) in [
        ('SwapOffer', 1),
        ('Lot', 1),
        ('Stream(Instrument)', 1),
        ('Key', 4),
        ('Constant', 4),
        ('Integer', 5),
        ('YesNo', 1),
      ]) {
        expect(inCard(find.text(label)), findsNWidgets(n), reason: label);
      }
      for (final (value, n) in [
        ('bob', 3),
        ('alice', 1),
        ('usd', 3),
        ('0', 1),
        ('2', 1),
        ('30', 2),
        ('7', 1),
        ('8', 1),
      ]) {
        expect(inCard(find.text(value)), findsNWidgets(n), reason: value);
      }
      // Shown and not edited.
      expect(inCard(find.byType(TextField)), findsNothing);
      // No term, of the offer or of any part of it.
      for (final term in [
        'offer(',
        'lot(alice, usd, 0, 2)',
        'bond(bob, usd, 30, 7)',
        'bond(bob, usd, 30, 8)',
      ]) {
        expect(find.textContaining(term), findsNothing, reason: term);
      }

      // The card's buttons are its answer's two constants, yes and no.
      expect(inCard(find.widgetWithText(ElevatedButton, 'yes')), findsOneWidget);
      expect(inCard(find.widgetWithText(ElevatedButton, 'no')), findsOneWidget);
    });

    testWidgets('the request form offers the twelve forms, each by its functor',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      drawRequestForm();
      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(
              agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      Finder inForm(Finder f) =>
          find.descendant(of: find.byKey(constructKey(0)), matching: f);
      expect(inForm(find.byType(ChoiceChip)), findsNWidgets(12));
      for (final label in const [
        'swap',
        'accept',
        'pay',
        'payout',
        'spend',
        'redeem',
        'mature',
        'deposit',
        'release',
        'return'
      ]) {
        expect(inForm(find.widgetWithText(ChoiceChip, label)), findsOneWidget,
            reason: label);
      }
      // The two mints share a functor, and are told apart by their arity.
      expect(inForm(find.widgetWithText(ChoiceChip, 'mint/2')), findsOneWidget);
      expect(inForm(find.widgetWithText(ChoiceChip, 'mint/3')), findsOneWidget);
      // No compose form beside it: the request form is the one way in.
      expect(find.byType(FloatingActionButton), findsNothing);
    });
  });

  test('the scalars of a term are its leaves, grouped per list element', () {
    expect(scalarsOf(tryParseTerm('lot(alice, usd, 0, 2)')!),
        ['alice', 'usd', '0', '2']);
    expect(scalarGroups(tryParseTerm('[bond(bob, usd, 30, 7), bond(bob, usd, 30, 8)]')!),
        [
          ['bob', 'usd', '30', '7'],
          ['bob', 'usd', '30', '8'],
        ]);
    expect(scalarGroups(const GAtom('release')), [
      ['release']
    ]);
  });
}
