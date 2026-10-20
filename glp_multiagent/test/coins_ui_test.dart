/// The currency mini-app's screen, over the person channel of the compiled
/// coins agent (vGLP, Definition "Person Channel, Person Writer, GLP with
/// Persons, Grant"), through the Dart bridge (sections/elicitation.tex, the
/// paragraph "The implementation").
///
/// The three mechanisms the compiled program needs of its screen, exercised
/// on [coinsManifest] with the draws fed as the dispatcher of
/// `programs/currencies/coins/currency/coins_agent.glp` draws them (its
/// `construct/4`, the request form and the swap responder's card): a draw
/// opens a construct by its identifier, with its widget and its view; the
/// person's act on a construct is granted as `input(Id, P, R)`, at the
/// position `P` the view marks `input(P)`; and the request form, the
/// question `Stream(Request)?`, stays open, one request per submission
/// (vGLP, Remark "Persistence"), while a card goes on its withdrawal.  The
/// deployed mediator's card vocabulary opens nothing on this screen.
library;

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

/// The widget of the agent's question, `Stream(Request)?`, as
/// `coins_agent.glp` draws it: an input box of a menu of the four forms of
/// `Request`, each field `field(L, W)`, labelled by the name of its declared
/// type, a `Key` a peer field (vGLP, Definition "Widget Declaration, Default
/// Widget").
const _requestForm = "input_box(menu([form(mint, [field('Amount', number)]), "
    "form(swap, [field('Friend', peer), field('GiveCoin', peer), field('GiveAmount', number), "
    "field('WantCoin', peer), field('WantAmount', number)]), "
    "form(pay, [field('Friend', peer), field('Amount', number)]), "
    "form(redeem, [field('Friend', peer), field('WantCoin', peer)])]))";

/// The widget of the swap responder's question, `Approval ::= approval(Offer,
/// YesNo?)` in writer mode: the offer shown, and the buttons yes and no ---
/// the offer, a tuple the program writes, a form of shown fields each
/// labelled by its declared type's name, its lot and its coins forms of their
/// own, as the compiler emits it (programs/currencies/coins/currency/
/// coins_agent.glp; vGLP #8 Cowork, 2026-10-13 08:15 UTC, item b).
const _offer = "field('Offer', form(offer, [field('From', shown), "
    "field('Want', form(lot, [field('Issuer', shown), field('Amount', shown)])), "
    "field('Offered', thread(form(coin, [field('Issuer', shown), "
    "field('Serial', shown)])))]))";
const _card = "form(approval, [$_offer, field('YesNo', buttons([yes, no]))])";

/// The widget of the agent's output to its person, as `coins_agent.glp`
/// draws it: a thread whose elements are drawn by the widget of the reserved
/// type `Output ::= ScreenContent`, a menu of the screen's forms, each headed
/// by its functor and its fields labelled by their declared types, the
/// envelope stripped (vGLP 169b008, Definition "Canonical Compilation";
/// Currencies, 2026-10-14 08:48 UTC, item 2).
const _output =
    "thread(menu([form(holdings, [field('Holdings', thread(form(lot, [field"
    "('Issuer', shown), field('Amount', shown)])))]), form(opened, [field('"
    "Friend', shown)]), form(minted, [field('Amount', shown)]), form(paid, "
    "[field('Friend', shown), field('Amount', shown)]), form(received, [fie"
    "ld('From', shown), field('Amount', shown)]), form(returned, [field('Fr"
    "om', shown), field('Amount', shown)]), form(swap_done, [field('Friend'"
    ", shown)]), form(swap_declined, [field('Friend', shown)]), form(swap_f"
    "ailed, [field('From', shown)]), form(declined, [field('From', shown)])"
    ", form(swap_same, [field('Friend', shown), field('GiveCoin', shown), f"
    "ield('GiveAmount', shown)]), form(redeemed, [field('Friend', shown), f"
    "ield('WantCoin', shown)]), form(menu, [field('Friend', shown), field('"
    "Issuers', thread)]), form(presented, [field('From', shown), field('Wan"
    "tCoin', shown)]), form(unfulfilled, [field('From', shown), field('Want"
    "Coin', shown)]), form(refused, [field('Friend', shown)]), form(redeem_"
    "same, [field('Friend', shown)]), form(lacking, [field('Issuer', shown)"
    ", field('Amount', shown)]), form(no_conversation, [field('Friend', sho"
    "wn)])]))";

void main() {
  late List<String> sent;
  late UiRuntime r;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: coinsManifest, onSend: (t) => sent.add(formatTerm(t)));
  });

  /// The agent's question, drawn as construct [id] over the view `input([])`:
  /// the whole variable is the person's.
  void drawRequestForm({int id = 0}) =>
      r.handle(fixtureTerm('draw($id, $_requestForm, input([]))'));

  /// The card of a swap [from] proposes, drawn as construct [id]: the offer
  /// shown, the answer the person's at position [2].
  void drawCard(int id, String from, String want, String offered) =>
      r.handle(fixtureTerm(
          'draw($id, $_card, approval(offer($from, $want, $offered), input([2])))'));

  /// The position of the question construct [id] holds at [at] of its view.
  GList position(int id, List<int> at) {
    var v = r.bridge.construct(id)!.view;
    for (final i in at) {
      v = ctorArgs(v).$2[i - 1];
    }
    return questionAt(v)!;
  }

  group('a draw opens a construct', () {
    test('the request form is one construct, its view the person\'s whole '
        'variable, and no card', () {
      drawRequestForm();
      final c = r.bridge.constructs.single;
      expect(c.id, 0);
      expect(formatTerm(c.widget), _requestForm);
      expect(formatTerm(c.view), 'input([])');
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });

    test('a card is a construct of its own, the offer in its view', () {
      drawRequestForm();
      drawCard(7, 'alice', 'lot(bob, 2)', '[coin(alice, 1), coin(alice, 2)]');
      expect(r.bridge.constructs.map((c) => c.id), [0, 7]);
      final card = r.bridge.construct(7)!;
      expect(formatTerm(card.widget), _card);
      final (_, args) = ctorArgs(card.view);
      expect(formatTerm(args[0]),
          'offer(alice, lot(bob, 2), [coin(alice, 1), coin(alice, 2)])');
      expect(formatTerm(position(7, [2])), '[2]');
      expect(r.inbox, isEmpty);
    });

    test('the swap card\'s answer is a row of two buttons, yes and no', () {
      drawCard(7, 'alice', 'lot(bob, 2)', '[]');
      final (form, fields) = ctorArgs(r.bridge.construct(7)!.widget);
      expect(form, 'form');
      final ws = (fields[1] as GList).items;
      expect(formatTerm(fields[0]), 'approval');
      expect(formatTerm(ws[0]), _offer,
          reason: 'the offer, shown, each field labelled by its type');
      expect(formatTerm(ws[1]), "field('YesNo', buttons([yes, no]))",
          reason: 'YesNo ::= yes ; no, a row of buttons');
    });

    test('a draw of an open construct redraws it in place', () {
      drawCard(7, 'alice', 'lot(bob, 2)', '[]');
      drawCard(7, 'alice', 'lot(bob, 2)', '[coin(alice, 1)]');
      expect(r.bridge.constructs.map((c) => c.id), [7]);
      expect(formatTerm(ctorArgs(r.bridge.construct(7)!.view).$2[0]),
          'offer(alice, lot(bob, 2), [coin(alice, 1)])');
    });

    test('the deployed mediator\'s card vocabulary opens nothing', () {
      r.handle(fixtureTerm('card(agent_1, ctx_agent_1, req(1))'));
      r.handle(fixtureTerm(
          'card(respond_swap_1, ctx_respond_swap_1(alice, lot(bob, 2), '
          '[coin(alice, 1), coin(alice, 2)]), req(7))'));
      expect(r.bridge.constructs, isEmpty);
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });
  });

  group('a grant is input(Id, P, R)', () {
    test('a request is granted on the request form at its question, []', () {
      drawRequestForm();
      r.bridge.grant(0, position(0, []), tryParseTerm('mint(3)')!);
      expect(sent, ['input(0, [], mint(3))']);
    });

    test('the swap request carries its friend, the coin and amount given and the coin and amount wanted', () {
      drawRequestForm();
      r.bridge.grant(
          0, position(0, []), tryParseTerm('swap(bob, alice, 2, bob, 2)')!);
      expect(sent, ['input(0, [], swap(bob, alice, 2, bob, 2))']);
    });

    test('a card\'s answer is granted at the position its view marks', () {
      drawCard(7, 'alice', 'lot(bob, 2)', '[]');
      r.bridge.grant(7, position(7, [2]), const GAtom('yes'));
      expect(sent, ['input(7, [2], yes)']);
    });

    test('a refusal is an answer, no, granted as yes is', () {
      drawCard(5, 'alice', 'lot(bob, 2)', '[coin(alice, 1)]');
      r.bridge.grant(5, position(5, [2]), const GAtom('no'));
      expect(sent, ['input(5, [2], no)']);
    });
  });

  group('the request form stands; a card goes on its withdrawal', () {
    test('a submission leaves the request form open for the next request', () {
      drawRequestForm(id: 11);
      r.bridge.grant(11, position(11, []), tryParseTerm('mint(1)')!);
      expect(r.bridge.construct(11), isNotNull);
      r.bridge.grant(11, position(11, []), tryParseTerm('pay(bob, 1)')!);
      expect(sent, ['input(11, [], mint(1))', 'input(11, [], pay(bob, 1))']);
    });

    test('withdraw(Id) removes that construct and no other', () {
      drawRequestForm();
      drawCard(5, 'alice', 'lot(bob, 2)', '[coin(alice, 1)]');
      r.handle(fixtureTerm('withdraw(5)'));
      expect(r.bridge.construct(5), isNull);
      expect(r.bridge.constructs.map((c) => c.id), [0]);
    });

    test('a withdrawn card takes no grant', () {
      drawCard(5, 'alice', 'lot(bob, 2)', '[coin(alice, 1)]');
      r.bridge.grant(5, position(5, [2]), const GAtom('yes'));
      r.handle(fixtureTerm('withdraw(5)'));
      r.bridge.grant(5, tryParseTerm('[2]')! as GList, const GAtom('no'));
      expect(sent, ['input(5, [2], yes)']);
    });

    test('two proposers\' swaps are two cards', () {
      drawCard(5, 'alice', 'lot(bob, 2)', '[coin(alice, 1)]');
      drawCard(8, 'charlie', 'lot(bob, 1)', '[]');
      expect(r.bridge.constructs.map((c) => c.id), [5, 8]);
      expect(
          r.bridge.constructs
              .map((c) => formatTerm(ctorArgs(ctorArgs(c.view).$2[0]).$2[0])),
          ['alice', 'charlie']);
    });
  });

  // The agent's output to its person, its screen's content C, reaches the
  // person only as the thread of construct 0, drawn at dispatch start and
  // again with every message so far, each by the widget of the reserved type
  // Output (`Output ::= ScreenContent`); no view of the manifest names a message
  // (vGLP 169b008, Definition "Canonical Compilation"; vGLP, 2026-10-13 19:25
  // UTC, items 2 and 3).
  group('the agent\'s output: the thread of construct 0', () {
    test('the thread is a construct, its view the messages so far', () {
      r.handle(fixtureTerm('draw(0, $_output, [])'));
      r.handle(fixtureTerm('draw(0, $_output, [opened(bob), minted(3)])'));
      final c = r.bridge.construct(0)!;
      expect(formatTerm(c.widget), _output);
      expect(formatTerm(c.view), '[opened(bob), minted(3)]');
      expect(r.store.lists, isEmpty);
      expect(r.store.balances, isEmpty);
    });

    test('a screen message on the channel reaches no view: the manifest names '
        'none', () {
      r.handle(fixtureTerm('msg(agent, person, minted(3))'));
      r.handle(fixtureTerm(
          'msg(agent, person, holdings([lot(alice, 3), lot(bob, 2)]))'));
      expect(r.store.lists['screen'], isNull);
      expect(r.store.balances, isEmpty);
    });

    test('a draw is the bridge\'s, and reaches no view', () {
      drawRequestForm(id: 1);
      r.handle(fixtureTerm('withdraw(1)'));
      expect(r.store.lists, isEmpty);
      expect(r.store.balances, isEmpty);
    });
  });
}
