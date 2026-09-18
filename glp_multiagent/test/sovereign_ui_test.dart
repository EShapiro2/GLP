/// The sovereign mini-app's screen, over the person channel of a compiled vGLP
/// program (vGLP, Definition "Canonical Compilation").
///
/// The manifest is the image of the twelve `panel(sovereign)` display
/// declarations of `sovereign_agent.vglp`, by vGLP's derivation in
/// `/Grassroots/vGLP/docs/sovereign-manifest-derivation.md`. What is exercised
/// here is what that screen adds to the coins one: seven standing forms in one
/// panel rather than four, each with its own standing ReqId; two cards, each
/// one Accept and the decline its clause's else-branch earns; and a context
/// value that is not a scalar — a lot, and a list of bonds — which the
/// mediator flattens to its scalars for the card.
///
/// The cards are fed as the lines the mediator writes. The program's own
/// live-person harness is Currencies' and is not on disc yet, so the run this
/// screen will carry is not what is tested here — the vocabulary between the
/// two is the canonical compilation's and is the same for every compiled vGLP
/// program.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/sovereign_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  late List<String> sent;
  late UiRuntime r;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: sovereignManifest, onSend: sent.add);
  });

  /// The seven asks the compiled agent poses as soon as it runs, `agent_1` to
  /// `agent_7` in the order the request clauses are written — Mint, Swap, Pay,
  /// Redeem, Deposit, Release, Return.
  void poseRequestCards({int from = 1}) {
    for (var i = 0; i < 7; i++) {
      r.handleLine('card(agent_${i + 1}, ctx_agent_${i + 1}, req(${from + i}))');
    }
  }

  /// The swap card: bob offers two of his own bonds for two alice-bonds.
  void poseSwapCard({int id = 20}) => r.handleLine(
      'card(respond_swap_1, ctx_respond_swap_1(bob, lot(alice, usd, 0, 2), '
      '[bond(bob, usd, 30, 7), bond(bob, usd, 30, 8)]), req($id))');

  /// The transfer card: the escrow agent releases one bond to the beneficiary.
  void poseTransferCard({int id = 30}) => r.handleLine(
      'card(respond_transfer_1, ctx_respond_transfer_1(release, escrow, '
      '[bond(charlie, usd, 0, 1)]), req($id))');

  group('the seven forms', () {
    test('all seven stand, and none opens an inbox card', () {
      poseRequestCards();
      expect(r.standing.keys.toSet(), {
        'agent_1',
        'agent_2',
        'agent_3',
        'agent_4',
        'agent_5',
        'agent_6',
        'agent_7',
      });
      expect(formatTerm(r.standing['agent_5']!), 'req(5)');
      expect(r.inbox, isEmpty);
    });

    test('each form is in the sovereign panel, labelled as its clause is', () {
      for (final e in const {
        'agent_1': 'Mint',
        'agent_2': 'Swap',
        'agent_3': 'Pay',
        'agent_4': 'Redeem',
        'agent_5': 'Deposit',
        'agent_6': 'Release',
        'agent_7': 'Return',
      }.entries) {
        final (panel, form) = r.manifest.standingForm(e.key)!;
        expect(panel.id, 'sovereign');
        expect(form.label, e.value);
      }
    });

    test('the fields of each form are its clause writers, in order', () {
      void check(String clause, List<(String, FieldType, String)> fields) {
        final form = _form(r, clause);
        expect(form.args.map((f) => f.name).toList(),
            fields.map((f) => f.$1).toList(),
            reason: '$clause writers');
        expect(form.args.map((f) => f.type).toList(),
            fields.map((f) => f.$2).toList(),
            reason: '$clause widgets');
        expect(form.args.map((f) => f.label).toList(),
            fields.map((f) => f.$3).toList(),
            reason: '$clause labels');
      }

      check('agent_1', const [
        ('F', FieldType.text, 'Denomination'),
        ('K', FieldType.number, 'Number of bonds'),
        ('D', FieldType.date, 'Maturity'),
      ]);
      check('agent_2', const [
        ('Q', FieldType.peer, 'Counterparty'),
        ('F', FieldType.text, 'Denomination'),
        ('U', FieldType.peer, 'Issuer of the bonds given'),
        ('D', FieldType.date, 'Maturity of the bonds given'),
        ('K', FieldType.number, 'Number of the bonds given'),
        ('V', FieldType.peer, 'Issuer of the bonds wanted'),
        ('D1', FieldType.date, 'Maturity of the bonds wanted'),
        ('K1', FieldType.number, 'Number of the bonds wanted'),
      ]);
      check('agent_3', const [
        ('Q', FieldType.peer, 'Payee'),
        ('F', FieldType.text, 'Denomination'),
        ('D', FieldType.date, 'Maturity'),
        ('K', FieldType.number, 'Number'),
      ]);
      check('agent_4', const [
        ('Q', FieldType.peer, 'Issuer presented to'),
        ('F', FieldType.text, 'Denomination'),
        ('D', FieldType.date, 'Maturity of the coin presented'),
        ('R', FieldType.peer, 'Issuer wanted'),
        ('D1', FieldType.date, 'Maturity wanted'),
      ]);
      check('agent_5', const [
        ('E', FieldType.peer, 'Escrow agent'),
        ('F', FieldType.text, 'Denomination'),
        ('U', FieldType.peer, 'Issuer'),
        ('D', FieldType.date, 'Maturity'),
        ('K', FieldType.number, 'Number'),
      ]);
      check('agent_6', const [
        ('Q', FieldType.peer, 'Beneficiary'),
        ('F', FieldType.text, 'Denomination'),
        ('U', FieldType.peer, 'Issuer'),
        ('D', FieldType.date, 'Maturity'),
        ('K', FieldType.number, 'Number'),
      ]);
      check('agent_7', const [
        ('P', FieldType.peer, 'Depositor'),
        ('F', FieldType.text, 'Denomination'),
        ('U', FieldType.peer, 'Issuer'),
        ('D', FieldType.date, 'Maturity'),
        ('K', FieldType.number, 'Number'),
      ]);
    });
  });

  group('a submit grants its clause against its own standing card', () {
    test('Mint grants xs_agent_1 against the ReqId of agent_1', () {
      poseRequestCards();
      r.submitCommand(_form(r, 'agent_1'),
          {'F': const GAtom('usd'), 'K': const GInt(5), 'D': const GInt(0)});
      expect(sent, ['answer(req(1), xs_agent_1(usd, 5, 0))']);
    });

    test('Swap carries its eight person inputs in the question order', () {
      poseRequestCards();
      r.submitCommand(_form(r, 'agent_2'), {
        'Q': const GAtom('bob'),
        'F': const GAtom('usd'),
        'U': const GAtom('alice'),
        'D': const GInt(0),
        'K': const GInt(15),
        'V': const GAtom('bob'),
        'D1': const GInt(0),
        'K1': const GInt(15),
      });
      expect(sent,
          ['answer(req(2), xs_agent_2(bob, usd, alice, 0, 15, bob, 0, 15))']);
    });

    test('each of the seven answers the ReqId of ITS clause', () {
      poseRequestCards(from: 41);
      r.submitCommand(_form(r, 'agent_7'), {
        'P': const GAtom('charlie'),
        'F': const GAtom('usd'),
        'U': const GAtom('frank'),
        'D': const GInt(0),
        'K': const GInt(5),
      });
      r.submitCommand(_form(r, 'agent_3'), {
        'Q': const GAtom('alice'),
        'F': const GAtom('usd'),
        'D': const GInt(0),
        'K': const GInt(5),
      });
      expect(sent, [
        'answer(req(47), xs_agent_7(charlie, usd, frank, 0, 5))',
        'answer(req(43), xs_agent_3(alice, usd, 0, 5))',
      ]);
    });

    test('answering consumes the ask; the next card restores the form', () {
      poseRequestCards();
      r.submitCommand(_form(r, 'agent_1'),
          {'F': const GAtom('usd'), 'K': const GInt(1), 'D': const GInt(0)});
      expect(r.standing.containsKey('agent_1'), isFalse);
      r.submitCommand(_form(r, 'agent_1'),
          {'F': const GAtom('usd'), 'K': const GInt(2), 'D': const GInt(0)});
      expect(sent.length, 1);
      r.handleLine('card(agent_1, ctx_agent_1, req(12))');
      r.submitCommand(_form(r, 'agent_1'),
          {'F': const GAtom('usd'), 'K': const GInt(2), 'D': const GInt(0)});
      expect(sent.last, 'answer(req(12), xs_agent_1(usd, 2, 0))');
    });
  });

  group('the two cards', () {
    test('the swap card is one Accept and a decline', () {
      poseSwapCard();
      final card = r.inbox.single;
      expect(card.panel.id, 'sovereign');
      expect(card.itemKey, 'bob');
      expect(card.asks.keys.toSet(), {'respond_swap_1'});
      expect(card.liveAnswers.map((a) => a.label), ['Accept', 'Decline']);
    });

    test('Accept grants xs_respond_swap_1(yes) on the card\'s own ReqId', () {
      poseSwapCard(id: 21);
      final card = r.inbox.single;
      r.answerCard(card, _answer(card, 'Accept'));
      expect(sent, ['answer(req(21), xs_respond_swap_1(yes))']);
      expect(r.inbox, isEmpty);
    });

    test('the decline grants decline(Id) on the card\'s own ReqId', () {
      poseSwapCard(id: 22);
      final card = r.inbox.single;
      r.answerCard(card, _answer(card, 'Decline'));
      expect(sent, ['decline(req(22))']);
      expect(r.inbox, isEmpty);
    });

    test('the transfer card answers and declines its own clause', () {
      poseTransferCard(id: 31);
      final card = r.inbox.single;
      expect(card.asks.keys.toSet(), {'respond_transfer_1'});
      expect(formatTerm(card.fields['Kind']!), 'release');
      r.answerCard(card, _answer(card, 'Accept'));
      expect(sent, ['answer(req(31), xs_respond_transfer_1(yes))']);

      poseTransferCard(id: 32);
      final next = r.inbox.single;
      r.answerCard(next, _answer(next, 'Decline'));
      expect(sent.last, 'decline(req(32))');
    });

    test('the two cards stand together and are distinct', () {
      poseSwapCard(id: 23);
      poseTransferCard(id: 33);
      expect(r.inbox.length, 2);
      r.handleLine('closed(req(23))');
      expect(r.inbox.single.asks.keys.single, 'respond_transfer_1');
    });

    test('a clause the manifest does not name opens nothing', () {
      r.handleLine('card(respond_swap_2, ctx_respond_swap_2(bob, '
          'lot(alice, usd, 0, 2), []), req(9))');
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });
  });

  group('the declared views', () {
    test('every screen message but holdings lands in the default display', () {
      r.handleLine('msg(agent, person, minted(2, 30))');
      r.handleLine('msg(agent, person, transferred(release, frank))');
      expect(r.store.lists['screen']!.map(formatTerm),
          ['minted(2, 30)', 'transferred(release, frank)']);
    });
  });

  group('a compound context is shown as its scalars', () {
    testWidgets('a lot and a list of bonds show their scalars, not their terms',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      poseRequestCards();
      poseSwapCard(id: 24);
      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(
              agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      // `Want` is the lot lot(alice, usd, 0, 2) — its four scalars, and the
      // term itself nowhere.
      expect(find.text('lot(alice, usd, 0, 2)'), findsNothing);
      expect(find.text('alice'), findsOneWidget);
      expect(find.text('0'), findsOneWidget);
      expect(find.text('2'), findsOneWidget);

      // `Offered` is two bonds — one group of four scalars per bond, so the
      // serials 7 and 8 and the maturity 30 twice.
      expect(find.text('bond(bob, usd, 30, 7)'), findsNothing);
      expect(find.text('7'), findsOneWidget);
      expect(find.text('8'), findsOneWidget);
      expect(find.text('30'), findsNWidgets(2));
      // The denomination of all three groups.
      expect(find.text('usd'), findsNWidgets(3));
      // `From` is a scalar and stays one value; `bob` is also each bond's
      // issuer, so it is the context value and the two groups.
      expect(find.text('bob'), findsNWidgets(3));

      // The card's buttons are the Accept of its clause and the decline its
      // else-branch earns.
      expect(find.widgetWithText(ElevatedButton, 'Accept'), findsOneWidget);
      expect(find.widgetWithText(OutlinedButton, 'Decline'), findsOneWidget);
    });

    testWidgets('the panel offers the seven forms, each by its label',
        (tester) async {
      tester.view.physicalSize = const Size(600, 1200);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);

      poseRequestCards();
      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(
              agentId: 'alice', runtime: r, muteNotices: true)));
      await tester.pumpAndSettle();

      await tester.tap(find.byType(FloatingActionButton));
      await tester.pumpAndSettle();
      for (final label in const [
        'Mint',
        'Swap',
        'Pay',
        'Redeem',
        'Deposit',
        'Release',
        'Return'
      ]) {
        expect(find.text(label), findsOneWidget, reason: label);
      }
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

CommandDesc _form(UiRuntime r, String clause) =>
    r.manifest.standingForm(clause)!.$2;

AnswerDesc _answer(InboxCard card, String label) =>
    card.liveAnswers.firstWhere((a) => a.label == label);
