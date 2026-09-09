/// The currency mini-app's screen, over the person channel of a compiled vGLP
/// program (vGLP, Definition "Canonical Compilation").
///
/// The three mechanisms every compiled vGLP program needs, exercised on the
/// manifest derived from `coins_agent.vglp`'s display declarations: a card
/// matched by the value of its first argument with its context destructured; a
/// nested answer `answer(Id, xs_C(...))`; and standing cards — a persistent
/// clause's form bound to the latest card of that clause, and sibling clauses
/// of equal context drawn as one card, each button answering its own ReqId.
library;

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  late List<String> sent;
  late UiRuntime r;

  setUp(() {
    sent = [];
    r = UiRuntime(manifest: coinsManifest, onSend: sent.add);
  });

  /// The four asks the compiled agent poses as soon as it runs, in the order
  /// `coins_agent.glp` emits them (lines 68, 70, 72, 74).
  void poseRequestCards({int from = 1}) {
    r.handleLine('card(agent_1, ctx_agent_1, req($from))');
    r.handleLine('card(agent_2, ctx_agent_2, req(${from + 1}))');
    r.handleLine('card(agent_3, ctx_agent_3, req(${from + 2}))');
    r.handleLine('card(agent_4, ctx_agent_4, req(${from + 3}))');
  }

  group('a card is matched by its clause', () {
    test('the four request clauses stand, and none opens an inbox card', () {
      poseRequestCards();
      expect(r.standing.keys.toSet(),
          {'agent_1', 'agent_2', 'agent_3', 'agent_4'});
      expect(formatTerm(r.standing['agent_3']!), 'req(3)');
      expect(r.inbox, isEmpty);
    });

    test('a transient clause opens a card, its context destructured', () {
      r.handleLine(
          'card(respond_swap_2, ctx_respond_swap_2(alice, lot(bob, 2), '
          '[coin(alice, 1), coin(alice, 2)]), req(7))');
      final card = r.inbox.single;
      expect(card.panel.id, 'coins');
      expect(card.itemKey, 'alice');
      expect(formatTerm(card.fields['Want']!), 'lot(bob, 2)');
      expect(formatTerm(card.fields['Offered']!),
          '[coin(alice, 1), coin(alice, 2)]');
    });

    test('a clause the manifest does not name opens nothing', () {
      r.handleLine('card(other_1, ctx_other_1(alice), req(9))');
      expect(r.inbox, isEmpty);
      expect(r.standing, isEmpty);
    });
  });

  group('a nested fill', () {
    test('a form grants answer(Id, xs_C(v1, ..., vi))', () {
      poseRequestCards();
      final mint = _form(r, 'agent_1');
      r.submitCommand(mint, {'K': GInt(3)});
      expect(sent, ['answer(req(1), xs_agent_1(3))']);
    });

    test('the swap form carries its five person inputs in order', () {
      poseRequestCards();
      r.submitCommand(_form(r, 'agent_2'), {
        'Q': GAtom('bob'),
        'U': GAtom('alice'),
        'K': GInt(2),
        'V': GAtom('bob'),
        'K1': GInt(2),
      });
      expect(sent, ['answer(req(2), xs_agent_2(bob, alice, 2, bob, 2))']);
    });

    test('a card button grants the ground term of its own clause', () {
      r.handleLine(
          'card(respond_swap_2, ctx_respond_swap_2(alice, lot(bob, 2), []), '
          'req(7))');
      final card = r.inbox.single;
      r.answerCard(card, _answer(card, 'Accept'));
      expect(sent, ['answer(req(7), xs_respond_swap_2(yes))']);
    });
  });

  group('standing cards', () {
    test('a fresh card of a clause replaces the one before it', () {
      poseRequestCards();
      r.handleLine('card(agent_1, ctx_agent_1, req(11))');
      expect(formatTerm(r.standing['agent_1']!), 'req(11)');
      r.submitCommand(_form(r, 'agent_1'), {'K': GInt(1)});
      expect(sent, ['answer(req(11), xs_agent_1(1))']);
    });

    test('answering consumes the ask; the next card restores the form', () {
      poseRequestCards();
      r.submitCommand(_form(r, 'agent_1'), {'K': GInt(3)});
      expect(r.standing.containsKey('agent_1'), isFalse);
      // A second submission with no card standing grants nothing.
      r.submitCommand(_form(r, 'agent_1'), {'K': GInt(4)});
      expect(sent, ['answer(req(1), xs_agent_1(3))']);
      // The goal poses the next question.
      r.handleLine('card(agent_1, ctx_agent_1, req(12))');
      r.submitCommand(_form(r, 'agent_1'), {'K': GInt(4)});
      expect(sent.last, 'answer(req(12), xs_agent_1(4))');
    });

    test('closed(Id) retires a standing card', () {
      poseRequestCards();
      r.handleLine('closed(req(2))');
      expect(r.standing.containsKey('agent_2'), isFalse);
      expect(r.standing.containsKey('agent_1'), isTrue);
    });
  });

  group('sibling clauses of equal context are one card', () {
    const ctx = 'alice, lot(bob, 2), [coin(alice, 1)]';

    setUp(() {
      r.handleLine('card(respond_swap_1, ctx_respond_swap_1($ctx), req(5))');
      r.handleLine('card(respond_swap_2, ctx_respond_swap_2($ctx), req(6))');
    });

    test('one card, two asks, a button each', () {
      final card = r.inbox.single;
      expect(card.asks.length, 2);
      expect(card.liveAnswers.map((a) => a.label), ['Accept', 'Decline']);
    });

    test('each button answers its own ReqId', () {
      final card = r.inbox.single;
      r.answerCard(card, _answer(card, 'Decline'));
      expect(sent, ['answer(req(5), xs_respond_swap_1(no))']);
      expect(r.inbox, isEmpty);
    });

    test('a different context is a different card', () {
      r.handleLine(
          'card(respond_swap_2, ctx_respond_swap_2(charlie, lot(bob, 1), []), '
          'req(8))');
      expect(r.inbox.length, 2);
      expect(r.inbox.map((c) => c.itemKey).toSet(), {'alice', 'charlie'});
    });

    test('an aborted sibling stops being offered; the card stands', () {
      r.handleLine('closed(req(5))');
      final card = r.inbox.single;
      expect(card.liveAnswers.map((a) => a.label), ['Accept']);
    });

    test('the deadline retires both asks and the card with them', () {
      r.handleLine('closed(req(6))');
      r.handleLine('closed(req(5))');
      expect(r.inbox, isEmpty);
    });
  });

  group('the declared views', () {
    test('the balances view is fed by its pattern and replaces its rows', () {
      r.handleLine(
          'msg(agent, person, holdings([lot(alice, 3), lot(bob, 2)]))');
      expect(r.store.balances['balances']!.map((k, v) => MapEntry(k, formatTerm(v))),
          {'alice': '3', 'bob': '2'});
      r.handleLine('msg(agent, person, holdings([lot(bob, 2)]))');
      expect(r.store.balances['balances']!.keys, ['bob']);
    });

    test('every other screen message lands in the default display', () {
      r.handleLine('msg(agent, person, minted(3))');
      r.handleLine('msg(agent, person, swap_done(bob))');
      r.handleLine('msg(agent, person, opened(bob))');
      expect(r.store.lists['screen']!.map(formatTerm),
          ['minted(3)', 'swap_done(bob)', 'opened(bob)']);
    });

    test('a holdings message does not also reach the default display', () {
      r.handleLine('msg(agent, person, holdings([lot(alice, 1)]))');
      expect(r.store.lists['screen'], isEmpty);
    });
  });
}

CommandDesc _form(UiRuntime r, String clause) =>
    r.manifest.standingForm(clause)!.$2;

AnswerDesc _answer(InboxCard card, String label) =>
    card.liveAnswers.firstWhere((a) => a.label == label);
