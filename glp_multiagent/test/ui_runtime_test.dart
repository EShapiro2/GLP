import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

void main() {
  group('boundary parser', () {
    test('parses a notify struct with nested req id', () {
      final t = tryParseTerm('befriend(alice, req(1))');
      expect(t, isA<GStruct>());
      final s = t as GStruct;
      expect(s.functor, 'befriend');
      expect((s.args[0] as GAtom).name, 'alice');
      expect((s.args[1] as GStruct).functor, 'req');
      expect(((s.args[1] as GStruct).args[0] as GInt).value, 1);
    });

    test('bare atom notify', () {
      expect(tryParseTerm('rejected'), isA<GAtom>());
    });

    test('rejects log lines (trailing text / leading prompt / bracket)', () {
      expect(tryParseTerm('[INIT] Loaded GLP program'), isNull);
      expect(tryParseTerm('> connect(bob)'), isNull);
      expect(tryParseTerm('[GOAL] Started agent_init(alice, NetIn)'), isNull);
    });

    test('format round-trips', () {
      expect(
          formatTerm(GStruct('decision',
              [GAtom('yes'), GAtom('alice'), GStruct('req', [GInt(1)])])),
          'decision(yes, alice, req(1))');
    });
  });

  // The person's decline, vGLP Definition "Canonical Compilation": "the card
  // of an ask of a clause with an else-branch carries a decline, which grants
  // decline(ReqId) with the identifier of the ask". Until the mediator lost
  // its deadline on 2026-09-15 no person could reach an else-branch — the
  // timer answered for them — so this is the only route to one.
  group('a card of a clause with an else-branch carries a decline', () {
    // Two sibling clauses over one context, of which only `offer_2` has an
    // else-branch: its reply type is `then(Xs) ; else` and its goal carries an
    // `ask(else, _)` clause.
    final manifest = Manifest(
      title: 'else-branch',
      activity: const [],
      panels: [
        Panel(
          id: 'p',
          name: 'P',
          inbox: const [
            InboxDesc(
              args: ['from'],
              itemKey: 'from',
              title: '{from} offers',
              clauses: ['offer_1', 'offer_2'],
              elseBranch: ['offer_2'],
              answers: [
                AnswerDesc(
                  label: 'Accept',
                  cmdCtor: '',
                  clause: 'offer_2',
                  answerCtor: 'xs_offer_2',
                  fill: [ConstFill(GAtom('yes'))],
                ),
              ],
            ),
          ],
        ),
      ],
    );

    late List<String> sent;
    late UiRuntime r;

    setUp(() {
      sent = [];
      r = UiRuntime(manifest: manifest, onSend: (t) => sent.add(formatTerm(t)));
      r.handle(fixtureTerm('card(offer_1, ctx_offer_1(alice), req(1))'));
      r.handle(fixtureTerm('card(offer_2, ctx_offer_2(alice), req(2))'));
    });

    test('the decline is offered beside the declared buttons', () {
      expect(r.inbox.single.liveAnswers.map((a) => a.label),
          ['Accept', 'Decline'],
          reason: 'one decline, for the one clause with an else-branch');
    });

    test('its tap grants decline(ReqId) of its own ask, and nothing else', () {
      final card = r.inbox.single;
      final decline = card.liveAnswers.firstWhere((a) => a.decline);
      r.answerCard(card, decline);
      expect(sent, ['decline(req(2))']);
      expect(r.inbox, isEmpty,
          reason: 'the else-branch reduces the goal, which retires the card');
    });

    test('an answer is still an answer', () {
      final card = r.inbox.single;
      r.answerCard(card, card.liveAnswers.firstWhere((a) => !a.decline));
      expect(sent, ['answer(req(2), xs_offer_2(yes))']);
    });

    test('no decline once that ask is closed, the sibling standing', () {
      r.handle(fixtureTerm('closed(req(2))'));
      expect(r.inbox.single.liveAnswers, isEmpty,
          reason: 'the accept and the decline were both of the closed ask');
      expect(r.inbox.single.asks.keys, ['offer_1']);
    });
  });
}
