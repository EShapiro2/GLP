import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassapp_ui.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

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

  group('GrassApp runtime via manifest only', () {
    late List<String> sent;
    late UiRuntime r;

    setUp(() {
      sent = [];
      r = UiRuntime(manifest: grassrootsManifest, onSend: sent.add);
    });

    test('befriend card belongs to the Friends panel, keyed by the offerer', () {
      r.handleLine('befriend(alice, req(1))');
      expect(r.inbox.length, 1);
      final card = r.inbox.first;
      expect(card.desc.notifyCtor, 'befriend');
      expect(card.panel.id, 'friends');
      expect(card.itemKey, 'alice');
      final accept = card.desc.answers.firstWhere((a) => a.label == 'Accept');
      r.answerCard(card, accept);
      expect(sent, ['decision(yes, alice, req(1))']);
      expect(r.inbox, isEmpty);
    });

    test(
        'trade_proposed card belongs to the Currencies panel, keyed by the proposer',
        () {
      r.handleLine('trade_proposed(alice, bob, 0, 1, req(3))');
      final card = r.inbox.single;
      expect(card.panel.id, 'currencies');
      expect(card.itemKey, 'alice');
      final accept = card.desc.answers.firstWhere((a) => a.label == 'Accept');
      r.answerCard(card, accept);
      expect(sent, ['accept_trade(alice, req(3))']);
    });

    test('an escrow deposit raises the cancel card AND records what is locked',
        () {
      r.handleLine('escrow_deposited(frank, 900, frank, 0, 5, req(7))');

      // The card offers the one choice the depositor still has.
      final card = r.inbox.single;
      expect(card.desc.notifyCtor, 'escrow_deposited');
      expect(card.panel.id, 'currencies');
      expect(card.itemKey, 'frank');
      expect(card.desc.answers.single.label, 'Cancel escrow');

      // The same notify also puts the locked bonds in the wallet — they are in
      // nobody's holdings while escrowed, so this is the only record of them.
      expect(formatTerm(r.store.escrow['escrow']!['frank']!),
          'esc(frank, 0, 5, 900)');
    });

    test('escrow expiry retires the cancel card unanswered (dismissed-by)', () {
      r.handleLine('escrow_deposited(frank, 900, frank, 0, 5, req(7))');
      expect(r.inbox, hasLength(1));

      // Nobody answered: the timer won, so the choice is gone and so is the
      // card — and the locked bonds stop being shown as locked.
      r.handleLine('escrow_expired(frank)');
      expect(r.inbox, isEmpty);
      expect(r.store.escrow['escrow'], isEmpty);
      expect(sent, isEmpty);
    });

    test('a dismissing notify only retires the card for its own item', () {
      r.handleLine('escrow_deposited(frank, 900, frank, 0, 5, req(7))');
      r.handleLine('escrow_deposited(dana, 900, alice, 0, 2, req(8))');
      expect(r.inbox, hasLength(2));

      r.handleLine('escrow_expired(dana)');
      expect(r.inbox.single.itemKey, 'frank',
          reason: "dana's expiry must not retire frank's escrow card");
      expect(r.store.escrow['escrow']!.keys, ['frank']);
    });

    test('cancelling an escrow answers the card with its req id', () {
      r.handleLine('escrow_deposited(frank, 900, frank, 0, 5, req(7))');
      final card = r.inbox.single;
      r.answerCard(card, card.desc.answers.single);
      expect(sent, ['cancel_escrow(frank, req(7))']);
      expect(r.inbox, isEmpty);
    });

    test('connected adds the friend AND opens the conversation', () {
      r.handleLine('connected(bob)');
      expect(r.store.lists['friends']!.map(formatTerm).toList(), ['bob']);
      expect(r.store.threads['chats']!.containsKey('bob'), isTrue);
    });

    test('connected is idempotent (no duplicate friend)', () {
      r.handleLine('connected(bob)');
      r.handleLine('connected(bob)');
      expect(r.store.lists['friends']!.length, 1);
    });

    test('unfriended removes the friend from the list (paper §5)', () {
      r.handleLine('connected(bob)');
      r.handleLine('connected(carol)');
      r.handleLine('unfriended(bob)');
      expect(r.store.lists['friends']!.map(formatTerm).toList(), ['carol']);
    });

    test('rejected(who) and bare rejected add no friend', () {
      r.handleLine('rejected(carol)');
      r.handleLine('rejected');
      expect(r.store.lists['friends'], isEmpty);
    });

    test('received from a friend extends the Chats conversation', () {
      r.handleLine("received(alice, 'hi')");
      expect(r.inbox, isEmpty);
      expect(r.store.threads['chats']!['alice']!.length, 1);
    });

    test('balance_report sets a keyed (issuer, maturity) holding', () {
      r.handleLine('balance_report(bob, alice, 0, 2)');
      expect(
          formatTerm(r.store.holdings['holdings']!['bob']!['alice@0']!), '2');
    });

    test('submitCommand builds the ground UserCmd from a panel form', () {
      final friends =
          grassrootsManifest.panels.firstWhere((p) => p.id == 'friends');
      final connect = friends.commands.firstWhere((c) => c.ctor == 'connect');
      r.submitCommand(connect, {'target': GAtom('bob')});
      expect(sent, ['connect(bob)']);
    });

    test('unfriend command builds the ground UserCmd (paper §5 Request)', () {
      final friends =
          grassrootsManifest.panels.firstWhere((p) => p.id == 'friends');
      final unfriend = friends.commands.firstWhere((c) => c.ctor == 'unfriend');
      r.submitCommand(unfriend, {'friend': GAtom('bob')});
      expect(sent, ['unfriend(bob)']);
    });

    test('Friends state view is declared so the panel renders empty first', () {
      expect(
          grassrootsManifest.state.any(
              (v) => v.key == 'friends' && v.kind == StateKind.list),
          isTrue);
      expect(r.store.lists.containsKey('friends'), isTrue);
    });
  });
}
