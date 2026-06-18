import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/grassroots.dart';
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

    test('swap_offer card belongs to the Coins panel, keyed by the proposer',
        () {
      r.handleLine('swap_offer(alice, alice, 1, bob, 1, req(3))');
      final card = r.inbox.single;
      expect(card.panel.id, 'coins');
      expect(card.itemKey, 'alice');
      final accept = card.desc.answers.firstWhere((a) => a.label == 'Accept');
      r.answerCard(card, accept);
      expect(sent, ['accept_swap(alice, req(3))']);
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

    test('balance_report sets a keyed holding', () {
      r.handleLine('balance_report(bob, alice, 2)');
      expect(formatTerm(r.store.holdings['holdings']!['bob']!['alice']!), '2');
    });

    test('submitCommand builds the ground UserCmd from a panel form', () {
      final friends =
          grassrootsManifest.panels.firstWhere((p) => p.id == 'friends');
      final connect = friends.commands.firstWhere((c) => c.ctor == 'connect');
      r.submitCommand(connect, {'target': GAtom('bob')});
      expect(sent, ['connect(bob)']);
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
