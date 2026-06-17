import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/gsg.dart';
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
      expect(formatTerm(GStruct('decision', [GAtom('yes'), GAtom('alice'), GStruct('req', [GInt(1)])])),
          'decision(yes, alice, req(1))');
    });
  });

  group('GSG runtime via manifest only', () {
    late List<String> sent;
    late UiRuntime r;

    setUp(() {
      sent = [];
      r = UiRuntime(manifest: gsgManifest, onSend: sent.add);
    });

    test('befriend notify becomes an inbox card; Accept sends decision(yes,..)', () {
      r.handleLine('befriend(alice, req(1))');
      expect(r.inbox.length, 1);
      final card = r.inbox.first;
      expect(card.desc.notifyCtor, 'befriend');
      final accept = card.desc.answers.firstWhere((a) => a.label == 'Accept');
      r.answerCard(card, accept);
      expect(sent, ['decision(yes, alice, req(1))']);
      expect(r.inbox, isEmpty);
    });

    test('befriend_intro Decline sends reject_intro WITHOUT the req id', () {
      r.handleLine('befriend_intro(alice, bob, req(2))');
      final card = r.inbox.single;
      final decline = card.desc.answers.firstWhere((a) => a.label == 'Decline');
      r.answerCard(card, decline);
      expect(sent, ['reject_intro(bob)']);
    });

    test('connected adds the friend to the Friends list (no feed)', () {
      r.handleLine('connected(bob)');
      final friends = r.store.lists['friends']!;
      expect(friends.map(formatTerm).toList(), ['bob']);
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

    test('received (messaging) is ignored by the GSG manifest', () {
      r.handleLine('received(bob, hi)');
      expect(r.inbox, isEmpty);
      expect(r.store.lists['friends'], isEmpty);
    });

    test('submitCommand builds the ground UserCmd from form values', () {
      final connect = gsgManifest.commands.firstWhere((c) => c.ctor == 'connect');
      r.submitCommand(connect, {'target': GAtom('bob')});
      expect(sent, ['connect(bob)']);
    });

    test('Friends state view is declared so the section renders empty first', () {
      expect(gsgManifest.state.any((v) => v.key == 'friends' && v.kind == StateKind.list), isTrue);
      expect(r.store.lists.containsKey('friends'), isTrue);
    });
  });
}
