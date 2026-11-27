/// The logic of the described screens apart from their drawing (lib/ui_runtime/described.dart), over the
/// compiled descriptions `programs/gce/ui/*.ui.json`: a typed name that starts filled (`NEW AS`), a choice
/// kept to the screen's own value, a button's plain label (`IS CALLED`), a chain's steps, the place of the
/// landmarks, a list that is told of (`TOLD`), and the lines of a conversation.
library;

import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/described.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

UiDescription _read(String n) => UiDescription.parse(File('../programs/gce/ui/$n.ui.json').readAsStringSync());

void main() {
  final groups = _read('Groups'), friendship = _read('Friendship');
  UiComponent action(UiDescription ui, String screen, String schema) =>
      ui.screens[screen]!.components.firstWhere((c) => c.kind == 'action' && c.j['schema'] == schema);

  test('NEW AS: Group1, or the first number no name in the person\'s facts has', () {
    final create = action(groups, 'Chats', 'create'), look = create.places[2]!.look;
    expect(look?['fresh'], {'stem': 'Group', 'n': 1});
    final none = DescribedCore(groups, const SocialState(GAtom('alice'), [], [], []), null);
    expect(none.freshName(look), 'Group1');
    final some = DescribedCore(groups,
        const SocialState(GAtom('alice'), [GStruct('leader', [GAtom('group1')]), GStruct('leader', [GAtom('group2')])], [], []), null);
    expect(some.freshName(look), 'Group3');
    expect(none.freshName(null), isNull);
  });

  test('IS CALLED: a button reads its label; one with none, its name', () {
    expect(action(groups, 'Chats', 'create').label, 'New group');
    expect(action(groups, 'Chats', 'acquaint').label, 'Add a contact');
    expect(action(groups, 'Chats', 'close').label, 'Close a group');
    // A label kept to one screen: Join is "Add to group" on a group's screen, "Add to a group of mine" on a person's.
    expect(action(groups, 'Group', 'join').label, 'Add to group');
    expect(action(groups, 'Chat', 'join').label, 'Add to a group of mine');
    expect(action(groups, 'Chat', 'unfriend').label, 'Unfriend');
  });

  test('a chain: its steps, each an action, sharing the names of their places', () {
    final chain = friendship.screens['Home']!.components.firstWhere((c) => c.kind == 'chain');
    expect(chain.button, 'Make friends');
    expect(chain.where, 'primary');
    expect([for (final s in chain.steps) s.schema], ['acquaint', 'befriend']);
    expect([for (final s in chain.steps) s.places[2]!.name], ['Bob', 'Bob']);
  });

  test('a choice kept to the screen\'s value: only the subject\'s, where the list holds it', () {
    const ui = UiDescription('Home', {
      'Group': UiScreen('Group', true, 'member', [])
    }, {});
    final core = DescribedCore(ui, const SocialState(GAtom('alice'), [], [], []), null);
    expect(const PlaceSource(from: 'leader', same: 1).same, 1);
    expect(PlaceSource.of({'from': 'leader', 'same': 1, 'name': 'Grupo'}).same, 1);
    // 0: the subject itself, as on a person's screen.
    expect(PlaceSource.of({'from': 'knows', 'same': 0, 'name': 'Bob'}).same, 0);
    expect(core.names(const GStruct('member', [GAtom('hikers'), GAtom('bob')]), const GAtom('hikers')), isTrue);
  });

  test('landmarks: placed as the UI style says, on top where it says nothing', () {
    expect(_read('App').look['landmarks'], isNull);
    final ui = UiDescription.parse('{"start": "Home", "landmarks": ["More"], "look": {"landmarks": "bottom"}, '
        '"screens": {"Home": {"about": null, "components": []}, "More": {"about": null, "components": []}}}');
    expect(ui.look['landmarks'], 'bottom');
    expect(ui.landmarks, ['More']);
  });

  test('the lines of a conversation: the text, whether the person wrote it, and who did', () {
    const ui = UiDescription('Home', {}, {'message': [1]});
    const hi = GStruct('message', [GAtom('bob'), GString('hi')]);
    const mine = GStruct('message', [GAtom('bob'), GStruct('speech', [GStruct('utterance', [GString('yo')]), GAtom('alice')])]);
    final core = DescribedCore(ui, const SocialState(GAtom('alice'), [hi, mine], [], []), null);
    final ls = core.lines(['message'], const GAtom('bob'));
    expect(ls, hasLength(2));
    expect(core.line(ls[0].$1, ls[0].$2), ('hi', false));
    expect(formatTerm(core.author(ls[0].$1, ls[0].$2)!), 'bob');
    expect(core.line(ls[1].$1, ls[1].$2).$2, isTrue);
    expect(core.namesRequest(const GStruct('befriend', [GAtom('alice'), GAtom('bob')]), const GStruct('befriend', [GAtom('alice'), GAtom('bob')])), isTrue);
    expect(core.namesRequest(const GStruct('unfriend', [GAtom('alice')]), const GStruct('befriend', [GAtom('alice'), GAtom('bob')])), isFalse);
  });

  test('a fact in the contract\'s words: the holder you, the verb without its s', () {
    final app = _read('App');
    expect(app.phrase('trusts_developer', ['Carol']), 'you trust Carol as a developer');
    expect(app.phrase('identity', []), 'you have an identity');
    expect(app.phrase('no_such', []), isNull);
  });
}
