// The two readers of the view and the request question of a canonical
// platform (lib/ui_runtime/named_widgets.dart, `SocialState.read` and
// `socialMenu`).  "On the program's output to its person ... the agent goal
// writes its view view(k, s, K, A), its own key k, the machine state s of
// its app state, the persons K it knows and the approvals A its person made
// that are pending"; the request question is "a goal of a volitional
// procedure whose question, of the request type Request?, takes one request"
// (vGLP 12f629d, Section 5.4, "The compilation"); the compiler carries "the
// tab's name and its predicate into the view the agent writes" (Section 5.4,
// "The contract's words"); and "A declared widget is drawn also with the last
// value drawn on construct 0" (Section 6.2).  vGLP's compiler prints the
// view `view(Key, List(Atom), List(Key), List(Approved), List(Tab))` and the
// request question `(Request?)*request(Stream(Trigger))`, its construct the
// bare `input(P)` (programs/vglp/cva/compiler/text.glp at GLP 7d857bcd).
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

/// The widget of the request question, `named(W, D)`: `W` the declared widget
/// whole, its buttons (programs/vglp/cva/compiler/README.md, 6;
/// glp_runtime/lib/vglp/canonical.dart, `DeclaredWidget.drawnAs`), and `D` the
/// default widget of `Request ::= propose(Proposal)`, the form `propose`,
/// over [proposals].
String _requestWidget(String buttons, String proposals) =>
    "named('social_menu([${buttons.replaceAll("'", r"\'")}])', "
    "form(propose, [field('Proposal', menu([$proposals]))]))";

/// Two proposals: establish, its button Establish, where the state lacks
/// identity; and befriend, with no button.
final _requestW = _requestWidget(
    'button("Establish", [act(establish, 1, [forb(identity, [])], [], [], [], [])])',
    "label('Establish', form(establish, [field('Peer', peer)])), "
        "form(befriend, [field('Peer', peer), field('Peer', peer)])");

/// The ways of GKK, GSG and GSN's acts and GB's Swap, as the compiler prints
/// them: Befriend, Bob bound by knows(Bob), friend(Bob) forbidden; Introduce,
/// two persons known; Message, a friend and a text; Forward, an item and a
/// person field, proved; Swap, two acts of one button, each bound by what is
/// held, the second also by a maturity.
final _waysW = _requestWidget(
    'button("Befriend", [act(befriend, 1, [req(identity, []), req(knows, [at(2)]), forb(friend, [at(2)])], [2], [], [], [])]), '
    'button("Introduce", [act(introduce, 1, [req(knows, [at(2)]), req(knows, [at(3)])], [2, 3], [], [], [])]), '
    'button("Message", [act(message, 1, [req(friend, [at(2)])], [2], [field(3, "text", text)], [], [])]), '
    'button("Forward", [act(fwd, 1, [req(item, [at(3)]), proved(identity, [], knows, [at(2)]), unless(blocked, [at(2)])], [3], [field(2, "person", person)], [], [])]), '
    'button("Swap", [act(swap_c, 1, [req(coin, [at(2)])], [2], [field(3, "amount", number)], [], []), act(swap_d, 1, [req(coin, [at(2)])], [2], [field(3, "amount", number), field(4, "maturity", number)], [], [])]), '
    'button("Leave", [act(leave, 2, [req(member, [grp(1, 3)])], [1, 3], [], [], [])]), '
    'button("Sign", [act(sign, 1, [req(identity, [])], [], [field(2, "text", text)], [], [at(1), say(" signs.")])]), '
    'button("Acquaint", [act(acquaint, 1, [req(identity, [])], [], [field(2, "person", key)], [], [])])',
    "form(befriend, [field('Peer', peer), field('Peer', peer)]), "
        "form(introduce, [field('Peer', peer), field('Peer', peer), field('Peer', peer)]), "
        "form(message, [field('Peer', peer), field('Peer', peer), field('String', text)]), "
        "form(fwd, [field('Peer', peer), field('Peer', peer), field('String', text)]), "
        "form(swap_c, [field('Peer', peer), field('Peer', peer), field('Integer', number)]), "
        "form(swap_d, [field('Peer', peer), field('Peer', peer), field('Integer', number), field('Integer', number)]), "
        "form(leave, [field('Peer', peer), field('Peer', peer), field('String', text)]), "
        "form(sign, [field('Peer', peer), field('String', text)]), "
        "form(acquaint, [field('Peer', peer), field('Peer', peer)])");

/// A view with [atoms], knowing [known], one tab for each predicate of [tabs].
String _view(String atoms, String known, String tabs) =>
    "view(alice, [$atoms], [$known], [], [$tabs])";

/// Alice's view: her key, an empty state, the persons she knows, her
/// approval pending and the tab Contacts.
const _aliceView = "view(alice, [], [bob, carol], [befriend(alice, bob)], "
    "[tab('Contacts', knows)])";

/// Tap the control [what] of the request question, scrolled into view.
Future<void> _tap(WidgetTester tester, String what) async {
  final f = find.byKey(socialKey(1, what));
  await tester.ensureVisible(f);
  await tester.pumpAndSettle();
  await tester.tap(f);
  await tester.pumpAndSettle();
}

class _Screen {
  final WidgetTester tester;
  final List<String> grants = [];
  late final Bridge bridge =
      Bridge(onGrant: (g) => grants.add(formatTerm(g)));

  _Screen(this.tester);

  /// Draw the request question, construct 1, its view [view], with [output]
  /// the last value drawn on construct 0, where given, its widget [w].
  Future<void> draw(String view, {String? output, String? w}) async {
    expect(
        bridge.read(
            fixtureTerm('draw(1, ${w ?? _requestW}, request_r($view))')),
        isTrue);
    final c = bridge.construct(1)!;
    await tester.pumpWidget(MaterialApp(
      home: Scaffold(
        body: SingleChildScrollView(
          child: ConstructView(
            key: UniqueKey(),
            construct: c,
            family: ConstructFamily.standard,
            output: output == null ? null : fixtureTerm(output),
            grant: (p, r) => bridge.grant(c.id, p, r),
          ),
        ),
      ),
    ));
    await tester.pumpAndSettle();
  }
}

void main() {
  test(
      'SocialState.read on view(Me, S, K, A, Ts): the key, the atoms, the '
      'persons known, the approvals pending and the tabs; the notices '
      'written after it', () {
    final (ok, s) = SocialState.read(fixtureTerm(
        "[view(bob, [identity], [], [], [tab('Contacts', knows)]), "
        "view(alice, [identity, knows(bob), bond(bob, 25)], [bob, carol], "
        "[befriend(alice, bob)], [tab('Contacts', knows), "
        "tab('Bonds', bond, counted)]), "
        "refused(befriend(alice, bob), bob, declined)]"));
    expect(ok, isTrue);
    expect(formatTerm(s!.me), 'alice');
    expect(s.atoms.map(formatTerm), ['identity', 'knows(bob)', 'bond(bob, 25)']);
    expect(s.known.map(formatTerm), ['bob', 'carol']);
    expect(s.pending.map(formatTerm), ['befriend(alice, bob)']);
    expect(s.lists, [
      (name: 'Contacts', predicate: 'knows', counted: false),
      (name: 'Bonds', predicate: 'bond', counted: true),
    ]);
    expect(s.notices.map((n) => formatTerm(n.by)), ['bob']);
  });

  test(
      'SocialState.read: a view without the key or the tabs is not the '
      'view the compiler prints', () {
    expect(SocialState.read(fixtureTerm('[view([identity], [bob])]')),
        (false, null));
    expect(
        SocialState.read(
            fixtureTerm("[view([identity], [bob], [], [tab('Contacts', knows)])]")),
        (false, null));
    // No view written yet.
    final (ok, s) = SocialState.read(fixtureTerm('[]'));
    expect(ok, isTrue);
    expect(s, isNull);
  });

  test('the requests the bare request type offers, read off its default widget',
      () {
    final w = fixtureTerm(_requestW);
    expect(namedWidget(w)!.$1, socialMenuName);
    final o = RequestOffers.of(w);
    expect(o.proposals, {'establish/1', 'befriend/2'});
    expect(o.label('establish', 1), 'Establish');
    expect([for (final b in o.buttons) b.label], ['Establish']);
    expect(o.buttons.single.acts.single.schema, 'establish');
  });

  test('a way is a binding of the places of the atoms the act requires, the '
      'state lacking what the binding determines it forbids', () {
    final o = RequestOffers.of(fixtureTerm(_waysW));
    final befriend = ActInfo(o.buttons[0].acts.single);
    final (_, s) = SocialState.read(fixtureTerm(
        '[${_view("identity, knows(bob), knows(carol), friend(carol)", "bob, carol", "")}]'));
    expect([for (final w in befriend.ways(s!.atoms, s.me)) formatTerm(w[2]!)],
        ['bob']);
    // Ordered pairs of the persons known, a way each.
    final introduce = ActInfo(o.buttons[1].acts.single);
    expect(introduce.ways(s.atoms, s.me).length, 4);
    // The proposer's place is the viewer's: Leave at place 2, the leader bound by the group.
    final leave = ActInfo(o.buttons[5].acts.single);
    final (_, m) = SocialState.read(fixtureTerm(
        '[${_view("member(dot(community(bob), speech(utterance('friends'), bob)))", "bob", "")}]'));
    final w = leave.ways(m!.atoms, m.me).single;
    expect(formatTerm(w[1]!), 'bob');
    expect(formatTerm(w[2]!), 'alice');
  });

  testWidgets('one button for each way, beside the atom that binds it; none '
      'where a required atom is absent or a forbidden one present',
      (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, knows(bob), knows(carol), friend(carol)", "bob, carol", "tab('Contacts', knows), tab('Friends', friend)")}]');
    expect(find.byKey(socialKey(1, 'act:befriend:bob')), findsOneWidget);
    // friend(carol) forbids it with Carol.
    expect(find.byKey(socialKey(1, 'act:befriend:carol')), findsNothing);
    // No friend(bob): no Message with Bob.
    expect(find.byKey(socialKey(1, 'act:message:bob')), findsNothing);
    expect(find.byKey(socialKey(1, 'act:message:carol')), findsOneWidget);
  });

  testWidgets('the request a button makes carries the values of its way',
      (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, knows(bob)", "bob", "tab('Contacts', knows)")}]');
    await _tap(tester, 'act:befriend:bob');
    expect(s.grants, ['input(1, [], propose(befriend(alice, bob)))']);
  });

  testWidgets('Introduce beside each person known, a picker of the others; '
      'the request carries both', (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, knows(bob), knows(carol)", "bob, carol", "tab('Contacts', knows)")}]');
    await _tap(tester, 'act:introduce:bob');
    expect(find.byKey(socialKey(1, 'pick:introduce:bob:carol')), findsOneWidget);
    await _tap(tester, 'pick:introduce:bob:carol');
    await _tap(tester, 'send:introduce:bob');
    expect(s.grants, ['input(1, [], propose(introduce(alice, bob, carol)))']);
  });

  testWidgets('a button that names several acts, once for the ways that bind '
      'the same values; the request of the act whose fields are filled',
      (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, coin(bob), coin(bob), coin(carol)", "", "tab('Coins', coin, counted)")}]');
    expect(find.byKey(socialKey(1, 'act:swap:bob')), findsOneWidget);
    expect(find.byKey(socialKey(1, 'act:swap:carol')), findsOneWidget);
    await _tap(tester, 'act:swap:bob');
    // Its fields, each name once.
    expect(find.byKey(socialKey(1, 'field:swap:bob:1')), findsOneWidget);
    expect(find.byKey(socialKey(1, 'field:swap:bob:2')), findsOneWidget);
    expect(find.byKey(socialKey(1, 'field:swap:bob:3')), findsNothing);
    await tester.ensureVisible(find.byKey(socialKey(1, 'field:swap:bob:1')));
    await tester.enterText(find.byKey(socialKey(1, 'field:swap:bob:1')), '2');
    await tester.enterText(find.byKey(socialKey(1, 'field:swap:bob:2')), '9');
    await tester.pumpAndSettle();
    await _tap(tester, 'send:swap:bob');
    expect(s.grants, ['input(1, [], propose(swap_d(alice, bob, 2, 9)))']);
  });

  testWidgets('a person field offers the persons the viewer knows, proved and '
      'not forbidden; with no one to offer the button is not drawn',
      (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, item(a), knows(bob), knows(carol), blocked(carol)", "bob, carol, dan", "tab('Items', item)")}]');
    await _tap(tester, 'act:fwd:a');
    expect(find.byKey(socialKey(1, 'pick:fwd:a:2:bob')), findsOneWidget);
    // unless(blocked, [at(2)]), and Dan not proved by knows(dan).
    expect(find.byKey(socialKey(1, 'pick:fwd:a:2:carol')), findsNothing);
    expect(find.byKey(socialKey(1, 'pick:fwd:a:2:dan')), findsNothing);
    await _tap(tester, 'pick:fwd:a:2:bob');
    await _tap(tester, 'send:fwd:a');
    expect(s.grants, ['input(1, [], propose(fwd(alice, bob, a)))']);

    final none = _Screen(tester);
    await none.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, item(a)", "", "tab('Items', item)")}]');
    expect(find.byKey(socialKey(1, 'act:fwd:a')), findsNothing);
  });

  testWidgets('the acquaintance\'s field takes the key of a person the viewer '
      'does not know; a way no atom of a list binds is on the first tab, with '
      'the text of its form', (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])',
        w: _waysW,
        output: '[${_view("identity, knows(bob)", "bob", "tab('Contacts', knows)")}]');
    final send = find.byKey(socialKey(1, 'send:acquaint:'));
    Future<bool> enabled(String key) async {
      await tester.enterText(find.byKey(socialKey(1, 'field:acquaint::2')), key);
      await tester.pumpAndSettle();
      return tester.widget<ElevatedButton>(send).onPressed != null;
    }

    expect(await enabled('bob'), isFalse);
    expect(await enabled('alice'), isFalse);
    expect(await enabled('carol'), isTrue);
    await _tap(tester, 'send:acquaint:');
    expect(s.grants, ['input(1, [], propose(acquaint(alice, carol)))']);
    expect(find.text('Alice signs.'), findsOneWidget);
  });

  testWidgets(
      'socialMenu on the bare question input(P): the key and the persons '
      'known from the last view drawn on construct 0, the request granted at '
      'the question\'s position', (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])', output: '[$_aliceView]');
    // The person's own name, from the view's key.
    expect(
        tester.widget<Text>(find.byKey(socialKey(1, 'name'))).data, 'Alice');
    await tester.tap(find.widgetWithText(ElevatedButton, 'Establish'));
    await tester.pumpAndSettle();
    expect(s.grants, ['input(1, [], propose(establish(alice)))']);
  });

  testWidgets(
      'socialMenu with no view drawn on construct 0 yet: drawn waiting, no '
      'name and no button', (tester) async {
    final s = _Screen(tester);
    await s.draw('input([])', output: '[]');
    expect(find.text('…'), findsOneWidget);
    expect(find.byKey(socialKey(1, 'name')), findsNothing);
    expect(find.byType(ElevatedButton), findsNothing);
    expect(s.grants, isEmpty);
  });
}
