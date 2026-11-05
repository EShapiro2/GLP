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

/// The widget of the request question, `named(social_menu, D)`, D the
/// default widget of `Request ::= propose(Proposal)`, the form `propose`,
/// over two proposals: establish, its button Establish drawn on the tab
/// Contacts where the state lacks identity, as the compiler prints a
/// pragma's act (glp_runtime/lib/vglp/canonical.dart, `ActPlace`); and
/// befriend, with no label and no place.
const _requestW = "named(social_menu, form(propose, [field('Proposal', "
    "menu([act(place('Contacts', 1, [], [], [forb(identity, [])], [], []), "
    "label('Establish', form(establish, [field('Peer', peer)]))), "
    "form(befriend, [field('Peer', peer), field('Peer', peer)])]))]))";

/// Alice's view: her key, an empty state, the persons she knows, her
/// approval pending and the tab Contacts.
const _aliceView = "view(alice, [], [bob, carol], [befriend(alice, bob)], "
    "[tab('Contacts', knows)])";

class _Screen {
  final WidgetTester tester;
  final List<String> grants = [];
  late final Bridge bridge =
      Bridge(onGrant: (g) => grants.add(formatTerm(g)));

  _Screen(this.tester);

  /// Draw the request question, construct 1, its view [view], with [output]
  /// the last value drawn on construct 0, where given.
  Future<void> draw(String view, {String? output}) async {
    expect(bridge.read(fixtureTerm('draw(1, $_requestW, request_r($view))')),
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
    final o = RequestOffers.of(namedWidget(fixtureTerm(_requestW))!.$2);
    expect(o.proposals, {'establish/1', 'befriend/2'});
    expect(o.label('establish', 1), 'Establish');
    expect(o.tabOf('establish'), 'Contacts');
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
