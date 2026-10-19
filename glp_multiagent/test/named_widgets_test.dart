// The named widgets of the grassroots app's construct family
// (lib/ui_runtime/named_widgets.dart): the Dart widgets a widget declaration
// `T =::= W` names (vGLP, Definition "Widget Declaration": "In the grassroots
// app, W names a Dart widget: the construct family draws it, and its type
// fixes the terms it forms").  Each is drawn over a view as the bridge holds
// it, from a draw `draw(Id, named(W, D), f(V))`, D the default widget of the
// type, "drawn with the default widget of its type beside its name, from
// which it reads the type's constructors" (vGLP 92e6d3b, Sections 6.2 and
// 6.3), and each of its buttons grants what the default widget of the type
// grants for the same view, so that the construct process forms the same
// answers from it.
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

import 'fixture_term.dart';

/// The default widget of a Speech the program writes, as the canonical
/// compilation emits it for every canonical CVA platform
/// (programs/apps/gsn, the construct of `approval_w`).
const _speech = "rec('Speech', form(speech, [field('Utterance', "
    "rec('Utterance', menu([form(utterance, [field('String', shown)]), "
    "form(speech, [field('Utterance', ref('Utterance')), "
    "field('Peer', shown)])]))), field('Peer', shown)]))";

/// The default widget of `Approval ::= approval(Attempt, YesNo?)` over the
/// classes [classes], each `form(F, [...])`, as the canonical compilation
/// emits it (programs/apps/gsg and gsn, the construct of `approval_w`).
String _defaultApproval(List<String> classes) =>
    "form(approval, [field('Attempt', form(attempt, [field('Peer', shown), "
    "field('Integer', shown), field('Integer', shown), "
    "field('Class', menu([${classes.join(', ')}]))])), "
    "field('YesNo', buttons([yes, no]))])";

const _gsgClasses = [
  "form(establish, [field('Peer', shown)])",
  "form(befriend, [field('Peer', shown), field('Peer', shown)])",
  "form(unfriend, [field('Peer', shown), field('Peer', shown)])",
];

const _gsnJoin =
    "form(join, [field('Peer', shown), field('Peer', shown), field('Speech', $_speech)])";

const _gsnMessage =
    "form(message, [field('Peer', shown), field('Peer', shown), field('Speech', $_speech)])";

/// The widget of a question of `Approval` where the declaration
/// `Approval =::= approval_sentence.` holds, over the classes [classes]:
/// `named(approval_sentence, D)`, D the default widget of `Approval` (vGLP
/// 92e6d3b, the construct process in the appendix of proofs).
String _named(List<String> classes) =>
    'named($approvalSentenceName, ${_defaultApproval(classes)})';

/// One construct on a screen of its own, drawn by the standard family from
/// the draw `draw(1, W, approval_w(V))`, its grants as the bridge returns
/// them, `input(Id, P, R)`.
class _Screen {
  final WidgetTester tester;
  final List<String> grants = [];
  late final Bridge bridge =
      Bridge(onGrant: (g) => grants.add(formatTerm(g)));

  _Screen(this.tester);

  Future<OpenConstruct> draw(String widget, String view) async {
    expect(
        bridge.read(fixtureTerm('draw(1, $widget, approval_w($view))')), isTrue);
    final c = bridge.construct(1)!;
    await tester.pumpWidget(MaterialApp(
      home: Scaffold(
        body: SingleChildScrollView(
          child: ConstructView(
            key: UniqueKey(),
            construct: c,
            family: ConstructFamily.standard,
            grant: (p, r) => bridge.grant(c.id, p, r),
          ),
        ),
      ),
    ));
    await tester.pumpAndSettle();
    return c;
  }

  Future<void> tap(Finder f) async {
    expect(f, findsOneWidget);
    await tester.tap(f);
    await tester.pumpAndSettle();
  }

  /// The texts approval_sentence draws for construct 1, in order.
  List<String> texts() => [
        for (final t in tester.widgetList<Text>(find.descendant(
            of: find.byKey(nodeKey(1, approvalSentenceName, [])),
            matching: find.byType(Text))))
          t.data ?? '',
      ];

  /// The grant of the button [label] of the default widget over [view], and that of the button [named] of approval_sentence over it.
  Future<(String, String)> both(
      String defaultWidget, String view, String label, String named) async {
    await draw(defaultWidget, view);
    await tap(find.widgetWithText(ElevatedButton, label));
    final byDefault = grants.removeLast();
    await draw('named($approvalSentenceName, $defaultWidget)', view);
    await tap(find.widgetWithText(ElevatedButton, named));
    final byName = grants.removeLast();
    return (byDefault, byName);
  }
}

void main() {
  test('the standard family has approval_sentence', () {
    expect(ConstructFamily.standard.named.keys, contains('approval_sentence'));
  });

  testWidgets(
      'befriend(A, Me): the sentence "Alice asks to be your friend." and the '
      'buttons Accept and Decline, and nothing else', (tester) async {
    final s = _Screen(tester);
    const view =
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), input([2]))';
    await s.draw(_named(_gsgClasses), view);
    expect(s.texts(), ['Alice asks to be your friend.', 'Accept', 'Decline']);
    expect(find.byKey(sentenceKey(1)), findsOneWidget);
    expect(find.byKey(answerKey(1, 'yes', [2])), findsOneWidget);
    expect(find.byKey(answerKey(1, 'no', [2])), findsOneWidget);
    expect(find.byType(ElevatedButton), findsNWidgets(2));
  });

  testWidgets(
      'befriend(A, Me): Accept grants yes and Decline grants no at [2], as '
      'the default widget of Approval does over the same view', (tester) async {
    final s = _Screen(tester);
    const view =
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), input([2]))';
    final def = _defaultApproval(_gsgClasses);
    final (dYes, nYes) = await s.both(def, view, 'yes', 'Accept');
    expect(dYes, 'input(1, [2], yes)');
    expect(nYes, dYes);
    final (dNo, nNo) = await s.both(def, view, 'no', 'Decline');
    expect(dNo, 'input(1, [2], no)');
    expect(nNo, dNo);
  });

  testWidgets(
      'join(A, Me, n): the sentence "Alice asks you to join the group '
      'friends.", the group named by its name, and the buttons Join and '
      'Decline, and nothing else', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(alice, 5, 5, join(alice, bob, '
        'speech(utterance(friends), alice))), input([2]))';
    await s.draw(_named([..._gsgClasses, _gsnJoin]), view);
    expect(s.texts(),
        ['Alice asks you to join the group friends.', 'Join', 'Decline']);
    expect(find.byType(ElevatedButton), findsNWidgets(2));
  });

  testWidgets(
      'join(A, Me, n): Join grants yes and Decline grants no at [2], as the '
      'default widget of Approval does over the same view', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(alice, 5, 5, join(alice, bob, '
        'speech(utterance(friends), alice))), input([2]))';
    final def = _defaultApproval([..._gsgClasses, _gsnJoin]);
    final (dYes, nYes) = await s.both(def, view, 'yes', 'Join');
    expect(dYes, 'input(1, [2], yes)');
    expect(nYes, dYes);
    final (dNo, nNo) = await s.both(def, view, 'no', 'Decline');
    expect(dNo, 'input(1, [2], no)');
    expect(nNo, dNo);
  });

  testWidgets(
      'a join initiated by the person it adds is not join(A, Me, n): a '
      'sentence built from the class, its speech act under it, and Yes and '
      'No', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(bob, 6, 6, join(alice, bob, '
        'speech(utterance(friends), alice))), input([2]))';
    await s.draw(_named([..._gsgClasses, _gsnJoin]), view);
    expect(s.texts(),
        ['Bob asks to join you.', 'friends', 'by you', 'Yes', 'No']);
  });

  testWidgets(
      'a class GAI\'s task does not word: a sentence built from it, each '
      'speech act by its text, by its first signer, via each who signed it '
      'after, the person asked named you, and Yes and No', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(dan, 1, 1, message(dan, bob, '
        'speech(speech(speech(utterance(flood), dan), eve), bob))), '
        'input([2]))';
    await s.draw(_named([_gsnMessage]), view);
    expect(s.texts(), [
      'Dan asks to message you.',
      'flood',
      'by Dan, via Eve, via you',
      'Yes',
      'No',
    ]);
    final (d, n) = await s.both(
        _defaultApproval([_gsnMessage]), view, 'yes', 'Yes');
    expect(n, d);
  });

  testWidgets(
      'a class naming two persons besides its initiator: each named, none '
      'you, the view not naming the agent asked', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named(_gsgClasses),
        'approval(attempt(alf, 3, 1, befriend(alf, bob, alice)), input([2]))');
    expect(find.text('Alf asks to befriend Bob, Alice.'), findsOneWidget);
    expect(find.textContaining('you'), findsNothing);
  });

  testWidgets(
      'a schema with an underscore reads as two words, and the initiator '
      'named in the class only once', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named(_gsgClasses),
        'approval(attempt(bob, 4, 4, post_own(bob, alice)), input([2]))');
    expect(find.text('Bob asks to post own you.'), findsOneWidget);
  });

  testWidgets(
      'the question answered: the sentence and the label of the button '
      'granting the answer, and no button', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named(_gsgClasses),
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), no)');
    expect(s.texts(), ['Alice asks to be your friend.', 'Decline']);
    expect(find.byType(ElevatedButton), findsNothing);
  });

  testWidgets(
      'a view that is no approval question of a canonical platform: named, '
      'and taking no input', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named(_gsgClasses), 'menu(alice, [], input([3]))');
    expect(
        find.text('approval_sentence cannot draw menu(alice, [], input([3]))'),
        findsOneWidget);
    expect(find.byType(ElevatedButton), findsNothing);
    await s.draw(_named(_gsgClasses), 'approval(nothing, input([2]))');
    expect(find.byType(ElevatedButton), findsNothing);
    expect(s.grants, isEmpty);
  });
}
