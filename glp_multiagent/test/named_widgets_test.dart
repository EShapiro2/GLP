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

/// GAI's agent_befriend, `agent_befriend(r?, q?, p? ; y)` (programs/gai/GAI.glp),
/// as programs/apps/gai/platform.vglp prints its Class.
const _gaiAgentBefriend = "form(agent_befriend, [field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown), field('Speech', $_speech)])";

/// CSSN's child_befriend, `child_befriend(r?, s?, p?, q?)`
/// (programs/cssn/CSSN.glp), as programs/cssn/platform.vglp prints its Class.
const _cssnChildBefriend = "form(child_befriend, [field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown), field('Peer', shown)])";

/// Alice asks that their child Carol and Bob's child Dave befriend.
const _childBefriendView = 'approval(attempt(alice, 4, 4, child_befriend('
    'carol, dave, alice, bob)), input([2]))';

/// CSSN's enter and enter_2, `enter(p?, r?)` and `enter_2(p?, q?, r?)`
/// (programs/cssn/CSSN.glp), as programs/cssn/platform.vglp prints their
/// Class.
const _cssnEnter = "form(enter, [field('Peer', shown), field('Peer', shown)])";
const _cssnEnter2 = "form(enter_2, [field('Peer', shown), "
    "field('Peer', shown), field('Peer', shown)])";

/// The last views of the agents asked: Bob, Dave's parent and Alice's friend;
/// Carol, Alice's child; and Dave, Bob's child.
const _bobParentOutput = '[view([friend(alice), parenting(dave), '
    'friend(dave), identity], [alice, carol, dave])]';
const _carolOutput = '[view([friend(alice), child, parent(alice), identity], '
    '[alice, bob, dave])]';
const _daveOutput = '[view([friend(bob), child, parent(bob), identity], '
    '[alice, bob, carol])]';

/// The code of Alf, 'The code of Alf' signed by acme_ltd, its developer, signed
/// by Alice; and Alf's attestation, the code signed over by Alice at enter
/// (programs/apps/gai/person.glp).
const _code = "speech(speech(utterance('The code of Alf'), acme_ltd), alice)";
const _attestation = 'speech($_code, alice)';

/// Alf, Alice's AI agent, asks to befriend Bob with its attestation.
const _agentBefriendView = 'approval(attempt(alf, 3, 3, agent_befriend(alf, '
    'bob, alice, $_attestation)), input([2]))';

/// The last view Alice's agent wrote: she is Alf's principal and its friend.
const _aliceOutput = '[view([principal_of(alf), friend(alf), item($_code), '
    'authored($_code), friend(bob), identity], [alf, bob])]';

/// The last view Bob's agent wrote: he knows Alf and Alice, Alice his friend.
const _bobOutput = '[view([friend(alice), identity], [alf, alice])]';

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

  /// Draw construct 1, with [output], where given, as the last value drawn
  /// on construct 0, the views the agent wrote, with which a declared widget
  /// is drawn (vGLP e924f73, Section 6.2).
  Future<OpenConstruct> draw(String widget, String view,
      {String? output}) async {
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
            output: output == null ? null : fixtureTerm(output),
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
      String defaultWidget, String view, String label, String named,
      {String? output}) async {
    await draw(defaultWidget, view);
    await tap(find.widgetWithText(ElevatedButton, label));
    final byDefault = grants.removeLast();
    await draw('named($approvalSentenceName, $defaultWidget)', view,
        output: output);
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
      'agent_befriend(R, Q, P, Y) at the principal, whose view holds '
      'principal_of(R): "Alf asks to befriend Bob.", "Approve?", Yes and No, '
      'and nothing else; Yes grants yes and No grants no at [2], as the '
      'default widget does', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_gaiAgentBefriend]), _agentBefriendView,
        output: _aliceOutput);
    expect(s.texts(), ['Alf asks to befriend Bob.', 'Approve?', 'Yes', 'No']);
    expect(find.byType(ElevatedButton), findsNWidgets(2));
    final def = _defaultApproval([_gaiAgentBefriend]);
    final (dYes, nYes) = await s.both(def, _agentBefriendView, 'yes', 'Yes',
        output: _aliceOutput);
    expect(dYes, 'input(1, [2], yes)');
    expect(nYes, dYes);
    final (dNo, nNo) = await s.both(def, _agentBefriendView, 'no', 'No',
        output: _aliceOutput);
    expect(dNo, 'input(1, [2], no)');
    expect(nNo, dNo);
  });

  testWidgets(
      'agent_befriend(R, Q, P, Y) at the other party: "Alf, an AI agent of '
      'Alice, asks to befriend you.", "Developer: Acme Ltd", the first signer of '
      'the attestation, "Befriend Alf?", Yes and No, and nothing else',
      (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_gaiAgentBefriend]), _agentBefriendView,
        output: _bobOutput);
    expect(s.texts(), [
      'Alf, an AI agent of Alice, asks to befriend you.',
      'Developer: Acme Ltd',
      'Befriend Alf?',
      'Yes',
      'No',
    ]);
    final (d, n) = await s.both(_defaultApproval([_gaiAgentBefriend]),
        _agentBefriendView, 'yes', 'Yes',
        output: _bobOutput);
    expect(d, 'input(1, [2], yes)');
    expect(n, d);
  });

  testWidgets(
      'agent_befriend answered: the sentence, its lines and question, and the '
      'label of the button granting the answer, and no button',
      (tester) async {
    final s = _Screen(tester);
    await s.draw(
        _named([_gaiAgentBefriend]),
        'approval(attempt(alf, 3, 3, agent_befriend(alf, bob, alice, '
        '$_attestation)), yes)',
        output: _bobOutput);
    expect(s.texts(), [
      'Alf, an AI agent of Alice, asks to befriend you.',
      'Developer: Acme Ltd',
      'Befriend Alf?',
      'Yes',
    ]);
    expect(find.byType(ElevatedButton), findsNothing);
  });

  testWidgets(
      'agent_befriend with no view drawn, whom it is asked not told: the '
      'sentence built from the class, its speech act under it, and Yes and No',
      (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_gaiAgentBefriend]), _agentBefriendView);
    expect(s.texts(), [
      'Alf asks to agent befriend Bob, Alice.',
      'The code of Alf',
      'by Acme Ltd, via Alice, via Alice',
      'Yes',
      'No',
    ]);
  });

  test('the developer an attestation names is its first signer', () {
    expect(
        formatTerm(SocialState.developerIn(fixtureTerm(_attestation))!), 'acme_ltd');
    expect(SocialState.developerIn(const GAtom('alice')), isNull);
    final (_, s) = SocialState.read(fixtureTerm(
        '[view([agent_of(alf, alice, $_attestation), friend(alf)], [alf])]'));
    expect(formatTerm(s!.developerOf(const GAtom('alf'))!), 'acme_ltd');
    expect(s.developerOf(const GAtom('alice')), isNull);
    expect(formatTerm(s.attestationOf(const GAtom('alf'))!),
        formatTerm(fixtureTerm(_attestation)));
  });

  test('a principal\'s AI agents, those retired, and the items held', () {
    final (_, s) = SocialState.read(fixtureTerm('[view([retired_of(alf), '
        'principal_of(alf), principal_of(ada), item($_code), '
        "authored($_code), agent_of(rex, carol, $_attestation), "
        'identity], [])]'));
    expect(s!.aiAgents.map(formatTerm), ['alf', 'ada']);
    expect(s.isPrincipalOf(const GAtom('ada')), isTrue);
    expect(s.isPrincipalOf(const GAtom('bob')), isFalse);
    expect(s.hasRetired(const GAtom('alf')), isTrue);
    expect(s.hasRetired(const GAtom('ada')), isFalse);
    expect(s.isAgent, isFalse);
    expect(s.holdsItem(fixtureTerm(_code)), isTrue);
    expect(s.holdsItem(fixtureTerm(_attestation)), isFalse);
    // A known AI agent: her own, or one she holds recorded.
    expect(s.isKnownAgent(const GAtom('alf')), isTrue);
    expect(s.isKnownAgent(const GAtom('rex')), isTrue);
    expect(s.isKnownAgent(const GAtom('carol')), isFalse);
  });

  test('a person by name: the key, its underscores as spaces and each word '
      'capitalised', () {
    expect(personName(const GAtom('alice')), 'Alice');
    expect(personName(const GAtom('alf')), 'Alf');
    expect(personName(const GAtom('acme_ltd')), 'Acme Ltd');
    expect(personName(const GAtom('acme')), 'Acme');
  });

  testWidgets(
      'child_befriend(R, S, P, Q) at the other child\'s parent: "Alice asks '
      'that their child Carol and your child Dave befriend.", "Approve?", Yes '
      'and No, granting what the default widget grants (CSSN #8 Cowork, '
      'Code_inbox.md, 2026-10-20 13:13 UTC, item 1)', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_cssnChildBefriend]), _childBefriendView,
        output: _bobParentOutput);
    expect(s.texts(), [
      'Alice asks that their child Carol and your child Dave befriend.',
      'Approve?',
      'Yes',
      'No',
    ]);
    final (d, n) = await s.both(_defaultApproval([_cssnChildBefriend]),
        _childBefriendView, 'yes', 'Yes',
        output: _bobParentOutput);
    expect(d, 'input(1, [2], yes)');
    expect(n, d);
  });

  testWidgets(
      'child_befriend at a child: "Alice asks that you and Dave befriend, '
      'with Dave\'s parent Bob.", "Befriend Dave?"; at the other child, asked '
      'by the other child\'s parent, "Alice asks that you and their child '
      'Carol befriend.", "Befriend Carol?" (CSSN #8 Cowork, Code_inbox.md, '
      '2026-10-20 13:13 UTC, item 5)', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_cssnChildBefriend]), _childBefriendView,
        output: _carolOutput);
    expect(s.texts(), [
      "Alice asks that you and Dave befriend, with Dave's parent Bob.",
      'Befriend Dave?',
      'Yes',
      'No',
    ]);
    await s.draw(_named([_cssnChildBefriend]), _childBefriendView,
        output: _daveOutput);
    expect(s.texts(), [
      'Alice asks that you and their child Carol befriend.',
      'Befriend Carol?',
      'Yes',
      'No',
    ]);
    // Asked by a person who is neither child's parent: both named.
    await s.draw(
        _named([_cssnChildBefriend]),
        'approval(attempt(grace, 4, 4, child_befriend(carol, dave, alice, '
        'bob)), input([2]))',
        output: _daveOutput);
    expect(s.texts(), [
      "Grace asks that you and Carol befriend, with Carol's parent Alice.",
      'Befriend Carol?',
      'Yes',
      'No',
    ]);
  });

  testWidgets(
      'enter(P, R) at the child: "Alice enters you as their child.", '
      '"Approve?", Yes and No, granting what the default widget grants '
      '(CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 4)',
      (tester) async {
    final s = _Screen(tester);
    const view =
        'approval(attempt(alice, 2, 2, enter(alice, carol)), input([2]))';
    await s.draw(_named([_cssnEnter]), view, output: '[view([], [alice])]');
    expect(s.texts(),
        ['Alice enters you as their child.', 'Approve?', 'Yes', 'No']);
    final (d, n) = await s.both(
        _defaultApproval([_cssnEnter]), view, 'yes', 'Yes',
        output: '[view([], [alice])]');
    expect(d, 'input(1, [2], yes)');
    expect(n, d);
  });

  testWidgets(
      'enter_2(P, Q, R): at the other parent, "Bob asks to enter Dave as '
      'your child, with you."; at the child, "Bob enters you as their '
      'child."; each "Approve?", Yes and No (CSSN #8 Cowork, Code_inbox.md, '
      '2026-10-20 13:13 UTC, item 4)', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(bob, 3, 3, enter_2(bob, grace, dave)), '
        'input([2]))';
    await s.draw(_named([_cssnEnter2]), view,
        output: '[view([friend(bob), identity], [bob, dave])]');
    expect(s.texts(), [
      'Bob asks to enter Dave as your child, with you.',
      'Approve?',
      'Yes',
      'No'
    ]);
    await s.draw(_named([_cssnEnter2]), view,
        output: '[view([], [bob, grace])]');
    expect(s.texts(),
        ['Bob enters you as their child.', 'Approve?', 'Yes', 'No']);
    // No view drawn: whom it is asked is not told, and the sentence is
    // built from the class.
    await s.draw(_named([_cssnEnter2]), view);
    expect(s.texts(), ['Bob asks to enter 2 Grace, Dave.', 'Yes', 'No']);
  });

  testWidgets(
      'child_befriend with no view drawn, whom it is asked not told: the '
      'sentence built from the class', (tester) async {
    final s = _Screen(tester);
    await s.draw(_named([_cssnChildBefriend]), _childBefriendView);
    expect(s.texts(), [
      'Alice asks to child befriend Carol, Dave, Bob.',
      'Yes',
      'No',
    ]);
  });

  test('what a CSSN state says of each person: child, parent, friend', () {
    final (_, alice) = SocialState.read(fixtureTerm(
        '[view([friend(bob), parenting(carol), friend(carol), identity], '
        '[bob, carol, dave])]'));
    expect(alice!.children.map(formatTerm), ['carol']);
    expect(alice.isChild, isFalse);
    expect(alice.relationOf(const GAtom('carol')), 'your child');
    expect(alice.relationOf(const GAtom('bob')), 'your friend');
    expect(alice.relationOf(const GAtom('dave')), isNull);
    final (_, carol) = SocialState.read(fixtureTerm(_carolOutput));
    expect(carol!.isChild, isTrue);
    expect(carol.parents.map(formatTerm), ['alice']);
    expect(carol.relationOf(const GAtom('alice')), 'your parent');
    expect(carol.hasWards, isTrue);
    final (_, gsg) = SocialState.read(
        fixtureTerm('[view([friend(bob), identity], [bob])]'));
    expect(gsg!.hasWards, isFalse);
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
