// The named widgets of the grassroots app's construct family
// (lib/ui_runtime/named_widgets.dart): the Dart widgets a widget declaration
// `T =::= W` names (vGLP, Definition "Widget Declaration": "In the grassroots
// app, W names a Dart widget: the construct family draws it, and its type
// fixes the terms it forms").  Each is drawn over a view as the bridge holds
// it, from a draw `draw(Id, W, f(V))`, and each of its buttons grants what
// the default widget of the type grants for the same view, so that the
// construct process forms the same answers from it.
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

  /// The grant of the button [label] of the default widget over [view], and
  /// that of the button granting [c] of approval_sentence over it.
  Future<(String, String)> both(
      String defaultWidget, String view, String label, String c) async {
    await draw(defaultWidget, view);
    await tap(find.widgetWithText(ElevatedButton, label));
    final byDefault = grants.removeLast();
    await draw(approvalSentenceName, view);
    await tap(find.byKey(answerKey(1, c, [2])));
    final byName = grants.removeLast();
    return (byDefault, byName);
  }
}

void main() {
  test('the standard family has approval_sentence', () {
    expect(ConstructFamily.standard.named.keys, contains('approval_sentence'));
  });

  testWidgets(
      'approval_sentence over a friendship asked of bob: the sentence, '
      '"Approve?", and Yes and No', (tester) async {
    final s = _Screen(tester);
    const view =
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), input([2]))';
    await s.draw(approvalSentenceName, view);
    expect(find.byKey(nodeKey(1, approvalSentenceName, [])), findsOneWidget);
    expect(find.text('alice asks to befriend you.'), findsOneWidget);
    expect(find.text('attempt 2, rank 2'), findsOneWidget);
    expect(find.text('Approve?'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'Yes'), findsOneWidget);
    expect(find.widgetWithText(ElevatedButton, 'No'), findsOneWidget);
    expect(find.textContaining('cannot draw'), findsNothing);
  });

  testWidgets(
      'approval_sentence grants yes and no at [2], as the default widget of '
      'Approval does over the same view', (tester) async {
    final s = _Screen(tester);
    const view =
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), input([2]))';
    final def = _defaultApproval(_gsgClasses);
    final (dYes, nYes) = await s.both(def, view, 'yes', 'yes');
    expect(dYes, 'input(1, [2], yes)');
    expect(nYes, dYes);
    final (dNo, nNo) = await s.both(def, view, 'no', 'no');
    expect(dNo, 'input(1, [2], no)');
    expect(nNo, dNo);
  });

  testWidgets(
      'approval_sentence over joining a group: the group\'s name, a speech '
      'act, under the sentence with its signer, and the same grants as the '
      'default widget', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(alice, 5, 5, join(alice, bob, '
        'speech(utterance(friends), alice))), input([2]))';
    await s.draw(approvalSentenceName, view);
    expect(find.text('alice asks to join you.'), findsOneWidget);
    expect(find.text('friends'), findsOneWidget);
    expect(find.text('by alice'), findsOneWidget);
    expect(find.text('attempt 5, rank 5'), findsOneWidget);
    final def = _defaultApproval([..._gsgClasses, _gsnJoin]);
    final (dYes, nYes) = await s.both(def, view, 'yes', 'yes');
    expect(dYes, 'input(1, [2], yes)');
    expect(nYes, dYes);
    final (dNo, nNo) = await s.both(def, view, 'no', 'no');
    expect(nNo, dNo);
  });

  testWidgets(
      'a speech act signed again: its text, by its first signer, via each '
      'who signed it after; the person asked named you in it', (tester) async {
    final s = _Screen(tester);
    const view = 'approval(attempt(dan, 1, 1, message(dan, bob, '
        'speech(speech(speech(utterance(flood), dan), eve), bob))), '
        'input([2]))';
    await s.draw(approvalSentenceName, view);
    expect(find.text('dan asks to message you.'), findsOneWidget);
    expect(find.text('flood'), findsOneWidget);
    expect(find.text('by dan, via eve, via you'), findsOneWidget);
    final (d, n) = await s.both(
        _defaultApproval([_gsnMessage]), view, 'yes', 'yes');
    expect(n, d);
  });

  testWidgets(
      'a class naming two persons besides its initiator: each named, none '
      'you, the view not naming the agent asked', (tester) async {
    final s = _Screen(tester);
    await s.draw(approvalSentenceName,
        'approval(attempt(alf, 3, 1, befriend(alf, bob, alice)), input([2]))');
    expect(find.text('alf asks to befriend bob, alice.'), findsOneWidget);
    expect(find.textContaining('you'), findsNothing);
  });

  testWidgets(
      'a schema with an underscore reads as two words, and the initiator '
      'named in the class only once', (tester) async {
    final s = _Screen(tester);
    await s.draw(approvalSentenceName,
        'approval(attempt(bob, 4, 4, post_own(bob, alice)), input([2]))');
    expect(find.text('bob asks to post own you.'), findsOneWidget);
  });

  testWidgets(
      'a view that is no approval question of a canonical platform: named, '
      'and taking no input', (tester) async {
    final s = _Screen(tester);
    await s.draw(approvalSentenceName, 'menu(alice, [], input([3]))');
    expect(
        find.text('approval_sentence cannot draw menu(alice, [], input([3]))'),
        findsOneWidget);
    expect(find.byType(ElevatedButton), findsNothing);
    await s.draw(approvalSentenceName, 'approval(nothing, input([2]))');
    expect(find.byType(ElevatedButton), findsNothing);
    expect(s.grants, isEmpty);
  });
}
