// The atoms of a tab of an app compiled from a contract with pragmas
// (lib/ui_runtime/named_widgets.dart, `_listed` and `_wordedAtom`): "The tab
// lists the atoms of that predicate in the state, each with its count where
// the contract adds or deletes the predicate with a multiplicity, the view
// carrying the whole state" (vGLP 4aa7e60, sections/parts/act-schemas-local.tex,
// Section 5.4, "The contract's words").  Each view is written as a compiled
// app writes it, `view(Me, S, K, A, Ts)`, Me the agent's own key and Ts its
// tabs (vGLP 12f629d, Section 5.4), `tab(Name, P)` and, where the contract adds
// or deletes P with a multiplicity, `tab(Name, P, counted)`.
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';

import 'fixture_term.dart';

/// Draw the last view written, [views], on construct 0, with no request
/// question, outside a screen of tabs: every list, one under another.
Future<void> _draw(WidgetTester tester, String views) async {
  final (ok, state) = SocialState.read(fixtureTerm(views));
  expect(ok, isTrue);
  expect(state, isNotNull);
  await tester.pumpWidget(MaterialApp(
    home: Scaffold(
      body: SingleChildScrollView(
        child: SocialTab(id: 0, state: state, place: null),
      ),
    ),
  ));
  await tester.pumpAndSettle();
}

/// The texts drawn in the entry of the atom keyed [k], in order.
List<String> _texts(WidgetTester tester, String k) => [
      for (final t in tester.widgetList<Text>(find.descendant(
          of: find.byKey(socialKey(0, 'atom:$k')),
          matching: find.byType(Text))))
        t.data ?? '',
    ];

/// The keys of the atoms drawn, in order.
List<String> _atomKeys(WidgetTester tester) => [
      for (final w in tester.widgetList(find.byWidgetPredicate((w) =>
          w.key is ValueKey<String> &&
          (w.key as ValueKey<String>).value.startsWith('0:social:atom:'))))
        (w.key as ValueKey<String>).value.substring('0:social:atom:'.length),
    ];

void main() {
  testWidgets(
      'GB\'s Bonds tab with bond(bob, 25) and bond(bob, 28) in the state: each '
      'atom with its count', (tester) async {
    await _draw(
        tester,
        "[view(alice, [identity, bond(bob, 25), bond(bob, 25), bond(bob, 28), "
        "bond(alice, 30), bond(bob, 25)], [alice, bob], [], "
        "[tab('Bonds', bond, counted)])]");
    // Each atom once, in the order of the state.
    expect(_atomKeys(tester),
        ['bond(bob, 25)', 'bond(bob, 28)', 'bond(alice, 30)']);
    expect(_texts(tester, 'bond(bob, 25)'), ['B', 'Bob  3', '25']);
    expect(_texts(tester, 'bond(bob, 28)'), ['B', 'Bob  1', '28']);
    expect(_texts(tester, 'bond(alice, 30)'), ['A', 'Alice  1', '30']);
  });

  testWidgets(
      'GSNP\'s item(x): each atom by its speech act, with its signers',
      (tester) async {
    const flood = "speech(speech(utterance('Flood on Main Street'), dan), bob)";
    const road = "speech(utterance('Road closed'), carol)";
    await _draw(
        tester,
        "[view(alice, [identity, friend(bob), item($flood), item($road)], "
        "[bob, carol, dan], [], [tab('Items', item)])]");
    expect(_atomKeys(tester), [
      "speech(speech(utterance('Flood on Main Street'), dan), bob)",
      "speech(utterance('Road closed'), carol)",
    ]);
    // Each item by its text, and under it its signers, its author first.
    expect(_texts(tester, flood),
        ['F', 'Flood on Main Street', 'Dan, Bob']);
    expect(_texts(tester, road), ['R', 'Road closed', 'Carol']);
  });
}
