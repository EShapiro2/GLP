// The rows of a tab of an app compiled from a contract with pragmas
// (lib/ui_runtime/named_widgets.dart, `_rows` and `_wordedRow`): "The tab
// lists the atoms of that predicate in the state, one row per item, an atom's
// item being its first argument; a row shows the atoms of the state naming
// its item, with its count where the contract adds or deletes the predicate
// with a multiplicity, the view carrying the whole state" (vGLP 5b4859a,
// sections/parts/act-schemas-local.tex:5, Section 5.4, "The contract's
// words"; GLP #11 Cowork, Code_inbox.md, 2026-10-31 14:21 UTC).  Each view is
// written as a compiled app writes it, `view(S, K, A, Ts)`, Ts its tabs,
// `tab(Name, P)` and, where the contract adds or deletes P with a
// multiplicity, `tab(Name, P, counted)`.
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

/// The texts drawn in the row keyed [k], in order.
List<String> _texts(WidgetTester tester, String k) => [
      for (final t in tester.widgetList<Text>(find.descendant(
          of: find.byKey(socialKey(0, 'row:$k')),
          matching: find.byType(Text))))
        t.data ?? '',
    ];

/// The keys of the rows drawn, in order.
List<String> _rowKeys(WidgetTester tester) => [
      for (final w in tester.widgetList(find.byWidgetPredicate((w) =>
          w.key is ValueKey<String> &&
          (w.key as ValueKey<String>).value.startsWith('0:social:row:'))))
        (w.key as ValueKey<String>).value.substring('0:social:row:'.length),
    ];

void main() {
  testWidgets(
      'GB\'s Bonds tab with bond(bob, 25) and bond(bob, 28) in the state: one '
      'row, bob, showing both atoms, each with its count', (tester) async {
    await _draw(
        tester,
        "[view([identity, bond(bob, 25), bond(bob, 25), bond(bob, 28), "
        "bond(alice, 30), bond(bob, 25)], [alice, bob], [], "
        "[tab('Bonds', bond, counted)])]");
    // One row per item, in the order the items first appear in the state.
    expect(_rowKeys(tester), ['bob', 'alice']);
    // The row of bob carries both of its atoms, each by its maturity with the count of its copies.
    expect(_texts(tester, 'bob'), ['B', 'Bob', '25  3', '28  1']);
    // A row of one atom: its count beside the item, as before.
    expect(_texts(tester, 'alice'), ['A', 'Alice  1', '30']);
  });

  testWidgets(
      'GSNP\'s item(x): a row per item, x the first argument, as before',
      (tester) async {
    const flood = "speech(speech(utterance('Flood on Main Street'), dan), bob)";
    const road = "speech(utterance('Road closed'), carol)";
    await _draw(
        tester,
        "[view([identity, friend(bob), item($flood), item($road)], "
        "[bob, carol, dan], [], [tab('Items', item)])]");
    expect(_rowKeys(tester), [
      "speech(speech(utterance('Flood on Main Street'), dan), bob)",
      "speech(utterance('Road closed'), carol)",
    ]);
    // Each item by its text, and under it its signers, its author first.
    expect(_texts(tester, flood),
        ['F', 'Flood on Main Street', 'Dan, Bob']);
    expect(_texts(tester, road), ['R', 'Road closed', 'Carol']);
  });
}
