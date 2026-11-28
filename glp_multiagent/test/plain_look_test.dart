// The plain look (lib/ui_runtime/plain_look.dart): a name of the program in
// words, a value as written, a key whole (vGLP, Remark "Construct Families";
// GLP #68, 2026-11-11: "A value the program or a person wrote must be shown as
// written", and "A key the person answers about is shown whole").
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/plain_look.dart';

import 'fixture_term.dart';

const _key =
    'ab3d68ee6df60e25adb0567238cc9e4ad2eb552b02a4316027007adb30b16f58';

/// Draw construct 1, its widget [w] and its value [v], in the standard family.
Future<void> _draw(WidgetTester tester, String w, String v) async {
  final bridge = Bridge(onGrant: (_) {});
  expect(bridge.read(fixtureTerm('draw(1, $w, $v)')), isTrue);
  final c = bridge.construct(1)!;
  await tester.pumpWidget(MaterialApp(
    home: Scaffold(
      body: ConstructView(
        key: UniqueKey(),
        construct: c,
        family: ConstructFamily.standard,
        grant: (p, r) => bridge.grant(c.id, p, r),
      ),
    ),
  ));
  await tester.pumpAndSettle();
}

void main() {
  tearDown(() => plainLook.value = false);

  test('off, every name is written as it is', () {
    plainLook.value = false;
    expect(plainWords('open_place'), 'open_place');
    expect(plainWords('NoticeContent'), 'NoticeContent');
  });

  test('on, a name is written in words', () {
    plainLook.value = true;
    expect(plainWords('open_place'), 'Open place');
    expect(plainWords('list_unlisted'), 'List unlisted');
    expect(plainWords('NoticeContent'), 'Notice content');
    expect(plainWords('trust'), 'Trust');
  });

  test('on, a key and what is not a name are written as they are', () {
    plainLook.value = true;
    expect(plainWords(_key), _key);
    expect(plainWords('Hi Bob, this is Alice'), 'Hi Bob, this is Alice');
    expect(plainWords('12'), '12');
    expect(plainWords(''), '');
  });

  testWidgets('on, a value that looks like a name is shown as written',
      (tester) async {
    plainLook.value = true;
    await _draw(tester, 'shown', 'my_file');
    expect(find.text('my_file'), findsOneWidget);
    expect(find.text('My file'), findsNothing);
  });

  testWidgets('on, a key shown is shown whole', (tester) async {
    plainLook.value = true;
    await _draw(tester, 'shown', "'$_key'");
    expect(find.text(_key), findsOneWidget);
  });
}
