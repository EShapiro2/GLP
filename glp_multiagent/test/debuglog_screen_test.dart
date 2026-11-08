/// The screen as a debuggable build reports it (lib/debuglog/screen_text.dart
/// and app_reporting.dart; GLP/phones/docs/debugging.md, "The phone side",
/// item 3, `screen`): each construct by its identifier with the texts it
/// draws, in order, a field with what is typed into it, as the screenshot
/// harness reads a construct's texts; reported when it changes, and not when
/// it does not; and the person's act on a construct, with the texts that
/// construct showed.
library;

import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/debuglog/app_reporting.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart'
    show constructKey;
import 'package:glp_multiagent/ui_runtime/term.dart';

void main() {
  testWidgets('a screen change is reported as the texts of each construct, '
      'and an act with its construct\'s texts', (tester) async {
    final tmp = Directory.systemTemp.createTempSync('glp_debuglog_screen_');
    addTearDown(() => tmp.deleteSync(recursive: true));
    final reporter = (await tester.runAsync(() => DebugReporter.open(
          dir: Directory('${tmp.path}/debuglog'),
          password: 'p',
          base: 'http://127.0.0.1:9/log',
          period: const Duration(hours: 1),
        )))!;
    final screen = ScreenReporter(reporter);
    final surface = GlobalKey();
    final field = TextEditingController();

    await tester.pumpWidget(MaterialApp(
      home: Scaffold(
        body: KeyedSubtree(
          key: surface,
          child: Column(children: [
            KeyedSubtree(
              key: constructKey(0),
              child: const Column(children: [Text('You opened the place'), Text('Bob is nearby')]),
            ),
            KeyedSubtree(
              key: constructKey(3),
              child: Column(children: [
                const Text('Befriend'),
                TextField(controller: field),
              ]),
            ),
            const Text('Ready'),
          ]),
        ),
      ),
    ));

    List<Map<String, dynamic>> events() => [
          for (final l in File('${tmp.path}/debuglog/${reporter.run}.jsonl')
              .readAsLinesSync())
            jsonDecode(l) as Map<String, dynamic>,
        ];

    Element root() => surface.currentContext! as Element;

    screen.screen(root());
    expect(events().single['kind'], 'screen');
    expect(events().single['source'], 'screen');
    expect(events().single['constructs'], [
      {'id': 0, 'texts': ['You opened the place', 'Bob is nearby']},
      {'id': 3, 'texts': ['Befriend', '']},
      {'id': null, 'texts': ['Ready']},
    ]);

    screen.screen(root());
    expect(events(), hasLength(1), reason: 'the same screen is not reported again');

    field.text = 'Bob';
    await tester.pump();
    screen.screen(root());
    expect(events(), hasLength(2));
    expect(events().last['constructs'][1], {'id': 3, 'texts': ['Befriend', 'Bob']});

    screen.act(const GStruct('input', [GInt(3), GList([GInt(1)]), GAtom('yes')]));
    final act = events().last;
    expect(act['source'], 'screen');
    expect(act['kind'], 'act');
    expect(act['construct'], 3);
    expect(act['act'], 'input(3, [1], yes)');
    expect(act['texts'], ['Befriend', 'Bob']);

    await tester.runAsync(reporter.close);
  });
}
