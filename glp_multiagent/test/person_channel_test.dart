// The person channel carries terms, never text.  GLP-Spec appendix-guards.tex,
// "Output to the person": send_to_user(Stream) "emits each element of Stream
// to the person operating the agent" --- a term, not a line.  The person's
// bridge therefore takes the term the '_output' kernel was given, carried as
// the isolate protocol carries every term (isolate_protocol.dart,
// screenTermOf and AgentNotify), and parses no text (GLP #3 Cowork,
// 2026-10-11 12:59 UTC, "21:40 AND 03:16, (iii)"); vGLP, sections/
// elicitation.tex, the paragraph "The implementation": the bridge draws each
// construct's widget, and "everything crossing to Dart is a ground term or a
// construct identifier".
//
// Until 2026-10-11 the agent printed each element and the screen parsed the
// line: a string with a space, `Hi there`, and a constant the print left
// bare, `form(., ...)`, did not parse, and never arrived.
import 'dart:isolate';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'fixture_term.dart';
import 'programs_dir.dart';

/// One panel, whose one view is the default display: every screen message
/// the program writes, shown as its scalars.
const _manifest = Manifest(
  title: 'person channel',
  panels: [
    Panel(id: 'screen', name: 'screen', views: [
      ScreenView(
          pattern: 'S',
          content: 'S',
          kind: ViewKind.list,
          label: 'Screen',
          store: 'screen'),
    ]),
  ],
  activity: [],
);

void main() {
  group('the screen\'s term of what the agent emits', () {
    test('a string is the string of its characters, a space or a capital in '
        'it, and comes back as the runtime\'s one string', () {
      for (final s in ['Hi there', 'G', 'it\'s', '42', 'a', '.']) {
        final g = screenTermOf(rt.ConstTerm(s));
        expect(g, isA<GAtom>(), reason: s);
        expect((g as GAtom).name, s);
        expect((runtimeTermOf(g) as rt.ConstTerm).value, s);
      }
    });

    test('numbers, the empty list, lists and structures, whole', () {
      expect((screenTermOf(rt.ConstTerm(7)) as GInt).value, 7);
      expect((screenTermOf(rt.ConstTerm(2.5)) as GReal).value, 2.5);
      expect((screenTermOf(rt.ConstTerm(rt.nil)) as GList).items, isEmpty);
      final said = rt.StructTerm('said', [
        rt.ConstTerm('Hi there'),
        rt.StructTerm('.', [
          rt.ConstTerm(1),
          rt.StructTerm('.', [rt.ConstTerm('b c'), rt.ConstTerm(rt.nil)]),
        ]),
      ]);
      expect(formatTerm(screenTermOf(said)), "said('Hi there', [1, 'b c'])");
      // A list cell ending elsewhere is the structure '.'(H, T).
      final cell = screenTermOf(rt.StructTerm('.', [
        rt.ConstTerm('a'),
        rt.StructTerm('.', [rt.ConstTerm('b'), rt.ConstTerm('t')]),
      ]));
      expect(cell, isA<GStruct>());
      expect((cell as GStruct).functor, '.');
      expect(formatTerm(cell), '[a, b | t]');
    });

    test('what is no ground term of the screen\'s is refused, not shown', () {
      expect(() => screenTermOf(rt.ModuleTerm(Object(), name: 'm')),
          throwsArgumentError);
    });
  });

  group('the canonical print of a screen term', () {
    test('a string single-quoted where it would not read bare, never in '
        'double quotes; an atom and a GString of one text alike', () {
      expect(formatTerm(const GAtom('hello')), 'hello');
      expect(formatTerm(const GString('hello')), 'hello');
      expect(formatTerm(const GAtom('Hi there')), "'Hi there'");
      expect(formatTerm(const GString('Hi there')), "'Hi there'");
      expect(formatTerm(const GAtom('G')), "'G'");
      expect(formatTerm(const GStruct('form', [GAtom('.'), GList([])])),
          "form('.', [])");
      expect(formatTerm(const GStruct('.', [GAtom('a'), GList([GAtom('b')])])),
          '[a, b]');
    });

    test('a stored key names its term, which the store holds and the surface '
        'shows, never reading the key back', () {
      final r = UiRuntime(manifest: _manifest, onSend: (_) {});
      final group = fixtureTerm("group_id(alice, 'Book club')");
      final key = r.store.keyOf(group);
      expect(key, "group_id(alice, 'Book club')");
      expect(identical(r.store.termOf(key), group), isTrue);
      expect(r.store.displayKey(key), 'alice Book club');
    });
  });

  test('a person\'s text with a space reaches the agent across the isolate '
      'boundary and comes back to the screen whole', () async {
    final programs = programsDir();
    final reply = ReceivePort();
    SendPort? commands;
    final notifies = <GTerm>[];
    final ui = UiRuntime(
        manifest: _manifest,
        onSend: (g) => commands?.send(UserInput(g)));
    reply.listen((m) {
      if (m is AgentReady) {
        commands = m.commandPort;
      } else if (m is AgentNotify) {
        notifies.add(m.term);
        ui.handle(m.term);
      } else if (m is AgentError) {
        fail('the agent: ${m.error}');
      }
    });
    final isolate = await Isolate.spawn(
        agentIsolateEntry,
        InitAgent(
          agentId: 'bob',
          program: '$programs/tests/vglp/bridge',
          goalLabel: 'inputs_init/3',
          rootSelfGlpPath: '$programs/self.glp',
          replyPort: reply.sendPort,
        ));
    addTearDown(() {
      isolate.kill(priority: Isolate.immediate);
      reply.close();
    });

    Future<bool> until(bool Function() done) async {
      final end = DateTime.now().add(const Duration(seconds: 20));
      while (DateTime.now().isBefore(end)) {
        if (done()) return true;
        await Future<void>.delayed(const Duration(milliseconds: 50));
      }
      return done();
    }

    // The fixture's seven questions are drawn, the text field among them.
    expect(
        await until(() =>
            commands != null &&
            ui.bridge.constructs.any((c) => formatTerm(c.widget) == 'text')),
        isTrue,
        reason: 'no text field drawn: ${notifies.map(formatTerm).toList()}');
    final text =
        ui.bridge.constructs.firstWhere((c) => formatTerm(c.widget) == 'text');

    // The person types `Hi there` and submits it.
    ui.bridge.grant(text.id, const GList([]), const GString('Hi there'));

    expect(
        await until(() =>
            notifies.any((t) => formatTerm(t) == "got(text, 'Hi there')")),
        isTrue,
        reason: 'got(text, ...) never arrived: '
            '${notifies.map(formatTerm).toList()}');
    final got =
        notifies.firstWhere((t) => formatTerm(t) == "got(text, 'Hi there')")
            as GStruct;
    expect(got.args[1], isA<GAtom>());
    expect((got.args[1] as GAtom).name, 'Hi there');
    expect(ui.store.lists['screen']!.map(formatTerm),
        contains("got(text, 'Hi there')"));
    expect(displayText(got), 'text Hi there');
  });

  testWidgets('a draw with \'.\' reaches the bridge whole and draws: a list '
      'cell the program writes, a question in it', (tester) async {
    final programs = programsDir();
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    final sent = <String>[];
    final ui = UiRuntime(
        manifest: _manifest, onSend: (g) => sent.add(formatTerm(g)));
    engine.runtime.outputTermCallback = (t) => ui.handle(screenTermOf(t));
    expect(
        engine.loadSource('''
procedure test.
test :- send_to_user([
    draw(1, form('.', [buttons([yes, no]), shown]), [input([1])]),
    draw(2, form('.', [text, shown]), [input([1]) | done])]).
''', filename: 'draws.glp'),
        isTrue);
    final result = (await tester.runAsync(() => engine.runGoal('test')))!;
    expect(result.succeeded, isTrue, reason: '${result.error}');

    // Both arrived whole, the functor '.' as written.
    expect(ui.bridge.constructs.map((c) => formatTerm(c.widget)), [
      "form('.', [buttons([yes, no]), shown])",
      "form('.', [text, shown])",
    ]);
    expect(formatTerm(ui.bridge.construct(1)!.view), '[input([1])]');
    expect(formatTerm(ui.bridge.construct(2)!.view), '[input([1]) | done]');

    tester.view.physicalSize = const Size(800, 1600);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    await tester.pumpWidget(
        MaterialApp(home: AgentSurface(agentId: 'bob', runtime: ui)));

    // Each drawn as the form of the list cell: the head a question, the tail
    // shown, and nothing undrawable.
    expect(find.byKey(nodeKey(1, 'form', [])), findsOneWidget);
    expect(find.byKey(nodeKey(1, 'buttons', [1])), findsOneWidget);
    expect(find.byKey(nodeKey(1, 'shown', [2])), findsOneWidget);
    expect(find.byKey(nodeKey(2, 'form', [])), findsOneWidget);
    expect(find.byKey(nodeKey(2, 'text', [1])), findsOneWidget);
    expect(find.byKey(nodeKey(2, 'shown', [2])), findsOneWidget);
    expect(find.byKey(nodeKey(1, 'undrawable', [])), findsNothing);
    expect(find.byKey(nodeKey(2, 'undrawable', [])), findsNothing);

    // The person answers the head of the first: the grant at its position.
    await tester.tap(find.descendant(
        of: find.byKey(constructKey(1)),
        matching: find.widgetWithText(ElevatedButton, 'yes')));
    await tester.pump();
    expect(sent, ['input(1, [1], yes)']);
  });
}
