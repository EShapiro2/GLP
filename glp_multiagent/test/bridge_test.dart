// The Dart bridge of vGLP (sections/elicitation.tex, subsection "The
// Implementation: Compiling vGLP onto GLP", the paragraph "The
// implementation"), end to end: a compiled module, programs/tests/vglp/
// bridge/widgets.vglp, run by the calls of its compilation ---
// `widgets # A(MCh?, Asks), widgets # dispatch(Asks?, ch(Gs?, Ds), MCh)`,
// its entries in that directory's self.glp --- in the agent host, the
// person channel's draws handed to the bridge line by line and its grants
// injected on the person's input.  Each construct is drawn as the widget its
// draw names in the grassroots app's construct family (Definition "Widget
// Declaration, Default Widget"), answered through the screen, returned as
// input(Id, P, R), received by the module's goal --- which writes got(Q, X)
// on its output to the person --- and withdrawn.
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

import 'programs_dir.dart';

/// One panel, whose one view is the default display: every screen message
/// the program writes, shown as its scalars.
const _manifest = Manifest(
  title: 'bridge',
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

/// The person at the screen of one run of an entry of the fixture.
class _Person {
  final WidgetTester tester;
  final AgentRuntime agent;
  final UiRuntime ui;

  /// Every grant the bridge returned, in order.
  final List<String> grants = [];

  final List<GTerm> _unsent = [];
  final List<String> _lines = [];
  int _fed = 0;

  _Person._(this.tester, this.agent, this.ui);

  static Future<_Person> boot(WidgetTester tester, String entry) async {
    final programs = programsDir();
    final agent = AgentRuntime(
      agentId: 'bob',
      program: '$programs/tests/vglp/bridge',
      goalLabel: '$entry/3',
      rootSelfGlpPath: '$programs/self.glp',
    );
    late final _Person person;
    final ui = UiRuntime(
        manifest: _manifest,
        onSend: (t) {
          person.grants.add(formatTerm(t));
          person._unsent.add(t);
        });
    person = _Person._(tester, agent, ui);
    agent.onOutput = person._lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};
    await tester.runAsync(() => agent.initialize());
    person._replay();
    tester.view.physicalSize = const Size(800, 3000);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);
    await tester.pumpWidget(
        MaterialApp(home: AgentSurface(agentId: 'bob', runtime: ui)));
    return person;
  }

  /// Hand the bridge what the person channel has carried since last time.
  void _replay() {
    for (; _fed < _lines.length; _fed++) {
      final l = _lines[_fed];
      if (l.startsWith('< ')) ui.handleLine(l.substring(2));
    }
  }

  /// Put the grants made on the person's input, run the agent on them, and
  /// show what it draws and writes.
  Future<void> settle() async {
    while (_unsent.isNotEmpty) {
      final g = _unsent.removeAt(0);
      await tester.runAsync(() => agent.injectUserInput(runtimeTermOf(g)));
    }
    _replay();
    await tester.pump();
  }

  /// The open constructs whose widget is [w].
  List<OpenConstruct> drawn(String w) => [
        for (final c in ui.bridge.constructs)
          if (formatTerm(c.widget) == w) c,
      ];

  /// The one open construct whose widget is [w].
  OpenConstruct the(String w) {
    final cs = drawn(w);
    expect(cs, hasLength(1), reason: 'one construct drawn as $w');
    return cs.single;
  }

  /// What the program has written to the person, beside the draws.
  List<String> get screen =>
      [for (final t in ui.store.lists['screen'] ?? const <GTerm>[]) formatTerm(t)];

  Finder inside(OpenConstruct c, Finder f) =>
      find.descendant(of: find.byKey(constructKey(c.id)), matching: f);

  Future<void> tap(Finder f) async {
    await tester.tap(f);
    await tester.pump();
    await settle();
  }

  Future<void> type(OpenConstruct c, String kind, List<int> at, String text) async {
    await tester.enterText(
        find.descendant(
            of: find.byKey(nodeKey(c.id, kind, at)),
            matching: find.byType(TextField)),
        text);
    await tester.pump();
  }

  Future<void> submit(OpenConstruct c, [List<int> at = const []]) =>
      tap(find.byKey(submitKey(c.id, at)));

  bool submittable(OpenConstruct c, [List<int> at = const []]) =>
      tester.widget<ElevatedButton>(find.byKey(submitKey(c.id, at))).onPressed !=
      null;

  /// The construct is off the screen and out of the bridge.
  void withdrawn(OpenConstruct c) {
    expect(ui.bridge.construct(c.id), isNull,
        reason: 'construct ${c.id} withdrawn');
    expect(find.byKey(constructKey(c.id)), findsNothing);
  }

  void open(OpenConstruct c) {
    expect(ui.bridge.construct(c.id), isNotNull,
        reason: 'construct ${c.id} open');
    expect(find.byKey(constructKey(c.id)), findsOneWidget);
  }
}

void main() {
  testWidgets(
      'the default widgets of the constant and primitive types: each drawn as '
      'the widget its draw names, answered, returned as input(Id, P, R), '
      'received by the module, and withdrawn', (tester) async {
    final p = await _Person.boot(tester, 'inputs_init');

    // Seven questions open at once, each the person's whole variable, [].
    expect(p.ui.bridge.constructs, hasLength(7));
    for (final c in p.ui.bridge.constructs) {
      expect(formatTerm(c.view), 'input([])');
    }
    final button = p.the('button(ok)');
    final buttons = p.the('buttons([yes, no])');
    final text = p.the('text');
    final date = p.the('date');
    final peer = p.the('peer');
    final numbers = p.drawn('number');
    expect(numbers, hasLength(2), reason: 'Integer? and Real?');

    // Each drawn as its widget: a button, a row of buttons, a text field, a
    // number field, a date picker and a peer field.
    expect(find.byKey(nodeKey(button.id, 'button', [])), findsOneWidget);
    expect(p.inside(button, find.widgetWithText(ElevatedButton, 'ok')),
        findsOneWidget);
    expect(find.byKey(nodeKey(buttons.id, 'buttons', [])), findsOneWidget);
    expect(p.inside(buttons, find.byType(ElevatedButton)), findsNWidgets(2));
    expect(find.byKey(nodeKey(text.id, 'text', [])), findsOneWidget);
    expect(find.byKey(nodeKey(date.id, 'date', [])), findsOneWidget);
    expect(p.inside(date, find.text('day 0')), findsOneWidget);
    expect(find.byKey(nodeKey(peer.id, 'peer', [])), findsOneWidget);
    for (final n in numbers) {
      expect(find.byKey(nodeKey(n.id, 'number', [])), findsOneWidget);
    }
    expect(p.screen, isEmpty);

    // A button: the tap grants its constant.
    await p.tap(p.inside(button, find.widgetWithText(ElevatedButton, 'ok')));
    expect(p.grants.last, 'input(${button.id}, [], ok)');
    expect(p.screen, contains('got(button, ok)'));
    p.withdrawn(button);

    // A row of buttons: the tap grants the one tapped.
    await p.tap(p.inside(buttons, find.widgetWithText(ElevatedButton, 'no')));
    expect(p.grants.last, 'input(${buttons.id}, [], no)');
    expect(p.screen, contains('got(buttons, no)'));
    p.withdrawn(buttons);

    // A text field: a String.
    await p.type(text, 'text', [], 'hello world');
    await p.submit(text);
    expect(p.grants.last, 'input(${text.id}, [], "hello world")');
    expect(p.screen, contains('got(text, "hello world")'));
    p.withdrawn(text);

    // A peer field: an agent identifier, and nothing else.
    await p.type(peer, 'peer', [], 'Carol Smith');
    expect(p.submittable(peer), isFalse);
    await p.type(peer, 'peer', [], 'carol');
    await p.submit(peer);
    expect(p.grants.last, 'input(${peer.id}, [], carol)');
    expect(p.screen, contains('got(peer, carol)'));
    p.withdrawn(peer);

    // A date picker: the local date, an Integer.
    for (var i = 0; i < 3; i++) {
      await tester.tap(find.byKey(ValueKey('${date.id}:date:[]:later')));
      await tester.pump();
    }
    expect(p.inside(date, find.text('day 3')), findsOneWidget);
    await p.submit(date);
    expect(p.grants.last, 'input(${date.id}, [], 3)');
    expect(p.screen, contains('got(date, 3)'));
    p.withdrawn(date);

    // Two number fields, the Integer question's and the Real question's,
    // one widget over two types.  2.5 is granted on both: the Real question
    // forms its answer, and the Integer question refuses it and stays open.
    for (final n in numbers) {
      await p.type(n, 'number', [], '2.5');
      await p.submit(n);
      expect(p.grants.last, 'input(${n.id}, [], 2.5)');
    }
    expect(p.screen, contains('got(real, 2.5)'));
    final open = [for (final n in numbers) if (p.ui.bridge.construct(n.id) != null) n];
    expect(open, hasLength(1), reason: 'the Integer question refused 2.5');
    final integer = open.single;
    p.open(integer);
    await p.type(integer, 'number', [], '7');
    await p.submit(integer);
    expect(p.grants.last, 'input(${integer.id}, [], 7)');
    expect(p.screen, contains('got(integer, 7)'));
    p.withdrawn(integer);

    // Every question answered and every construct withdrawn; the program's
    // own output shown in its view, and no draw among it.
    expect(p.ui.bridge.constructs, isEmpty);
    expect(p.screen, hasLength(7));
    expect(find.text('hello world'), findsOneWidget);
    expect(find.text('carol'), findsOneWidget);
  });

  testWidgets(
      'the default widgets of the structured types: a form, a menu of forms, '
      'a picker, a thread and an input box', (tester) async {
    final p = await _Person.boot(tester, 'forms_init');

    final form = p.the('form(pay, [peer, number])');
    final menu = p.the('menu([form(send, [peer, number]), form(post, [text])])');
    final picker = p.the('picker');
    final feed = p.the('form(feed, [thread, buttons([yes, no])])');
    final box = p.the('input_box(text)');
    expect(p.ui.bridge.constructs, hasLength(5));

    // A form: one field per argument, granted as the tuple.
    expect(find.byKey(nodeKey(form.id, 'form', [])), findsOneWidget);
    expect(find.byKey(nodeKey(form.id, 'peer', [1])), findsOneWidget);
    expect(find.byKey(nodeKey(form.id, 'number', [2])), findsOneWidget);
    expect(p.submittable(form), isFalse);
    await p.type(form, 'peer', [1], 'dana');
    await p.type(form, 'number', [2], '5');
    await p.submit(form);
    expect(p.grants.last, 'input(${form.id}, [], pay(dana, 5))');
    expect(p.screen, contains('got(form, pay(dana, 5))'));
    p.withdrawn(form);

    // A menu of forms: the alternative chosen, then its form.
    expect(find.byKey(nodeKey(menu.id, 'menu', [])), findsOneWidget);
    expect(p.inside(menu, find.text('send')), findsOneWidget);
    expect(p.inside(menu, find.text('post')), findsOneWidget);
    expect(p.submittable(menu), isFalse);
    await tester.tap(p.inside(menu, find.text('post')));
    await tester.pump();
    expect(find.byKey(nodeKey(menu.id, 'text', [1])), findsOneWidget);
    await p.type(menu, 'text', [1], 'hello');
    await p.submit(menu);
    expect(p.grants.last, 'input(${menu.id}, [], post("hello"))');
    expect(p.screen, contains('got(menu, post("hello"))'));
    p.withdrawn(menu);

    // A picker: the list the program writes, the choice at its own position.
    expect(formatTerm(picker.view), 'choose([alice, bob, carol], input([2]))');
    expect(find.byKey(nodeKey(picker.id, 'picker', [])), findsOneWidget);
    for (final peer in ['alice', 'bob', 'carol']) {
      expect(p.inside(picker, find.widgetWithText(OutlinedButton, peer)),
          findsOneWidget);
    }
    await p.tap(p.inside(picker, find.widgetWithText(OutlinedButton, 'bob')));
    expect(p.grants.last, 'input(${picker.id}, [2], bob)');
    expect(p.screen, contains('got(pick, bob)'));
    p.withdrawn(picker);

    // A thread the program writes, and the person's yes or no beside it: a
    // form whose first field is shown, element by element, and not edited.
    expect(formatTerm(feed.view), 'feed(["one", "two"], input([2]))');
    expect(find.byKey(nodeKey(feed.id, 'form', [])), findsOneWidget);
    expect(find.byKey(nodeKey(feed.id, 'thread', [1])), findsOneWidget);
    expect(find.byKey(nodeKey(feed.id, 'buttons', [2])), findsOneWidget);
    expect(p.inside(feed, find.text('one')), findsOneWidget);
    expect(p.inside(feed, find.text('two')), findsOneWidget);
    expect(p.inside(feed, find.byType(TextField)), findsNothing);
    await p.tap(p.inside(feed, find.widgetWithText(ElevatedButton, 'yes')));
    expect(p.grants.last, 'input(${feed.id}, [2], yes)');
    expect(p.screen, contains('got(feed, yes)'));
    p.withdrawn(feed);

    // An input box: the text field kept open, one line per submission, each
    // at the stream's own position.
    expect(find.byKey(nodeKey(box.id, 'input_box', [])), findsOneWidget);
    await p.type(box, 'text', [], 'first');
    await p.submit(box);
    expect(p.grants.last, 'input(${box.id}, [], "first")');
    expect(p.screen, contains('got(line, "first")'));
    p.open(box);
    expect(
        tester
            .widget<TextField>(p.inside(box, find.byType(TextField)))
            .controller!
            .text,
        isEmpty);
    await p.type(box, 'text', [], 'second');
    await p.submit(box);
    expect(p.grants.last, 'input(${box.id}, [], "second")');
    expect(p.screen, contains('got(line, "second")'));
    p.open(box);
    expect(p.ui.bridge.constructs.map((c) => c.id), [box.id]);
  });

  testWidgets(
      'two questions open at once, answered out of order: each grant reaches '
      'its own construct, and each goal its own answer', (tester) async {
    final p = await _Person.boot(tester, 'cards_init');

    final cards = p.drawn('form(card, [shown, buttons([yes, no])])');
    expect(cards, hasLength(2));
    final views = {for (final c in cards) formatTerm(c.view)};
    expect(views, {'card(alice, input([2]))', 'card(bob, input([2]))'});
    for (final c in cards) {
      // The peer the program writes, shown and not edited, and the person's
      // yes or no at [2].
      expect(find.byKey(nodeKey(c.id, 'shown', [1])), findsOneWidget);
      expect(find.byKey(nodeKey(c.id, 'buttons', [2])), findsOneWidget);
    }
    String peerOf(OpenConstruct c) =>
        formatTerm(((c.view as GStruct).args[0]));
    final first = cards[0], second = cards[1];
    final firstPeer = peerOf(first), secondPeer = peerOf(second);
    expect(p.inside(first, find.text(firstPeer)), findsOneWidget);
    expect(p.inside(second, find.text(secondPeer)), findsOneWidget);

    // The second drawn is answered first.
    await p.tap(p.inside(second, find.widgetWithText(ElevatedButton, 'no')));
    expect(p.grants.last, 'input(${second.id}, [2], no)');
    expect(p.screen, ['got($secondPeer, no)']);
    p.withdrawn(second);
    p.open(first);

    await p.tap(p.inside(first, find.widgetWithText(ElevatedButton, 'yes')));
    expect(p.grants.last, 'input(${first.id}, [2], yes)');
    expect(p.screen, ['got($secondPeer, no)', 'got($firstPeer, yes)']);
    p.withdrawn(first);

    expect(p.grants, [
      'input(${second.id}, [2], no)',
      'input(${first.id}, [2], yes)',
    ]);
    expect(p.ui.bridge.constructs, isEmpty);
  });

  group('the bridge on the person channel', () {
    late List<String> sent;
    late UiRuntime r;

    setUp(() {
      sent = [];
      r = UiRuntime(manifest: _manifest, onSend: (t) => sent.add(formatTerm(t)));
    });

    test('a draw opens a construct, a redraw replaces its view, a withdraw '
        'removes it, and the rest is the program\'s output', () {
      r.handleLine('draw(4, picker, choose([alice], input([2])))');
      r.handleLine('got(x, 1)');
      r.handleLine('draw(4, picker, choose([alice, bob], input([2])))');
      expect(r.bridge.constructs.map((c) => c.id), [4]);
      expect(formatTerm(r.bridge.construct(4)!.view),
          'choose([alice, bob], input([2]))');
      expect(r.store.lists['screen']!.map(formatTerm), ['got(x, 1)']);
      r.bridge.grant(4, const GList([GInt(2)]), const GAtom('bob'));
      expect(sent, ['input(4, [2], bob)']);
      // The construct stays until the dispatcher withdraws it.
      expect(r.bridge.construct(4), isNotNull);
      r.handleLine('withdraw(4)');
      expect(r.bridge.constructs, isEmpty);
      // A construct withdrawn takes no grant.
      r.bridge.grant(4, const GList([GInt(2)]), const GAtom('bob'));
      expect(sent, hasLength(1));
      expect(r.store.lists['screen']!.map(formatTerm), ['got(x, 1)']);
    });

    test('a draw or a withdraw naming no construct identifier is not the '
        'bridge\'s', () {
      r.handleLine('draw(alice, text, input([]))');
      r.handleLine('withdraw(req(1))');
      expect(r.bridge.constructs, isEmpty);
      expect(r.store.lists['screen']!.map(formatTerm),
          ['draw(alice, text, input([]))', 'withdraw(req(1))']);
    });

    testWidgets(
        'a declared widget is the Dart widget the family names; one the family '
        'lacks is drawn as such and takes no input', (tester) async {
      r.handleLine('draw(1, stars, input([]))');
      r.handleLine('draw(2, gauge, input([]))');
      final family = ConstructFamily(named: {
        'stars': (context, c, view, at, grant) => TextButton(
            onPressed: () => grant(questionAt(view)!, const GInt(5)),
            child: const Text('five stars')),
      });
      await tester.pumpWidget(MaterialApp(
          home: AgentSurface(agentId: 'bob', runtime: r, family: family)));
      expect(find.byKey(nodeKey(1, 'stars', [])), findsOneWidget);
      await tester.tap(find.text('five stars'));
      expect(sent, ['input(1, [], 5)']);
      expect(find.byKey(nodeKey(2, 'undrawable', [])), findsOneWidget);
      expect(find.text('No widget gauge in this construct family'),
          findsOneWidget);
      expect(
          find.descendant(
              of: find.byKey(constructKey(2)),
              matching: find.byType(ElevatedButton)),
          findsNothing);
    });

    test('a real crosses the boundary as a Real', () {
      final t = tryParseTerm('got(real, 2.5)') as GStruct;
      expect(t.args[1], isA<GReal>());
      expect((t.args[1] as GReal).value, 2.5);
      expect(tryParseTerm('-1.5e-3'), isA<GReal>());
      expect(tryParseTerm('7'), isA<GInt>());
      expect(formatTerm(const GReal(2.5)), '2.5');
    });
  });
}
