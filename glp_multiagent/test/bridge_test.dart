// The Dart bridge of vGLP (sections/elicitation.tex, subsection "The
// Implementation: Compiling vGLP onto GLP", the paragraph "The
// implementation"), end to end: a compiled module, programs/tests/vglp/
// bridge/widgets.vglp, run by the calls of its compilation ---
// `widgets # A(MCh?, Asks), widgets # dispatch(Asks?, ch(Gs?, Ds), MCh)`,
// its entries in that directory's self.glp --- in the agent host, the
// person channel's draws handed to the bridge term by term, whole, as the
// agent emits them (AgentRuntime.onUserOutput; the bridge parses no text),
// and its grants injected on the person's input.  Each construct is drawn as the widget its
// draw names in the grassroots app's construct family (Definition "Widget
// Declaration, Default Widget"), answered through the screen, returned as
// input(Id, P, R), received by the module's goal --- which writes got(Q, X)
// on its output to the person --- and withdrawn.
import 'dart:io';

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

import 'fixture_term.dart';
import 'programs_dir.dart';

/// One panel, a compiled vGLP program's: the constructs the bridge holds
/// open, the program's output to its person among them as the thread of
/// construct 0 (vGLP 169b008, Definition "Canonical Compilation"; vGLP,
/// 2026-10-13 19:25 UTC, items 2 and 3); and one view, of whatever else
/// reaches the screen that is no draw of the bridge's, which a compiled
/// program sends none of.
const _manifest = Manifest(
  title: 'bridge',
  panels: [
    Panel(id: 'screen', name: 'screen', constructs: true, views: [
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

  /// Every term the person channel carried, as the agent emitted it.
  final List<GTerm> _carried = [];
  int _fed = 0;

  _Person._(this.tester, this.agent, this.ui);

  /// Boot [entry] of the fixture in programs/tests/vglp/[dir].
  static Future<_Person> boot(WidgetTester tester, String entry,
      {String dir = 'bridge'}) async {
    final programs = programsDir();
    final agent = AgentRuntime(
      agentId: 'bob',
      program: '$programs/tests/vglp/$dir',
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
    agent.onUserOutput = (t) => person._carried.add(screenTermOf(t));
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
    for (; _fed < _carried.length; _fed++) {
      ui.handle(_carried[_fed]);
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

  /// What the program has written to the person: the elements of the output
  /// thread, construct 0, drawn by the dispatcher.
  List<String> get screen {
    final v = ui.bridge.construct(0)?.view;
    return [if (v is GList) for (final t in v.items) formatTerm(t)];
  }

  /// The constructs of the program's questions: every open construct but the
  /// output thread, construct 0.
  List<OpenConstruct> get questions =>
      [for (final c in ui.bridge.constructs) if (c.id != 0) c];

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

  /// Send the program the person's word [t], a grant that is not input(Id,
  /// P, R), which the dispatcher passes to the program on its own end of the
  /// person channel (programs/vglp/dispatcher.glp, split_grants/3).
  Future<void> say(GTerm t) async {
    _unsent.add(t);
    await settle();
  }

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
    expect(p.questions, hasLength(7));
    for (final c in p.questions) {
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

    // A text field: a String.  The person's text, a space in it, reaches
    // the program and comes back whole, a term, its print the canonical one
    // (until 2026-10-11 it came back as a line the screen could not read,
    // and never arrived).
    await p.type(text, 'text', [], 'hello world');
    await p.submit(text);
    expect(p.grants.last, "input(${text.id}, [], 'hello world')");
    expect(p.screen, contains("got(text, 'hello world')"));
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
    expect(p.questions, isEmpty);
    expect(p.screen, hasLength(7));
    expect(find.text('hello world'), findsOneWidget);
    expect(find.text('carol'), findsOneWidget);
  });

  testWidgets(
      'the default widgets of the structured types: a form, a menu of forms, '
      'a picker, a thread and an input box', (tester) async {
    final p = await _Person.boot(tester, 'forms_init');

    final form =
        p.the("form(pay, [field('Peer', peer), field('Integer', number)])");
    final menu = p.the("menu([form(send, [field('Peer', peer), field('Integer', "
        "number)]), form(post, [field('String', text)])])");
    final picker = p.the('picker');
    final feed = p.the("form(feed, [field('Stream(String)', thread), "
        "field('YesNo', buttons([yes, no]))])");
    final box = p.the('input_box(text)');
    expect(p.questions, hasLength(5));

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
    expect(p.grants.last, 'input(${menu.id}, [], post(hello))');
    expect(p.screen, contains('got(menu, post(hello))'));
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
    expect(formatTerm(feed.view), 'feed([one, two], input([2]))');
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
    expect(p.grants.last, 'input(${box.id}, [], first)');
    expect(p.screen, contains('got(line, first)'));
    p.open(box);
    expect(
        tester
            .widget<TextField>(p.inside(box, find.byType(TextField)))
            .controller!
            .text,
        isEmpty);
    await p.type(box, 'text', [], 'second');
    await p.submit(box);
    expect(p.grants.last, 'input(${box.id}, [], second)');
    expect(p.screen, contains('got(line, second)'));
    p.open(box);
    expect(p.questions.map((c) => c.id), [box.id]);
  });

  testWidgets(
      'two questions open at once, answered out of order: each grant reaches '
      'its own construct, and each goal its own answer', (tester) async {
    final p = await _Person.boot(tester, 'cards_init');

    final cards = p.drawn(
        "form(card, [field('Peer', shown), field('YesNo', buttons([yes, no]))])");
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
    expect(p.questions, isEmpty);
  });

  // vGLP #8 Cowork, 2026-10-11 15:47 UTC, item 2, Task D: a stream or a list
  // the program writes whose elements hold questions.  "Present hands the
  // person each element with the writers inside it (Definition "vmaGLP
  // Transition System"), each such writer a question (Definition
  // "Question"), drawn by Definition "Widget Declaration" (a stream the
  // program writes: a list or a thread; a question by the widget of its
  // type).  Draw each element as it is presented and take the answers to its
  // questions as any grant."  The fixture is programs/tests/vglp/cards.
  group('a stream or a list the program writes whose elements hold questions',
      () {
    const card = "form(card, [field('Integer', shown), "
        "field('YesNo', buttons([yes, no]))])";

    /// The button [label] of the question at [at] of construct [c].
    Finder button(OpenConstruct c, List<int> at, String label) =>
        find.descendant(
            of: find.byKey(nodeKey(c.id, 'buttons', at)),
            matching: find.widgetWithText(ElevatedButton, label));

    testWidgets(
        'a panel of two cards, a list the program writes: one construct, the '
        'list a thread of cards, each drawn by its widget at its own '
        'position, answered out of order, and the program receiving both '
        'answers', (tester) async {
      final p = await _Person.boot(tester, 'panel_init', dir: 'cards');
      final panel =
          p.the("form(panel, [field('List(Card)', thread($card))])");
      expect(p.questions, hasLength(1));
      // The two cards presented at once, the second while the first's
      // question is open: the k-th card's answer at the path through the
      // list cells, [1, 1, 2] and [1, 2, 1, 2].
      expect(formatTerm(panel.view),
          'panel([card(1, input([1, 1, 2])), card(2, input([1, 2, 1, 2]))])');
      expect(find.byKey(nodeKey(panel.id, 'thread', [1])), findsOneWidget);
      for (final (n, at) in [
        ('1', [1, 1]),
        ('2', [1, 2, 1])
      ]) {
        expect(find.byKey(nodeKey(panel.id, 'form', at)), findsOneWidget);
        final shown = find.byKey(nodeKey(panel.id, 'shown', [...at, 1]));
        expect(find.descendant(of: shown, matching: find.text(n)),
            findsOneWidget);
        expect(find.byKey(nodeKey(panel.id, 'buttons', [...at, 2])),
            findsOneWidget);
      }
      expect(p.screen, isEmpty);

      // The second card answered first: its grant reaches its question, and
      // the program its answer; the panel stays, the first card open.
      await p.tap(button(panel, [1, 2, 1, 2], 'no'));
      expect(p.grants.last, 'input(${panel.id}, [1, 2, 1, 2], no)');
      expect(p.screen, ['got(2, no)']);
      p.open(panel);

      await p.tap(button(panel, [1, 1, 2], 'yes'));
      expect(p.grants.last, 'input(${panel.id}, [1, 1, 2], yes)');
      expect(p.screen, ['got(2, no)', 'got(1, yes)']);
      // Every question answered and the list closed: withdrawn.
      p.withdrawn(panel);
      expect(p.grants, [
        'input(${panel.id}, [1, 2, 1, 2], no)',
        'input(${panel.id}, [1, 1, 2], yes)',
      ]);
    });

    testWidgets(
        'a stream of cards the program writes: its third card drawn as the '
        'program writes it, once the first two are answered, and answered '
        'as they were', (tester) async {
      final p = await _Person.boot(tester, 'deal_init', dir: 'cards');
      final deal = p.the('thread($card)');
      expect(formatTerm(deal.view),
          '[card(1, input([1, 2])), card(2, input([2, 1, 2]))]');
      expect(find.byKey(nodeKey(deal.id, 'buttons', [2, 2, 1, 2])),
          findsNothing);

      // One answered: the program waits for the other, and writes nothing.
      await p.tap(button(deal, [1, 2], 'yes'));
      expect(p.grants.last, 'input(${deal.id}, [1, 2], yes)');
      expect(p.screen, isEmpty);
      expect(formatTerm(p.ui.bridge.construct(deal.id)!.view),
          '[card(1, input([1, 2])), card(2, input([2, 1, 2]))]');

      // Both answered: the program receives them and writes the third card,
      // drawn under the first two at its own position.
      await p.tap(button(deal, [2, 1, 2], 'yes'));
      expect(p.grants.last, 'input(${deal.id}, [2, 1, 2], yes)');
      expect(p.screen, ['got(1, yes)', 'got(2, yes)']);
      p.open(deal);
      expect(
          formatTerm(p.ui.bridge.construct(deal.id)!.view),
          '[card(1, input([1, 2])), card(2, input([2, 1, 2])), '
          'card(3, input([2, 2, 1, 2]))]');
      final third = find.byKey(nodeKey(deal.id, 'shown', [2, 2, 1, 1]));
      expect(find.descendant(of: third, matching: find.text('3')),
          findsOneWidget);

      await p.tap(button(deal, [2, 2, 1, 2], 'no'));
      expect(p.grants.last, 'input(${deal.id}, [2, 2, 1, 2], no)');
      expect(p.screen, ['got(1, yes)', 'got(2, yes)', 'got(3, no)']);
      // The stream closed and every question answered: withdrawn.
      p.withdrawn(deal);
    });

    testWidgets(
        'a stream the program writes whose elements are themselves '
        'questions: each element drawn by its type\'s widget at its own '
        'position', (tester) async {
      final p = await _Person.boot(tester, 'votes_init', dir: 'cards');
      final votes = p.the('thread(buttons([yes, no]))');
      expect(formatTerm(votes.view), '[input([1]), input([2, 1])]');
      await p.tap(button(votes, [2, 1], 'no'));
      expect(p.grants.last, 'input(${votes.id}, [2, 1], no)');
      expect(p.screen, ['got(2, no)']);
      p.open(votes);
      await p.tap(button(votes, [1], 'yes'));
      expect(p.grants.last, 'input(${votes.id}, [1], yes)');
      expect(p.screen, ['got(2, no)', 'got(1, yes)']);
      p.withdrawn(votes);
    });
  });

  // The pending drawing (vGLP #8 Cowork, 2026-10-11 20:48 UTC, "19:09 (D,
  // Q1)", and 22:51 UTC, Udi approved; vGLP 118e04d, Definition "Construct
  // Process": "each widget position the program has not yet written whole
  // --- the root, a field, an element of any stream, the thread's included
  // --- drawn as pending, and a stream's unwritten tail not drawn, so that
  // the questions beside it are offered at once").  The fixture is
  // programs/tests/vglp/pending: the program writes what it leaves unwritten
  // from the person's word, peer(P) or word(W).
  group('a position the program has not yet written whole is drawn pending',
      () {
    const card = "form(card, [field('Peer', shown), "
        "field('YesNo', buttons([yes, no]))])";

    /// The button [label] of the question at [at] of construct [c].
    Finder button(OpenConstruct c, List<int> at, String label) =>
        find.descendant(
            of: find.byKey(nodeKey(c.id, 'buttons', at)),
            matching: find.widgetWithText(ElevatedButton, label));

    /// The text [t] inside the [kind] widget at [at] of construct [c].
    Finder text(OpenConstruct c, String kind, List<int> at, String t) =>
        find.descendant(
            of: find.byKey(nodeKey(c.id, kind, at)), matching: find.text(t));

    testWidgets(
        'a card whose Peer the program writes after the ask: the Peer drawn '
        'pending, the question beside it offered at once, and the card drawn '
        'again once the Peer is written', (tester) async {
      final p = await _Person.boot(tester, 'late_init', dir: 'pending');
      final c = p.the(card);
      expect(formatTerm(c.view), 'card(pending([1]), input([2]))');
      expect(find.byKey(nodeKey(c.id, 'pending', [1])), findsOneWidget);
      expect(find.byKey(nodeKey(c.id, 'shown', [1])), findsNothing);
      expect(button(c, [2], 'yes'), findsOneWidget);

      await p.say(const GStruct('peer', [GAtom('bob')]));
      expect(formatTerm(p.ui.bridge.construct(c.id)!.view),
          'card(bob, input([2]))');
      expect(find.byKey(nodeKey(c.id, 'pending', [1])), findsNothing);
      expect(text(c, 'shown', [1], 'bob'), findsOneWidget);

      await p.tap(button(c, [2], 'yes'));
      expect(p.grants.last, 'input(${c.id}, [2], yes)');
      expect(p.screen, ['got(0, yes)']);
      p.withdrawn(c);
    });

    testWidgets(
        'the same card answered while its Peer is still pending: the program '
        'receives the answer, and the construct withdraws', (tester) async {
      final p = await _Person.boot(tester, 'late_init', dir: 'pending');
      final c = p.the(card);
      expect(find.byKey(nodeKey(c.id, 'pending', [1])), findsOneWidget);
      await p.tap(button(c, [2], 'no'));
      expect(p.grants, ['input(${c.id}, [2], no)']);
      expect(p.screen, ['got(0, no)']);
      p.withdrawn(c);
    });

    testWidgets(
        'a stream partly written: its first card pending, drawn by its '
        'type\'s widget, its second\'s Peer pending, its third whole, its tail '
        'not drawn; the third answered first and the second while its Peer '
        'is pending; each card drawn again as it is written', (tester) async {
      final p = await _Person.boot(tester, 'deal_init', dir: 'pending');
      final d = p.the('thread($card)');
      expect(
          formatTerm(d.view),
          '[pending([1]), card(pending([2, 1, 1]), input([2, 1, 2])), '
          'card(bob, input([2, 2, 1, 2]))]');
      // The first card pending at its own position, drawn by the card's
      // widget, a form, as a placeholder naming it; the second's Peer
      // pending beside its question.
      expect(text(d, 'pending', [1], 'card \u2026'), findsOneWidget);
      expect(find.byKey(nodeKey(d.id, 'pending', [2, 1, 1])), findsOneWidget);
      expect(button(d, [1, 2], 'yes'), findsNothing);

      // The third answered first: its grant passes the pending first card
      // and the second card's question.
      await p.tap(button(d, [2, 2, 1, 2], 'yes'));
      expect(p.screen, ['got(3, yes)']);
      expect(
          formatTerm(p.ui.bridge.construct(d.id)!.view),
          '[card(carol, input([1, 2])), card(pending([2, 1, 1]), '
          'input([2, 1, 2])), card(bob, input([2, 2, 1, 2]))]');
      expect(find.byKey(nodeKey(d.id, 'pending', [1])), findsNothing);
      expect(text(d, 'shown', [1, 1], 'carol'), findsOneWidget);

      // The second answered while its Peer is pending; then the Peer
      // written, and the card drawn again.
      await p.tap(button(d, [2, 1, 2], 'no'));
      expect(p.screen, ['got(3, yes)', 'got(2, no)']);
      await p.say(const GStruct('peer', [GAtom('dave')]));
      expect(text(d, 'shown', [2, 1, 1], 'dave'), findsOneWidget);
      expect(find.byKey(nodeKey(d.id, 'pending', [2, 1, 1])), findsNothing);

      await p.tap(button(d, [1, 2], 'yes'));
      expect(p.screen, ['got(3, yes)', 'got(2, no)', 'got(1, yes)']);
      // Every card answered and the stream closed: withdrawn.
      p.withdrawn(d);
      expect(p.grants, [
        'input(${d.id}, [2, 2, 1, 2], yes)',
        'input(${d.id}, [2, 1, 2], no)',
        'input(${d.id}, [1, 2], yes)',
      ]);
    });

    testWidgets(
        'a thread element written before its argument is bound: drawn '
        'pending at its own position beside the question, then whole',
        (tester) async {
      final p = await _Person.boot(tester, 'board_init', dir: 'pending');
      // Each note a tuple the program writes, a form labelled by its
      // argument's declared type (vGLP #8 Cowork, 2026-10-13 08:15 UTC).
      final b = p.the("form(board, [field('Stream(Note)', "
          "thread(form(note, [field('Constant', shown)]))), "
          "field('YesNo', buttons([yes, no]))])");
      expect(formatTerm(b.view), 'board([note(first), pending([1, 2, 1])], '
          'input([2]))');
      expect(text(b, 'thread', [1], 'first'), findsOneWidget);
      expect(find.byKey(nodeKey(b.id, 'pending', [1, 2, 1])), findsOneWidget);
      expect(button(b, [2], 'yes'), findsOneWidget);

      await p.say(const GStruct('word', [GAtom('second')]));
      expect(formatTerm(p.ui.bridge.construct(b.id)!.view),
          'board([note(first), note(second)], input([2]))');
      expect(find.byKey(nodeKey(b.id, 'pending', [1, 2, 1])), findsNothing);
      expect(text(b, 'thread', [1], 'second'), findsOneWidget);

      await p.tap(button(b, [2], 'yes'));
      expect(p.screen, ['got(0, yes)']);
      p.withdrawn(b);
    });
  });

  // GLP, 2026-10-11 12:59 UTC, "03:24": the one-clause fixture,
  // programs/tests/vglp/one_clause, restated in vGLP's syntax --- three
  // volitional procedures, each with one clause, its question in writer mode,
  // the offering peer shown --- runs through the dispatcher and the bridge.
  // Its entry asks all three at once of three peers, and puts the decisions
  // on the program's own output to the person, which the dispatcher merges
  // with the draws.
  testWidgets(
      'the one-clause program: its three cards drawn at once, the offering '
      'peer shown and the person\'s position marked, each answered through '
      'the screen, its decision reaching the program, and withdrawn',
      (tester) async {
    final p = await _Person.boot(tester, 'responder_init', dir: 'one_clause');
    final card = p.the("form(card, [field('Peer', shown), "
        "field('YesNo', buttons([yes, no]))])");
    final greeting = p.the("form(greeting, [field('Peer', shown), "
        "field('Name', text)])");
    final choice = p.the("form(choice, [field('Peer', shown), "
        "field('Constant', text)])");
    expect(p.questions, hasLength(3));
    expect(formatTerm(card.view), 'card(bob, input([2]))');
    expect(formatTerm(greeting.view), 'greeting(carol, input([2]))');
    expect(formatTerm(choice.view), 'choice(dana, input([2]))');
    for (final (c, peer) in [(card, 'bob'), (greeting, 'carol'), (choice, 'dana')]) {
      final shown = find.byKey(nodeKey(c.id, 'shown', [1]));
      expect(find.descendant(of: shown, matching: find.text(peer)),
          findsOneWidget, reason: 'the offering peer shown before any input');
    }
    expect(p.screen, isEmpty);

    // respond's card: the tap grants no, an answer its clause takes.
    await p.tap(find.descendant(
        of: find.byKey(nodeKey(card.id, 'buttons', [2])),
        matching: find.widgetWithText(ElevatedButton, 'no')));
    expect(p.grants.last, 'input(${card.id}, [2], no)');
    expect(p.screen, contains('decided(no, bob)'));
    p.withdrawn(card);

    // greet's card: the person gives a name.
    await p.type(greeting, 'text', [2], 'alice');
    await p.submit(greeting, [2]);
    expect(p.grants.last, 'input(${greeting.id}, [2], alice)');
    expect(p.screen, contains('greeted(alice, carol)'));
    p.withdrawn(greeting);

    // pick's card: the person gives a choice.
    await p.type(choice, 'text', [2], 'go');
    await p.submit(choice, [2]);
    expect(p.grants.last, 'input(${choice.id}, [2], go)');
    expect(p.screen, contains('picked(go)'));
    p.withdrawn(choice);

    expect(p.questions, isEmpty);
    expect(p.screen, hasLength(3));
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
      r.handle(fixtureTerm('draw(4, picker, choose([alice], input([2])))'));
      r.handle(fixtureTerm('got(x, 1)'));
      r.handle(fixtureTerm('draw(4, picker, choose([alice, bob], input([2])))'));
      expect(r.bridge.constructs.map((c) => c.id), [4]);
      expect(formatTerm(r.bridge.construct(4)!.view),
          'choose([alice, bob], input([2]))');
      expect(r.store.lists['screen']!.map(formatTerm), ['got(x, 1)']);
      r.bridge.grant(4, const GList([GInt(2)]), const GAtom('bob'));
      expect(sent, ['input(4, [2], bob)']);
      // The construct stays until the dispatcher withdraws it.
      expect(r.bridge.construct(4), isNotNull);
      r.handle(fixtureTerm('withdraw(4)'));
      expect(r.bridge.constructs, isEmpty);
      // A construct withdrawn takes no grant.
      r.bridge.grant(4, const GList([GInt(2)]), const GAtom('bob'));
      expect(sent, hasLength(1));
      expect(r.store.lists['screen']!.map(formatTerm), ['got(x, 1)']);
    });

    test('a drawn value under its ask\'s functor, or construct 0\'s under '
        'output, is drawn as the view under it (vGLP, Definition "Construct '
        'Process")', () {
      r.handle(fixtureTerm('draw(4, picker, choice_w(choose([alice], '
          'input([2]))))'));
      expect(formatTerm(r.bridge.construct(4)!.view),
          'choose([alice], input([2]))');
      r.handle(fixtureTerm('draw(5, input_box(text), '
          'stream_string_r(input([])))'));
      expect(formatTerm(r.bridge.construct(5)!.view), 'input([])');
      r.handle(fixtureTerm('draw(0, thread, output([opened(bob)]))'));
      expect(formatTerm(r.bridge.construct(0)!.view), '[opened(bob)]');
      // A value under no ask's functor is the view itself.
      r.handle(fixtureTerm('draw(6, text, input([]))'));
      expect(formatTerm(r.bridge.construct(6)!.view), 'input([])');
    });

    test('a draw or a withdraw naming no construct identifier is not the '
        'bridge\'s', () {
      r.handle(fixtureTerm('draw(alice, text, input([]))'));
      r.handle(fixtureTerm('withdraw(req(1))'));
      expect(r.bridge.constructs, isEmpty);
      expect(r.store.lists['screen']!.map(formatTerm),
          ['draw(alice, text, input([]))', 'withdraw(req(1))']);
    });

    testWidgets(
        'a declared widget is the Dart widget the family names; one the family '
        'lacks is drawn as such and takes no input', (tester) async {
      r.handle(fixtureTerm('draw(1, stars, input([]))'));
      r.handle(fixtureTerm('draw(2, gauge, input([]))'));
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

    // vGLP #6 Cowork, 2026-10-11 12:55 UTC, Task B, from Definition "Widget
    // Declaration, Default Widget" and Remark "Construct Families".
    Future<void> surface(WidgetTester tester) => tester.pumpWidget(
        MaterialApp(home: AgentSurface(agentId: 'bob', runtime: r)));
    Finder field(int id, String kind, List<int> at) => find.descendant(
        of: find.byKey(nodeKey(id, kind, at)), matching: find.byType(TextField));
    Finder inside(int id, Finder f) =>
        find.descendant(of: find.byKey(constructKey(id)), matching: f);

    testWidgets(
        'a list the person writes: the widget of its element, kept open, and '
        'a button that ends the list, granting [end], a term no element can be '
        '(item 1; vGLP #7 Cowork, 2026-10-11 14:31 UTC, item 3); a stream has '
        'no such button', (tester) async {
      r.handle(fixtureTerm('draw(1, input_list(number), input([]))'));
      r.handle(fixtureTerm('draw(2, input_box(number), input([]))'));
      await surface(tester);
      expect(find.byKey(nodeKey(1, 'input_list', [])), findsOneWidget);
      await tester.enterText(field(1, 'number', []), '4');
      await tester.pump();
      await tester.tap(find.byKey(submitKey(1, [])));
      await tester.pump();
      expect(sent, ['input(1, [], 4)']);
      expect(tester.widget<TextField>(field(1, 'number', [])).controller!.text,
          isEmpty);
      await tester.tap(find.byKey(endKey(1, [])));
      await tester.pump();
      expect(sent, ['input(1, [], 4)', 'input(1, [], [end])']);
      expect(find.byKey(nodeKey(2, 'input_box', [])), findsOneWidget);
      expect(find.byKey(endKey(2, [])), findsNothing);
    });

    testWidgets(
        'a stream inside a form the person writes is no field of it: the form '
        'is submitted with [] there, and once drawn with its answer the '
        'stream is drawn under it, a question of its own (item 3)',
        (tester) async {
      const w = "form(post, [field('String', text), "
          "question('Stream(String)', input_box(text))])";
      r.handle(fixtureTerm('draw(3, $w, input([]))'));
      await surface(tester);
      // One field, labelled by its declared type's name, and no input box.
      expect(field(3, 'text', [1]), findsOneWidget);
      expect(inside(3, find.byType(TextField)), findsOneWidget);
      expect(find.byKey(nodeKey(3, 'input_box', [2])), findsNothing);
      expect(inside(3, find.text('Stream(String)')), findsNothing);
      await tester.enterText(field(3, 'text', [1]), 'hi');
      await tester.pump();
      await tester.tap(find.byKey(submitKey(3, [])));
      await tester.pump();
      expect(sent, ['input(3, [], post(hi, []))']);
      // The answer, its stream at [2] open: the form shown, the input box
      // under it.
      r.handle(fixtureTerm("draw(3, $w, post('hi', input([2])))"));
      await tester.pump();
      expect(find.byKey(nodeKey(3, 'shown', [1])), findsOneWidget);
      expect(inside(3, find.text('hi')), findsOneWidget);
      expect(find.byKey(nodeKey(3, 'input_box', [2])), findsOneWidget);
      expect(inside(3, find.text('Stream(String)')), findsOneWidget);
      await tester.enterText(field(3, 'text', [2]), 'x');
      await tester.pump();
      await tester.tap(find.byKey(submitKey(3, [2])));
      await tester.pump();
      expect(sent.last, 'input(3, [2], x)');
      expect(find.text('No widget', findRichText: true), findsNothing);
    });

    testWidgets(
        'a peer field takes an agent identifier, a name or a key, and nothing '
        'else (item 5)', (tester) async {
      r.handle(fixtureTerm('draw(5, peer, input([]))'));
      await surface(tester);
      bool submittable() =>
          tester.widget<ElevatedButton>(find.byKey(submitKey(5, []))).onPressed !=
          null;
      await tester.enterText(field(5, 'peer', []), 'Carol Smith');
      await tester.pump();
      expect(submittable(), isFalse);
      await tester.enterText(field(5, 'peer', []), '0a1b2c');
      await tester.pump();
      await tester.tap(find.byKey(submitKey(5, [])));
      await tester.pump();
      expect(sent, ["input(5, [], '0a1b2c')"]);
    });

    testWidgets(
        'a form field is labelled by the name of its declared type, and no '
        'field by its widget\'s kind (item 6)', (tester) async {
      r.handle(fixtureTerm("draw(6, form(lot, [field('Key', peer), "
          "field('Integer', number), field('Date', date)]), input([]))"));
      await surface(tester);
      for (final label in ['Key', 'Integer', 'Date']) {
        expect(inside(6, find.text(label)), findsOneWidget, reason: label);
      }
      for (final kind in ['peer', 'number', 'date', 'text']) {
        expect(inside(6, find.text(kind)), findsNothing, reason: kind);
      }
    });

    testWidgets(
        'a tuple the program writes is a form of shown fields, each labelled '
        'by the name of its declared type, a tuple inside it a form of its '
        'own, a union drawn by its alternative\'s form, and nothing edited '
        '(Definition "Widget Declaration, Default Widget"; vGLP #8 Cowork, '
        '2026-10-13 08:15 UTC, item b)', (tester) async {
      const coin = "form(coin, [field('Key', shown), field('Integer', shown)])";
      r.handle(fixtureTerm("draw(11, form(card, [field('Offer', form(offer, "
          "[field('Key', shown), field('Lot', form(lot, [field('Key', shown), "
          "field('Integer', shown)])), field('Stream(Item)', thread(menu(["
          "$coin, form(bond, [field('Key', shown), field('Integer', shown), "
          "field('Date', shown)]), shown])))])), "
          "field('YesNo', buttons([yes, no]))]), "
          "card(offer(bob, lot(carol, 7), [coin(alice, 1), bond(dan, 5, 30), "
          "none]), input([2])))"));
      await surface(tester);
      for (final (label, n) in [
        ('Offer', 1),
        ('Lot', 1),
        ('Stream(Item)', 1),
        ('Key', 4),
        ('Integer', 3),
        ('Date', 1),
        ('YesNo', 1),
      ]) {
        expect(inside(11, find.text(label)), findsNWidgets(n), reason: label);
      }
      // The lot a form of its own, inside the offer's.
      expect(find.byKey(nodeKey(11, 'form', [1])), findsOneWidget);
      expect(find.byKey(nodeKey(11, 'form', [1, 2])), findsOneWidget);
      expect(
          find.descendant(
              of: find.byKey(nodeKey(11, 'shown', [1, 2, 1])),
              matching: find.text('carol')),
          findsOneWidget);
      // Each element of the stream drawn by its alternative's form, a
      // constant alternative shown.
      expect(find.byKey(nodeKey(11, 'form', [1, 3, 1])), findsOneWidget);
      expect(find.byKey(nodeKey(11, 'form', [1, 3, 2, 1])), findsOneWidget);
      expect(inside(11, find.text('none')), findsOneWidget);
      for (final v in ['bob', 'alice', 'dan', '7', '1', '5', '30']) {
        expect(inside(11, find.text(v)), findsOneWidget, reason: v);
      }
      // Each form headed by its functor, nested ones and a thread's elements
      // too (vGLP f9b63d6).
      for (final (heading, at) in [
        ('card', const <int>[]),
        ('offer', const [1]),
        ('lot', const [1, 2]),
        ('coin', const [1, 3, 1]),
        ('bond', const [1, 3, 2, 1]),
      ]) {
        expect(
            find.descendant(
                of: find.byKey(nodeKey(11, 'heading', at)),
                matching: find.text(heading)),
            findsOneWidget,
            reason: '$heading at $at');
      }
      // Shown and not edited: nothing but the answer's buttons takes input.
      expect(inside(11, find.byType(TextField)), findsNothing);
      expect(inside(11, find.byType(ElevatedButton)), findsNWidgets(2));
      await tester.tap(inside(11, find.widgetWithText(ElevatedButton, 'yes')));
      await tester.pump();
      expect(sent, ['input(11, [2], yes)']);
    });

    testWidgets(
        'a menu\'s chip names its alternative\'s functor, and its arity where '
        'two share a functor (item 7)', (tester) async {
      r.handle(fixtureTerm("draw(7, menu([form(mint, [field('Integer', number)]), "
          "form(mint, [field('Integer', number), field('Integer', number)]), "
          "form(pay, [field('Key', peer), field('Integer', number)]), "
          'button(quit)]), input([]))'));
      await surface(tester);
      for (final chip in ['mint/1', 'mint/2', 'pay', 'quit']) {
        expect(inside(7, find.text(chip)), findsOneWidget, reason: chip);
      }
      expect(inside(7, find.text('mint')), findsNothing);
      await tester.tap(inside(7, find.text('mint/2')));
      await tester.pump();
      expect(field(7, 'number', [1]), findsOneWidget);
      expect(field(7, 'number', [2]), findsOneWidget);
      await tester.enterText(field(7, 'number', [1]), '3');
      await tester.enterText(field(7, 'number', [2]), '30');
      await tester.pump();
      await tester.tap(find.byKey(submitKey(7, [])));
      await tester.pump();
      expect(sent, ['input(7, [], mint(3, 30))']);
    });

    testWidgets(
        'a widget position the program has not yet written whole, pending(P), '
        'is drawn by its type\'s widget as a placeholder taking no input, the '
        'questions beside it offered at once, and drawn again once written '
        '(vGLP 118e04d, Definition "Construct Process")', (tester) async {
      const card = "form(card, [field('Peer', shown), "
          "field('YesNo', buttons([yes, no]))])";
      // The root pending: the card's form named, nothing to answer.
      r.handle(fixtureTerm('draw(8, $card, pending([]))'));
      // A field pending beside a question.
      r.handle(fixtureTerm('draw(9, $card, card(pending([1]), input([2])))'));
      // A picker whose list's second element is pending.
      r.handle(fixtureTerm('draw(10, picker, '
          'choose([alice, pending([1, 2, 1])], input([2])))'));
      await surface(tester);
      expect(find.byKey(nodeKey(8, 'pending', [])), findsOneWidget);
      expect(inside(8, find.text('card \u2026')), findsOneWidget);
      expect(inside(8, find.byType(ElevatedButton)), findsNothing);
      expect(find.byKey(nodeKey(9, 'pending', [1])), findsOneWidget);
      expect(inside(9, find.text('\u2026')), findsOneWidget);
      await tester.tap(inside(9, find.widgetWithText(ElevatedButton, 'yes')));
      await tester.pump();
      expect(sent, ['input(9, [2], yes)']);
      expect(find.byKey(nodeKey(10, 'pending', [1, 2, 1])), findsOneWidget);
      expect(inside(10, find.byType(OutlinedButton)), findsOneWidget);
      // Written: drawn again, the placeholder gone.
      r.handle(fixtureTerm('draw(8, $card, card(bob, input([2])))'));
      r.handle(fixtureTerm('draw(10, picker, '
          'choose([alice, bob], input([2])))'));
      await tester.pump();
      expect(find.byKey(nodeKey(8, 'pending', [])), findsNothing);
      expect(inside(8, find.text('bob')), findsOneWidget);
      expect(inside(8, find.byType(ElevatedButton)), findsNWidgets(2));
      expect(find.byKey(nodeKey(10, 'pending', [1, 2, 1])), findsNothing);
      expect(inside(10, find.byType(OutlinedButton)), findsNWidgets(2));
      expect(find.text('No widget', findRichText: true), findsNothing);
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
  group('the initial goal: every call of a volitional procedure holds a '
      'writer at each output position', () {
    // vGLP, Definition "Volitional Procedure, Interactive Type, Interactive
    // Term, Ordinary Clause, vGLP Program": "every call of a volitional
    // procedure, in a clause body or in the initial goal, holds a writer at
    // each output position"; Definition "Canonical Compilation": "the GLP
    // module that forms the program's initial goal imports that module's
    // compiled procedures".  The bridge's goal, M # q(...), is the body of the
    // entry the host posts, and is checked when the program links (IGLP's
    // Task 1 of GLP 2026-10-13 08:11 UTC; GLP 2026-10-13 09:20 UTC, item 2).
    // The program is written under programs/tests/vglp/ for the run and
    // removed after, as glp_runtime's initial_goal_test's is.
    late String name;
    late Directory fixture;
    void write(String answers) {
      name = 'initgoal_bridge_${pid}_${DateTime.now().microsecondsSinceEpoch}';
      fixture = Directory('${programsDir()}/tests/vglp/$name')..createSync();
      File('${fixture.path}/asker.vglp').writeAsStringSync('''
Peer  ::= Constant.
YesNo ::= yes ; no.
Card  ::= card(Peer, YesNo?).

exported procedure (Card)*ask(Peer?, Stream(YesNo)).
(card(P?, A))*ask(P, [A?]) :- ground(P?) | true.
''');
      File('${fixture.path}/self.glp').writeAsStringSync('''
Peer     ::= Constant.
YesNo    ::= yes ; no.
Card     ::= card(Peer, YesNo?).
Question ::= card_w(Card).
Ask(Q)   ::= ask(Constant, Q).
Drawn    ::= card_w(_) ; YesNo.

imported procedure asker#ask(Peer?, Stream(YesNo), Stream(Ask(Question))).
imported procedure asker#dispatch(Stream(Ask(Question))?,
    PersonChannel(Drawn)?, Channel(Stream(_), Stream(_))).

exported procedure ask_init(Constant?, PersonIn(Drawn)?, Stream(_)?).
ask_init(_, Gs, _) :-
    asker # ask(bob, $answers, Asks),
    asker # dispatch(Asks?, ch(Gs?, Ds), _),
    send_to_user(Ds?).
''');
    }

    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    test('a constant at the output position: the agent is refused at load, '
        'the call, its clause and the position named', () async {
      write('[yes]');
      final programs = programsDir();
      final agent = AgentRuntime(
        agentId: 'bob',
        program: '$programs/tests/vglp/$name',
        goalLabel: 'ask_init/3',
        rootSelfGlpPath: '$programs/self.glp',
      );
      agent.onLog = (_, __) {};
      agent.onSendMadMessage = (_, __) async {};
      await expectLater(
          agent.initialize(),
          throwsA(predicate((e) =>
              e.toString().contains('The call asker # ask(bob, [yes], '
                  'Asks) of the volitional procedure asker#ask/2, in the '
                  'clause ask_init(_, Gs, _) at line 13') &&
              e.toString().contains('holds [yes] at output position 2, '
                  'which is not a writer'))));
    });

    testWidgets('a writer at the output position: admitted, and the card '
        'drawn', (tester) async {
      write('_');
      final p = await _Person.boot(tester, 'ask_init', dir: name);
      expect(p.questions, hasLength(1));
      expect(formatTerm(p.questions.single.view),
          'card(bob, input([2]))');
    });
  });
}
