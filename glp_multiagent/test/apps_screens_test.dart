/// The screens of GSC's three apps, GSG's, GSN's and GSNP's (`programs/apps/gsg`, `programs/apps/gsn`, `programs/apps/gsnp`), and of GAI's (`programs/apps/gai`), each person's phone as the bridge draws it, written to PNG files for GAI's paper (`programs/apps/<app>/screens/`).
///
/// Each app's directory is a certified program: the canonical CVA platform of its contract, `platform.vglp`, compiled to GLP when the directory loads (vGLP, Section 6.3), and the mini-app's entry `miniapp/4` (`miniapp.glp`), which runs the platform's agent with the person's key and its dispatcher on the agent's asks.  The engine loads the directory and posts the entry for alice and bob over one conversation, as the plays of each directory do (`plays.glp`); for GSNP's app for dan, eve, carol, bob and fay, over four conversations, dan's with eve, eve's with carol, carol's with bob and bob's with fay, the persons of the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4); and for GAI's app for alice, alf and bob, over three conversations, alice's with alf, alf's with bob and bob's with alice, the agents of the example of GAI's paper (Section 3.2), alf scripted by the app's person (`programs/apps/gai/person.glp`), its script the AI agent's program, and alice and bob at their phones, the test playing the paper's Table 2 exactly.  Each person's channel is `ch(Grants?, Draws)` (vGLP, Definition "Person Channel, Person Writer, GLP with Persons, Grant"): the host hands each person's bridge every term of `Draws` as it arrives (bridge.dart), and extends `Grants` with each grant the bridge returns, `input(Id, P, R)`.
///
/// The agent writes its view, `view(S, K)`, its machine state and the persons it knows, on construct 0 when it starts and whenever either changes (vGLP b7e3517, Section 5.4, "The compilation"), drawn by `last_state`, the widget the platform declares for `Stream(Output)`.  The app declares `Menu =::= social_menu.` and `Approval =::= approval_sentence.` (`programs/apps/<app>/self.vglp`): the request question is drawn with the last view on construct 0 (vGLP e924f73, Section 6.2) as the app's tabs, Friends and, where the request type offers `create`, Groups, and each approval question as one sentence and two buttons at the top of the tab it concerns (`lib/ui_runtime/named_widgets.dart`, `tab_screen.dart`), as GAI's task words them (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, and its sentence for a join at 11:35 UTC).  Every act of a person at a phone is a tap or a keystroke on a control of the tabs, found by its key (`socialKey`, `tabKey`, `constructKey`): Join, Add, Befriend, Accept, End friendship, Send, Create, Add member and the rest; a scripted agent's acts are its scripted person's, as in the app's plays.  No term is hand-fed to the bridge or to the program.
///
/// Each screen is the person's phone, 390 by 844 logical pixels, rendered by the widget itself (RepaintBoundary.toImage) at a device pixel ratio of 3, in the light theme.  The test font draws every glyph as a box, so the screens are set in Roboto, the Material font, with MaterialIcons, both loaded from the Flutter SDK's own copies (`bin/cache/artifacts/material_fonts`).  The PNG files are written only when the test runs with `--update-goldens`:
///
///     flutter test --update-goldens test/apps_screens_test.dart
///
/// Without it the test takes every act and checks every screen, and writes nothing into the tree.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart'
    show runtimeTermOf, screenTermOf;
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/bridge.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/named_widgets.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/tab_screen.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/external_io.dart' show InputInjector;
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'programs_dir.dart';

/// The phone's screen, in logical pixels, and the ratio it is rendered at.
const Size _phone = Size(390, 844);
const double _ratio = 3;

/// The gap around each phone in the test's view.
const double _gap = 8;

/// Roboto and MaterialIcons, from the Flutter SDK's copies.
Future<void> _loadFonts() async {
  final roots = [
    if (Platform.environment['FLUTTER_ROOT'] case final String r) r,
    '/opt/homebrew/share/flutter',
  ];
  final dir = roots
      .map((r) => '$r/bin/cache/artifacts/material_fonts')
      .firstWhere((d) => Directory(d).existsSync(),
          orElse: () => fail('no material_fonts under $roots'));
  Future<ByteData> bytes(String f) async =>
      ByteData.view(File('$dir/$f').readAsBytesSync().buffer);
  final roboto = FontLoader('Roboto');
  for (final f in [
    'Roboto-Regular.ttf',
    'Roboto-Italic.ttf',
    'Roboto-Medium.ttf',
    'Roboto-Bold.ttf',
  ]) {
    roboto.addFont(bytes(f));
  }
  await roboto.load();
  final icons = FontLoader('MaterialIcons')
    ..addFont(bytes('MaterialIcons-Regular.otf'));
  await icons.load();
}

/// The app's one panel, the compiled program's: the constructs the bridge holds open, the agent's output among them as construct 0.
Manifest _manifest(String app) => Manifest(
      title: app,
      panels: [Panel(id: app, name: app.toUpperCase(), constructs: true)],
      activity: const [],
    );

/// What no screen shows: a term, a functor, a type name, an attempt number or a Submit button (GAI's task, its preamble).
const List<String> _notShown = [
  'Submit',
  'propose',
  'menu',
  'Key',
  'Request',
  'Proposal',
  'Peer',
  'Speech',
  'Utterance',
  'utterance',
  'speech(',
  'attempt',
  'rank',
  'befriend(',
  'friend(',
  'identity',
  'received',
  'member(',
  'listed',
  'item(',
  'view(',
  'agent_of',
  'principal',
  'retired',
  'enter(',
];

/// One person at their phone: their bridge, the writer of their grants, and the stream of draws their mini-app writes.
class _Person {
  final String name;
  final InputInjector grants;
  final HeapCell draws;
  final GlobalKey shot = GlobalKey();

  /// The grants the bridge returned, not yet put on the person channel.
  final List<GTerm> unsent = [];

  /// How many terms of the draws the bridge has been handed.
  int fed = 0;

  late final UiRuntime ui;

  _Person(this.name, String app, this.grants, this.draws) {
    ui = UiRuntime(manifest: _manifest(app), onSend: unsent.add);
  }

  /// The open request question, `menu(Me, Known, Request?)`, drawn by `social_menu` (vGLP, Section 5.4): the agent asks the next one when it reads the answer to the last, so its construct changes with each request.
  OpenConstruct get request => _one('menu');

  /// The open approval question, `approval(Attempt, YesNo?)`, drawn by `approval_sentence`.
  OpenConstruct get approval => _one('approval');

  List<OpenConstruct> questions(String functor) => [
        for (final c in ui.bridge.constructs)
          if (c.id != 0 && ctorArgs(c.view).$1 == functor) c,
      ];

  OpenConstruct _one(String functor) {
    final cs = questions(functor);
    expect(cs, hasLength(1),
        reason: '$name: one $functor question open, of '
            '${ui.bridge.constructs.map((c) => formatTerm(c.view)).toList()}');
    return cs.single;
  }

  /// The last view the agent wrote on construct 0, `view(S, K)`.
  SocialState get state {
    final (ok, s) = SocialState.read(ui.bridge.construct(0)?.view);
    expect(ok && s != null, isTrue, reason: '$name: no view on construct 0');
    return s!;
  }

  /// The atoms of the last view, each as its term.
  List<String> get atoms => [for (final a in state.atoms) formatTerm(a)];

  /// The persons the last view shows known, each as its term.
  List<String> get known => [for (final k in state.known) formatTerm(k)];
}

/// The persons of a run, each running the app's mini-app on their own phone, a conversation between each two of a pair: by default a chain, a conversation between each two next to each other in it: Alice and Bob, over one conversation; or Dan, Eve, Carol, Bob and Fay, over four.  An agent of the run may be scripted, its acts taken by the app's scripted person (`programs/apps/<app>/person.glp`) under its role and not by taps: in GAI's app Alf, Alice's AI agent, whose program takes its acts (GAI's paper, Section 3.1: "after that the AI agent acts on its own").  A scripted agent has no phone.
class _Run {
  final WidgetTester tester;
  final String app;
  final GlpEngine engine;
  final PostedGoal play;
  final List<_Person> persons;

  /// The screen each scripted agent's person reports, by name: the stream of what it saw (`Seen`, `programs/apps/<app>/self.glp`).
  final Map<String, HeapCell> seen;

  _Run._(this.tester, this.app, this.engine, this.play, this.persons,
      [this.seen = const {}]);

  _Person get alice => persons[0];
  _Person get bob => persons[1];

  /// The person [name] of the run.
  _Person person(String name) => persons.singleWhere((p) => p.name == name);

  static Future<_Run> start(WidgetTester tester, String app,
      {List<String> chain = const ['alice', 'bob'],
      List<(String, String)>? pairs,
      Set<String> scripted = const {}}) async {
    await _loadFonts();
    final phones = [for (final p in chain) if (!scripted.contains(p)) p];
    // Shadows drawn as shadows, not as the test's solid outlines; restored by [finish].
    debugDisableShadows = false;
    tester.view.physicalSize = Size(phones.length * (_phone.width + 2 * _gap),
        _phone.height + 2 * _gap);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final programs = programsDir();
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    expect(engine.loadProgram('$programs/apps/$app'), isTrue,
        reason: 'programs/apps/$app does not load');
    // The entry once for each agent, as the plays activate it (plays.glp), with a conversation between the executions of each two agents of a pair, the ends of one root channel, C_p_q written by p and read by q; each agent's grants, G<i>, and its draws, D<i>: a person's grants the caller's to write and their draws read off their stream, and a scripted agent's both its scripted person's, which reports its screen on S<i>.
    final links = pairs ??
        [for (var i = 0; i + 1 < chain.length; i++) (chain[i], chain[i + 1])];
    String conversation(String p, String q) =>
        'conversation($q, ch(C_${q}_$p?, C_${p}_$q))';
    final goals = [
      for (var i = 0; i < chain.length; i++) ...[
        'miniapp(${chain[i]}, ch(G$i?, D$i), ch([${[
          for (final (a, b) in links)
            if (a == chain[i])
              conversation(a, b)
            else if (b == chain[i])
              conversation(b, a),
        ].join(', ')}], U$i), [])',
        if (scripted.contains(chain[i]))
          'miniapp_person(${chain[i]}, ${chain[i]}, ch(D$i?, G$i), S$i)',
      ],
    ];
    final play = engine.postGoal(goals.join(', '), inputs: [
      for (var i = 0; i < chain.length; i++)
        if (!scripted.contains(chain[i])) 'G$i'
    ]);
    final run = _Run._(tester, app, engine, play, [
      for (var i = 0; i < chain.length; i++)
        if (!scripted.contains(chain[i]))
          _Person(chain[i], app, play.inputs['G$i']!, play.variables['D$i']!),
    ], {
      for (var i = 0; i < chain.length; i++)
        if (scripted.contains(chain[i])) chain[i]: play.variables['S$i']!,
    });
    run._drain();
    run._feed();
    await tester.pumpWidget(run._phones());
    await tester.pumpAndSettle();
    return run;
  }

  /// The last view the scripted agent [name]'s person reports, `view(S, K)`: its atoms, each as its term.
  List<String> scriptedAtoms(String name) {
    final views = [
      for (final t in _drawn(seen[name]!))
        if (ctorArgs(t) case ('view', [GList(:final items), _])) items
    ];
    expect(views, isNotEmpty, reason: '$name: no view reported');
    return [for (final a in views.last) formatTerm(a)];
  }

  /// The run reduced until quiescent, as each event of an agent is (IGLP, Implementation Notes; vGLP, Section 5.4, "Triggers": the agent takes its own transactions until none holds).
  void _drain() {
    final r = play.scheduler.drainToQuiescence(maxCycles: 20000000);
    expect(r.status, isNot(ExecutionStatus.capped),
        reason: 'the run did not quiesce');
    expect(r.status, isNot(ExecutionStatus.failed), reason: 'a goal failed');
  }

  /// [t] with every bound variable in it replaced by its value.
  rt.Term _deep(rt.Term t) {
    final d = engine.runtime.heap.dereference(t);
    return d is rt.StructTerm
        ? rt.StructTerm(d.functor, [for (final a in d.args) _deep(a)])
        : d;
  }

  /// The terms of the draws stream at [writer] written so far, each whole.
  List<GTerm> _drawn(HeapCell writer) {
    final out = <GTerm>[];
    var cell = engine.runtime.heap.dereference(rt.VarRef(writer));
    while (cell is rt.StructTerm && cell.functor == '.' && cell.args.length == 2) {
      out.add(screenTermOf(_deep(cell.args[0])));
      cell = engine.runtime.heap.dereference(cell.args[1]);
    }
    return out;
  }

  /// Hand each person's bridge what their draws carried since last time.
  void _feed() {
    for (final p in persons) {
      final ds = _drawn(p.draws);
      for (; p.fed < ds.length; p.fed++) {
        p.ui.handle(ds[p.fed]);
      }
    }
  }

  /// Put each grant the bridges returned on its person channel, run the program on it, and draw what it draws.
  Future<void> settle() async {
    for (var more = true; more;) {
      more = false;
      for (final p in persons) {
        while (p.unsent.isNotEmpty) {
          more = true;
          p.grants
              .inject(runtimeTermOf(p.unsent.removeAt(0)))
              .forEach(engine.runtime.gq.enqueue);
          _drain();
        }
      }
    }
    _feed();
    await tester.pumpAndSettle();
  }

  /// The phones side by side, each the person's agent surface on a screen of its own.
  Widget _phones() => MaterialApp(
        debugShowCheckedModeBanner: false,
        theme: ThemeData(
            useMaterial3: true,
            brightness: Brightness.light,
            fontFamily: 'Roboto',
            colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
            elevatedButtonTheme: ElevatedButtonThemeData(
                style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.green,
                    foregroundColor: Colors.white))),
        home: ColoredBox(
          color: const Color(0xFF2B2B33),
          child: Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              for (final p in persons)
                Padding(
                  padding: const EdgeInsets.all(_gap),
                  child: RepaintBoundary(
                    key: p.shot,
                    child: SizedBox.fromSize(
                      size: _phone,
                      child: MediaQuery(
                        data: const MediaQueryData(
                            size: _phone, devicePixelRatio: _ratio),
                        child: AgentSurface(
                            agentId: p.name, runtime: p.ui, muteNotices: true),
                      ),
                    ),
                  ),
                ),
            ],
          ),
        ),
      );

  // === The persons' acts on their tabs ======================================

  /// [f] within [p]'s phone.
  Finder on(_Person p, Finder f) =>
      find.descendant(of: find.byKey(p.shot), matching: f);

  /// The control or row [what] of [p]'s open request question (`socialKey`).
  Finder control(_Person p, String what) =>
      on(p, find.byKey(socialKey(p.request.id, what)));

  /// The name [text] on the row [what] of [p]'s open request question, which a person taps to open the row.
  Finder name(_Person p, String what, String text) =>
      find.descendant(of: control(p, what), matching: find.text(text));

  /// The control of [p]'s open request question whose key begins [prefix] and ends [suffix], for a group's controls, whose keys carry the group's leader and name (`groupKey`).
  Finder groupControl(_Person p, String prefix, [String suffix = '']) {
    final head = '${p.request.id}:social:$prefix';
    return on(
        p,
        find.byWidgetPredicate((w) =>
            w.key is ValueKey<String> &&
            (w.key as ValueKey<String>).value.startsWith(head) &&
            (w.key as ValueKey<String>).value.endsWith(suffix)));
  }

  /// The list of the tab [p] has open.
  Finder _list(_Person p) => on(
      p,
      find.byWidgetPredicate((w) =>
          w is ListView &&
          w.key is ValueKey<String> &&
          (w.key as ValueKey<String>).value.startsWith('tab-view:')));

  /// [p]'s open tab scrolled until [f] is built, the list building only what is near the screen.
  Future<void> reveal(_Person p, Finder f) async {
    if (f.evaluate().isNotEmpty) return;
    await tester.scrollUntilVisible(f, 120,
        scrollable:
            find.descendant(of: _list(p), matching: find.byType(Scrollable)).first);
    await tester.pumpAndSettle();
  }

  /// [p] taps [f]: the grant it makes, if any, put on their person channel, the program run on it, and the phones drawn again.
  Future<void> tap(_Person p, Finder f) async {
    await reveal(p, f);
    expect(f, findsOneWidget);
    await tester.ensureVisible(f);
    await tester.pumpAndSettle();
    await tester.tap(f);
    await tester.pumpAndSettle();
    await settle();
  }

  /// [p] types [text] into the field [f].
  Future<void> type(_Person p, Finder f, String text) async {
    await reveal(p, f);
    expect(f, findsOneWidget);
    await tester.enterText(f, text);
    await tester.pumpAndSettle();
  }

  /// [p] opens the tab [t].
  Future<void> tab(_Person p, ScreenTab t) =>
      tap(p, on(p, find.byKey(tabKey(t))));

  /// [p] taps the button [label] of their open approval question.
  Future<void> answer(_Person p, String label) => tap(
      p,
      on(
          p,
          find.descendant(
              of: find.byKey(constructKey(p.approval.id)),
              matching: find.widgetWithText(ElevatedButton, label))));

  /// [p] establishes their identity (GSG:1): Join.
  Future<void> join(_Person p) => tap(p, control(p, 'join'));

  /// [p] asks to know [q]: their key typed into Add a person by key, and Add.
  Future<void> add(_Person p, String q) async {
    await type(p, control(p, 'add-field'), q);
    await tap(p, control(p, 'add'));
  }

  /// The texts on [p]'s phone, in order.
  List<String> texts(_Person p) => [
        for (final t in tester.widgetList<Text>(on(p, find.byType(Text))))
          if (t.data != null) t.data!,
      ];

  /// The texts construct [c] draws on [p]'s phone, in order.
  List<String?> textsOf(_Person p, OpenConstruct c) => [
        for (final t in tester.widgetList<Text>(on(
            p,
            find.descendant(
                of: find.byKey(constructKey(c.id)),
                matching: find.byType(Text)))))
          t.data,
      ];

  /// Nothing on [p]'s phone is a term, a functor, a type name, an attempt number or a Submit button.
  void plain(_Person p) {
    final all = texts(p).join('\n');
    for (final w in _notShown) {
      expect(all.contains(w), isFalse,
          reason: '${p.name}: "$w" on the screen:\n$all');
    }
  }

  /// [p]'s open tab scrolled to its top.
  Future<void> top(_Person p) async {
    FocusManager.instance.primaryFocus?.unfocus();
    final s = tester.state<ScrollableState>(
        find.descendant(of: _list(p), matching: find.byType(Scrollable)).first);
    s.position.jumpTo(0);
    await tester.pumpAndSettle();
  }

  /// [p]'s phone as it stands, written to [file] in the app's screens.
  Future<void> shoot(_Person p, String file) async {
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    final boundary =
        p.shot.currentContext!.findRenderObject()! as RenderRepaintBoundary;
    final png = await tester.runAsync(() async {
      final image = await boundary.toImage(pixelRatio: _ratio);
      final data = await image.toByteData(format: ui.ImageByteFormat.png);
      image.dispose();
      return data!.buffer.asUint8List();
    });
    expect(png, isNotNull);
    if (autoUpdateGoldenFiles) {
      final dir = Directory('${programsDir()}/apps/$app/screens')
        ..createSync(recursive: true);
      File('${dir.path}/$file').writeAsBytesSync(png!);
      // ignore: avoid_print
      print('wrote programs/apps/$app/screens/$file');
    }
  }

  /// [p] and [q] establish their identities (GSG:1) and ask to know each other, each on their Friends tab: Join, then the other's key and Add.
  Future<void> meet(_Person p, _Person q) async {
    for (final x in [p, q]) {
      if (!x.state.identity) await join(x);
      expect(x.state.identity, isTrue, reason: '${x.name}: no identity');
    }
    await add(p, q.name);
    await add(q, p.name);
    expect(p.known, contains(q.name));
    expect(q.known, contains(p.name));
  }

  /// [p] offers [q] friendship (GSG:2): Befriend on [q]'s row; and [q], asked, taps Accept.
  Future<void> befriend(_Person p, _Person q, {String? shot}) async {
    await tap(p, control(p, 'befriend:${q.name}'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(attempt(${p.name}, '));
    expect(formatTerm(a.view),
        endsWith(', befriend(${p.name}, ${q.name})), input([2]))'));
    await top(q);
    final asks = '${personName(GAtom(p.name))} asks to be your friend.';
    expect(textsOf(q, a), [asks, 'Accept', 'Decline']);
    // At the top of Friends, before the person's own name.
    expect(tester.getTopLeft(on(q, find.text(asks))).dy,
        lessThan(tester.getTopLeft(control(q, 'name')).dy));
    if (shot != null) {
      plain(q);
      await shoot(q, shot);
    }
    await answer(q, 'Accept');
    expect(q.questions('approval'), isEmpty);
    expect(p.atoms, contains('friend(${q.name})'));
    expect(q.atoms, contains('friend(${p.name})'));
  }

  /// The test's painting settings restored, as the test framework requires.
  void finish() => debugDisableShadows = true;
}

void main() {
  testWidgets(
      'GSG\'s app on two phones: Bob asked to be Alice\'s friend; Alice\'s '
      'Friends with Bob her friend; and after Bob ends the friendship',
      (tester) async {
    final run = await _Run.start(tester, 'gsg');
    final alice = run.alice;
    final bob = run.bob;

    // One tab, Friends: GSG offers no group.
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.friends))), findsOneWidget);
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsNothing);

    // Before her identity is established, her name and Join, which proposes establish(alice).
    expect(run.control(alice, 'name'), findsOneWidget);
    expect(run.control(alice, 'join'), findsOneWidget);
    expect(alice.atoms, isEmpty);

    // Each joins (GSG:1) and adds the other by key; each then knows the other.
    await run.meet(alice, bob);
    expect(run.control(alice, 'join'), findsNothing);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(ElevatedButton, 'Befriend')),
        findsOneWidget);

    // (a) Alice taps Befriend on Bob's row (GSG:2): Bob's Friends tab, the question "Alice asks to be your friend." and Accept and Decline at its top.
    await run.befriend(alice, bob, shot: 'gsg-a-bob-asked.png');

    // (b) Alice's Friends tab after the befriend commits: Bob her friend, with End friendship.
    await run.top(alice);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(OutlinedButton, 'End friendship')),
        findsOneWidget);
    expect(run.control(alice, 'befriend:bob'), findsNothing);
    expect(run.texts(alice), containsAllInOrder(['Alice', 'Bob', 'End friendship']));
    run.plain(alice);
    await run.shoot(alice, 'gsg-b-alice-friends.png');

    // Bob ends the friendship (GSG:3), his own act: End friendship on Alice's row.  Alice is not asked.
    await run.tap(bob, run.control(bob, 'unfriend:alice'));
    expect(alice.questions('approval'), isEmpty);
    expect(alice.atoms, ['identity']);
    expect(bob.atoms, ['identity']);

    // (c) Alice's Friends tab after Bob ends the friendship: Bob listed, with Befriend.
    await run.top(alice);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(ElevatedButton, 'Befriend')),
        findsOneWidget);
    expect(run.control(alice, 'unfriend:bob'), findsNothing);
    expect(run.texts(alice), containsAllInOrder(['Alice', 'Bob', 'Befriend']));
    run.plain(alice);
    await run.shoot(alice, 'gsg-c-alice-ended.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSN\'s app on two phones: Bob\'s chat with Alice\'s message, Bob '
      'asked to join the group friends, and his view of the group with his '
      'message delivered in it', (tester) async {
    final run = await _Run.start(tester, 'gsn');
    final alice = run.alice;
    final bob = run.bob;
    // The group friends, led by Alice: its name, a speech act of hers, and its community name, <alice>.friends, which a member atom holds, one argument (programs/apps/gsn/platform.vglp, Atom and CName).
    const friends = 'speech(utterance(friends), alice)';
    const friendsGroup = 'dot(community(alice), $friends)';

    // Two tabs: GSN's request type offers create.
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsOneWidget);

    // Identity, knowing each other and friendship, on the Friends tab as in GSG's app.
    await run.meet(alice, bob);
    await run.befriend(alice, bob);

    // A message to a friend (GSN:1): Alice taps Bob's row, which opens her chat with him, types Hi Bob, and taps Send.  Bob is not asked.
    await run.tap(alice, run.name(alice, 'person:bob', 'Bob'));
    expect(run.control(alice, 'chat:bob'), findsOneWidget);
    await run.type(alice, run.control(alice, 'chat-field:bob'), 'Hi Bob');
    await run.tap(alice, run.control(alice, 'send:bob'));
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, contains("received(speech(utterance('Hi Bob'), alice), alice)"));

    // (a) Bob's chat with Alice: he taps her row; her message, Hi Bob, with her name under it.
    await run.tap(bob, run.name(bob, 'person:alice', 'Alice'));
    expect(run.control(bob, 'chat:alice'), findsOneWidget);
    expect(run.texts(bob), containsAllInOrder(['Alice', 'Hi Bob', 'Alice']));
    run.plain(bob);
    await run.shoot(bob, 'gsn-a-bob-chat.png');
    await run.tap(bob, run.control(bob, 'back'));

    // A group (GSN:2): on her Groups tab Alice types friends into New group and taps Create; she is its leader and first member.
    await run.tab(alice, ScreenTab.groups);
    await run.type(alice, run.control(alice, 'group-field'), 'friends');
    await run.tap(alice, run.control(alice, 'create'));
    expect(alice.atoms,
        containsAll(['member($friendsGroup)', 'listed(alice, $friends)']));

    // Joining (GSN:3): in the group, Add member offers her friends not in it; she taps Bob.
    await run.tap(alice, run.groupControl(alice, 'join:', ':bob'));

    // (b) Bob's Groups tab, badged: the question "Alice asks you to join the group friends." and Join and Decline at its top.
    final a = bob.approval;
    expect(formatTerm(a.view),
        endsWith(', join(alice, bob, $friends)), input([2]))'));
    expect(
        find.descendant(
            of: run.on(bob, find.byKey(tabKey(ScreenTab.groups))),
            matching: find.text('1')),
        findsOneWidget);
    await run.tab(bob, ScreenTab.groups);
    await run.top(bob);
    const asks = 'Alice asks you to join the group friends.';
    expect(run.textsOf(bob, a), [asks, 'Join', 'Decline']);
    expect(run.tester.getTopLeft(run.on(bob, find.text(asks))).dy,
        lessThan(run.tester.getTopLeft(run.control(bob, 'group-field')).dy));
    run.plain(bob);
    await run.shoot(bob, 'gsn-b-bob-asked-to-join.png');

    // He taps Join: he is a member of the group, and Alice lists him in it.
    await run.answer(bob, 'Join');
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, contains('member($friendsGroup)'));
    expect(alice.atoms, contains('listed(bob, $friends)'));

    // A group message (GSN:5): in the group, Bob types Hi all and taps Send, which proposes post(bob, alice, x, n), in the order of post's head, post(p?, a ; x, n) (programs/gsc/GSN.glp).  Alice, its leader, is not asked; her agent opens the delivery, deliver(a, q ; x, n), which no role guards, and delivers the message to Bob.
    await run.tab(bob, ScreenTab.groups);
    await run.type(bob, run.groupControl(bob, 'post-field:'), 'Hi all');
    await run.tap(bob, run.groupControl(bob, 'post:'));
    expect(alice.questions('approval'), isEmpty);
    const hiAll = "speech(utterance('Hi all'), bob)";
    expect(alice.atoms, contains('posted($hiAll, $friends)'));
    expect(alice.atoms, contains('delivered($hiAll, bob, $friends)'));
    expect(bob.atoms, contains('received_in($hiAll, alice, $friends)'));

    // (c) Bob's view of the group friends, as Alice delivered his message to him: the group, led by Alice, the message Hi all with Bob's name under it, and no member list, the members being listed in the leader's state (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4).
    await run.top(bob);
    final g = run.groupControl(bob, 'group:');
    expect(g, findsOneWidget);
    final inGroup = [
      for (final t in run.tester.widgetList<Text>(
          find.descendant(of: g, matching: find.byType(Text))))
        if (t.data != null) t.data!,
    ];
    expect(inGroup, containsAllInOrder(['friends', 'Led by Alice', 'Hi all', 'Bob']));
    expect(inGroup.where((t) => t.startsWith('Members')), isEmpty);
    expect(inGroup, contains('Leave group'));
    // Alice's view of the group lists its members, she leading it.
    await run.tab(alice, ScreenTab.groups);
    await run.top(alice);
    expect(
        find.descendant(
            of: run.groupControl(alice, 'group:'),
            matching: find.text('Members: Bob, Alice')),
        findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gsn-c-bob-group.png');

    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSNP\'s app on five phones, the play of GAI\'s GSNP figure: Bob\'s '
      'chat with Carol, the item with its author and who forwarded it, and '
      'Forward; and the warranty question once he picks Fay', (tester) async {
    final run = await _Run.start(tester, 'gsnp',
        chain: const ['dan', 'eve', 'carol', 'bob', 'fay']);
    final dan = run.person('dan');
    final eve = run.person('eve');
    final carol = run.person('carol');
    final bob = run.person('bob');
    final fay = run.person('fay');

    // The friendships of the play (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4): Dan and Eve, Eve and Carol, Carol and Bob, Bob and Fay, each two meeting on their Friends tabs, the first tapping Befriend and the second Accept.
    for (final (p, q) in [(dan, eve), (eve, carol), (carol, bob), (bob, fay)]) {
      await run.meet(p, q);
      await run.befriend(p, q);
    }

    // Dan writes "Flood on Main Street" and sends it to Eve: in his chat with her he types it and taps Send, which signs it, sign(dan, x) (GSNP:1, Authorship), and, once his view holds authored(x), sends it, message(dan, eve, x) (GSN:1), carrying the item as he signed it.  Nobody is asked.
    const flood = "speech(utterance('Flood on Main Street'), dan)";
    await run.tap(dan, run.name(dan, 'person:eve', 'Eve'));
    await run.type(dan, run.control(dan, 'chat-field:eve'), 'Flood on Main Street');
    await run.tap(dan, run.control(dan, 'send:eve'));
    await run.settle();
    expect(dan.atoms, containsAll(['item($flood)', 'authored($flood)']));
    expect(eve.atoms, contains('item($flood)'));
    expect(eve.questions('approval'), isEmpty);

    // Each forwards the item to the next (GSNP:2, Forwarding): in their chat with the friend it came from, Forward, the friend picked, and Yes to the warranty question, which proposes forward(p, q, x); the friend then holds the item signed again by the one who forwarded it.  Nobody is asked.
    Future<void> forward(_Person p, _Person from, _Person to) async {
      await run.tap(p, run.name(p, 'person:${from.name}', personName(GAtom(from.name))));
      await run.tap(p, run.groupControl(p, 'forward:'));
      await run.tap(p, run.groupControl(p, 'forward-to:', ':${to.name}'));
      final w = run.groupControl(p, 'warranty:');
      expect(
          find.descendant(of: w, matching: find.text(
              'You warrant that you know ${personName(GAtom(from.name))} personally.')),
          findsOneWidget);
      await run.tap(p, run.groupControl(p, 'forward-yes:'));
      await run.tap(p, run.control(p, 'back'));
      expect(to.questions('approval'), isEmpty);
    }

    await forward(eve, dan, carol);
    const fromEve = 'speech($flood, eve)';
    expect(carol.atoms, contains('item($fromEve)'));
    await forward(carol, eve, bob);
    const fromCarol = 'speech($fromEve, carol)';
    expect(bob.atoms, contains('item($fromCarol)'));
    expect(bob.atoms, containsAll(['friend(carol)', 'friend(fay)']));
    expect(fay.atoms, contains('friend(bob)'));

    // (a) Bob's chat with Carol: the item "Flood on Main Street", "Author: Dan" and "via Eve, via Carol", and Forward under it.
    await run.tap(bob, run.name(bob, 'person:carol', 'Carol'));
    expect(run.control(bob, 'chat:carol'), findsOneWidget);
    await run.top(bob);
    final item = run.groupControl(bob, 'item:');
    expect(item, findsOneWidget);
    expect(
        [
          for (final t in run.tester.widgetList<Text>(
              find.descendant(of: item, matching: find.byType(Text))))
            t.data,
        ],
        ['Flood on Main Street', 'Author: Dan', 'via Eve, via Carol', 'Forward']);
    run.plain(bob);
    await run.shoot(bob, 'gsnp-a-bob-chat.png');

    // (b) The same screen after Bob taps Forward and picks Fay: the warranty question, "Forward to Fay?  You warrant that you know Carol personally.", with Yes and No.  Nothing is proposed until he answers.
    await run.tap(bob, run.groupControl(bob, 'forward:'));
    expect(run.groupControl(bob, 'forward-picker:'), findsOneWidget);
    await run.tap(bob, run.groupControl(bob, 'forward-to:', ':fay'));
    expect(run.groupControl(bob, 'forward-picker:'), findsNothing);
    final w = run.groupControl(bob, 'warranty:');
    expect(
        [
          for (final t in run.tester.widgetList<Text>(
              find.descendant(of: w, matching: find.byType(Text))))
            t.data,
        ],
        ['Forward to Fay?', 'You warrant that you know Carol personally.', 'Yes', 'No']);
    expect(bob.unsent, isEmpty);
    expect(fay.atoms, isNot(contains('item(speech($fromCarol, bob))')));
    run.plain(bob);
    await run.shoot(bob, 'gsnp-b-bob-forward.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GAI\'s app, the example of GAI\'s paper in the order of its Table 2, '
      'Alice and Bob at their phones and Alf\'s program scripted: Alice '
      'creating Alf in her AI agents tab; Alice and Bob asked to approve Alf '
      'befriending Bob; and Bob\'s chat with Alf, its item naming its '
      'principal and its developer', (tester) async {
    // Alice and Bob, persons, and Alf, Alice's AI agent, a conversation between each two (GAI's paper, Section 3.2, the example and Table "The components that change in the example"), on GAI's contract (programs/gai/GAI.glp).  The test plays Table 2 exactly, acting for Alice and Bob at their phones (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 3); Alf is scripted (programs/apps/gai/person.glp), its script the AI agent's program, which takes its acts: once entered, it proposes to befriend Bob with its attestation, and once his friend it signs the item "Today's summary of the news" (GSNP:1) and sends it to him by message (GSN:1).  The code of Alf is 'The code of Alf' signed by its developer acme_ltd, drawn Acme Ltd (the same, item 1).
    final run = await _Run.start(tester, 'gai',
        chain: const ['alice', 'alf', 'bob'],
        pairs: const [('alice', 'alf'), ('alf', 'bob'), ('bob', 'alice')],
        scripted: const {'alf'});
    final alice = run.person('alice');
    final bob = run.person('bob');
    const code =
        "speech(speech(utterance('The code of Alf'), acme_ltd), alice)";
    const attestation = 'speech($code, alice)';
    const summary = r"speech(utterance('Today\'s summary of the news'), alf)";

    // Two tabs, Friends and AI agents: GAI offers enter, and no group.
    for (final p in [alice, bob]) {
      expect(run.on(p, find.byKey(tabKey(ScreenTab.friends))), findsOneWidget);
      expect(run.on(p, find.byKey(tabKey(ScreenTab.agents))), findsOneWidget);
      expect(run.on(p, find.byKey(tabKey(ScreenTab.groups))), findsNothing);
    }

    // Establish(a), Establish(b): each joins (GSG:1), and each adds the other by key; Bob adds Alf too, and Alf's program asks to know Alice.
    await run.meet(alice, bob);
    if (!alice.known.contains('alf')) await run.add(alice, 'alf');
    await run.add(bob, 'alf');

    // Befriend(a, b): Alice taps Befriend on Bob's row, and Bob, asked, taps Accept (GSG:2).
    await run.befriend(alice, bob);

    // Sign(a; k), Enter(a, r; k): in her AI agents tab Alice types Alf's name, its code and its developer, and taps Create, which signs the code, sign(alice, k), and, once her view holds item(k), creates Alf with it, enter(alice, alf, k) (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 2); Alf's program answers yes, and Alf holds its attestation, the code signed over by Alice (Wards:1, GAI:1, GAI:2).
    await run.tab(alice, ScreenTab.agents);
    await run.type(alice, run.control(alice, 'agent-name'), 'alf');
    await run.type(alice, run.control(alice, 'agent-code'), 'The code of Alf');
    await run.type(alice, run.control(alice, 'agent-developer'), 'acme_ltd');
    await run.tap(alice, run.control(alice, 'create-agent'));
    for (var i = 0; i < 5 && !alice.atoms.contains('principal_of(alf)'); i++) {
      await run.settle();
    }
    expect(alice.atoms,
        containsAll(['item($code)', 'authored($code)', 'principal_of(alf)', 'friend(alf)', 'friend(bob)']));
    expect(run.scriptedAtoms('alf'),
        containsAll(['identity', 'agent', 'principal(alice)', 'friend(alice)', 'attestation($attestation)']));

    // (a) Alice's AI agents tab: the form as she filled it, "Code signed by Acme Ltd", "You attest that Alf runs it." and Create, and Alf listed with Retire.
    await run.top(alice);
    expect(
        run.texts(alice),
        containsAllInOrder([
          'Create an AI agent',
          'Code signed by Acme Ltd',
          'You attest that Alf runs it.',
          'Create',
          'Alf',
          'Retire'
        ]));
    expect(run.control(alice, 'retire:alf'), findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gai-b-alice-agents.png');

    // Agent befriend(r, b, a; y): Alf proposes to befriend Bob with Alice its principal and its attestation, and Alice and Bob are asked (Wards:2, GAI:2).
    final aa = alice.approval;
    expect(formatTerm(aa.view), startsWith('approval(attempt(alf, '));
    expect(formatTerm(aa.view),
        endsWith(', agent_befriend(alf, bob, alice, $attestation)), input([2]))'));

    // (b) Alice's Friends tab, badged: "Alf asks to befriend Bob.", "Approve?", Yes and No at its top.
    await run.tab(alice, ScreenTab.friends);
    await run.top(alice);
    expect(run.textsOf(alice, aa),
        ['Alf asks to befriend Bob.', 'Approve?', 'Yes', 'No']);
    expect(tester.getTopLeft(run.on(alice, find.text('Alf asks to befriend Bob.'))).dy,
        lessThan(tester.getTopLeft(run.control(alice, 'name')).dy));
    run.plain(alice);
    await run.shoot(alice, 'gai-c-alice-asked.png');
    await run.answer(alice, 'Yes');

    // (c) Bob's Friends tab, badged: "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme Ltd", "Befriend Alf?", Yes and No at its top; the developer the first signer of Alf's attestation (GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, (c) and item 5).
    final ba = bob.approval;
    expect(formatTerm(ba.view),
        endsWith(', agent_befriend(alf, bob, alice, $attestation)), input([2]))'));
    await run.top(bob);
    expect(run.textsOf(bob, ba), [
      'Alf, an AI agent of Alice, asks to befriend you.',
      'Developer: Acme Ltd',
      'Befriend Alf?',
      'Yes',
      'No'
    ]);
    expect(
        tester.getTopLeft(run.on(bob,
                find.text('Alf, an AI agent of Alice, asks to befriend you.')))
            .dy,
        lessThan(tester.getTopLeft(run.control(bob, 'name')).dy));
    run.plain(bob);
    await run.shoot(bob, 'gai-d-bob-asked.png');
    await run.answer(bob, 'Yes');
    expect(bob.questions('approval'), isEmpty);

    // Sign(r; x), Message(r, b; x): Bob's friend Alf, recorded with Alice its principal and its attestation (agent_of(r, p, y)), and the item Alf signed and sent him by message, signed once, by Alf (message(p?, q ; x) adds item(x) at q).  Nobody was asked for the sign or the message.
    expect(bob.atoms,
        containsAll(['friend(alf)', 'friend(alice)', 'agent_of(alf, alice, $attestation)', 'item($summary)']));
    expect(run.scriptedAtoms('alf'), containsAll(['item($summary)', 'authored($summary)']));

    // Alf's row in Bob's Friends tab names its developer (GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, item 5).
    expect(
        find.descendant(
            of: run.control(bob, 'person:alf'),
            matching: find.text('Developer: Acme Ltd')),
        findsOneWidget);

    // Bob's chat with Alf: he taps Alf's row; the item "Today's summary of the news", "Author: Alf, an AI agent of Alice", "Developer: Acme Ltd" (GAI:2, "Every act of the AI agent names its principal and its developer"), and Forward under it; under them the field Message and Send, as in GSNP's chat, GAI offering message.
    await run.tap(bob, run.name(bob, 'person:alf', 'Alf'));
    expect(run.control(bob, 'chat:alf'), findsOneWidget);
    await run.top(bob);
    final item = run.groupControl(bob, 'item:');
    expect(item, findsOneWidget);
    expect(
        [
          for (final t in run.tester.widgetList<Text>(
              find.descendant(of: item, matching: find.byType(Text))))
            t.data,
        ],
        [
          "Today's summary of the news",
          'Author: Alf, an AI agent of Alice',
          'Developer: Acme Ltd',
          'Forward'
        ]);
    expect(run.texts(bob).where((t) => t.startsWith('via')), isEmpty);
    expect(run.control(bob, 'chat-field:alf'), findsOneWidget);
    expect(run.control(bob, 'send:alf'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-a-bob-chat-alf.png');
    await run.tap(bob, run.control(bob, 'back'));

    // Retire(a, r): in her AI agents tab Alice taps Retire on Alf's row (GAI:4), which ends her friendship with it; Alf is asked nothing.  The paper's table after Retire.
    await run.tab(alice, ScreenTab.agents);
    await run.tap(alice, run.control(alice, 'retire:alf'));
    expect(alice.atoms,
        unorderedEquals(['retired_of(alf)', 'friend(bob)', 'principal_of(alf)', 'item($code)', 'authored($code)', 'identity']));
    expect(run.scriptedAtoms('alf'),
        unorderedEquals(['retired', 'item($summary)', 'authored($summary)', 'friend(bob)', 'identity', 'agent', 'principal(alice)', 'attestation($attestation)']));
    expect(
        find.descendant(
            of: run.control(alice, 'agent:alf'), matching: find.text('Retired')),
        findsOneWidget);

    // Unfriend(b, r): Bob ends his friendship with Alf, End friendship on Alf's row, unfriend_p(bob, alf), which requires at Alf only identity, so Bob ends it though he does not know that Alf is retired (Code_inbox.md, GAI #4 Cowork, 2026-10-19 16:57 UTC, item 4).  The paper's table after Unfriend: Bob's record of Alf and the item stay with him, and Alf has no friend.
    await run.tap(bob, run.control(bob, 'unfriend:alf'));
    expect(bob.atoms,
        unorderedEquals(['friend(alice)', 'item($summary)', 'agent_of(alf, alice, $attestation)', 'identity']));
    expect(run.scriptedAtoms('alf'),
        unorderedEquals(['retired', 'item($summary)', 'authored($summary)', 'identity', 'agent', 'principal(alice)', 'attestation($attestation)']));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));
}
