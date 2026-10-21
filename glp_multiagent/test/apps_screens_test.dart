/// The screens of GSC's three apps, GSG's, GSN's and GSNP's (`programs/apps/gsg`, `programs/apps/gsn`, `programs/apps/gsnp`), and of GAI's (`programs/apps/gai`), each person's phone as the bridge draws it, written to PNG files for GAI's paper (`programs/apps/<app>/screens/`); and of CSSN's mini-app (`programs/cssn`), for CSSN's paper (`programs/cssn/figures/`), below.
///
/// Each app's directory is a certified program: the canonical CVA platform of its contract, `platform.vglp`, compiled to GLP when the directory loads (vGLP, Section 6.3), and the mini-app's entry `miniapp/4` (`miniapp.glp`), which runs the platform's agent with the person's key and its dispatcher on the agent's asks.  The engine loads the directory and posts the entry for alice and bob over one conversation, as the plays of each directory do (`plays.glp`); for GSNP's app for dan, eve, carol, bob and fay, over four conversations, dan's with eve, eve's with carol, carol's with bob and bob's with fay, the persons of the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4); and for GAI's app for alice, alf and bob, over three conversations, alice's with alf, alf's with bob and bob's with alice, the agents of the example of GAI's paper (Section 3.2), alf scripted by the app's person (`programs/apps/gai/person.glp`), its script the AI agent's program, and alice and bob at their phones, the test playing the paper's Table 2 exactly.  Each person's channel is `ch(Grants?, Draws)` (vGLP, Definition "Person Channel, Person Writer, GLP with Persons, Grant"): the host hands each person's bridge every term of `Draws` as it arrives (bridge.dart), and extends `Grants` with each grant the bridge returns, `input(Id, P, R)`.
///
/// The agent writes its view, `view(S, K)`, its machine state and the persons it knows, on construct 0 when it starts and whenever either changes (vGLP b7e3517, Section 5.4, "The compilation"), drawn by `last_state`, the widget the platform declares for `Stream(Output)`.  The app declares `Menu =::= social_menu.` and `Approval =::= approval_sentence.` (`programs/apps/<app>/self.vglp`): the request question is drawn with the last view on construct 0 (vGLP e924f73, Section 6.2) as the app's tabs, Friends and, where the request type offers `create`, Groups, and each approval question as one sentence and two buttons at the top of the tab it concerns (`lib/ui_runtime/named_widgets.dart`, `tab_screen.dart`), as GAI's task words them (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, and its sentence for a join at 11:35 UTC).  Every act of a person at a phone is a tap or a keystroke on a control of the tabs, found by its key (`socialKey`, `tabKey`, `constructKey`): Join, Add, Befriend, Accept, End friendship, Send, Create, Add member and the rest; a scripted agent's acts are its scripted person's, as in the app's plays.  No term is hand-fed to the bridge or to the program.
///
/// And CSSN's mini-app (`programs/cssn`), in the scenario of CSSN's app (CSSN, Section 4.6, "In the Grassroots App", Figure "The child-safe mini-app in the grassroots app"): Alice, Bob, Carol and Dave, a conversation between each two, as CSSN's plays hand them (`programs/cssn/plays.glp`), one of them at their phone and the other three scripted by CSSN's person (`programs/cssn/person.glp`, the roles `fig_alice` to `fig_dave`), "each run has one live person, whose screen the figure shows" (the same); its screens written to `programs/cssn/figures/cssn-app-<n>.png`, n the figure's number of the screen.  CSSN's app declares `Menu =::= social_menu.` and `Approval =::= approval_sentence.` (`programs/cssn/self.vglp`), so its screens show sentences, as GAI's do, in the words Udi gave for the figure (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC): each person known with what the state says of them, "Carol, your child", and the requests composed as sentences, "Ask that Carol and Dave befriend, with Bob." and Submit ([_Cssn]).
///
/// The screens are set in Roboto, the Material font, with MaterialIcons, both loaded from the Flutter SDK's own copies (`bin/cache/artifacts/material_fonts`), in the light theme.  The scenario runs on two kinds of phone ([Screens]):
///
/// - Here, under `flutter test`: the persons' phones side by side in the test's view, each 390 by 844 logical pixels, rendered by the widget itself (RepaintBoundary.toImage) at a device pixel ratio of 3.  It takes every act and checks every screen, and writes nothing into the tree.
/// - On the iOS Simulator, an iPhone 16e, 390 by 844 points at 3 pixels a point: `integration_test/apps_screens_test.dart` plays this scenario with this file's [appsScreens], one person's phone on the screen at a time, and at each screen asks the host to take the simulator's screenshot, which the host frames as the phone and writes to `programs/apps/<app>/screens/`, and CSSN's to `programs/cssn/figures/` (`tool/sim_screens.sh`, `tool/sim_shot.py`).  Those are the PNG files of GAI's figures and of CSSN's Figure 3.  From `glp_multiagent/`:
///
///     bash tool/sim_screens.sh
library;

import 'dart:convert';
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

/// The phones the scenario plays on, and the `programs/` tree it loads.
class Screens {
  /// The checkout's `programs/`.
  final String programs;

  /// The host's service that takes the simulator's screenshot, `http://127.0.0.1:<port>` (`tool/sim_shot.py`); null when the phones are drawn side by side in the test's own view.
  final Uri? shots;

  /// The phones side by side in the test's view (`flutter test`).
  const Screens.flat(this.programs) : shots = null;

  /// One phone at a time on the iOS Simulator's screen, each screen taken by the host ([shots]).
  const Screens.simulator(this.programs, Uri this.shots);

  bool get simulator => shots != null;
}

/// Roboto and MaterialIcons, from the Flutter SDK's copies: on the simulator the SDK the host passes (`--dart-define=FLUTTER_ROOT=...`, `tool/sim_screens.sh`), its app reading the Mac's filesystem.
Future<void> _loadFonts() async {
  const defined = String.fromEnvironment('FLUTTER_ROOT');
  final roots = [
    if (defined.isNotEmpty) defined,
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
  'knows(',
  'meet(',
  'introduce(',
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
  final Screens screens;
  final String app;

  /// The app's directory under `programs/`, which the engine loads, and the directory under `programs/` its screens are written to.
  final String dir, out;

  final GlpEngine engine;
  final PostedGoal play;
  final List<_Person> persons;

  /// The screen each scripted agent's person reports, by name: the stream of what it saw (`Seen`, `programs/apps/<app>/self.glp`).
  final Map<String, HeapCell> seen;

  /// The writer of the lines a scripted parent's person reads off the phones of their children, by the parent's name ([watching]).
  final Map<String, InputInjector> watches;

  /// The children each scripted parent looks at the phones of, by the parent's name: in CSSN's app a parent's state does not change by the child's acts (CSSN, Section 4), so a scripted parent's person waits on the child's phone, `at(K, C)` (`programs/cssn/person.glp`), as in the village (`programs/cssn/plays.glp`); each line `seen(K, view(S, Ks))`, the last view on the child's phone, handed to it whenever it changes ([_look]).
  final Map<String, List<String>> watching;

  /// The last view handed to each parent of each child, by `parent:child`.
  final Map<String, String> _looked = {};

  /// On the simulator, the person whose phone is on the screen: every phone is built, one over another, and this one is drawn on top and takes the taps.
  late _Person _front = persons.first;

  _Run._(this.tester, this.screens, this.app, this.dir, this.out, this.engine,
      this.play, this.persons,
      {this.seen = const {},
      this.watches = const {},
      this.watching = const {}});

  _Person get alice => persons[0];
  _Person get bob => persons[1];

  /// The person [name] of the run.
  _Person person(String name) => persons.singleWhere((p) => p.name == name);

  /// [dir], the app's directory under `programs/`, is `apps/<app>` unless given, and [out], where its screens are written, its `screens/` unless given.  A scripted agent's person runs the script of its role in [roles], its own key where none is given; a scripted parent's in [watching] looks also at the phones of the children it names, by `village_person/5` (`programs/cssn/self.glp`).
  static Future<_Run> start(WidgetTester tester, Screens screens, String app,
      {String? dir,
      String? out,
      List<String> chain = const ['alice', 'bob'],
      List<(String, String)>? pairs,
      Set<String> scripted = const {},
      Map<String, String> roles = const {},
      Map<String, List<String>> watching = const {}}) async {
    final where = dir ?? 'apps/$app';
    await _loadFonts();
    final phones = [for (final p in chain) if (!scripted.contains(p)) p];
    if (!screens.simulator) {
      // Shadows drawn as shadows, not as the test's solid outlines; restored by [finish].  The simulator's binding draws them already.
      debugDisableShadows = false;
      tester.view.physicalSize = Size(
          phones.length * (_phone.width + 2 * _gap), _phone.height + 2 * _gap);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(tester.view.reset);
    }

    final programs = screens.programs;
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    expect(engine.loadProgram('$programs/$where'), isTrue,
        reason: 'programs/$where does not load');
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
        if (scripted.contains(chain[i]) && watching.containsKey(chain[i]))
          'village_person(${chain[i]}, ${roles[chain[i]] ?? chain[i]}, ch(D$i?, G$i), W$i?, S$i)'
        else if (scripted.contains(chain[i]))
          'miniapp_person(${chain[i]}, ${roles[chain[i]] ?? chain[i]}, ch(D$i?, G$i), S$i)',
      ],
    ];
    final play = engine.postGoal(goals.join(', '), inputs: [
      for (var i = 0; i < chain.length; i++)
        if (!scripted.contains(chain[i])) 'G$i'
        else if (watching.containsKey(chain[i])) 'W$i'
    ]);
    final run = _Run._(
        tester, screens, app, where, out ?? '$where/screens', engine, play, [
      for (var i = 0; i < chain.length; i++)
        if (!scripted.contains(chain[i]))
          _Person(chain[i], app, play.inputs['G$i']!, play.variables['D$i']!),
    ], seen: {
      for (var i = 0; i < chain.length; i++)
        if (scripted.contains(chain[i])) chain[i]: play.variables['S$i']!,
    }, watches: {
      for (var i = 0; i < chain.length; i++)
        if (scripted.contains(chain[i]) && watching.containsKey(chain[i]))
          chain[i]: play.inputs['W$i']!,
    }, watching: watching);
    run._drain();
    run._feed();
    while (run._look()) {
      run._drain();
      run._feed();
    }
    await tester.pumpWidget(run._phones());
    await tester.pumpAndSettle();
    return run;
  }

  /// What the scripted agent [name]'s person reports, each line as its term: the views, the notices, the requests it made, `did(R)`, and the answers it gave, `gave(K, A)` (`Seen`, `programs/cssn/self.glp`).
  List<String> scriptedSeen(String name) =>
      [for (final t in _drawn(seen[name]!)) formatTerm(t)];

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

  /// Put each grant the bridges returned on its person channel, run the program on it, and draw what it draws; and hand each scripted parent the views of their children's phones ([_look]), until none changes.
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
      _feed();
      if (_look()) {
        _drain();
        more = true;
      }
    }
    _feed();
    await tester.pumpAndSettle();
  }

  /// The last view on [name]'s phone, `view(S, Ks)`: a person's at their phone, as construct 0 draws it, and a scripted agent's, as its person reports it; null where none is written.
  GTerm? _lastView(String name) {
    if (seen[name] case final s?) {
      return [
        for (final t in _drawn(s))
          if (ctorArgs(t) case ('view', [GList(), GList()])) t
      ].lastOrNull;
    }
    final p = persons.where((q) => q.name == name).firstOrNull;
    final (ok, state) = SocialState.read(p?.ui.bridge.construct(0)?.view);
    if (!ok || state == null) return null;
    return GStruct('view', [GList(state.atoms), GList(state.known)]);
  }

  /// Hand each scripted parent in [watching] the last view on each child's phone they look at, `seen(K, view(S, Ks))`, where it changed since last handed; true where one was handed.
  bool _look() {
    var handed = false;
    for (final MapEntry(key: parent, value: children) in watching.entries) {
      final w = watches[parent];
      if (w == null) continue;
      for (final c in children) {
        final v = _lastView(c);
        if (v == null) continue;
        final t = formatTerm(v);
        if (_looked['$parent:$c'] == t) continue;
        _looked['$parent:$c'] = t;
        w
            .inject(runtimeTermOf(GStruct('seen', [GAtom(c), v])))
            .forEach(engine.runtime.gq.enqueue);
        handed = true;
      }
    }
    return handed;
  }

  /// The phones: side by side, each the person's agent surface on a screen of its own; on the simulator, each the person's agent surface on the whole screen, one over another, the person [_front] on top.
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
        home: screens.simulator ? _screen() : _side(),
      );

  /// On the simulator: every phone on the whole screen, [_front] last and so on top.
  Widget _screen() => Stack(
        fit: StackFit.expand,
        children: [
          for (final p in [
            for (final q in persons)
              if (q != _front) q,
            _front
          ])
            RepaintBoundary(
              key: p.shot,
              child: AgentSurface(
                  agentId: p.name, runtime: p.ui, muteNotices: true),
            ),
        ],
      );

  /// On the simulator, [p]'s phone brought to the screen, its state kept, before [p] acts or is shot.
  Future<void> show(_Person p) async {
    if (!screens.simulator || _front == p) return;
    _front = p;
    await tester.pumpWidget(_phones());
    await tester.pumpAndSettle();
  }

  /// The phones side by side in the test's view.
  Widget _side() => ColoredBox(
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
    await show(p);
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

  /// Nothing on [p]'s phone is a term, a functor, a type name, an attempt number or a Submit button, but the words [allow].
  void plain(_Person p, {Set<String> allow = const {}}) {
    final all = texts(p).join('\n');
    for (final w in _notShown) {
      if (allow.contains(w)) continue;
      expect(all.contains(w), isFalse,
          reason: '${p.name}: "$w" on the screen:\n$all');
    }
  }

  /// [p]'s open tab scrolled to its top.
  Future<void> top(_Person p) async {
    await show(p);
    FocusManager.instance.primaryFocus?.unfocus();
    final s = tester.state<ScrollableState>(
        find.descendant(of: _list(p), matching: find.byType(Scrollable)).first);
    s.position.jumpTo(0);
    await tester.pumpAndSettle();
  }

  /// [p]'s phone as it stands, the screen [file] of the app's screens: on the simulator taken by the host, which writes it to `programs/[out]/[file]`; here rendered, and written nowhere.
  Future<void> shoot(_Person p, String file) async {
    await show(p);
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    if (screens.simulator) return _take(file);
    final boundary =
        p.shot.currentContext!.findRenderObject()! as RenderRepaintBoundary;
    final png = await tester.runAsync(() async {
      final image = await boundary.toImage(pixelRatio: _ratio);
      final data = await image.toByteData(format: ui.ImageByteFormat.png);
      image.dispose();
      return data!.buffer.asUint8List();
    });
    expect(png, isNotNull);
  }

  /// On the simulator, the keyboard a field raised gone from the screen.
  Future<void> _keyboardGone() async {
    if (!screens.simulator) return;
    for (var i = 0; i < 50 && tester.view.viewInsets.bottom > 0; i++) {
      await tester.runAsync(
          () => Future<void>.delayed(const Duration(milliseconds: 100)));
      await tester.pumpAndSettle();
    }
    expect(tester.view.viewInsets.bottom, 0,
        reason: 'the keyboard is still on the screen');
  }

  /// The simulator's screen taken by the host as [file] of the app's screens, in `programs/[out]`, once the keyboard a field raised has left it.
  Future<void> _take(String file) async {
    await _keyboardGone();
    final (status, body) = (await tester.runAsync(() async {
      final client = HttpClient();
      try {
        final request = await client.getUrl(screens.shots!.replace(
            path: '/shot',
            queryParameters: {'app': app, 'dir': out, 'file': file}));
        final response = await request.close();
        return (
          response.statusCode,
          await response.transform(utf8.decoder).join()
        );
      } finally {
        client.close();
      }
    }))!;
    expect(status, HttpStatus.ok, reason: body);
    // ignore: avoid_print
    print(body);
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

  /// [p] asks to meet [q] in person (GKK:2): Meet on [q]'s row; and [q], asked "Alice asks to meet you in person." at the top of their first tab, taps Yes.  Each then knows the other, `knows(Q)`.  Where [shot] is given, [q]'s phone is shot asked.
  Future<void> meetInPerson(_Person p, _Person q, {String? shot}) async {
    await tap(p, control(p, 'meet:${q.name}'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(attempt(${p.name}, '));
    expect(formatTerm(a.view),
        endsWith(', meet(${p.name}, ${q.name}), none), input([2]))'));
    await top(q);
    final asks = '${personName(GAtom(p.name))} asks to meet you in person.';
    expect(textsOf(q, a), [asks, 'Yes', 'No']);
    // At the top of the tab, before the person's own name.
    expect(tester.getTopLeft(on(q, find.text(asks))).dy,
        lessThan(tester.getTopLeft(control(q, 'name')).dy));
    if (shot != null) {
      plain(q);
      await shoot(q, shot);
    }
    await answer(q, 'Yes');
    expect(q.questions('approval'), isEmpty);
    expect(p.atoms, contains('knows(${q.name})'));
    expect(q.atoms, contains('knows(${p.name})'));
  }

  /// [p] offers [q] friendship (GSG:1): Befriend on [q]'s row; and [q], asked, taps Accept.
  Future<void> befriend(_Person p, _Person q, {String? shot}) async {
    await tap(p, control(p, 'befriend:${q.name}'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(attempt(${p.name}, '));
    expect(formatTerm(a.view),
        endsWith(', befriend(${p.name}, ${q.name}), none), input([2]))'));
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
  void finish() {
    if (!screens.simulator) debugDisableShadows = true;
  }
}

// === CSSN's mini-app ========================================================

/// The persons of the scenario of CSSN's app (CSSN, Section 4.6, "In the Grassroots App"): Alice and Bob, adults, and Carol and Dave, Alice's child and Bob's; a conversation between each two, as CSSN's plays hand every two agents one (`programs/cssn/plays.glp`); and the role whose script each runs in every run in which it is scripted (`programs/cssn/person.glp`, `fig_alice` to `fig_dave`).
const List<String> _cssnFour = ['alice', 'bob', 'carol', 'dave'];
const List<(String, String)> _cssnPairs = [
  ('alice', 'bob'),
  ('alice', 'carol'),
  ('alice', 'dave'),
  ('bob', 'carol'),
  ('bob', 'dave'),
  ('carol', 'dave'),
];
const Map<String, String> _cssnRoles = {
  'alice': 'fig_alice',
  'bob': 'fig_bob',
  'carol': 'fig_carol',
  'dave': 'fig_dave',
};

/// CSSN's mini-app (`programs/cssn`) for the four, [live] at their phone and the other three scripted ("Each run has one live person, whose screen the figure shows, the other three being scripted persons that read the draws and grant on the constructs", CSSN, Section 4.6, "In the Grassroots App"), its screens written to `programs/cssn/figures`.  A scripted Alice looks at Carol's phone, on which she waits for Carol and Dave to have met before she asks that they befriend, her own state not changing by the meeting (`programs/cssn/person.glp`, `fig_alice`).
Future<_Run> _cssn(WidgetTester tester, Screens screens, String live) =>
    _Run.start(tester, screens, 'cssn',
        dir: 'cssn',
        out: 'cssn/figures',
        chain: _cssnFour,
        pairs: _cssnPairs,
        scripted: {for (final p in _cssnFour) if (p != live) p},
        roles: _cssnRoles,
        watching: live == 'alice'
            ? const {}
            : const {
                'alice': ['carol']
              });

/// A person's acts on CSSN's app as its declared widgets draw it (named_widgets.dart, `socialMenu` and `approvalSentence`): a request composed as a sentence, a person chosen into each of its places from a picker, and Submit (`_composers`).
extension _Cssn on _Run {
  /// [p] composes the request [kind], choosing each of [qs] into its places in order: each place tapped, which opens its picker, and the person picked.
  Future<void> compose(_Person p, String kind, List<String> qs) async {
    for (var i = 0; i < qs.length; i++) {
      await tap(p, control(p, 'slot:$kind:$i'));
      await tap(p, control(p, 'pick:$kind:$i:${qs[i]}'));
    }
  }

  /// The sentence of the request [kind] on [p]'s open request question, each place as the person chosen into it reads.
  String sentence(_Person p, String kind) {
    final rich = tester.widget<Text>(control(p, 'sentence:$kind'));
    var plain = rich.textSpan!.toPlainText();
    for (final t in tester.widgetList<Text>(find.descendant(
        of: control(p, 'sentence:$kind'), matching: find.byType(Text)))) {
      if (t.data != null) plain = plain.replaceFirst('￼', t.data!);
    }
    return plain;
  }

  /// The lines of the requests [kind] [p] sent, as drawn on the open request question.
  List<String> sent(_Person p, String kind) => [
        for (final t in tester.widgetList<Text>(on(
            p,
            find.byWidgetPredicate((w) =>
                w.key is ValueKey<String> &&
                (w.key as ValueKey<String>)
                    .value
                    .startsWith('${p.request.id}:social:sent:$kind:')))))
          t.data!,
      ];

  /// The names on the rows of the persons on [p]'s Friends tab, in order, each as drawn: the person's name and what the state says of them.
  List<String?> rowTitles(_Person p) => [
        for (final t in tester.widgetList<ListTile>(on(
            p,
            find.byWidgetPredicate((w) =>
                w is ListTile &&
                w.key is ValueKey<String> &&
                (w.key as ValueKey<String>)
                    .value
                    .startsWith('${p.request.id}:social:person:')))))
          (t.title as Text).data,
      ];

  /// [p]'s open tab scrolled so that [f] is at the foot of the screen, once the keyboard a field raised has left it.
  Future<void> foot(_Person p, Finder f) async {
    await show(p);
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    await _keyboardGone();
    await Scrollable.ensureVisible(tester.element(f), alignment: 1.0);
    await tester.pumpAndSettle();
  }
}

/// The scenario on the phones side by side in the test's view, reading the checkout's `programs/`.
void main() => appsScreens(Screens.flat(programsDir()));

/// The scenario, the four apps' screens in their figures' order, on [screens]: here, and on the simulator (`integration_test/apps_screens_test.dart`).
void appsScreens(Screens screens) {
  testWidgets(
      'GKK\'s app on three phones: Alice establishes her identity; Bob asked '
      'to meet Alice in person; and Bob\'s contacts once Alice has '
      'introduced Carol to him', (tester) async {
    final run = await _Run.start(tester, screens, 'gkk',
        chain: const ['alice', 'bob', 'carol'],
        pairs: const [('alice', 'bob'), ('alice', 'carol')]);
    final alice = run.alice;
    final bob = run.bob;
    final carol = run.person('carol');

    // One tab, Contacts: GKK offers meeting and no friendship, and no group.
    expect(
        run.on(alice,
            find.descendant(
                of: find.byKey(tabKey(ScreenTab.friends)),
                matching: find.text('Contacts'))),
        findsOneWidget);
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsNothing);

    // (a) Alice establishes her identity (GKK:1): her name and Join, which proposes establish(alice), before she taps it.
    expect(run.control(alice, 'name'), findsOneWidget);
    expect(run.control(alice, 'join'), findsOneWidget);
    expect(alice.atoms, isEmpty);
    run.plain(alice);
    await run.shoot(alice, 'gkk-a-alice-identity.png');
    await run.join(alice);
    expect(alice.atoms, ['identity']);

    // Bob and Carol join; Alice and Bob add each other by key, and Alice and Carol.  Each agent then knows the other, and Bob's row on Alice's phone offers Meet.
    await run.meet(alice, bob);
    await run.meet(alice, carol);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(ElevatedButton, 'Meet')),
        findsOneWidget);

    // (b) Alice taps Meet on Bob's row (GKK:2): Bob's Contacts tab, the question "Alice asks to meet you in person." and Yes and No at its top; he taps Yes, and each knows the other.  Then Alice and Carol meet.
    await run.meetInPerson(alice, bob, shot: 'gkk-b-bob-asked-to-meet.png');
    await run.meetInPerson(alice, carol);
    expect(alice.atoms,
        unorderedEquals(['identity', 'knows(bob)', 'knows(carol)']));
    expect(bob.atoms, unorderedEquals(['identity', 'knows(alice)']));

    // Alice, who knows Bob and Carol, introduces Carol to Bob (GKK:3): she composes "Introduce Carol to Bob." and taps Introduce, which proposes introduce(alice, bob, carol), in the order of introduce's head, introduce(p?, q? ; c) (programs/gsc/GKK.glp).
    await run.compose(alice, 'introduce', ['carol', 'bob']);
    expect(run.sentence(alice, 'introduce'), 'Introduce Carol to Bob.');
    await run.tap(alice, run.control(alice, 'submit:introduce'));
    expect(run.sent(alice, 'introduce'),
        ['You asked to introduce Carol to Bob.']);

    // Bob is asked, "Alice introduces Carol to you.", with Yes and No, and taps Yes; he then knows Carol, and Carol is not asked.
    final a = bob.approval;
    expect(formatTerm(a.view),
        endsWith(', introduce(alice, bob, carol), none), input([2]))'));
    await run.top(bob);
    expect(run.textsOf(bob, a), ['Alice introduces Carol to you.', 'Yes', 'No']);
    run.plain(bob);
    await run.answer(bob, 'Yes');
    expect(bob.questions('approval'), isEmpty);
    expect(carol.questions('approval'), isEmpty);
    expect(bob.atoms,
        unorderedEquals(['identity', 'knows(alice)', 'knows(carol)']));

    // (c) Bob's contacts once he has approved: Alice and Carol.
    await run.top(bob);
    expect(run.control(bob, 'person:alice'), findsOneWidget);
    expect(run.control(bob, 'person:carol'), findsOneWidget);
    expect(run.texts(bob), containsAllInOrder(['Bob', 'Alice', 'Carol']));
    run.plain(bob);
    await run.shoot(bob, 'gkk-c-bob-contacts.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSG\'s app on two phones: Bob asked to be Alice\'s friend; Alice\'s '
      'Friends with Bob her friend; and after Bob ends the friendship',
      (tester) async {
    final run = await _Run.start(tester, screens, 'gsg');
    final alice = run.alice;
    final bob = run.bob;

    // One tab, Friends: GSG offers no group.
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.friends))), findsOneWidget);
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsNothing);

    // Before her identity is established, her name and Join, which proposes establish(alice).
    expect(run.control(alice, 'name'), findsOneWidget);
    expect(run.control(alice, 'join'), findsOneWidget);
    expect(alice.atoms, isEmpty);

    // Each joins (GKK:1) and adds the other by key; each agent then knows the other, and Bob's row offers Meet, not Befriend: befriend requires that the two know each other (GSG:1).
    await run.meet(alice, bob);
    expect(run.control(alice, 'join'), findsNothing);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(ElevatedButton, 'Meet')),
        findsOneWidget);
    expect(run.control(alice, 'befriend:bob'), findsNothing);

    // Alice asks to meet Bob in person (GKK:2) and he taps Yes; each then knows the other, and Bob's row offers Befriend.
    await run.meetInPerson(alice, bob);
    expect(
        find.descendant(
            of: run.control(alice, 'person:bob'),
            matching: find.widgetWithText(ElevatedButton, 'Befriend')),
        findsOneWidget);
    expect(run.control(alice, 'meet:bob'), findsNothing);

    // (a) Alice taps Befriend on Bob's row (GSG:1): Bob's Friends tab, the question "Alice asks to be your friend." and Accept and Decline at its top.
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

    // Bob ends the friendship (GSG:2), his own act: End friendship on Alice's row.  Alice is not asked, and each still knows the other.
    await run.tap(bob, run.control(bob, 'unfriend:alice'));
    expect(alice.questions('approval'), isEmpty);
    expect(alice.atoms, unorderedEquals(['identity', 'knows(bob)']));
    expect(bob.atoms, unorderedEquals(['identity', 'knows(alice)']));

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
    final run = await _Run.start(tester, screens, 'gsn');
    final alice = run.alice;
    final bob = run.bob;
    // The group friends, led by Alice: its name, a speech act of hers, and its community name, <alice>.friends, which a member atom holds, one argument (programs/apps/gsn/platform.vglp, Atom and CName).
    const friends = 'speech(utterance(friends), alice)';
    const friendsGroup = 'dot(community(alice), $friends)';

    // Two tabs: GSN's request type offers create.
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsOneWidget);

    // Identity, knowing each other, meeting in person and friendship, on the Friends tab as in GSG's app.
    await run.meet(alice, bob);
    await run.meetInPerson(alice, bob);
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
        endsWith(', join(alice, bob, $friends), none), input([2]))'));
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
      'Forward; the warranty question once he picks Fay; and Fay\'s chat '
      'with Bob once he forwards it', (tester) async {
    final run = await _Run.start(tester, screens, 'gsnp',
        chain: const ['dan', 'eve', 'carol', 'bob', 'fay']);
    final dan = run.person('dan');
    final eve = run.person('eve');
    final carol = run.person('carol');
    final bob = run.person('bob');
    final fay = run.person('fay');

    // The friendships of the play (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4): Dan and Eve, Eve and Carol, Carol and Bob, Bob and Fay, each two adding each other on their Friends tabs, meeting in person, the first tapping Meet and the second Yes (GKK:2), and then befriending, the first tapping Befriend and the second Accept (GSG:1), befriend requiring that the two know each other (programs/gai/GSNP.glp at GLP 35c4211a).
    for (final (p, q) in [(dan, eve), (eve, carol), (carol, bob), (bob, fay)]) {
      await run.meet(p, q);
      await run.meetInPerson(p, q);
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

    // Bob taps Yes, which proposes forward(bob, fay, x) (GSNP:2, Forwarding): Fay holds the item signed again by Bob.  Nobody is asked.
    await run.tap(bob, run.groupControl(bob, 'forward-yes:'));
    const fromBob = 'speech($fromCarol, bob)';
    expect(fay.atoms, contains('item($fromBob)'));
    expect(fay.questions('approval'), isEmpty);

    // (c) Fay's chat with Bob (GAI #5 Cowork, Code_inbox.md, 2026-10-19 22:29 UTC): she taps his row; the item "Flood on Main Street", "Author: Dan" and "via Eve, via Carol, via Bob", and Forward under it.
    await run.tap(fay, run.name(fay, 'person:bob', 'Bob'));
    expect(run.control(fay, 'chat:bob'), findsOneWidget);
    await run.top(fay);
    final forwarded = run.groupControl(fay, 'item:');
    expect(forwarded, findsOneWidget);
    expect(
        [
          for (final t in run.tester.widgetList<Text>(
              find.descendant(of: forwarded, matching: find.byType(Text))))
            t.data,
        ],
        [
          'Flood on Main Street',
          'Author: Dan',
          'via Eve, via Carol, via Bob',
          'Forward'
        ]);
    run.plain(fay);
    await run.shoot(fay, 'gsnp-c-fay-chat.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GAI\'s app, the example of GAI\'s paper in the order of its Table 2, '
      'Alice and Bob at their phones and Alf\'s program scripted: Alice '
      'creating Alf in her AI agents tab; Alf and Bob meeting, Alice and Bob '
      'asked; Alice and Bob asked to approve Alf befriending Bob; Bob\'s '
      'Friends tab once Alf is his friend, Alf\'s row '
      'naming its principal and its developer; and Bob\'s chat with Alf, its '
      'item naming its principal and its developer', (tester) async {
    // Alice and Bob, persons, and Alf, Alice's AI agent, a conversation between each two (GAI's paper, Section 3.2, the example and Table "The components that change in the example"), on GAI's contract on GKK (programs/gai/GAI.glp at GLP 9d736804).  The test plays Table 2 exactly, acting for Alice and Bob at their phones (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 3; GAI #6 Cowork, 2026-10-20 20:12 UTC, item 4); Alf is scripted (programs/apps/gai/person.glp), its script the AI agent's program, which takes its acts: once entered, it proposes to meet Bob with its attestation, Alice its principal, and once they know each other, to befriend him, Alice its principal, and once his friend it signs the item "Today's summary of the news" (GSNP:1) and sends it to him by message (GSN:1).  The code of Alf is 'The code of Alf' signed by its developer acme_ltd, drawn Acme Ltd (the same, item 1).
    final run = await _Run.start(tester, screens, 'gai',
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

    // Establish(a), Establish(b): each joins (GKK:1), and each adds the other by key; Bob adds Alf too, and Alf's program asks to know Alice.
    await run.meet(alice, bob);
    if (!alice.known.contains('alf')) await run.add(alice, 'alf');
    await run.add(bob, 'alf');

    // Meet(a, b): Alice taps Meet on Bob's row, and Bob, asked, taps Yes (GKK:2).
    await run.meetInPerson(alice, bob);

    // Befriend(a, b): Alice taps Befriend on Bob's row, and Bob, asked, taps Accept (GSG:1).
    await run.befriend(alice, bob);

    // Sign(a; k), Enter(a, r; k): in her AI agents tab Alice types Alf's name, its code and its developer, and taps Create, which signs the code, sign(alice, k), and, once her view holds item(k), creates Alf with it, enter(alice, alf, k) (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 2); Alf's program answers yes, and Alf holds its attestation, the code signed over by Alice, and each knows the other (GnW:1, GAI:1, GAI:2).
    await run.tab(alice, ScreenTab.agents);
    await run.type(alice, run.control(alice, 'agent-name'), 'alf');
    await run.type(alice, run.control(alice, 'agent-code'), 'The code of Alf');
    await run.type(alice, run.control(alice, 'agent-developer'), 'acme_ltd');
    await run.tap(alice, run.control(alice, 'create-agent'));
    for (var i = 0; i < 5 && !alice.atoms.contains('principal_of(alf)'); i++) {
      await run.settle();
    }
    expect(alice.atoms,
        containsAll(['item($code)', 'authored($code)', 'principal_of(alf)', 'friend(alf)', 'knows(alf)', 'friend(bob)', 'knows(bob)']));
    expect(run.scriptedAtoms('alf'),
        containsAll(['identity', 'agent', 'principal(alice)', 'friend(alice)', 'knows(alice)', 'attestation($attestation)']));

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

    // Agent meet(r, b, a; y): Alf proposes to meet Bob with Alice its principal, presenting its attestation, and Alice and Bob are asked (GnW:2, GAI:2).  At Alice: "Alf asks to meet Bob.", "Approve?", Yes and No; at Bob, who sees Alf's principal and developer at the meeting: "Alf, an AI agent of Alice, asks to meet you.", "Developer: Acme Ltd", "Meet Alf?", Yes and No (the words drawn by analogy with Agent befriend's, Code #12's brief for GAI #6's task, 2026-10-20 20:12 UTC, item 4).
    final am = alice.approval;
    expect(formatTerm(am.view), startsWith('approval(attempt(alf, '));
    expect(formatTerm(am.view),
        endsWith(', agent_meet(alf, bob, alice, $attestation), none), input([2]))'));
    await run.tab(alice, ScreenTab.friends);
    await run.top(alice);
    expect(run.textsOf(alice, am),
        ['Alf asks to meet Bob.', 'Approve?', 'Yes', 'No']);
    await run.answer(alice, 'Yes');
    final bm = bob.approval;
    expect(formatTerm(bm.view),
        endsWith(', agent_meet(alf, bob, alice, $attestation), none), input([2]))'));
    await run.top(bob);
    expect(run.textsOf(bob, bm), [
      'Alf, an AI agent of Alice, asks to meet you.',
      'Developer: Acme Ltd',
      'Meet Alf?',
      'Yes',
      'No'
    ]);
    // While the question is open, Alf's row offers no Meet and no Befriend: the question names Alf an AI agent, and meet and befriend forbid agent at both parties (programs/gai/GAI.glp).
    expect(run.control(bob, 'person:alf'), findsOneWidget);
    expect(run.control(bob, 'meet:alf'), findsNothing);
    expect(run.control(bob, 'befriend:alf'), findsNothing);
    await run.answer(bob, 'Yes');
    expect(bob.atoms,
        containsAll(['knows(alf)', 'agent_of(alf, alice, $attestation)']));
    expect(run.scriptedAtoms('alf'), contains('knows(bob)'));

    // Agent befriend(r, b, a): Alf, who knows Bob by that meeting, proposes to befriend him with Alice its principal, and Alice and Bob are asked (GnW:2); Bob holds Alf recorded, agent_of(alf, alice, y), as every agent an AI agent knows does (GAI's paper, Section 3.3, part 2).
    final aa = alice.approval;
    expect(formatTerm(aa.view), startsWith('approval(attempt(alf, '));
    expect(formatTerm(aa.view),
        endsWith(', agent_befriend(alf, bob, alice), none), input([2]))'));

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

    // (c) Bob's Friends tab, badged: "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme Ltd", "Befriend Alf?", Yes and No at its top; the developer the first signer of the attestation of Bob's record of Alf, agent_of(alf, alice, y), which the meeting added (GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, (c) and item 5).
    final ba = bob.approval;
    expect(formatTerm(ba.view),
        endsWith(', agent_befriend(alf, bob, alice), none), input([2]))'));
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
    // While the question is open, Alf's row offers no Befriend: the question names Alf an AI agent, and befriend forbids agent at both parties (programs/gai/GAI.glp).
    expect(run.control(bob, 'person:alf'), findsOneWidget);
    expect(run.control(bob, 'meet:alf'), findsNothing);
    expect(run.control(bob, 'befriend:alf'), findsNothing);
    run.plain(bob);
    await run.shoot(bob, 'gai-d-bob-asked.png');
    await run.answer(bob, 'Yes');
    expect(bob.questions('approval'), isEmpty);

    // Sign(r; x), Message(r, b; x): Bob's friend Alf, recorded with Alice its principal and its attestation (agent_of(r, p, y)), and the item Alf signed and sent him by message, signed once, by Alf (message(p?, q ; x) adds item(x) at q).  Nobody was asked for the sign or the message.
    expect(bob.atoms,
        containsAll(['friend(alf)', 'friend(alice)', 'agent_of(alf, alice, $attestation)', 'item($summary)']));
    expect(run.scriptedAtoms('alf'), containsAll(['item($summary)', 'authored($summary)']));

    // (d) Bob's Friends tab after Alf befriends him, right after his Yes: Alf listed with "an AI agent of Alice" and "Developer: Acme Ltd" under his name, read from agent_of(alf, alice, y), and the item he sent, with End friendship, unfriend_p of a friend who is not Bob's own AI agent, and no Befriend; and Alice, his friend, with End friendship (GAI #5 Cowork, Code_inbox.md, 2026-10-20 08:01 UTC; GAI's paper, Section 3.1: "every friend of an AI agent sees its principal and its developer"; the developer line, the same, 2026-10-19 20:36 UTC, item 5).
    await run.top(bob);
    List<String?> row(String q) => [
          for (final t in tester.widgetList<Text>(find.descendant(
              of: run.control(bob, 'person:$q'), matching: find.byType(Text))))
            t.data,
        ];
    expect(row('alf'), [
      'A',
      'Alf',
      'an AI agent of Alice',
      'Developer: Acme Ltd',
      "Today's summary of the news",
      'End friendship'
    ]);
    expect(run.control(bob, 'unfriend:alf'), findsOneWidget);
    expect(run.control(bob, 'befriend:alf'), findsNothing);
    expect(row('alice'), ['A', 'Alice', 'End friendship']);
    expect(run.control(bob, 'unfriend:alice'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-e-bob-friends.png');

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
        unorderedEquals(['retired_of(alf)', 'friend(bob)', 'principal_of(alf)', 'knows(alf)', 'item($code)', 'authored($code)', 'knows(bob)', 'identity']));
    expect(run.scriptedAtoms('alf'),
        unorderedEquals(['retired', 'item($summary)', 'authored($summary)', 'friend(bob)', 'knows(bob)', 'identity', 'agent', 'principal(alice)', 'knows(alice)', 'attestation($attestation)']));
    expect(
        find.descendant(
            of: run.control(alice, 'agent:alf'), matching: find.text('Retired')),
        findsOneWidget);

    // Unfriend(b, r): Bob ends his friendship with Alf, End friendship on Alf's row, unfriend_p(bob, alf), which requires at Alf only identity, so Bob ends it though he does not know that Alf is retired (Code_inbox.md, GAI #4 Cowork, 2026-10-19 16:57 UTC, item 4).  The paper's table after Unfriend: Bob's record of Alf and the item stay with him, and Alf has no friend.
    await run.tap(bob, run.control(bob, 'unfriend:alf'));
    expect(bob.atoms,
        unorderedEquals(['friend(alice)', 'item($summary)', 'agent_of(alf, alice, $attestation)', 'knows(alf)', 'knows(alice)', 'identity']));
    expect(run.scriptedAtoms('alf'),
        unorderedEquals(['retired', 'item($summary)', 'authored($summary)', 'knows(bob)', 'identity', 'agent', 'principal(alice)', 'knows(alice)', 'attestation($attestation)']));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'CSSN\'s mini-app, the scenario of CSSN, Section 4.6, Alice at her '
      'phone: (1) her menu, Carol her child, Bob her friend and Dave; (2) her '
      'request that Carol and Dave befriend, with Bob, as she composes it, '
      'after they have met', (tester) async {
    final run = await _cssn(tester, screens, 'alice');
    final alice = run.person('alice');
    // Two tabs, Friends and Groups, CSSN's request type offering create; the request question drawn by social_menu and the approval questions by approval_sentence (programs/cssn/self.vglp).
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.friends))), findsOneWidget);
    expect(run.on(alice, find.byKey(tabKey(ScreenTab.groups))), findsOneWidget);
    expect(namedWidget(alice.request.widget)?.$1, socialMenuName);
    // Alice establishes her identity (GKK:1): Join.
    await run.join(alice);
    expect(alice.atoms, ['identity']);
    // Carol enters with Alice: Alice and Carol's phone each ask to know the other, and Alice enters Carol (CSSN:1, GnW:1; enter(p?, r?)), "Enter Carol as your child." and Submit, Carol's phone answering yes (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 4); the two then know each other (CSSN.glp at GLP 6ff16d78).
    await run.add(alice, 'carol');
    expect(alice.known, ['carol']);
    await run.compose(alice, 'enter', ['carol']);
    expect(run.sentence(alice, 'enter'), 'Enter Carol as your child.');
    await run.tap(alice, run.control(alice, 'submit:enter'));
    expect(alice.atoms, unorderedEquals(
        ['identity', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    expect(run.scriptedSeen('carol'), contains('gave(enter(alice, carol), yes)'));
    expect(run.sent(alice, 'enter'), ['You entered Carol as your child.']);
    // Dave has entered with Bob the same way; Alice and Bob each ask to know the other.
    expect(run.scriptedAtoms('bob'), unorderedEquals(
        ['identity', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    await run.add(alice, 'bob');
    // Alice and Bob meet (GKK:2): Meet on Bob's row, in GKK's words as in every app that extends GKK (GSC #5 Cowork, Code_inbox.md, 2026-10-20 20:21 UTC), Bob answering yes; Bob's agent then introduces Alice's and Dave's to each other, Bob knowing both, knows being the introduction predicate (CSSN, Section 4.6.1, "The program").
    await run.tap(alice, run.control(alice, 'meet:bob'));
    expect(run.scriptedSeen('bob'), contains('gave(meet(alice, bob), yes)'));
    expect(alice.atoms, contains('knows(bob)'));
    // Alice and Bob befriend (GSG:1): Befriend on Bob's row, Bob now known to her, and Bob answers yes.
    await run.tap(alice, run.control(alice, 'befriend:bob'));
    expect(alice.atoms, contains('friend(bob)'));
    expect(alice.known, ['bob', 'carol', 'dave']);
    // (1) Alice's menu: the persons her agent knows, each with what her state says of them, "Carol, your child", "Bob, your friend", "Dave", read off the last view; Carol's row offers no End friendship, unfriend forbidding parenting(q) at p, and Dave's Meet, Alice not having met him, and no Befriend; and the requests she may make.
    await run.top(alice);
    expect(run.rowTitles(alice), ['Carol, your child', 'Bob, your friend', 'Dave']);
    expect(run.control(alice, 'unfriend:carol'), findsNothing);
    expect(run.control(alice, 'unfriend:bob'), findsOneWidget);
    expect(run.control(alice, 'meet:dave'), findsOneWidget);
    expect(run.control(alice, 'befriend:dave'), findsNothing);
    expect(run.sentence(alice, 'child-meet-2'), 'Ask that … and … meet, with ….');
    expect(run.sentence(alice, 'child-befriend'), 'Ask that … and … befriend, with ….');
    run.plain(alice, allow: {'Submit'});
    await run.shoot(alice, 'cssn-app-1.png');
    // Alice asks that Carol and Dave meet, with Bob (CSSN:2; child_meet_2(r?, s?, p?, q?)): "Ask that Carol and Dave meet, with Bob." and Submit; Bob, Carol and Dave approve, and Carol and Dave know each other.
    await run.compose(alice, 'child-meet-2', ['carol', 'dave', 'bob']);
    expect(run.sentence(alice, 'child-meet-2'),
        'Ask that Carol and Dave meet, with Bob.');
    await run.tap(alice, run.control(alice, 'submit:child-meet-2'));
    expect(run.sent(alice, 'child-meet-2'),
        ['You asked that Carol and Dave meet, with Bob.']);
    for (final q in ['bob', 'carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_meet_2(carol, dave, alice, bob), yes)'));
    }
    expect(run.scriptedAtoms('carol'), contains('knows(dave)'));
    expect(run.scriptedAtoms('dave'), contains('knows(carol)'));
    // (2) Alice's request that Carol and Dave befriend, with Bob, as she composes it: "Ask that Carol and Dave befriend, with Bob." and Submit, child_befriend(carol, dave, alice, bob), its roles r, s, p and q in order (CSSN, Section 4.6, "Child befriending in vGLP").
    await run.compose(alice, 'child-befriend', ['carol', 'dave', 'bob']);
    expect(run.sentence(alice, 'child-befriend'),
        'Ask that Carol and Dave befriend, with Bob.');
    final submit = run.control(alice, 'submit:child-befriend');
    expect(tester.widget<ElevatedButton>(submit).onPressed, isNotNull);
    expect(alice.unsent, isEmpty);
    await run.foot(alice, submit);
    run.plain(alice, allow: {'Submit'});
    await run.shoot(alice, 'cssn-app-2.png');
    // Once sent: "You asked that Carol and Dave befriend, with Bob."  Bob, Carol and Dave approve, and Carol and Dave are friends (CSSN:3); Carol then ends the friendship (GSG:2).
    await run.tap(alice, submit);
    expect(run.sent(alice, 'child-befriend'),
        ['You asked that Carol and Dave befriend, with Bob.']);
    for (final q in ['bob', 'carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_befriend(carol, dave, alice, bob), yes)'));
    }
    expect(run.scriptedSeen('carol'),
        contains(startsWith('view([friend(dave), ')));
    expect(run.scriptedSeen('carol'),
        contains('did(propose(unfriend(carol, dave)))'));
    expect(run.scriptedAtoms('carol'), isNot(contains('friend(dave)')));
    expect(run.scriptedAtoms('dave'), isNot(contains('friend(carol)')));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'CSSN\'s mini-app, the scenario of CSSN, Section 4.6, Bob at his phone: '
      'asked to meet Alice and to approve the meeting of Carol and Dave; (3) '
      'asked to approve Alice\'s attempt that they befriend, "Approve?", with '
      'Yes and No; (6) his request declaring Dave of age', (tester) async {
    final run = await _cssn(tester, screens, 'bob');
    final bob = run.person('bob');
    // Bob establishes his identity (GKK:1); Dave enters with Bob, Bob and Dave's phone each asking to know the other and Dave's phone answering yes (CSSN:1, GnW:1).
    await run.join(bob);
    await run.add(bob, 'dave');
    await run.compose(bob, 'enter', ['dave']);
    await run.tap(bob, run.control(bob, 'submit:enter'));
    expect(bob.atoms, unorderedEquals(
        ['identity', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    // Carol has entered with Alice; Bob and Alice each ask to know the other, and Alice asks to meet Bob (GKK:2): "Alice asks to meet you in person.", Yes and No, GKK's words, which serve every app that extends GKK (GSC #5 Cowork, Code_inbox.md, 2026-10-20 20:21 UTC).
    expect(run.scriptedAtoms('alice'), unorderedEquals(
        ['identity', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    await run.add(bob, 'alice');
    expect(formatTerm(bob.approval.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', meet(alice, bob), none), input([2]))')));
    expect(run.textsOf(bob, bob.approval),
        ['Alice asks to meet you in person.', 'Yes', 'No']);
    await run.answer(bob, 'Yes');
    expect(bob.atoms, contains('knows(alice)'));
    // Alice proposes that they befriend (GSG:1), and Bob, asked, taps Accept.
    expect(formatTerm(bob.approval.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', befriend(alice, bob), none), input([2]))')));
    expect(run.textsOf(bob, bob.approval),
        ['Alice asks to be your friend.', 'Accept', 'Decline']);
    await run.answer(bob, 'Accept');
    expect(bob.atoms, contains('friend(alice)'));
    // Alice asks that Carol and Dave meet, with Bob (CSSN:2): "Alice asks that their child Carol and your child Dave meet.", "Approve?"; Carol and Dave have approved it, and Bob taps Yes.
    final m = bob.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', child_meet_2(carol, dave, alice, bob), none), input([2]))')));
    expect(run.textsOf(bob, m), [
      'Alice asks that their child Carol and your child Dave meet.',
      'Approve?',
      'Yes',
      'No'
    ]);
    await run.answer(bob, 'Yes');
    expect(run.scriptedAtoms('dave'), contains('knows(carol)'));
    // (3) Alice, Carol's phone showing that Carol knows Dave, requests that Carol and Dave befriend, with Bob, her agent knowing Dave's by Bob's agent's introduction; Bob is asked: "Alice asks that their child Carol and your child Dave befriend.", "Approve?", Yes and No, the initiator "their" (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 1).  Carol and Dave have approved it.
    final a = bob.approval;
    expect(formatTerm(a.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', child_befriend(carol, dave, alice, bob), none), input([2]))')));
    for (final q in ['carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_befriend(carol, dave, alice, bob), yes)'));
    }
    await run.top(bob);
    expect(run.textsOf(bob, a), [
      'Alice asks that their child Carol and your child Dave befriend.',
      'Approve?',
      'Yes',
      'No'
    ]);
    expect(
        tester.getTopLeft(run.on(bob, find.byKey(constructKey(a.id)))).dy,
        lessThan(tester.getTopLeft(run.control(bob, 'name')).dy));
    run.plain(bob, allow: {'Submit'});
    await run.shoot(bob, 'cssn-app-3.png');
    await run.answer(bob, 'Yes');
    expect(bob.questions('approval'), isEmpty);
    // Carol and Dave are friends (CSSN:3), and Carol ends the friendship (GSG:2).
    expect(run.scriptedSeen('dave'),
        contains(startsWith('view([friend(carol), ')));
    expect(run.scriptedAtoms('dave'), isNot(contains('friend(carol)')));
    expect(run.scriptedAtoms('carol'), isNot(contains('friend(dave)')));
    // (6) Bob's request declaring Dave of age, as he composes it: "Declare Dave of age." and Submit (CSSN:4, GnW:4; of_age(p?, r)); once sent, "Dave is of age.", and nobody is asked: it ends his parenting of Dave.
    await run.compose(bob, 'of-age', ['dave']);
    expect(run.sentence(bob, 'of-age'), 'Declare Dave of age.');
    final submit = run.control(bob, 'submit:of-age');
    await run.foot(bob, submit);
    run.plain(bob, allow: {'Submit'});
    await run.shoot(bob, 'cssn-app-6.png');
    await run.tap(bob, submit);
    expect(run.sent(bob, 'of-age'), ['Dave is of age.']);
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, unorderedEquals(['identity', 'friend(dave)', 'knows(dave)',
        'friend(alice)', 'knows(alice)']));
    expect(run.scriptedAtoms('dave'), unorderedEquals(
        ['identity', 'friend(bob)', 'knows(bob)', 'knows(carol)']));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'CSSN\'s mini-app, the scenario of CSSN, Section 4.6, Carol at her '
      'phone: asked to meet Dave, "Meet Dave?"; (4) asked to approve Alice\'s '
      'attempt that they befriend, "Befriend Dave?"; (5) her menu, Alice her '
      'parent, Dave her friend and Bob; and her ending of the friendship with '
      'Dave', (tester) async {
    final run = await _cssn(tester, screens, 'carol');
    final carol = run.person('carol');
    // Carol's phone at its setup, the parents holding it (CSSN, Section 4.6, "The User Interface"): no identity; it asks to know Alice, Alice enters Carol (CSSN:1, GnW:1; enter(p?, r?)), and Carol's phone, asked "Alice enters you as their child.", "Approve?" (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 4), answers Yes.
    expect(carol.atoms, isEmpty);
    await run.add(carol, 'alice');
    expect(formatTerm(carol.approval.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', enter(alice, carol), none), input([2]))')));
    expect(run.textsOf(carol, carol.approval),
        ['Alice enters you as their child.', 'Approve?', 'Yes', 'No']);
    await run.answer(carol, 'Yes');
    expect(carol.atoms, unorderedEquals(
        ['identity', 'parent(alice)', 'child', 'friend(alice)', 'knows(alice)']));
    // Dave has entered with Bob, and Alice and Bob have met and befriended; Alice asks that Carol and Dave meet, with Bob (CSSN:2), and Carol is asked: "Alice asks that you and Dave meet, with Dave's parent Bob.", "Meet Dave?", Yes and No (CSSN #9 Cowork, Code_inbox.md, 2026-10-20 19:44 UTC, item 4).  Bob and Dave have approved it.
    final m = carol.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', child_meet_2(carol, dave, alice, bob), none), input([2]))')));
    for (final q in ['bob', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_meet_2(carol, dave, alice, bob), yes)'));
    }
    expect(run.textsOf(carol, m), [
      "Alice asks that you and Dave meet, with Dave's parent Bob.",
      'Meet Dave?',
      'Yes',
      'No'
    ]);
    await run.answer(carol, 'Yes');
    expect(carol.atoms, contains('knows(dave)'));
    // (4) Alice, looking at Carol's phone, which shows that Carol knows Dave, requests that Carol and Dave befriend, with Bob, and Carol is asked: "Alice asks that you and Dave befriend, with Dave's parent Bob.", "Befriend Dave?", Yes and No.  Bob and Dave have approved it.
    final a = carol.approval;
    expect(formatTerm(a.view),
        allOf(startsWith('approval(attempt(alice, '),
            endsWith(', child_befriend(carol, dave, alice, bob), none), input([2]))')));
    for (final q in ['bob', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_befriend(carol, dave, alice, bob), yes)'));
    }
    await run.top(carol);
    expect(run.textsOf(carol, a), [
      "Alice asks that you and Dave befriend, with Dave's parent Bob.",
      'Befriend Dave?',
      'Yes',
      'No'
    ]);
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-4.png');
    await run.answer(carol, 'Yes');
    expect(carol.atoms, contains('friend(dave)'));
    expect(run.scriptedAtoms('dave'), contains('friend(carol)'));
    // (5) Carol's menu while Dave is her friend: "Alice, your parent", "Dave, your friend", "Bob", read off the last view, Alice's agent having introduced Carol's and Bob's to each other, Alice knowing both; Alice's row offers no End friendship, unfriend forbidding parent(q) at p, and Bob's no Befriend and no Meet, befriend and meet forbidding child at both.  Of the requests, a child composes only an introduction, "Introduce … to …." and Introduce, introduce(p?, q? ; c) forbidding child only at q (programs/cssn/CSSN.glp), the menu offering every request the person's own state allows (CSSN, Section 4.6.2).
    expect(carol.known, ['alice', 'bob', 'dave']);
    await run.top(carol);
    expect(run.rowTitles(carol), ['Alice, your parent', 'Dave, your friend', 'Bob']);
    expect(run.control(carol, 'unfriend:alice'), findsNothing);
    expect(run.control(carol, 'unfriend:dave'), findsOneWidget);
    expect(run.control(carol, 'befriend:bob'), findsNothing);
    expect(run.control(carol, 'meet:bob'), findsNothing);
    for (final k in ['meet', 'child-meet', 'child-meet-2', 'child-befriend',
        'of-age', 'enter', 'enter-2']) {
      expect(run.control(carol, 'compose:$k'), findsNothing, reason: k);
    }
    expect(run.sentence(carol, 'introduce'), 'Introduce … to ….');
    // A child's menu offers no adding a person by key (CSSN, Section 4.6.2; Appendix C.4): Carol's agent discovered Alice's at the setup of her phone, before the entering, and comes to know an agent only by a child meet a parent requests (CSSN:2).
    expect(run.control(carol, 'add-field'), findsNothing);
    expect(run.control(carol, 'add'), findsNothing);
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-5.png');
    // Carol ends the friendship with Dave (GSG:2; unfriend(p?, q)), her own act, End friendship on Dave's row: nobody is asked.
    await run.tap(carol, run.control(carol, 'unfriend:dave'));
    expect(carol.atoms, unorderedEquals([
      'identity',
      'parent(alice)',
      'child',
      'friend(alice)',
      'knows(alice)',
      'knows(dave)'
    ]));
    expect(run.scriptedAtoms('dave'), isNot(contains('friend(carol)')));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));
}
