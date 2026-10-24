/// The screens of GSC's three apps, GSG's, GSN's and GSNP's (`programs/apps/gsg`, `programs/apps/gsn`, `programs/apps/gsnp`), of GAI's (`programs/apps/gai`) and of GC's (`programs/apps/gc`), each person's phone as the bridge draws it, written to PNG files for GAI's paper and GSC's (`programs/apps/<app>/screens/`; GAI's figures from GSNP's app, the full platform); and of CSSN's mini-app (`programs/cssn`), for CSSN's paper (`programs/cssn/figures/`), below.
///
/// Each app's directory is a certified program: the canonical CVA platform of its contract, `platform.vglp`, compiled to GLP when the directory loads (vGLP, Section 6.3), and the mini-app's entry `miniapp/4` (`miniapp.glp`), which runs the platform's agent with the person's key and its dispatcher on the agent's asks.  The engine loads the directory and posts the entry for alice and bob over one conversation, as the plays of each directory do (`plays.glp`); for GSNP's app for dan, eve, carol, bob and fay, over four conversations, dan's with eve, eve's with carol, carol's with bob and bob's with fay, the persons of the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4), and again for acme_ltd, alice, alf and bob, over four conversations, the play of GAI's Figure "The GAI app" on the full platform (GAI #8 Cowork, Code_inbox.md, 2026-10-24 10:47 UTC), alf scripted by the app's person (`programs/apps/gsnp/person.glp`), its script the AI agent's program, and the three persons at their phones; and for GAI's app for alice, alf and bob, over three conversations, to alice's entering alf, no screen of which goes in GAI's paper.  Each person's channel is `ch(Grants?, Draws)` (vGLP, Definition "Person Channel, Person Writer, GLP with Persons, Grant"): the host hands each person's bridge every term of `Draws` as it arrives (bridge.dart), and extends `Grants` with each grant the bridge returns, `input(Id, P, R)`.
///
/// The agent writes its view, `view(S, K)`, its machine state and the persons it knows, on construct 0 when it starts and whenever either changes (vGLP b7e3517, Section 5.4, "The compilation"), drawn by `last_state`, the widget the platform declares for `Stream(Output)`.  The platform declares `Menu =::= social_menu` and `Approval =::= approval_sentence.` (`programs/apps/<app>/platform.vglp`, as vGLP's compiler prints it; GSNP's, GAI's and GC's apps declare them again in their `self.vglp`): the request question is drawn with the last view on construct 0 (vGLP e924f73, Section 6.2) as the app's tabs, Friends and, where the request type offers `create`, Groups, and, where it offers `mint`, Coins, and each approval question as one sentence and two buttons at the top of the tab it concerns (`lib/ui_runtime/named_widgets.dart`, `tab_screen.dart`), as GAI's task words them (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, and its sentence for a join at 11:35 UTC).  GKK's, GSG's and GSN's apps are compiled from contracts with pragmas, and their tabs are the lists the view names, each word on them the contract's (vGLP 1126f4f, Sections 5.4, 6.2 and 8).  Every act of a person at a phone is a tap or a keystroke on a control of the tabs, found by its key (`socialKey`, `tabKey`, `listTabKey`, `constructKey`): Join, Add, Befriend, Accept, End friendship, Send, Create, Add member and the rest; a scripted agent's acts are its scripted person's, as in the app's plays.  No term is hand-fed to the bridge or to the program.
///
/// And CSSN's mini-app (`programs/cssn`), in the scenario of CSSN's app (CSSN, Section 4.6, "In the Grassroots App", Figure "The child-safe mini-app in the grassroots app"): Alice, Bob, Carol and Dave, a conversation between each two, as CSSN's plays hand them (`programs/cssn/plays.glp`), one of them at their phone and the other three scripted by CSSN's person (`programs/cssn/person.glp`, the roles `fig_alice` to `fig_dave`), "each run has one live person, whose screen the figure shows" (the same); its screens written to `programs/cssn/figures/cssn-app-<n>.png`, n the figure's number of the screen.  CSSN's app is compiled from its contract and its pragmas, CSSN.txt's and those of the contracts it extends, so every word on its screens is the contract's (claude.md, "Sessions"; vGLP, Section 5.4, "The contract's words"): four tabs, Contacts, Friends, Groups and Children, the buttons of CSSN's acts beside a child's row in Children, Child meet, Child befriend and Of age among them, and the questions of CSSN's pragmas, "Alice asks that Carol and your child Dave befriend.".
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
  'received(',
  'member(',
  'listed',
  'item(',
  'view(',
  'agent_of',
  'principal',
  'retired',
  'enter(',
  'coin(',
  'mint(',
  'swap(',
  'pay(',
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

  /// Whether the app's platform is compiled from a contract with pragmas, GKK's, GSG's, GSN's, GSNP's, GAI's, GC's and CSSN's: every word on its screens is the contract's, its tabs the lists its view names (vGLP 1126f4f, Sections 5.4, 6.2 and 8).
  bool get worded =>
      const {'gkk', 'gsg', 'gsn', 'gsnp', 'gai', 'gc', 'cssn'}.contains(app);

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

  /// The last view the scripted agent [name]'s person reports, `view(S, K, A, Ts)` (vGLP, Section 5.4, "The compilation" and "The contract's words"): its atoms, each as its term.
  List<String> scriptedAtoms(String name) {
    final views = [
      for (final t in _drawn(seen[name]!))
        if (ctorArgs(t) case ('view', [GList(:final items), ...])) items
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

  /// The last view on [name]'s phone, `view(S, Ks, A, Ts)`, the machine state, the persons known, the approvals pending and the lists (vGLP, Section 5.4, "The compilation" and "The contract's words"): a person's at their phone, as construct 0 draws it, and a scripted agent's, as its person reports it; null where none is written.
  GTerm? _lastView(String name) {
    if (seen[name] case final s?) {
      return [
        for (final t in _drawn(s))
          if (ctorArgs(t) case ('view', [GList(), GList(), ...])) t
      ].lastOrNull;
    }
    final p = persons.where((q) => q.name == name).firstOrNull;
    final (ok, state) = SocialState.read(p?.ui.bridge.construct(0)?.view);
    if (!ok || state == null) return null;
    return GStruct('view', [
      GList(state.atoms),
      GList(state.known),
      GList(state.pending),
      GList([
        for (final l in state.lists ?? const <({String name, String predicate})>[])
          GStruct('tab', [GString(l.name), GAtom(l.predicate)]),
      ]),
    ]);
  }

  /// Hand each scripted parent in [watching] the last view on each child's phone they look at, `seen(K, view(S, Ks, A, Ts))`, where it changed since last handed; true where one was handed.
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

  /// [p] opens the tab of the list of the predicate [pred], on an app whose contract carries pragmas.
  Future<void> list(_Person p, String pred) =>
      tap(p, on(p, find.byKey(listTabKey(pred))));

  /// The name of the tab of the list of [pred] on [p]'s phone.
  Finder listNamed(_Person p, String pred, String name) => on(
      p,
      find.descendant(
          of: find.byKey(listTabKey(pred)), matching: find.text(name)));

  /// [p] taps the button [label] of their open approval question.
  Future<void> answer(_Person p, String label) => tap(
      p,
      on(
          p,
          find.descendant(
              of: find.byKey(constructKey(p.approval.id)),
              matching: find.widgetWithText(ElevatedButton, label))));

  /// [p] establishes their identity (GKK:1): Join; on an app whose contract carries pragmas, Establish, GKK's "[Establish]", on the tab of GKK, Contacts.
  Future<void> join(_Person p) async {
    if (!worded) return tap(p, control(p, 'join'));
    await list(p, 'knows');
    await tap(p, control(p, 'send:establish:'));
    expect(p.state.identity, isTrue, reason: '${p.name}: no identity');
  }

  /// [p] asks to meet [q], whose key they type into Meet's field for a peer not listed, Peer (vGLP, Section 6.2: "Meet with a peer not listed takes a field for the key"), and taps Meet, GKK's "[Meet]": their request's hello asks [q], at the top of [q]'s Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; [q] taps yes, their request of the same meeting, and the two-phase commit runs with both approvals standing, neither asked again (vGLP, Section 5.4, "The compilation").  Each then knows the other, `knows(Q)`.  Where [shot] is given, [q]'s phone is shot asked.
  Future<void> meetByKey(_Person p, _Person q, {String? shot}) async {
    await list(p, 'knows');
    await list(q, 'knows');
    await type(p, control(p, 'key:meet'), q.name);
    await tap(p, control(p, 'key-send:meet'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(hello(${p.name}, meet(${p.name}, ${q.name})), '));
    await top(q);
    final asks = '${personName(GAtom(p.name))} wishes that you know each other.';
    expect(textsOf(q, a), [asks, 'yes', 'no']);
    if (shot != null) {
      plain(q);
      await shoot(q, shot);
    }
    await answer(q, 'yes');
    expect(q.questions('approval'), isEmpty);
    expect(p.questions('approval'), isEmpty);
    expect(p.atoms, contains('knows(${q.name})'));
    expect(q.atoms, contains('knows(${p.name})'));
  }

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
    if (worded) {
      await list(p, 'knows');
      await list(q, 'knows');
    }
    for (final x in [p, q]) {
      if (!x.state.identity) await join(x);
      expect(x.state.identity, isTrue, reason: '${x.name}: no identity');
    }
    await add(p, q.name);
    await add(q, p.name);
    expect(p.known, contains(q.name));
    expect(q.known, contains(p.name));
  }

  /// [p] asks to meet [q] in person (GKK:2): Meet on [q]'s row; and [q], asked "Alice asks to meet you in person." at the top of their first tab, taps Yes.  Each then knows the other, `knows(Q)`.  Where [shot] is given, [q]'s phone is shot asked.  On an app whose contract carries pragmas, the words are GKK's: Meet, the question "Alice wishes that you know each other.", and the two answers, yes and no, which no pragma names.
  Future<void> meetInPerson(_Person p, _Person q, {String? shot}) async {
    if (worded) {
      await list(p, 'knows');
      await list(q, 'knows');
    }
    await tap(p, control(p, 'meet:${q.name}'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(attempt(${p.name}, '));
    expect(formatTerm(a.view),
        worded
            ? endsWith('input([3]))')
            : endsWith(', meet(${p.name}, ${q.name}), none), input([2]))'));
    if (worded) {
      expect(formatTerm(a.view), contains(', meet(${p.name}, ${q.name}), none), '));
    }
    await top(q);
    final asks = worded
        ? '${personName(GAtom(p.name))} wishes that you know each other.'
        : '${personName(GAtom(p.name))} asks to meet you in person.';
    expect(textsOf(q, a), [asks, worded ? 'yes' : 'Yes', worded ? 'no' : 'No']);
    // At the top of the tab, before the person's own name.
    expect(tester.getTopLeft(on(q, find.text(asks))).dy,
        lessThan(tester.getTopLeft(control(q, 'name')).dy));
    if (shot != null) {
      plain(q);
      await shoot(q, shot);
    }
    await answer(q, worded ? 'yes' : 'Yes');
    expect(q.questions('approval'), isEmpty);
    expect(p.atoms, contains('knows(${q.name})'));
    expect(q.atoms, contains('knows(${p.name})'));
  }

  /// [p] offers [q] friendship (GSG:1): Befriend on [q]'s row; and [q], asked, taps Accept.  On an app whose contract carries pragmas, Befriend on [q]'s row in the list of `knows`, Contacts, and [q] asked in the list of `friend`, Friends, in GSG's words, "Alice asks to be your friend.", the answers yes and no.
  Future<void> befriend(_Person p, _Person q, {String? shot}) async {
    if (worded) await list(p, 'knows');
    await tap(p, control(p, worded ? 'act:befriend:${q.name}' : 'befriend:${q.name}'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(attempt(${p.name}, '));
    expect(formatTerm(a.view),
        worded
            ? endsWith('input([3]))')
            : endsWith(', befriend(${p.name}, ${q.name}), none), input([2]))'));
    if (worded) {
      expect(formatTerm(a.view),
          contains(', befriend(${p.name}, ${q.name}), none), '));
      await list(q, 'friend');
    }
    await top(q);
    final asks = '${personName(GAtom(p.name))} asks to be your friend.';
    expect(textsOf(q, a),
        [asks, worded ? 'yes' : 'Accept', worded ? 'no' : 'Decline']);
    if (!worded) {
      // At the top of Friends, before the person's own name.
      expect(tester.getTopLeft(on(q, find.text(asks))).dy,
          lessThan(tester.getTopLeft(control(q, 'name')).dy));
    }
    if (shot != null) {
      plain(q);
      await shoot(q, shot);
    }
    await answer(q, worded ? 'yes' : 'Accept');
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

/// A person's acts on CSSN's app as the construct family draws it from the contract's pragmas (named_widgets.dart, `socialMenu` and `approvalSentence`): the button of an act beside the row that fills its variable, a person picked into each other place, and the button, labelled as the pragma labels it (vGLP, Section 6.2, "The words a platform shows").
extension _Cssn on _Run {
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
  // GKK's, GSG's, GSN's and GC's apps are compiled from their contracts with their pragmas, and every word on their screens is the contract's or one of the platform's four, yes, no, declined and unable (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"; Section 8): each tab a list the contract's pragmas name, Contacts, Friends, Groups and Coins, each button the label in brackets and each field the name in angle brackets, each question the sentence after them; a field no pragma names drawn by its type's name, as Meet's field for the peer's key, Peer (Section 6.2: "A construct family draws the label it is given, and the name of the type where it is given none").  Where a button is drawn is the request's widget term's (Section 6.2): beside each person or group the view lists, of an act that names one, and on the tab of its contract, of an act that names none, and Meet with a field for the key.
  testWidgets(
      'GKK\'s app on three phones: Alice establishes her identity; Bob asked '
      'to meet Alice; and Bob\'s contacts once Alice has introduced Carol to '
      'him', (tester) async {
    final run = await _Run.start(tester, screens, 'gkk',
        chain: const ['alice', 'bob', 'carol'],
        pairs: const [('alice', 'bob'), ('alice', 'carol')]);
    final alice = run.alice;
    final bob = run.bob;
    final carol = run.person('carol');

    // One tab, Contacts, the list of GKK's pragmas: "Contacts: the persons Alice knows."
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.on(alice, find.byKey(listTabKey('friend'))), findsNothing);
    expect(run.on(alice, find.byKey(listTabKey('member'))), findsNothing);

    // (a) Alice's Contacts before her identity is established: her name; Establish, GKK's "[Establish]", which names no one beside its proposer and is drawn on the tab of its contract; and Meet, GKK's "[Meet]", with the field for a peer's key, named by its type, Peer.
    expect(run.control(alice, 'name'), findsOneWidget);
    expect(
        find.descendant(
            of: run.control(alice, 'send:establish:'),
            matching: find.text('Establish')),
        findsOneWidget);
    expect(
        find.descendant(
            of: run.control(alice, 'key-send:meet'), matching: find.text('Meet')),
        findsOneWidget);
    // Meet's field for the peer's key, named GKK's "<person>" (vGLP e286276, Section 5.4, "The contract's words").
    expect(
        find.descendant(
            of: run.control(alice, 'key:meet'), matching: find.text('person')),
        findsOneWidget);
    expect(alice.atoms, isEmpty);
    run.plain(alice);
    await run.shoot(alice, 'gkk-a-alice-identity.png');
    await run.join(alice);
    expect(alice.atoms, ['identity']);
    await run.join(bob);
    await run.join(carol);

    // (b) Alice types Bob's key into Meet's field and taps Meet (GKK:2): her request's hello asks Bob, at the top of his Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; he taps yes, his request of the same meeting, and each then knows the other, neither asked again.  Then Alice and Carol meet.
    await run.meetByKey(alice, bob, shot: 'gkk-b-bob-asked-to-meet.png');
    await run.meetByKey(alice, carol);
    expect(alice.atoms,
        unorderedEquals(['identity', 'knows(bob)', 'knows(carol)']));
    expect(bob.atoms, unorderedEquals(['identity', 'knows(alice)']));

    // Alice, who knows Bob and Carol, introduces Carol to Bob (GKK:3): Introduce, GKK's "[Introduce]", beside Bob in her Contacts, Bob filling the first person of introduce(p?, q? ; c); she types Carol's key into its field, GKK's "<person>", a person the act names by key (vGLP e286276, Section 5.4, "The contract's words"), and taps Introduce.
    await run.list(alice, 'knows');
    await run.tap(alice, run.control(alice, 'act:introduce:bob'));
    expect(
        find.descendant(
            of: run.control(alice, 'field:introduce:bob:3'),
            matching: find.text('person')),
        findsOneWidget);
    await run.type(alice, run.control(alice, 'field:introduce:bob:3'), 'carol');
    expect(
        find.descendant(
            of: run.control(alice, 'send:introduce:bob'),
            matching: find.text('Introduce')),
        findsOneWidget);
    await run.tap(alice, run.control(alice, 'send:introduce:bob'));

    // Bob is asked GKK's question "Alice asks to introduce Carol to you.", with yes and no, and taps yes; he then knows Carol, and Carol is not asked.
    final a = bob.approval;
    expect(formatTerm(a.view),
        contains(', introduce(alice, bob, carol), none), '));
    await run.top(bob);
    expect(run.textsOf(bob, a),
        ['Alice asks to introduce Carol to you.', 'yes', 'no']);
    run.plain(bob);
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(carol.questions('approval'), isEmpty);
    expect(bob.atoms,
        unorderedEquals(['identity', 'knows(alice)', 'knows(carol)']));

    // (c) Bob's contacts once he has approved: a row for each atom of knows, in the order of his state, the newest first, Carol and then Alice, each with the buttons of the acts that name a person, Meet and Introduce.
    await run.top(bob);
    expect(run.control(bob, 'row:alice'), findsOneWidget);
    expect(run.control(bob, 'row:carol'), findsOneWidget);
    expect(run.control(bob, 'act:meet:alice'), findsOneWidget);
    expect(run.texts(bob), containsAllInOrder(['Bob', 'Carol', 'Alice']));
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

    // Two tabs, the lists of GKK's and GSG's pragmas: Contacts and Friends; no group.
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.on(alice, find.byKey(listTabKey('member'))), findsNothing);

    // Each establishes their identity (GKK:1), and Alice meets Bob by his key (GKK:2).  Bob's row in her Contacts then has the buttons of every act that names a person: Meet, Introduce, Befriend and Unfriend, GSG's "[Befriend]" and "[Unfriend]".
    await run.join(alice);
    await run.join(bob);
    await run.meetByKey(alice, bob);
    for (final f in ['meet', 'introduce', 'befriend', 'unfriend']) {
      expect(run.control(alice, 'act:$f:bob'), findsOneWidget, reason: f);
    }

    // (a) Alice taps Befriend beside Bob (GSG:1): Bob's Friends tab, the tab of GSG, GSG's question "Alice asks to be your friend." and yes and no at its top.
    await run.befriend(alice, bob, shot: 'gsg-a-bob-asked.png');

    // (b) Alice's Friends tab after the befriend commits: a row for Bob, with the buttons of the acts that name a person.
    await run.list(alice, 'friend');
    await run.top(alice);
    expect(run.control(alice, 'row:bob'), findsOneWidget);
    expect(run.control(alice, 'act:unfriend:bob'), findsOneWidget);
    expect(run.texts(alice), containsAllInOrder(['Friends', 'Bob', 'Unfriend']));
    run.plain(alice);
    await run.shoot(alice, 'gsg-b-alice-friends.png');

    // Bob ends the friendship (GSG:2), his own act: Unfriend beside Alice in his Friends tab.  Alice is not asked, and each still knows the other.
    await run.list(bob, 'friend');
    await run.tap(bob, run.control(bob, 'act:unfriend:alice'));
    expect(alice.questions('approval'), isEmpty);
    expect(alice.atoms, unorderedEquals(['identity', 'knows(bob)']));
    expect(bob.atoms, unorderedEquals(['identity', 'knows(alice)']));

    // (c) Alice's tabs after Bob ends the friendship: Friends lists no one, and Contacts lists Bob.
    expect(run.control(alice, 'row:bob'), findsNothing);
    await run.list(alice, 'knows');
    await run.top(alice);
    expect(run.control(alice, 'row:bob'), findsOneWidget);
    expect(run.texts(alice), containsAllInOrder(['Alice', 'Bob', 'Befriend']));
    run.plain(alice);
    await run.shoot(alice, 'gsg-c-alice-ended.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSN\'s app on two phones: Bob\'s Friends with Alice\'s message, Bob '
      'asked to join the group friends, his view of the group with his '
      'message delivered in it, and Alice\'s view of the group she leads',
      (tester) async {
    final run = await _Run.start(tester, screens, 'gsn');
    final alice = run.alice;
    final bob = run.bob;
    // The group friends, led by Alice: its name, a speech act of hers, and its community name, <alice>.friends, which a member atom holds, one argument (programs/apps/gsn/platform.vglp, Atom and CName).
    const friends = 'speech(utterance(friends), alice)';
    const friendsGroup = 'dot(community(alice), $friends)';

    // Three tabs, the lists of GKK's, GSG's and GSN's pragmas: Contacts, Friends and Groups.
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(alice, 'member', 'Groups'), findsOneWidget);

    // Identity, meeting and friendship, as in GSG's app.
    await run.join(alice);
    await run.join(bob);
    await run.meetByKey(alice, bob);
    await run.befriend(alice, bob);

    // A message to a friend (GSN:1): in her Friends tab Alice taps Message, GSN's "[Message]", beside Bob, types Hi Bob into its field text, GSN's "<text>", and taps Message.  Bob is not asked.
    await run.list(alice, 'friend');
    await run.tap(alice, run.control(alice, 'act:message:bob'));
    await run.type(alice, run.control(alice, 'field:message:bob:3'), 'Hi Bob');
    await run.tap(alice, run.control(alice, 'send:message:bob'));
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, contains("received(speech(utterance('Hi Bob'), alice), alice)"));

    // (a) Bob's Friends tab: Alice's row shows the atoms of his state naming her, her message Hi Bob among them (vGLP, Section 5.4: "a row shows the atoms of the state naming its item").
    await run.list(bob, 'friend');
    await run.top(bob);
    expect(
        find.descendant(
            of: run.control(bob, 'row:alice'), matching: find.text('Hi Bob')),
        findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gsn-a-bob-chat.png');

    // A group (GSN:2): on her Groups tab, the tab of GSN, Alice types friends into Create's field name, GSN's "<name>", and taps Create, GSN's "[Create]"; she is its leader and first member.
    await run.list(alice, 'member');
    await run.type(alice, run.control(alice, 'field:create::2'), 'friends');
    await run.tap(alice, run.control(alice, 'send:create:'));
    expect(alice.atoms,
        containsAll(['member($friendsGroup)', 'listed(alice, $friends)']));

    // Joining (GSN:3): Join, GSN's "[Join]", beside the group she leads, the group filling join's leader and name; she picks Bob into its person and taps Join.
    await run.tap(alice, run.control(alice, 'act:join:$friendsGroup'));
    await run.tap(
        alice, run.control(alice, 'pick:join:$friendsGroup:2:bob'));
    await run.tap(alice, run.control(alice, 'send:join:$friendsGroup'));

    // (b) Bob's Groups tab, badged: GSN's question "Alice asks you to join their group.", the group's name, friends, under it, and yes and no at its top.
    final a = bob.approval;
    expect(formatTerm(a.view), contains(', join(alice, bob, $friends), none), '));
    expect(
        find.descendant(
            of: run.on(bob, find.byKey(listTabKey('member'))),
            matching: find.text('1')),
        findsOneWidget);
    await run.list(bob, 'member');
    await run.top(bob);
    const asks = 'Alice asks you to join their group.';
    expect(run.textsOf(bob, a), [asks, 'friends', 'yes', 'no']);
    run.plain(bob);
    await run.shoot(bob, 'gsn-b-bob-asked-to-join.png');

    // He taps yes: he is a member of the group, and Alice lists him in it.
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, contains('member($friendsGroup)'));
    expect(alice.atoms, contains('listed(bob, $friends)'));

    // A group message (GSN:5): Post, GSN's "[Post]", beside the group in Bob's Groups, its field text, GSN's "<text>": he types Hi all and taps Post, which proposes post(bob, alice, x, n), in the order of post's head, post(p?, a ; x, n) (programs/gsc/GSN.glp).  Alice, its leader, is not asked; her agent opens the delivery and delivers the message to Bob.
    await run.list(bob, 'member');
    await run.tap(bob, run.control(bob, 'act:post:$friendsGroup'));
    await run.type(bob, run.control(bob, 'field:post:$friendsGroup:3'), 'Hi all');
    await run.tap(bob, run.control(bob, 'send:post:$friendsGroup'));
    expect(alice.questions('approval'), isEmpty);
    const hiAll = "speech(utterance('Hi all'), bob)";
    expect(alice.atoms, contains('posted($hiAll, $friends)'));
    expect(alice.atoms, contains('delivered($hiAll, bob, $friends)'));
    expect(bob.atoms, contains('received_in($hiAll, alice, $friends)'));

    // (c) Bob's view of the group: its row, the group's name and its leader, and the atoms of his state naming it, the message Hi all as Alice delivered it, and the buttons of the acts that name a group, Leave, GSN's "[Leave]", among them.
    await run.top(bob);
    final g = run.control(bob, 'row:$friendsGroup');
    expect(g, findsOneWidget);
    final inGroup = [
      for (final t in run.tester.widgetList<Text>(
          find.descendant(of: g, matching: find.byType(Text))))
        if (t.data != null) t.data!,
    ];
    expect(inGroup, containsAllInOrder(['friends, Alice', 'Hi all, Alice']));
    expect(inGroup, contains('Leave'));
    run.plain(bob);
    await run.shoot(bob, 'gsn-c-bob-group.png');

    // The leader's own message (GSN:5): Post own, GSN's "[Post own] <text>", beside the group in Alice's Groups; she types Welcome and taps Post own, which proposes post_own(alice, x, n); her agent delivers it to Bob, unasked.
    await run.list(alice, 'member');
    await run.tap(alice, run.control(alice, 'act:post_own:$friendsGroup'));
    await run.type(
        alice, run.control(alice, 'field:post_own:$friendsGroup:2'), 'Welcome');
    await run.tap(alice, run.control(alice, 'send:post_own:$friendsGroup'));
    expect(bob.questions('approval'), isEmpty);
    const welcome = "speech(utterance('Welcome'), alice)";
    expect(alice.atoms, contains('posted($welcome, $friends)'));
    expect(bob.atoms, contains('received_in($welcome, alice, $friends)'));

    // (d) Alice's view of the group she leads: the atoms of her state naming it, the members she lists, the messages posted and delivered, and Close, GSN's "[Close]".
    await run.top(alice);
    final ga = run.control(alice, 'row:$friendsGroup');
    final inHers = [
      for (final t in run.tester.widgetList<Text>(
          find.descendant(of: ga, matching: find.byType(Text))))
        if (t.data != null) t.data!,
    ];
    expect(inHers, containsAll(['Bob', 'Hi all', 'Welcome', 'Post own', 'Close']));
    run.plain(alice);
    await run.shoot(alice, 'gsn-d-alice-group.png');

    // Alice closes the group (GSN:4: "when the leader leaves, the group ends"): Close beside the group proposes close(alice, n), its listed role every member she lists; Bob's machine votes unasked, and the group ends at both.
    await run.tap(alice, run.control(alice, 'act:close:$friendsGroup'));
    expect(bob.questions('approval'), isEmpty);
    expect(alice.atoms, isNot(contains('member($friendsGroup)')));
    expect(bob.atoms, isNot(contains('member($friendsGroup)')));
    expect(alice.atoms, contains('closed($friends)'));

    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GC\'s app on two phones: Alice\'s Coins tab once she mints two '
      'Alice-coins; Bob asked to swap three Bob-coins for them; Bob\'s '
      'holdings after the swap; and Alice\'s once she pays two Bob-coins to '
      'Bob', (tester) async {
    // GC's app (programs/apps/gc), compiled down the cascade from GC.glp (programs/currencies/coins/GC.glp) and its pragmas, GC.txt: Alice and Bob establish their identities and meet (GKK:1, GKK:2); Alice mints two Alice-coins and Bob three Bob-coins (GC:1); Alice asks Bob to swap her two Alice-coins for his three Bob-coins and Bob answers yes (GC:2); Alice pays two Bob-coins to Bob (GC:3).
    final run = await _Run.start(tester, screens, 'gc');
    final alice = run.alice;
    final bob = run.bob;

    // Two tabs, the lists of GKK's and GC's pragmas: Contacts and Coins, "Coins: the coins Alice holds."
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'coin', 'Coins'), findsOneWidget);
    expect(run.on(alice, find.byKey(listTabKey('friend'))), findsNothing);

    // Each establishes their identity (GKK:1), and Alice meets Bob by his key (GKK:2).
    await run.join(alice);
    await run.join(bob);
    await run.meetByKey(alice, bob);

    // (a) Alice mints (GC:1): on her Coins tab, the tab of GC, she types 2 into Mint's field amount, GC's "<amount>", and taps Mint, GC's "[Mint]", which proposes mint(alice, 2); a row for her coins, Alice, and their count.
    await run.list(alice, 'coin');
    await run.type(alice, run.control(alice, 'field:mint::2'), '2');
    await run.tap(alice, run.control(alice, 'send:mint:'));
    expect(alice.atoms,
        unorderedEquals(['identity', 'knows(bob)', 'coin(alice)', 'coin(alice)']));
    await run.top(alice);
    expect(run.control(alice, 'row:alice'), findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gc-a-alice-mint.png');

    // Bob mints three Bob-coins likewise.
    await run.list(bob, 'coin');
    await run.type(bob, run.control(bob, 'field:mint::2'), '3');
    await run.tap(bob, run.control(bob, 'send:mint:'));

    // Alice asks Bob to swap (GC:2): Swap, GC's "[Swap]", beside Bob in her Contacts, its fields GC's "<issuer given> <amount given> <issuer wanted> <amount wanted>": alice, 2, bob, 3; which proposes swap(alice, bob, alice, 2, bob, 3), in the order of swap's head, swap(p?, q? ; u, k, v, k') (programs/currencies/coins/GC.glp).
    await run.list(alice, 'knows');
    await run.tap(alice, run.control(alice, 'act:swap:bob'));
    for (final (i, v) in [(3, 'alice'), (4, '2'), (5, 'bob'), (6, '3')]) {
      await run.type(alice, run.control(alice, 'field:swap:bob:$i'), v);
    }
    await run.tap(alice, run.control(alice, 'send:swap:bob'));

    // (b) Bob is asked at the top of his Coins tab, GC's question "Alice asks to swap coins with you.", with yes and no.
    final a = bob.approval;
    expect(formatTerm(a.view),
        contains(', swap(alice, bob, alice, 2, bob, 3), none), '));
    await run.list(bob, 'coin');
    await run.top(bob);
    const asks = 'Alice asks to swap coins with you.';
    expect(run.textsOf(bob, a), [asks, 'yes', 'no']);
    run.plain(bob);
    await run.shoot(bob, 'gc-b-bob-asked.png');

    // He taps yes, and the swap commits at both.
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(alice.atoms.where((x) => x == 'coin(bob)'), hasLength(3));
    expect(bob.atoms.where((x) => x == 'coin(alice)'), hasLength(2));

    // (c) Bob's holdings after the swap: a row for the Alice-coins he holds, with Pay, GC's "[Pay]", beside it.
    await run.top(bob);
    expect(run.control(bob, 'row:alice'), findsOneWidget);
    expect(run.control(bob, 'row:bob'), findsNothing);
    expect(run.control(bob, 'act:pay:alice'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gc-c-bob-holdings.png');

    // Alice pays two Bob-coins to Bob (GC:3): Pay beside her Bob-coins, its field amount, 2; which proposes pay(alice, bob, 2); Bob is not asked.
    await run.list(alice, 'coin');
    await run.tap(alice, run.control(alice, 'act:pay:bob'));
    await run.type(alice, run.control(alice, 'field:pay:bob:3'), '2');
    await run.tap(alice, run.control(alice, 'send:pay:bob'));
    expect(bob.questions('approval'), isEmpty);
    expect(alice.atoms.where((x) => x == 'coin(bob)'), hasLength(1));

    // (d) Alice's Coins tab once she has paid: one Bob-coin left.
    await run.top(alice);
    expect(run.control(alice, 'row:bob'), findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gc-d-alice-pay.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSNP\'s app on five phones, the play of GAI\'s GSNP figure, compiled '
      'from GSNP\'s contract and pragmas: the friendships, Dan signing the '
      'item on his Items tab and sending it to Eve by Message, Eve, Carol '
      'and Bob forwarding it by Forward, and the screens of Bob\'s and Fay\'s '
      'Items tabs', (tester) async {
    // GSNP's app (programs/apps/gsnp), compiled down the cascade from GSNP.glp (programs/gai/GSNP.glp) and its pragmas, GSNP.txt with those of GSN, GSG and GKK, which it extends: four tabs, Contacts, Friends, Groups and Items, "Items: the items Alice holds."
    final run = await _Run.start(tester, screens, 'gsnp',
        chain: const ['dan', 'eve', 'carol', 'bob', 'fay']);
    final dan = run.person('dan');
    final eve = run.person('eve');
    expect(run.listNamed(dan, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(dan, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(dan, 'member', 'Groups'), findsOneWidget);
    expect(run.listNamed(dan, 'item', 'Items'), findsOneWidget);

    // The friendships of the play (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4): Dan and Eve, Eve and Carol, Carol and Bob, Bob and Fay; each establishes their identity (GKK:1), the first of each two meets the second by their key, the second answering yes (GKK:2), and then befriends them, the second answering yes (GSG:1).
    for (final p in run.persons) {
      await run.join(p);
    }
    for (final (p, q) in [('dan', 'eve'), ('eve', 'carol'), ('carol', 'bob'), ('bob', 'fay')]) {
      await run.meetByKey(run.person(p), run.person(q));
      await run.befriend(run.person(p), run.person(q));
    }

    // Authorship (GSNP:1): on his AI agents tab, the tab of GAI, whose Developer line carries "[Sign] <text>" (GSNP.txt at a336b30d), Dan types "Flood on Main Street" into Sign's field text and taps Sign, which proposes sign(dan, x), x the speech act he signs; he holds it, its author.
    const flood = "speech(utterance('Flood on Main Street'), dan)";
    await run.list(dan, 'principal_of');
    await run.type(dan, run.control(dan, 'field:sign::2'), 'Flood on Main Street');
    await run.tap(dan, run.control(dan, 'send:sign:'));
    expect(dan.atoms, containsAll(['item($flood)', 'authored($flood)']));

    // A message (GSN:1): Message, GSN's "[Message] <text>", beside Eve in his Friends tab, its field text the same words, which proposes message(dan, eve, x), the item he authored; Eve is not asked, and holds the item as he signed it.
    await run.list(dan, 'friend');
    await run.tap(dan, run.control(dan, 'act:message:eve'));
    await run.type(dan, run.control(dan, 'field:message:eve:3'), 'Flood on Main Street');
    await run.tap(dan, run.control(dan, 'send:message:eve'));
    expect(eve.questions('approval'), isEmpty);
    expect(eve.atoms, contains('item($flood)'));

    // Eve's Items tab: the item's row, its text and its author, Dan (vGLP #20, Code_inbox.md, 2026-10-23 23:08 UTC, item 9: "an item's row its author and the agents that forwarded it"), and after them the buttons of the acts that name an item, Enter, Forward, Post own and Post forward, each drawn beside each item the view lists (vGLP 932c710, Section 6.2).
    await run.list(eve, 'item');
    await run.top(eve);
    final row = run.control(eve, 'row:$flood');
    expect(row, findsOneWidget);
    expect(
        [
          for (final t in run.tester.widgetList<Text>(
              find.descendant(of: row, matching: find.byType(Text))))
            if (t.data != null && t.data!.length > 1) t.data!,
        ].take(2),
        ['Flood on Main Street', 'Dan']);
    run.plain(eve);

    // Forwarding (GSNP:2): Forward, GSNP's "[Forward]", is drawn beside each item of the Items tab, the row filling the act's item (vGLP 932c710, Section 6.2: "The button of an act that names, beside the person proposing it, a person, a group or an item is drawn beside each one the view lists, ... the row filling the act's variable"); the person picks the friend it goes to and reads the warranty, "You warrant that you know personally the agent you received this from.", the text of its request form (Section 5.4, "The contract's words"), and taps Forward; nobody is asked.  Eve forwards it to Carol, and Carol to Bob.
    Future<void> forward(_Person p, String item, String to) async {
      await run.list(p, 'item');
      await run.tap(p, run.control(p, 'act:forward:$item'));
      await run.tap(p, run.control(p, 'pick:forward:$item:2:$to'));
      await run.tap(p, run.control(p, 'send:forward:$item'));
      expect(run.person(to).questions('approval'), isEmpty);
    }

    final carol = run.person('carol');
    final bob = run.person('bob');
    final fay = run.person('fay');
    final byEve = 'speech($flood, eve)';
    final byCarol = 'speech($byEve, carol)';
    await forward(eve, flood, 'carol');
    expect(carol.atoms, contains('item($byEve)'));
    await forward(carol, byEve, 'bob');
    expect(bob.atoms, contains('item($byCarol)'));

    // (a) Bob's Items tab: the item, its text, and under it its author and the agents that forwarded it, in order, Dan, Eve, Carol (GSNP:1, GSNP:2).
    List<String> rowTexts(_Person p, String k) => [
          for (final t in run.tester.widgetList<Text>(find.descendant(
              of: run.control(p, 'row:$k'), matching: find.byType(Text))))
            if (t.data != null && t.data!.length > 1) t.data!,
        ];
    await run.list(bob, 'item');
    await run.top(bob);
    expect(rowTexts(bob, byCarol).take(2), ['Flood on Main Street', 'Dan, Eve, Carol']);
    run.plain(bob);
    await run.shoot(bob, 'gsnp-a-bob-chat.png');

    // (b) Bob forwards it to Fay, his friend: Forward beside the item, Fay picked, the warranty above the button.  The persons he may pick are drawn under the name of their place's type, Peer, no pragma naming a field for a person the act names beside the proposer that the view lists (vGLP, Section 6.2: "a construct family draws the label it is given, and the name of the type where it is given none").
    await run.tap(bob, run.control(bob, 'act:forward:$byCarol'));
    await run.tap(bob, run.control(bob, 'pick:forward:$byCarol:2:fay'));
    expect(run.on(bob, find.text('You warrant that you know personally the agent you received this from.')), findsOneWidget);
    run.plain(bob, allow: const {'Peer'});
    await run.shoot(bob, 'gsnp-b-bob-forward.png');
    await run.tap(bob, run.control(bob, 'send:forward:$byCarol'));
    final byBob = 'speech($byCarol, bob)';
    expect(fay.atoms, contains('item($byBob)'));
    expect(fay.questions('approval'), isEmpty);

    // (c) Fay's Items tab: the item with Bob among its forwarders.
    await run.list(fay, 'item');
    await run.top(fay);
    expect(rowTexts(fay, byBob).take(2), ['Flood on Main Street', 'Dan, Eve, Carol, Bob']);
    run.plain(fay);
    await run.shoot(fay, 'gsnp-c-fay-chat.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSNP\'s app, the play of GAI\'s Figure "The GAI app" on the full '
      'platform: Acme Ltd signing Alf\'s code and sending it to Alice, and '
      'Alice entering Alf with it from her Items tab, Alf\'s program '
      'scripted', (tester) async {
    // Acme Ltd, Alice and Bob at their phones, and Alf, Alice's AI agent, scripted (programs/apps/gsnp/person.glp, the role alf), a conversation between Acme Ltd and Alice, Alice and Alf, Alice and Bob, and Alf and Bob (GAI #8 Cowork, Code_inbox.md, 2026-10-24 10:47 UTC: "its play has Acme Ltd establish, sign Alf's code, meet and befriend Alice and message her the code, and Alice enter Alf with it, so the Developer line stays Acme Ltd").
    final run = await _Run.start(tester, screens, 'gsnp',
        chain: const ['acme_ltd', 'alice', 'alf', 'bob'],
        pairs: const [
          ('acme_ltd', 'alice'),
          ('alice', 'alf'),
          ('alice', 'bob'),
          ('alf', 'bob')
        ],
        scripted: const {'alf'});
    final acme = run.person('acme_ltd');
    final alice = run.person('alice');
    final bob = run.person('bob');
    const code = "speech(utterance('The code of Alf'), acme_ltd)";
    for (final p in [acme, alice, bob]) {
      await run.join(p);
    }
    await run.meetByKey(acme, alice);
    await run.befriend(acme, alice);

    // Authorship (GSNP:1): on his AI agents tab, the tab of GAI, whose Developer line carries "[Sign] <text>", Acme Ltd signs the code; and a message (GSN:1): Message beside Alice in his Friends tab carries it to her as he signed it.
    await run.list(acme, 'principal_of');
    await run.type(acme, run.control(acme, 'field:sign::2'), 'The code of Alf');
    await run.tap(acme, run.control(acme, 'send:sign:'));
    expect(acme.atoms, contains('authored($code)'));
    await run.list(acme, 'friend');
    await run.tap(acme, run.control(acme, 'act:message:alice'));
    await run.type(acme, run.control(acme, 'field:message:alice:3'), 'The code of Alf');
    await run.tap(acme, run.control(acme, 'send:message:alice'));
    expect(alice.atoms, contains('item($code)'));
    await run.meetByKey(alice, bob);

    // Guardianship (GnW:1, GAI:1, GAI:2): Enter, GAI's "[Enter] <name> <code>", beside the code on Alice's Items tab, the row filling the code (vGLP 932c710, Section 6.2), Alf's key typed into the field name; Alf's program answers yes, and Alice is its principal.
    await run.list(alice, 'item');
    await run.tap(alice, run.control(alice, 'act:enter:$code'));
    await run.type(alice, run.control(alice, 'field:enter:$code:2'), 'alf');
    await run.tap(alice, run.control(alice, 'send:enter:$code'));
    expect(alice.atoms, containsAll(['principal_of(alf)', 'friend(alf)']));

    // (b) Alice's AI agents tab, "AI agents: those Alice is principal of.": Alf.
    await run.list(alice, 'principal_of');
    await run.top(alice);
    expect(run.control(alice, 'row:alf'), findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gai-b-alice-agents.png');

    // Agent meet (GnW:2, GAI:2): GAI's "[Agent meet]" beside Alf on Alice's AI agents tab, the button drawn at the principal's guarding role (vGLP 110caf4, Section 6.2: "the button of an act is drawn at each of its guarding roles, beside each row that fills its variable"); the row fills Alf, her machine fills the attestation from her record agent_of(alf, alice, y) (GAI.glp at a336b30d), and she picks Bob.  Alf's program answers yes; Bob is asked GAI's question "Alf, an AI agent, asks to meet you." and answers yes, recording Alf, its principal and its attestation.
    const attestation = 'speech($code, alice)';
    Future<void> agentAct(String f, String shot) async {
      await run.list(alice, 'principal_of');
      await run.tap(alice, run.control(alice, 'act:$f:alf'));
      await run.tap(alice, run.control(alice, 'pick:$f:alf:2:bob'));
      if (shot.isNotEmpty) {
        // The persons Alice may pick are drawn under the name of their place's type, Peer (vGLP, Section 6.2: "the name of the type where it is given none").
        run.plain(alice, allow: const {'Peer'});
        await run.shoot(alice, shot);
      }
      await run.tap(alice, run.control(alice, 'send:$f:alf'));
    }

    await agentAct('agent_meet', '');
    await run.list(bob, 'principal_of');
    await run.top(bob);
    expect(run.on(bob, find.text('Alf, an AI agent, asks to meet you.')), findsOneWidget);
    await run.answer(bob, 'yes');
    expect(bob.atoms, contains('agent_of(alf, alice, $attestation)'));

    // Agent befriend (GnW:2): the same, from Alice's AI agents tab.  (c) Alice's composer of the befriending, Bob picked; (d) Bob asked GAI's question "Alf, an AI agent, asks to befriend you.".
    await agentAct('agent_befriend', 'gai-c-alice-asked.png');
    await run.list(bob, 'principal_of');
    await run.top(bob);
    expect(run.on(bob, find.text('Alf, an AI agent, asks to befriend you.')), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-d-bob-asked.png');
    await run.answer(bob, 'yes');
    expect(bob.atoms, contains('friend(alf)'));

    // Alf's program signs an item and sends it to Bob (GSNP:1, GSN:1).  (e) Bob's Friends tab: Alf, his friend, its row showing his record of it, its principal and its code; (a) Bob's Items tab: the item Alf authored.
    const summary = r"speech(utterance('Today\'s summary of the news'), alf)";
    expect(bob.atoms, contains('item($summary)'));
    await run.list(bob, 'friend');
    await run.top(bob);
    expect(run.control(bob, 'row:alf'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-e-bob-friends.png');
    await run.list(bob, 'item');
    await run.top(bob);
    expect(run.control(bob, 'row:$summary'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-a-bob-chat-alf.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GAI\'s app, compiled from GAI\'s contract and pragmas: Alice signing '
      'the code of Alf on her AI agents tab and entering Alf with it, Alf\'s '
      'program scripted', (tester) async {
    // GAI's app (programs/apps/gai), compiled from GAI.glp and GAI.txt with the pragmas of GnW, GSG and GKK: three tabs, Contacts, Friends and AI agents; no screen of it goes in GAI's paper (GAI #8 Cowork, Code_inbox.md, 2026-10-24 10:47 UTC).
    final run = await _Run.start(tester, screens, 'gai',
        chain: const ['alice', 'alf', 'bob'],
        pairs: const [('alice', 'alf'), ('alf', 'bob'), ('bob', 'alice')],
        scripted: const {'alf'});
    final alice = run.person('alice');
    final bob = run.person('bob');
    const code = "speech(utterance('The code of Alf'), alice)";
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(alice, 'principal_of', 'AI agents'), findsOneWidget);
    for (final p in [alice, bob]) {
      await run.join(p);
    }
    await run.meetByKey(alice, bob);

    // Developer (GAI:1): GAI's "[Sign] <code>" on the AI agents tab; Alice signs the code, its developer.
    await run.list(alice, 'principal_of');
    await run.type(alice, run.control(alice, 'field:sign::2'), 'The code of Alf');
    await run.tap(alice, run.control(alice, 'send:sign:'));
    expect(alice.atoms, contains('item($code)'));

    // Guardianship (GnW:1): GAI's "[Enter] <name> <code>", its fields typed, GAI listing no item; Alf's program answers yes.
    await run.type(alice, run.control(alice, 'field:enter::2'), 'alf');
    await run.type(alice, run.control(alice, 'field:enter::3'), 'The code of Alf');
    await run.tap(alice, run.control(alice, 'send:enter:'));
    expect(alice.atoms, containsAll(['principal_of(alf)', 'friend(alf)']));
    await run.top(alice);
    expect(run.control(alice, 'row:alf'), findsOneWidget);
    run.plain(alice);
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  // CSSN's app is compiled from its contract and its pragmas, CSSN.txt and those of the contracts it extends, GKK, GSG, GSN and GnW (programs/cssn/CSSN.txt; vGLP, Section 5.4, "The contract's words"): four tabs, Contacts, Friends, Groups and Children, "Children: the children Alice is parenting."; each button the label in brackets, Enter, Child meet, Child befriend, Child adult befriend, Of age, Child join, Child unfriend, Child leave, Child close and Parent unfriend; each question the sentence of the role asked, "Alice asks that Carol and your child Dave befriend."; and the answers yes and no.  Enter is a meeting schema, enter(p?, r?): a parent types the child's key into its field, CSSN's "<child>", and the request's hello asks the child's phone the question of the child's role, "Alice asks to be your parent." (Section 5.4, "The compilation").  The scenario is CSSN's Figure 3's (CSSN, Section 4.6, "In the Grassroots App").
  testWidgets(
      'CSSN\'s mini-app, the scenario of CSSN, Section 4.6, Alice at her '
      'phone: (1) her Children tab, Carol her child with the buttons of the '
      'contract\'s acts, after she and Bob befriend; (2) her request that '
      'Carol and Dave befriend, with Bob, as she composes it, after they have '
      'met', (tester) async {
    final run = await _cssn(tester, screens, 'alice');
    final alice = run.person('alice');
    // Four tabs, the lists of GKK's, GSG's, GSN's and CSSN's pragmas: Contacts, Friends, Groups and Children.
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(alice, 'member', 'Groups'), findsOneWidget);
    expect(run.listNamed(alice, 'parenting', 'Children'), findsOneWidget);
    expect(namedWidget(alice.request.widget)?.$1, socialMenuName);
    // Alice establishes her identity (GKK:1): Establish, on Contacts.
    await run.join(alice);
    expect(alice.atoms, ['identity']);
    // Carol enters with Alice (CSSN:1, GnW:1; enter(p?, r?)), a meeting schema: on her Children tab Alice types Carol's key into Enter's field, CSSN's "<child>", and taps Enter, CSSN's "[Enter]"; her request's hello asks Carol's phone "Alice asks to be your parent.", the question of the child's role, and Carol's phone answers yes; the two then know each other, and Alice is Carol's parent and first friend.
    await run.list(alice, 'parenting');
    expect(
        find.descendant(
            of: run.control(alice, 'key:enter'), matching: find.text('child')),
        findsOneWidget);
    expect(
        find.descendant(
            of: run.control(alice, 'key-send:enter'),
            matching: find.text('Enter')),
        findsOneWidget);
    await run.type(alice, run.control(alice, 'key:enter'), 'carol');
    await run.tap(alice, run.control(alice, 'key-send:enter'));
    expect(run.scriptedSeen('carol'), contains('gave(enter(alice, carol), yes)'));
    expect(alice.atoms, unorderedEquals(
        ['identity', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    // Dave has entered with Bob the same way.
    expect(run.scriptedAtoms('bob'), unorderedEquals(
        ['identity', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    // Alice and Bob meet (GKK:2): on her Contacts Alice types Bob's key into Meet's field, GKK's "<person>", and taps Meet; Bob's phone is asked GKK's question and answers yes.  Bob's agent then introduces Alice's and Dave's to each other, Bob knowing both, knows being the introduction predicate (CSSN, Section 4.6.1, "The program"), so Alice's agent knows Dave's, though her state does not.
    await run.list(alice, 'knows');
    await run.type(alice, run.control(alice, 'key:meet'), 'bob');
    await run.tap(alice, run.control(alice, 'key-send:meet'));
    expect(run.scriptedSeen('bob'), contains('gave(meet(alice, bob), yes)'));
    expect(alice.atoms, contains('knows(bob)'));
    // Alice and Bob befriend (GSG:1): Befriend beside Bob in her Contacts, and Bob answers yes.
    await run.tap(alice, run.control(alice, 'act:befriend:bob'));
    expect(run.scriptedSeen('bob'), contains('gave(befriend(alice, bob), yes)'));
    expect(alice.atoms, contains('friend(bob)'));
    expect(alice.known, unorderedEquals(['bob', 'carol', 'dave']));
    expect(alice.atoms, isNot(contains('knows(dave)')));
    // (1) Alice's Children tab: Carol's row, the one atom of parenting, with the buttons of the acts that name a child of the proposer's, Child meet (child_meet and child_meet_2, one button each, both CSSN's "[Child meet]"), Child befriend, Child adult befriend, Of age and Child unfriend; and Enter with its field child.  Dave, whom her agent knows, is on no tab: the tabs list the atoms of her state, and a person her agent knows is offered where she picks a person into a place.
    await run.list(alice, 'parenting');
    await run.top(alice);
    expect(run.control(alice, 'row:carol'), findsOneWidget);
    expect(run.control(alice, 'row:bob'), findsNothing);
    expect(run.control(alice, 'row:dave'), findsNothing);
    for (final f in [
      'child_meet',
      'child_meet_2',
      'child_befriend',
      'child_adult_befriend',
      'of_age',
      'child_unfriend'
    ]) {
      expect(run.control(alice, 'act:$f:carol'), findsOneWidget, reason: f);
    }
    expect(
        find.descendant(
            of: run.control(alice, 'act:child_befriend:carol'),
            matching: find.text('Child befriend')),
        findsOneWidget);
    // The person's own name is drawn on the first tab, Contacts, and not here.
    expect(run.texts(alice),
        containsAllInOrder(['Children', 'Carol', 'Child meet']));
    run.plain(alice);
    await run.shoot(alice, 'cssn-app-1.png');
    // Alice asks that Carol and Dave meet, with Bob (CSSN:2; child_meet_2(r?, s?, p?, q?)): Child meet beside Carol, who fills the first child, r; she picks Dave into the second, s, and Bob into the other parent, q, each place named by its type, Peer, no pragma naming it, and taps Child meet, which proposes child_meet_2(carol, dave, alice, bob).  Bob, Carol and Dave approve, and Carol and Dave know each other.
    await run.tap(alice, run.control(alice, 'act:child_meet_2:carol'));
    await run.tap(alice, run.control(alice, 'pick:child_meet_2:carol:2:dave'));
    await run.tap(alice, run.control(alice, 'pick:child_meet_2:carol:4:bob'));
    await run.tap(alice, run.control(alice, 'send:child_meet_2:carol'));
    for (final q in ['bob', 'carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_meet_2(carol, dave, alice, bob), yes)'));
    }
    expect(run.scriptedAtoms('carol'), contains('knows(dave)'));
    expect(run.scriptedAtoms('dave'), contains('knows(carol)'));
    expect(alice.atoms, isNot(contains('knows(dave)')));
    // (2) Alice's request that Carol and Dave befriend, with Bob, as she composes it: Child befriend beside Carol, Dave picked into s and Bob into q, and the button Child befriend, which proposes child_befriend(carol, dave, alice, bob), its roles r, s, p and q in order (CSSN, Section 4.6, "Child befriending in vGLP").
    await run.tap(alice, run.control(alice, 'act:child_befriend:carol'));
    await run.tap(alice, run.control(alice, 'pick:child_befriend:carol:2:dave'));
    await run.tap(alice, run.control(alice, 'pick:child_befriend:carol:4:bob'));
    final send = run.control(alice, 'send:child_befriend:carol');
    expect(tester.widget<ElevatedButton>(send).onPressed, isNotNull);
    expect(alice.unsent, isEmpty);
    await run.foot(alice, send);
    run.plain(alice, allow: {'Peer'});
    await run.shoot(alice, 'cssn-app-2.png');
    // She taps Child befriend: Bob, Carol and Dave approve, and Carol and Dave are friends (CSSN:3); Carol then ends the friendship (GSG:2).
    await run.tap(alice, send);
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
      'asked to approve Alice\'s attempt that they befriend, with yes and no; '
      '(6) his Children tab with Dave and Of age, which declares Dave of age',
      (tester) async {
    final run = await _cssn(tester, screens, 'bob');
    final bob = run.person('bob');
    // Bob establishes his identity (GKK:1); Dave enters with Bob (CSSN:1, GnW:1): on his Children tab Bob types Dave's key into Enter's field and taps Enter, and Dave's phone answers yes.
    await run.join(bob);
    await run.list(bob, 'parenting');
    await run.type(bob, run.control(bob, 'key:enter'), 'dave');
    await run.tap(bob, run.control(bob, 'key-send:enter'));
    expect(run.scriptedSeen('dave'), contains('gave(enter(bob, dave), yes)'));
    expect(bob.atoms, unorderedEquals(
        ['identity', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    // Carol has entered with Alice; Alice asks to meet Bob by his key (GKK:2): her request's hello asks Bob, at the top of his Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; he taps yes.
    expect(run.scriptedAtoms('alice'), unorderedEquals(
        ['identity', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    final h = bob.approval;
    expect(formatTerm(h.view),
        startsWith('approval(hello(alice, meet(alice, bob)), '));
    await run.list(bob, 'knows');
    await run.top(bob);
    expect(run.textsOf(bob, h),
        ['Alice wishes that you know each other.', 'yes', 'no']);
    await run.answer(bob, 'yes');
    expect(bob.atoms, contains('knows(alice)'));
    // Alice proposes that they befriend (GSG:1): Bob is asked, on his Friends tab, GSG's question "Alice asks to be your friend.", and taps yes.
    final b = bob.approval;
    expect(formatTerm(b.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', befriend(alice, bob), none), ')));
    await run.list(bob, 'friend');
    await run.top(bob);
    expect(run.textsOf(bob, b), ['Alice asks to be your friend.', 'yes', 'no']);
    await run.answer(bob, 'yes');
    expect(bob.atoms, contains('friend(alice)'));
    // Alice asks that Carol and Dave meet, with Bob (CSSN:2): Bob is asked, on his Children tab, the question of the other parent's role, "Alice asks that Carol and your child Dave meet."; Carol and Dave have approved it, and Bob taps yes.
    final m = bob.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_meet_2(carol, dave, alice, bob), none), ')));
    await run.list(bob, 'parenting');
    await run.top(bob);
    expect(run.textsOf(bob, m),
        ['Alice asks that Carol and your child Dave meet.', 'yes', 'no']);
    await run.answer(bob, 'yes');
    expect(run.scriptedAtoms('dave'), contains('knows(carol)'));
    // (3) Alice, Carol's phone showing that Carol knows Dave, requests that Carol and Dave befriend, with Bob, her agent knowing Dave's by Bob's agent's introduction; Bob is asked, on his Children tab, badged, the question of the other parent's role, "Alice asks that Carol and your child Dave befriend.", with yes and no.  Carol and Dave have approved it.
    final a = bob.approval;
    expect(formatTerm(a.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_befriend(carol, dave, alice, bob), none), ')));
    for (final q in ['carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_befriend(carol, dave, alice, bob), yes)'));
    }
    expect(
        find.descendant(
            of: run.on(bob, find.byKey(listTabKey('parenting'))),
            matching: find.text('1')),
        findsOneWidget);
    await run.top(bob);
    expect(run.textsOf(bob, a),
        ['Alice asks that Carol and your child Dave befriend.', 'yes', 'no']);
    // At the top of the tab, above Dave's row.
    expect(
        tester.getTopLeft(run.on(bob, find.byKey(constructKey(a.id)))).dy,
        lessThan(tester.getTopLeft(run.control(bob, 'row:dave')).dy));
    run.plain(bob);
    await run.shoot(bob, 'cssn-app-3.png');
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    // Carol and Dave are friends (CSSN:3), and Carol ends the friendship (GSG:2); Bob's state does not change by either.
    expect(run.scriptedSeen('dave'),
        contains(startsWith('view([friend(carol), ')));
    expect(run.scriptedAtoms('dave'), isNot(contains('friend(carol)')));
    expect(run.scriptedAtoms('carol'), isNot(contains('friend(dave)')));
    // (6) Bob's Children tab: Dave's row with the buttons of the acts that name a child of the proposer's, Of age among them, CSSN's "[Of age]" (CSSN:4, GnW:4; of_age(p?, r)), which names Dave and nothing else, so it proposes of_age(bob, dave) when tapped, nobody being asked: its one guarding role is Bob's, and Coming of age's pragma carries no question.
    await run.top(bob);
    expect(run.control(bob, 'row:dave'), findsOneWidget);
    expect(
        find.descendant(
            of: run.control(bob, 'act:of_age:dave'),
            matching: find.text('Of age')),
        findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'cssn-app-6.png');
    await run.tap(bob, run.control(bob, 'act:of_age:dave'));
    expect(bob.questions('approval'), isEmpty);
    expect(run.scriptedSeen('dave'), isNot(contains(startsWith('gave(of_age'))));
    // It ends his parenting of Dave, and Dave is a child no more: Dave leaves Bob's Children, and the two stay friends.
    expect(bob.atoms, unorderedEquals(['identity', 'friend(dave)', 'knows(dave)',
        'friend(alice)', 'knows(alice)']));
    expect(run.control(bob, 'row:dave'), findsNothing);
    expect(run.scriptedAtoms('dave'), unorderedEquals(
        ['identity', 'friend(bob)', 'knows(bob)', 'knows(carol)']));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'CSSN\'s mini-app, the scenario of CSSN, Section 4.6, Carol at her '
      'phone: her phone asked, at its setup, "Alice asks to be your parent."; '
      'asked to meet Dave; (4) asked to approve Alice\'s attempt that they '
      'befriend; (5) her Friends, Alice and Dave; and her ending of the '
      'friendship with Dave', (tester) async {
    final run = await _cssn(tester, screens, 'carol');
    final carol = run.person('carol');
    // Carol's phone at its setup, the parents holding it (CSSN, Section 4.6, "The User Interface"): no identity.  Alice's request to enter Carol (CSSN:1, GnW:1; enter(p?, r?)), a meeting schema, reaches it as a hello, which asks, on the Children tab, the tab of CSSN, the question of the child's role, "Alice asks to be your parent.", with yes and no; the phone answers yes, its request of the same entering.
    expect(carol.atoms, isEmpty);
    final e = carol.approval;
    expect(formatTerm(e.view),
        startsWith('approval(hello(alice, enter(alice, carol)), '));
    await run.list(carol, 'parenting');
    await run.top(carol);
    expect(run.textsOf(carol, e), ['Alice asks to be your parent.', 'yes', 'no']);
    await run.answer(carol, 'yes');
    expect(carol.atoms, unorderedEquals(
        ['identity', 'parent(alice)', 'child', 'friend(alice)', 'knows(alice)']));
    // Dave has entered with Bob, and Alice and Bob have met and befriended; Alice asks that Carol and Dave meet, with Bob (CSSN:2), and Carol is asked, on her Children tab, the question of the first child's role, "Alice asks that you and Dave meet.".  Bob and Dave have approved it; she taps yes.
    final m = carol.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_meet_2(carol, dave, alice, bob), none), ')));
    for (final q in ['bob', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_meet_2(carol, dave, alice, bob), yes)'));
    }
    await run.top(carol);
    expect(run.textsOf(carol, m),
        ['Alice asks that you and Dave meet.', 'yes', 'no']);
    await run.answer(carol, 'yes');
    expect(carol.atoms, contains('knows(dave)'));
    // (4) Alice, looking at Carol's phone, which shows that Carol knows Dave, requests that Carol and Dave befriend, with Bob, and Carol is asked, on her Children tab, badged, "Alice asks that you and Dave befriend.", with yes and no.  Bob and Dave have approved it.
    final a = carol.approval;
    expect(formatTerm(a.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_befriend(carol, dave, alice, bob), none), ')));
    for (final q in ['bob', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_befriend(carol, dave, alice, bob), yes)'));
    }
    await run.top(carol);
    expect(run.textsOf(carol, a),
        ['Alice asks that you and Dave befriend.', 'yes', 'no']);
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-4.png');
    await run.answer(carol, 'yes');
    expect(carol.atoms, contains('friend(dave)'));
    expect(run.scriptedAtoms('dave'), contains('friend(carol)'));
    // (5) Carol's Friends while Dave is her friend: the atoms of friend in her state, Alice, her parent and first friend, and Dave, each with the buttons of the acts that name a friend, Unfriend among them; Bob, whom Alice's agent introduced to hers at the level of the machines, Alice knowing both, is known to her agent and on no tab, her state naming him in no atom.
    expect(carol.known, unorderedEquals(['alice', 'bob', 'dave']));
    await run.list(carol, 'friend');
    await run.top(carol);
    expect(run.control(carol, 'row:alice'), findsOneWidget);
    expect(run.control(carol, 'row:dave'), findsOneWidget);
    expect(run.control(carol, 'row:bob'), findsNothing);
    expect(run.control(carol, 'act:unfriend:dave'), findsOneWidget);
    // The rows in the order of her state, the newest first: Dave, then Alice.
    expect(run.texts(carol),
        containsAllInOrder(['Friends', 'Dave', 'Unfriend', 'Alice', 'Unfriend']));
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-5.png');
    // Carol ends the friendship with Dave (GSG:2; unfriend(p?, q)), her own act, Unfriend beside Dave: nobody is asked.
    await run.tap(carol, run.control(carol, 'act:unfriend:dave'));
    expect(carol.questions('approval'), isEmpty);
    expect(carol.atoms, unorderedEquals([
      'identity',
      'parent(alice)',
      'child',
      'friend(alice)',
      'knows(alice)',
      'knows(dave)'
    ]));
    expect(run.control(carol, 'row:dave'), findsNothing);
    expect(run.scriptedAtoms('dave'), isNot(contains('friend(carol)')));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));
}
