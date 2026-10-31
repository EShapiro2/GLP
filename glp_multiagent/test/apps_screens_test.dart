/// The screens of GSC's three apps, GSG's, GSN's and GSNP's (`programs/apps/gsg`, `programs/apps/gsn`, `programs/apps/gsnp`), of GAI's (`programs/apps/gai`), of GC's (`programs/apps/gc`) and of GFF's (`programs/apps/gff`), each person's phone as the bridge draws it, written to PNG files for GAI's paper and GSC's (`programs/apps/<app>/screens/`; GAI's figures from GSNP's app, the full platform); and of CSSN's mini-app (`programs/cssn`), for CSSN's paper (`programs/cssn/figures/`), below.
///
/// Each app's directory is a certified program: the canonical CVA platform of its contract, `platform.vglp`, compiled to GLP when the directory loads (vGLP, Section 6.3), and the mini-app's entry `miniapp/4` (`miniapp.glp`), which runs the platform's agent with the person's key and its dispatcher on the agent's asks.  The engine loads the directory and posts the entry for alice and bob over one conversation, as the plays of each directory do (`plays.glp`); for GSNP's app for dan, eve, carol, bob and fay, over four conversations, dan's with eve, eve's with carol, carol's with bob and bob's with fay, the persons of the play of GAI's GSNP figure (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4), and again for acme_ltd, alice, alf and bob, over four conversations, the play of GAI's Figure "The GAI app" on the full platform (GAI #8 Cowork, Code_inbox.md, 2026-10-24 10:47 UTC), alf scripted by the app's person (`programs/apps/gsnp/person.glp`), its script the AI agent's program, and the three persons at their phones; and for GAI's app for alice, alf and bob, over three conversations, the example of GAI's paper in its order, from their identities to alice's retiring alf, no screen of which goes in GAI's paper.  Each person's channel is `ch(Grants?, Draws)` (vGLP, Definition "Person Channel, Person Writer, GLP with Persons, Grant"): the host hands each person's bridge every term of `Draws` as it arrives (bridge.dart), and extends `Grants` with each grant the bridge returns, `input(Id, P, R)`.
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
  'acquaint(',
  'acquaint(',
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
  'bond(',
  'date(',
  'dated',
  'mint_',
  'swap_',
  'redeem_',
  'deposit_',
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
      this.watching = const {},
      this.calendars = const {}});

  /// The writer of each agent's calendar, by name, where the app requires the day: the stream of dates the device writes from its clock, one a day (GSG, miniapp/4; vGLP, Section 5.4, "Act schemas as local specifications"), which the test writes ([day]).
  final Map<String, InputInjector> calendars;

  /// The date [d] written on the calendar of [name], and the run reduced and drawn again: on a device the clock writes it.
  Future<void> day(String name, int d) async {
    calendars[name]!
        .inject(runtimeTermOf(GInt(d)))
        .forEach(engine.runtime.gq.enqueue);
    _drain();
    await settle();
  }

  _Person get alice => persons[0];
  _Person get bob => persons[1];

  /// Whether the app's platform is compiled from a contract with pragmas, GKK's, GSG's, GSN's, GSNP's, GAI's, GC's, GB's and CSSN's: every word on its screens is the contract's, its tabs the lists its view names (vGLP 1126f4f, Sections 5.4, 6.2 and 8).
  bool get worded => const {'gkk', 'gsg', 'gsn', 'gsnp', 'gai', 'gc', 'gb', 'gff', 'cssn'}
      .contains(app);

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
      Map<String, List<String>> watching = const {},
      int? firstDay}) async {
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
        ].join(', ')}], U$i), ${firstDay == null ? '[]' : 'K$i?'})',
        if (scripted.contains(chain[i]) && watching.containsKey(chain[i]))
          'village_person(${chain[i]}, ${roles[chain[i]] ?? chain[i]}, ch(D$i?, G$i), W$i?, S$i)'
        else if (scripted.contains(chain[i]))
          'miniapp_person(${chain[i]}, ${roles[chain[i]] ?? chain[i]}, ch(D$i?, G$i), S$i)',
      ],
    ];
    final play = engine.postGoal(goals.join(', '), inputs: [
      for (var i = 0; i < chain.length; i++)
        if (!scripted.contains(chain[i])) 'G$i'
        else if (watching.containsKey(chain[i])) 'W$i',
      if (firstDay != null)
        for (var i = 0; i < chain.length; i++) 'K$i',
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
    }, watching: watching, calendars: {
      if (firstDay != null)
        for (var i = 0; i < chain.length; i++) chain[i]: play.inputs['K$i']!,
    });
    if (firstDay != null) {
      for (final c in run.calendars.values) {
        c.inject(runtimeTermOf(GInt(firstDay))).forEach(engine.runtime.gq.enqueue);
      }
    }
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
        for (final l in state.lists ?? const <({String name, String predicate, bool counted})>[])
          GStruct('tab', [GString(l.name), GAtom(l.predicate), if (l.counted) GAtom('counted')]),
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

  /// [p] asks to come to know [q] by acquaint, GKK's Acquaintance (GKK:2), typing [q]'s key into its field, GKK's "<person>", on the tab of GKK, Contacts, as for an acquaintance with a peer not listed (vGLP, Section 6.2: "The button of an act that names no entry, and of an acquaintance with a peer not listed, is drawn on the tab of its contract"), and taps its button, GKK's "[Acquaint]": their request's hello asks [q], at the top of [q]'s Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; [q] taps yes, their request of the same acquaintance, and the two-phase commit runs with both approvals standing, neither asked again (vGLP, Section 5.4, "The compilation").  Each then knows the other, `knows(Q)`.  Where [shot] is given, [q]'s phone is shot asked.
  Future<void> meetByKey(_Person p, _Person q, {String? shot}) async {
    await list(p, 'knows');
    await list(q, 'knows');
    await type(p, control(p, 'key:acquaint'), q.name);
    await tap(p, control(p, 'key-send:acquaint'));
    final a = q.approval;
    expect(formatTerm(a.view),
        startsWith('approval(hello(${p.name}, acquaint(${p.name}, ${q.name})), '));
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

  /// Nothing on [p]'s phone is a term, a functor, a type name, an attempt number or a Submit button, but the words [allow].  Each word is looked for where a word begins, so that a person's name holding it, as Frank holds rank, is not taken for it.
  void plain(_Person p, {Set<String> allow = const {}}) {
    final all = texts(p).join('\n');
    for (final w in _notShown) {
      if (allow.contains(w)) continue;
      expect(all.contains(RegExp('(?<![A-Za-z])${RegExp.escape(w)}')), isFalse,
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
  Future<void> acquaint(_Person p, _Person q) async {
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
            : endsWith(', acquaint(${p.name}, ${q.name}), none), input([2]))'));
    if (worded) {
      expect(formatTerm(a.view), contains(', acquaint(${p.name}, ${q.name}), none), '));
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
      'to come to know Alice; and Bob\'s contacts once Alice has introduced Carol to '
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

    // (a) Alice's Contacts before her identity is established: her name; and Establish, GKK's "[Establish]", which names no entry and is drawn on the tab of its contract.  No Acquaint: "The button of an act that names no entry, and of a meeting with a peer not listed, is drawn on the tab of its contract, where the viewer's state holds the atoms the act requires at the proposer's role that name no other role or variable" (vGLP 516a0a2, Section 6.2), and meet requires identity at Alice's role.
    expect(run.control(alice, 'name'), findsOneWidget);
    expect(
        find.descendant(
            of: run.control(alice, 'send:establish:'),
            matching: find.text('Establish')),
        findsOneWidget);
    expect(run.control(alice, 'key-send:acquaint'), findsNothing);
    expect(alice.atoms, isEmpty);
    run.plain(alice);
    await run.shoot(alice, 'gkk-a-alice-identity.png');
    await run.join(alice);
    expect(alice.atoms, ['identity']);
    // Once her identity is established, Acquaint, GKK's "[Acquaint]", with its field for the peer's key, GKK's "<person>" (Section 5.4, "The contract's words").
    expect(
        find.descendant(
            of: run.control(alice, 'key-send:acquaint'), matching: find.text('Acquaint')),
        findsOneWidget);
    expect(
        find.descendant(
            of: run.control(alice, 'key:acquaint'), matching: find.text('person')),
        findsOneWidget);
    await run.join(bob);
    await run.join(carol);

    // (b) Alice types Bob's key into Acquaint's field and taps Acquaint (GKK:2): her request's hello asks Bob, at the top of his Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; he taps yes, his request of the same acquaintance, and each then knows the other, neither asked again.  Then Alice and Carol meet.
    await run.meetByKey(alice, bob, shot: 'gkk-b-bob-asked-to-meet.png');
    await run.meetByKey(alice, carol);
    expect(alice.atoms,
        unorderedEquals(['identity', 'knows(bob)', 'knows(carol)']));
    expect(bob.atoms, unorderedEquals(['identity', 'knows(alice)']));

    // Alice, who knows Bob and Carol, introduces Carol to Bob (GKK:3): Introduce, GKK's "[Introduce]", in Bob's row of her Contacts, Bob taking the second role of introduce, with its field beside it, GKK's "<person>", Carol, introduce's agent variable c, which Alice's role requires held, knows(c), so it is picked among the persons she knows: "A button is drawn in the row of a tab's entry, a person who takes a role of the act or an item the act names, with the act's fields beside it" (vGLP, Section 6.2); "a variable is picked among the items the viewer holds where the proposer's role requires it held" (vGLP acd3e74, Section 5.4, "The contract's words").  She picks Carol and taps Introduce.
    await run.list(alice, 'knows');
    await run.tap(alice, run.control(alice, 'act:introduce:bob'));
    expect(run.on(alice, find.text('person')), findsWidgets);
    expect(run.control(alice, 'field:introduce:bob:3'), findsNothing);
    await run.tap(alice, run.control(alice, 'pick:introduce:bob:3:carol'));
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

    // (c) Bob's contacts once he has approved: a row for each atom of knows, in the order of his state, the newest first, Carol and then Alice, each with Introduce, its field a key; Acquaint, which requires no atom of knows at his role, in no row: "A button is drawn in the row of a tab's entry, whose person takes a role of the act, ... in a row of the tab whose predicate the act requires or deletes at the proposer's role naming the entry" (vGLP 516a0a2, Section 6.2).
    await run.top(bob);
    expect(run.control(bob, 'row:alice'), findsOneWidget);
    expect(run.control(bob, 'row:carol'), findsOneWidget);
    expect(run.control(bob, 'act:acquaint:alice'), findsNothing);
    expect(run.control(bob, 'act:introduce:alice'), findsOneWidget);
    expect(run.control(bob, 'act:introduce:carol'), findsOneWidget);
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

    // Each establishes their identity (GKK:1), and Alice comes to know Bob by his key (GKK:2).  Bob's row in her Contacts then has the buttons of the acts that require knows of him at her role, Introduce, its field a key, and GSG's "[Befriend]"; not Acquaint, which requires no atom of knows, nor GSG's "[Unfriend]", which requires friend of him: "A button is drawn in the row of a tab's entry ... in a row of the tab whose predicate the act requires or deletes at the proposer's role naming the entry" (vGLP 516a0a2, Section 6.2).
    await run.join(alice);
    await run.join(bob);
    await run.meetByKey(alice, bob);
    expect(run.control(alice, 'act:befriend:bob'), findsOneWidget);
    expect(run.control(alice, 'act:introduce:bob'), findsOneWidget);
    for (final f in ['acquaint', 'unfriend']) {
      expect(run.control(alice, 'act:$f:bob'), findsNothing, reason: f);
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

    // Identity, acquaintance and friendship, as in GSG's app.
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
    // The message is a speech act, drawn with its signer: "A speech act, in a row, a question or a field, is drawn with its signers, innermost first, by the names the viewer's state holds for them" (vGLP 516a0a2, Section 6.2).
    expect(
        find.descendant(
            of: run.control(bob, 'row:alice'),
            matching: find.text('Hi Bob (Alice)')),
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
    // The group's name, a speech act of Alice's, is drawn with its signer: "A speech act, in a row, a question or a field, is drawn with its signers" (vGLP 516a0a2, Section 6.2).
    expect(run.textsOf(bob, a), [asks, 'friends\nAlice', 'yes', 'no']);
    run.plain(bob);
    await run.shoot(bob, 'gsn-b-bob-asked-to-join.png');

    // He taps yes: he is a member of the group, and Alice lists him in it.
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(bob.atoms, contains('member($friendsGroup)'));
    expect(alice.atoms, contains('listed(bob, $friends)'));

    // A group message (GSN:4): Post, GSN's "[Post]", beside the group in Bob's Groups, its field text, GSN's "<text>": he types Hi all and taps Post, which proposes post(bob, alice, x, n), in the order of post's head, post(p?, a ; x, n) (programs/gsc/GSN.glp).  Alice, its leader, is not asked; her agent opens the delivery and delivers the message to Bob.
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
    expect(inGroup, containsAllInOrder(['friends, Alice', 'Hi all (Bob), Alice']));
    expect(inGroup, contains('Leave'));
    run.plain(bob);
    await run.shoot(bob, 'gsn-c-bob-group.png');

    // The leader's own message (GSN:4): Post own, GSN's "[Post own] <text>", beside the group in Alice's Groups; she types Welcome and taps Post own, which proposes post_own(alice, x, n); her agent delivers it to Bob, unasked.
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
    // Close, GSN's "[Close]", in the group's row: close requires member of the group at the leader's role, so its button is drawn in the row of the tab of member, the row filling the group, and GSN.txt gives it no field: "A button is drawn in the row of a tab's entry, a person who takes a role of the act or an item the act names, ... in a row of the tab whose predicate the act requires or deletes at the proposer's role naming the entry" (vGLP ffcb093, Section 6.2).
    expect(inHers, containsAll(['Bob', 'Hi all (Bob)', 'Welcome (Alice)', 'Post own', 'Close']));
    expect(run.control(alice, 'act:close:$friendsGroup'), findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gsn-d-alice-group.png');

    // Alice closes the group by leaving it (GSN:2): Close in the group's row proposes close(alice, n), its listed role every member she lists; Bob's machine votes unasked, and the group ends at both.
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

    // Each establishes their identity (GKK:1), and Alice comes to know Bob by his key (GKK:2), Acquaint.
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
    // The issuer given, a variable swap requires held at Alice's role, coin(u), is picked among the issuers of the coins she holds (vGLP acd3e74, Section 5.4, "The contract's words"); the rest are typed.
    await run.tap(alice, run.control(alice, 'pick:swap:bob:3:alice'));
    for (final (i, v) in [(4, '2'), (5, 'bob'), (6, '3')]) {
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
    // After it, each value of the binding the question does not name, by the name of its field (vGLP 110caf4, Section 5.4, "The contract's words").
    expect(run.textsOf(bob, a), [
      asks,
      'issuer given: Alice',
      'amount given: 2',
      'issuer wanted: Bob',
      'amount wanted: 3',
      'yes',
      'no'
    ]);
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
      'GB\'s app on three phones: Alice mints bonds maturing at day 10; Bob '
      'asked to lend, two Alice-coins for three Bob-bonds maturing at day 5; '
      'Alice redeems a matured Bob-bond against an Alice-coin Bob holds; and '
      'Carol, the escrow agent, asked to accept Alice\'s deposit', (tester) async {
    // GB's app (programs/apps/gb), compiled down the cascade from GB.glp (programs/currencies/bonds/GB.glp) and its pragmas, GB.txt, the tasks of Currencies (Code_inbox.md, 2026-10-20 19:57 and 20:27 UTC, 2026-10-23 22:31 UTC, point 4): each establishes their identity (GKK:1) and gets the first day as their date, from the calendar (GB:2, first_date); Alice comes to know Bob and Carol, Acquaint (GKK:2); (a) Alice mints bonds maturing at day 10 (GB:1); Alice asks Bob for a loan, her coins for his bonds of larger face value maturing later, and (b) Bob is asked and answers yes (GB:3); Bob's date advances to day 5, the calendar giving the day (GB:2, advance); (c) Alice redeems a matured Bob-bond against an Alice-coin Bob holds (GB:5); and (d) Carol, the escrow agent, is asked to accept Alice's deposit of two Alice-coins, and answers yes (GB:7).
    final run = await _Run.start(tester, screens, 'gb',
        chain: const ['alice', 'bob', 'carol'],
        pairs: const [('alice', 'bob'), ('alice', 'carol')],
        firstDay: 1);
    final alice = run.alice;
    final bob = run.bob;
    final carol = run.person('carol');

    // Three tabs, the lists of GKK's and GB's pragmas: Contacts, Coins and Bonds, "Coins: the coins Alice holds." and "Bonds: the bonds Alice holds."
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'coin', 'Coins'), findsOneWidget);
    expect(run.listNamed(alice, 'bond', 'Bonds'), findsOneWidget);

    for (final p in [alice, bob, carol]) {
      await run.join(p);
      expect(p.atoms, contains('date(1)'));
    }
    await run.meetByKey(alice, bob);
    await run.meetByKey(alice, carol);

    // Alice mints five Alice-coins (GB:1): on her Coins tab, the tab of GB, Mint coin, GB's "[Mint coin] <amount>".
    await run.list(alice, 'coin');
    await run.type(alice, run.control(alice, 'field:mint_coin::2'), '5');
    await run.tap(alice, run.control(alice, 'send:mint_coin:'));
    expect(alice.atoms.where((x) => x == 'coin(alice)'), hasLength(5));

    // (a) Alice mints three Alice-bonds maturing at day 10 (GB:1), later than her date, day 1: Mint bond, GB's "[Mint bond] <maturity> <amount>", which proposes mint_bond(alice, 10, 3).
    await run.type(alice, run.control(alice, 'field:mint_bond::2'), '10');
    await run.type(alice, run.control(alice, 'field:mint_bond::3'), '3');
    await run.top(alice);
    run.plain(alice);
    await run.shoot(alice, 'gb-a-alice-mint-bond.png');
    await run.tap(alice, run.control(alice, 'send:mint_bond:'));
    expect(alice.atoms.where((x) => x == 'bond(alice, 10)'), hasLength(3));

    // Bob mints four Bob-bonds maturing at day 5.
    await run.list(bob, 'coin');
    await run.type(bob, run.control(bob, 'field:mint_bond::2'), '5');
    await run.type(bob, run.control(bob, 'field:mint_bond::3'), '4');
    await run.tap(bob, run.control(bob, 'send:mint_bond:'));
    expect(bob.atoms.where((x) => x == 'bond(bob, 5)'), hasLength(4));

    // Alice asks Bob for a loan (GB:3): Swap, GB's "[Swap]", one button for the four schemas swap_cc to swap_bb, beside Bob in her Contacts, its fields GB's "<issuer given> <maturity given> <amount given> <issuer wanted> <maturity wanted> <amount wanted>": alice, none, 2, bob, 5, 3, the maturity given left empty for a coin; which proposes swap_cb(alice, bob, alice, 2, bob, 5, 3).
    await run.list(alice, 'knows');
    await run.tap(alice, run.control(alice, 'act:swap:bob'));
    // The issuer given, a variable each swap requires held at Alice's role, is picked among the issuers of the coins and the bonds she holds (vGLP acd3e74, Section 5.4, "The contract's words"); the rest are typed.
    await run.tap(alice, run.control(alice, 'pick:swap:bob:1:alice'));
    for (final (i, v) in [(3, '2'), (4, 'bob'), (5, '5'), (6, '3')]) {
      await run.type(alice, run.control(alice, 'field:swap:bob:$i'), v);
    }
    await run.tap(alice, run.control(alice, 'send:swap:bob'));

    // (b) Bob is asked, GB's question "Alice asks to swap coins or bonds with you.", with yes and no.
    final a = bob.approval;
    expect(formatTerm(a.view),
        contains(', swap_cb(alice, bob, alice, 2, bob, 5, 3), none), '));
    await run.list(bob, 'coin');
    await run.top(bob);
    expect(run.textsOf(bob, a), [
      'Alice asks to swap coins or bonds with you.',
      'issuer given: Alice',
      'amount given: 2',
      'issuer wanted: Bob',
      'maturity wanted: 5',
      'amount wanted: 3',
      'yes',
      'no'
    ]);
    run.plain(bob);
    await run.shoot(bob, 'gb-b-bob-asked-loan.png');
    await run.answer(bob, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(alice.atoms.where((x) => x == 'bond(bob, 5)'), hasLength(3));
    expect(bob.atoms.where((x) => x == 'coin(alice)'), hasLength(2));

    // Bob's date advances to day 5, the maturity of his bonds: the calendar gives the day, and his machine takes advance once a day until his date is the day (GB:2).
    await run.day('bob', 5);
    expect(bob.atoms, contains('date(5)'));

    // (c) Alice redeems a matured Bob-bond against an Alice-coin Bob holds (GB:5): on her Bonds tab, Redeem, GB's "[Redeem] <maturity given> <issuer wanted> <maturity wanted>", beside her Bob-bonds, maturity given 5 and issuer wanted alice, the maturity wanted left empty for a coin; which proposes redeem_bc(alice, bob, 5, alice); Bob is not asked.
    await run.list(alice, 'bond');
    await run.tap(alice, run.control(alice, 'act:redeem:bob'));
    await run.type(alice, run.control(alice, 'field:redeem:bob:1'), '5');
    await run.type(alice, run.control(alice, 'field:redeem:bob:2'), 'alice');
    run.plain(alice);
    await run.shoot(alice, 'gb-c-alice-redeem.png');
    await run.tap(alice, run.control(alice, 'send:redeem:bob'));
    expect(bob.questions('approval'), isEmpty);
    expect(alice.atoms.where((x) => x == 'bond(bob, 5)'), hasLength(2));
    expect(alice.atoms.where((x) => x == 'coin(alice)'), hasLength(4));

    // Alice deposits two Alice-coins with Carol (GB:7): Deposit, GB's "[Deposit] <issuer> <maturity> <amount>", beside Carol in her Contacts, alice, none, 2; which proposes deposit_c(alice, carol, alice, 2).
    await run.list(alice, 'knows');
    await run.tap(alice, run.control(alice, 'act:deposit:carol'));
    await run.tap(alice, run.control(alice, 'pick:deposit:carol:1:alice'));
    await run.type(alice, run.control(alice, 'field:deposit:carol:3'), '2');
    await run.tap(alice, run.control(alice, 'send:deposit:carol'));

    // (d) Carol is asked, GB's question "Alice asks to deposit coins or bonds with you.", with yes and no; she answers yes, and holds the two Alice-coins.
    final d = carol.approval;
    expect(formatTerm(d.view),
        contains(', deposit_c(alice, carol, alice, 2), none), '));
    await run.list(carol, 'coin');
    await run.top(carol);
    expect(run.textsOf(carol, d), [
      'Alice asks to deposit coins or bonds with you.',
      'issuer: Alice',
      'amount: 2',
      'yes',
      'no'
    ]);
    run.plain(carol);
    await run.shoot(carol, 'gb-d-carol-asked-deposit.png');
    await run.answer(carol, 'yes');
    expect(carol.atoms.where((x) => x == 'coin(alice)'), hasLength(2));
    expect(alice.atoms.where((x) => x == 'coin(alice)'), hasLength(2));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'The village market of GC\'s paper on GC\'s app, six villagers at their '
      'phones: the six operations of its Appendix, each act a tap, and each '
      'phone\'s Coins tab at the end, holding Table 1\'s holdings',
      (tester) async {
    // The village market of GC's paper (Appendix "Village Market Demonstration", programs/currencies/coins/village.glp's script) on GC's app, programs/apps/gc (Currencies #11 Cowork, Code_inbox.md, 2026-10-21 07:25 UTC, item 6; Currencies #12 Cowork, 2026-10-23 22:31 UTC, point 3): Alice the baker, Bob the farmer, Charlie the carpenter, Diana the doctor, Eve the teacher and Frank the fisherman, each at their phone, a conversation between each two who transact, seven in all.  Each establishes their identity (GKK:1) and comes to know those they transact with, Acquaint (GKK:2); each mints their coins, 30, 39, 25, 35, 20 and 28 (GC:1), the coins of each issuer the six operations need; then the six operations, each an act of the contract: symmetric mutual credit, Alice with Bob and with Charlie 15 coins each, Charlie with Eve 10, Eve with Frank 10 (GC:2); asymmetric mutual credit, Diana's 20 diana-coins for 24 bob-coins and her 15 for 18 frank-coins (GC:2); Eve's portfolio swap, 5 frank-coins to Charlie for 5 alice-coins (GC:2); the payments, Bob to Alice 5, Eve to Charlie 6, Frank to Diana 3, Eve to Alice 3, each in coins of the payee (GC:3); Frank's redemption of 5 diana-coins, one at a time, each against a frank-coin Diana holds (GC:4); and Alice's sale of 10 bob-coins to Eve for 4 frank-coins (GC:2).  At the end each phone's Coins tab lists the holdings of GC's Table 1, a row for each issuer with its count, and is shot: the six phones of GC's Figure fig:village-screens.
    const chain = ['alice', 'bob', 'charlie', 'diana', 'eve', 'frank'];
    const pairs = [
      ('alice', 'bob'),
      ('alice', 'charlie'),
      ('alice', 'eve'),
      ('bob', 'diana'),
      ('charlie', 'eve'),
      ('diana', 'frank'),
      ('eve', 'frank'),
    ];
    final run =
        await _Run.start(tester, screens, 'gc', chain: chain, pairs: pairs);
    final v = {for (final n in chain) n: run.person(n)};

    // The coins of [q] [p] holds, by the last view.
    int held(String p, String q) =>
        v[p]!.atoms.where((x) => x == 'coin($q)').length;

    // [p] mints [k] coins (GC:1): on their Coins tab, Mint's field amount, GC's "<amount>", and Mint, GC's "[Mint]".
    Future<void> mint(String p, int k) async {
      final before = held(p, p);
      await run.list(v[p]!, 'coin');
      await run.type(v[p]!, run.control(v[p]!, 'field:mint::2'), '$k');
      await run.tap(v[p]!, run.control(v[p]!, 'send:mint:'));
      expect(held(p, p), before + k, reason: '$p mints $k');
    }

    // [p] asks [q] to swap [k] coins of [u] for [k1] coins of [w] (GC:2): Swap, GC's "[Swap]", beside [q] in their Contacts, its fields GC's "<issuer given> <amount given> <issuer wanted> <amount wanted>"; [q] is asked, GC's question "Alice asks to swap coins with you." with the binding's values after it, and answers yes; the swap commits at both.
    Future<void> swap(
        String p, String q, String u, int k, String w, int k1) async {
      final (pu, pw, qu, qw) = (held(p, u), held(p, w), held(q, u), held(q, w));
      await run.list(v[p]!, 'knows');
      await run.tap(v[p]!, run.control(v[p]!, 'act:swap:$q'));
      await run.tap(v[p]!, run.control(v[p]!, 'pick:swap:$q:3:$u'));
      for (final (i, x) in [(4, '$k'), (5, w), (6, '$k1')]) {
        await run.type(v[p]!, run.control(v[p]!, 'field:swap:$q:$i'), x);
      }
      await run.tap(v[p]!, run.control(v[p]!, 'send:swap:$q'));
      final a = v[q]!.approval;
      expect(formatTerm(a.view),
          contains(', swap($p, $q, $u, $k, $w, $k1), none), '));
      await run.list(v[q]!, 'coin');
      await run.top(v[q]!);
      expect(run.textsOf(v[q]!, a), [
        '${personName(GAtom(p))} asks to swap coins with you.',
        'issuer given: ${personName(GAtom(u))}',
        'amount given: $k',
        'issuer wanted: ${personName(GAtom(w))}',
        'amount wanted: $k1',
        'yes',
        'no'
      ]);
      await run.answer(v[q]!, 'yes');
      expect(v[q]!.questions('approval'), isEmpty);
      expect((held(p, u), held(p, w), held(q, u), held(q, w)),
          (pu - k, pw + k1, qu + k, qw - k1),
          reason: '$p swaps $k $u-coins for $k1 $w-coins with $q');
    }

    // [p] pays [k] coins of [q] to [q] (GC:3): Pay, GC's "[Pay]", beside their [q]-coins on their Coins tab, its field amount; nobody is asked.
    Future<void> pay(String p, String q, int k) async {
      final (pq, qq) = (held(p, q), held(q, q));
      await run.list(v[p]!, 'coin');
      await run.tap(v[p]!, run.control(v[p]!, 'act:pay:$q'));
      await run.type(v[p]!, run.control(v[p]!, 'field:pay:$q:3'), '$k');
      await run.tap(v[p]!, run.control(v[p]!, 'send:pay:$q'));
      expect(v[q]!.questions('approval'), isEmpty);
      expect((held(p, q), held(q, q)), (pq - k, qq + k),
          reason: '$p pays $k $q-coins to $q');
    }

    // [p] redeems a [q]-coin against an [r]-coin [q] holds (GC:4): Redeem, GC's "[Redeem]", beside their [q]-coins on their Coins tab, its field GC's "<issuer wanted>"; nobody is asked.
    Future<void> redeem(String p, String q, String r) async {
      final (pq, pr, qq, qr) = (held(p, q), held(p, r), held(q, q), held(q, r));
      await run.list(v[p]!, 'coin');
      await run.tap(v[p]!, run.control(v[p]!, 'act:redeem:$q'));
      await run.type(v[p]!, run.control(v[p]!, 'field:redeem:$q:3'), r);
      await run.tap(v[p]!, run.control(v[p]!, 'send:redeem:$q'));
      expect(v[q]!.questions('approval'), isEmpty);
      expect((held(p, q), held(p, r), held(q, q), held(q, r)),
          (pq - 1, pr + 1, qq + 1, qr - 1),
          reason: '$p redeems a $q-coin against an $r-coin');
    }

    // Each establishes their identity (GKK:1), and each two who transact come to know each other, the first by the other's key, Acquaint (GKK:2).
    for (final p in chain) {
      await run.join(v[p]!);
    }
    for (final (p, q) in pairs) {
      await run.meetByKey(v[p]!, v[q]!);
    }

    // Each mints their coins (GC:1): the coins of each issuer held across the six at the end are the coins they minted, 30 alice-coins, 39 bob-coins, 25 charlie-coins, 35 diana-coins, 20 eve-coins and 28 frank-coins (GC's paper, Appendix "Village Market Demonstration").
    for (final (p, k) in [
      ('alice', 30),
      ('bob', 39),
      ('charlie', 25),
      ('diana', 35),
      ('eve', 20),
      ('frank', 28)
    ]) {
      await mint(p, k);
    }

    // 1. Symmetric mutual credit.
    await swap('alice', 'bob', 'alice', 15, 'bob', 15);
    await swap('alice', 'charlie', 'alice', 15, 'charlie', 15);
    await swap('charlie', 'eve', 'charlie', 10, 'eve', 10);
    await swap('eve', 'frank', 'eve', 10, 'frank', 10);
    // 2. Asymmetric mutual credit.
    await swap('diana', 'bob', 'diana', 20, 'bob', 24);
    await swap('diana', 'frank', 'diana', 15, 'frank', 18);
    // 3. Portfolio swap.
    await swap('eve', 'charlie', 'frank', 5, 'alice', 5);
    // 4. Pay.
    await pay('bob', 'alice', 5);
    await pay('eve', 'charlie', 6);
    await pay('frank', 'diana', 3);
    await pay('eve', 'alice', 3);
    // 5. Redeem.
    for (var i = 0; i < 5; i++) {
      await redeem('frank', 'diana', 'frank');
    }
    // 6. Sale of debt.
    await swap('alice', 'eve', 'bob', 10, 'frank', 4);

    // The holdings of GC's Table 1, each villager's coins by issuer, their own included.
    const table = {
      'alice': {'bob': 5, 'alice': 8, 'charlie': 15, 'frank': 4},
      'bob': {'alice': 10, 'diana': 20},
      'charlie': {'alice': 10, 'eve': 10, 'charlie': 6, 'frank': 5},
      'diana': {'bob': 24, 'frank': 13, 'diana': 8},
      'eve': {'charlie': 4, 'frank': 1, 'alice': 2, 'bob': 10},
      'frank': {'diana': 7, 'eve': 10, 'frank': 5},
    };
    for (final p in chain) {
      final coins = <String, int>{};
      for (final a in v[p]!.state.atoms) {
        if (ctorArgs(a) case ('coin', [final GAtom u])) {
          coins[u.name] = (coins[u.name] ?? 0) + 1;
        }
      }
      expect(coins, table[p], reason: "$p's holdings");
      // The phone at the end: its Coins tab, a row for each issuer with its count, the view carrying the whole state (vGLP, Section 5.4, "The contract's words").
      await run.list(v[p]!, 'coin');
      await run.top(v[p]!);
      for (final MapEntry(key: u, value: k) in table[p]!.entries) {
        expect(run.name(v[p]!, 'row:$u', '${personName(GAtom(u))}  $k'),
            findsOneWidget,
            reason: "$p's row of $u-coins");
      }
      run.plain(v[p]!);
      await run.shoot(v[p]!, 'gc-village-$p.png');
    }
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 30)));

  testWidgets(
      'GSNP\'s app on five phones, the play of GAI\'s GSNP figure, compiled '
      'from GSNP\'s contract and pragmas: the friendships, Dan signing the '
      'item on his AI agents tab and sending it to Eve by Message, Eve, Carol '
      'and Bob forwarding it by Forward from their Friends tabs, and the '
      'screens of Bob\'s Items and Friends tabs and Fay\'s Items tab', (tester) async {
    // GSNP's app (programs/apps/gsnp), compiled down the cascade from GSNP.glp (programs/gai/GSNP.glp) and its pragmas, GSNP.txt with those of GSN, GAI, GSG and GKK, which it extends: five tabs, Contacts, Friends, Groups, AI agents and Items, "AI agents: the AI agents Alice is a guardian of." and "Items: the items Alice holds."
    final run = await _Run.start(tester, screens, 'gsnp',
        chain: const ['dan', 'eve', 'carol', 'bob', 'fay']);
    final dan = run.person('dan');
    final eve = run.person('eve');
    expect(run.listNamed(dan, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(dan, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(dan, 'member', 'Groups'), findsOneWidget);
    expect(run.listNamed(dan, 'guardian_of', 'AI agents'), findsOneWidget);
    expect(run.listNamed(dan, 'item', 'Items'), findsOneWidget);

    // The friendships of the play (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 4): Dan and Eve, Eve and Carol, Carol and Bob, Bob and Fay; each establishes their identity (GKK:1), the first of each two comes to know the second by their key, the second answering yes (GKK:2), and then befriends them, the second answering yes (GSG:1).
    for (final p in run.persons) {
      await run.join(p);
    }
    for (final (p, q) in [('dan', 'eve'), ('eve', 'carol'), ('carol', 'bob'), ('bob', 'fay')]) {
      await run.meetByKey(run.person(p), run.person(q));
      await run.befriend(run.person(p), run.person(q));
    }

    // Signing (GAI:1): on his AI agents tab, the tab of GAI, whose line "AI developer and guardian accountability: Alice [Sign] <text>" draws Sign there, Dan types "Flood on Main Street" into Sign's field text and taps Sign, which proposes sign(dan, x), x the speech act he signs; he holds it, its author.
    const flood = "speech(utterance('Flood on Main Street'), dan)";
    await run.list(dan, 'guardian_of');
    await run.type(dan, run.control(dan, 'field:sign::2'), 'Flood on Main Street');
    await run.tap(dan, run.control(dan, 'send:sign:'));
    expect(dan.atoms, containsAll(['item($flood)', 'authored($flood)']));

    // A message (GSN:1): Message, GSNP's "Alice [Message] <item>", beside Eve in his Friends tab, the row filling the friend, its field item the same words, which proposes message(dan, eve, x), the item he authored; Eve is not asked, and holds the item as he signed it.
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

    // Provenance (GSNP:1): Forward, GSNP's "Alice [Forward]", is drawn in each friend's row of the Friends tab, the row filling the friend it goes to (vGLP, Section 6.2: "an act that names an entry is drawn in that entry's row"), which opens its form; the person picks the item from the items they hold and taps Forward; nobody is asked.  Eve forwards it to Carol, and Carol to Bob.
    Future<void> openForward(_Person p, String item, String to) async {
      await run.list(p, 'friend');
      await run.top(p);
      await run.tap(p, run.control(p, 'act:forward:$to'));
      await run.tap(p, run.control(p, 'pick:forward:$to:3:$item'));
    }

    Future<void> forward(_Person p, String item, String to) async {
      await openForward(p, item, to);
      await run.tap(p, run.control(p, 'send:forward:$to'));
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

    // (b) Bob forwards it to Fay, his friend: Forward in Fay's row of his Friends tab, the item picked, and the button.  The item picked is drawn under the name of its place's type, Speech, no pragma naming a field for it (vGLP, Section 6.2: "a construct family draws the label it is given, and the name of the type where it is given none").
    await openForward(bob, byCarol, 'fay');
    run.plain(bob, allow: const {'Speech'});
    await run.shoot(bob, 'gsnp-b-bob-forward.png');
    await run.tap(bob, run.control(bob, 'send:forward:fay'));
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
      'platform, in the order of GAI\'s example: Acme Ltd signing Alf\'s code '
      'and sending it to Alice, Alice and Bob befriending, Alice signing her '
      'attestation and entering Alf with it, Alf\'s acquaintance with Bob and '
      'its befriending him, Alf\'s item to Bob, and Alice retiring Alf, '
      'Alf\'s program scripted', (tester) async {
    // Acme Ltd, Alice and Bob at their phones, and Alf, Alice's AI agent, scripted (programs/apps/gsnp/person.glp, the role alf), a conversation between Acme Ltd and Alice, Alice and Alf, Alice and Bob, and Alf and Bob.  The play is the example of GAI's paper in its order (Section 3.2, the example after Example "An AI agent coming to know a person, and befriending them", and Table "The components that change in the example"; Code_inbox.md, GAI #10 Cowork, 2026-10-31 12:54 and 14:14 UTC), with Acme Ltd's signing of Alf's code before it: Establish Alice and Bob; Acquaint; Befriend; Alice signs the attestation y; Enter Alf with y; Agent acquaint Alf and Bob, Alice willing; Agent befriend Alf and Bob, Alice willing; Alf signs an item and messages it to Bob; and Retire Alf, which ends Alf's friendships; no unfriend.  Its screens are those of GAI's Figure "The GAI app", gai-b to gai-e, and the last of its Figure "The GSNP app", gai-a, as their captions say.
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
    const y = "speech(utterance('Alf runs code signed by Acme Ltd'), alice)";
    const summary = r"speech(utterance('Today\'s summary of the news'), alf)";
    for (final p in [acme, alice, bob]) {
      await run.join(p);
    }

    // Acme Ltd, the developer, comes to know Alice and befriends her (GKK:2, GSG:1), signs Alf's code on its AI agents tab, GAI's "Alice [Sign] <text>" (GAI:1), and sends it to her by Message beside her in its Friends tab, GSNP's "Alice [Message] <item>" (GSN:1); Alice is not asked, and holds it as Acme Ltd signed it.
    await run.meetByKey(acme, alice);
    await run.befriend(acme, alice);
    await run.list(acme, 'guardian_of');
    await run.type(acme, run.control(acme, 'field:sign::2'), 'The code of Alf');
    await run.tap(acme, run.control(acme, 'send:sign:'));
    expect(acme.atoms, contains('authored($code)'));
    await run.list(acme, 'friend');
    await run.tap(acme, run.control(acme, 'act:message:alice'));
    await run.type(acme, run.control(acme, 'field:message:alice:3'), 'The code of Alf');
    await run.tap(acme, run.control(acme, 'send:message:alice'));
    expect(alice.questions('approval'), isEmpty);
    expect(alice.atoms, contains('item($code)'));

    // Establish (GKK:1), Acquaint (GKK:2) and Befriend (GSG:1), Alice proposing and Bob answering yes.
    await run.meetByKey(alice, bob);
    await run.befriend(alice, bob);

    // Alice signs her attestation y (GAI:2): GAI's "Alice [Sign] <text>" on her AI agents tab, the text typed.
    await run.list(alice, 'guardian_of');
    await run.type(alice, run.control(alice, 'field:sign::2'), 'Alf runs code signed by Acme Ltd');
    await run.tap(alice, run.control(alice, 'send:sign:'));
    expect(alice.atoms, containsAll(['item($y)', 'authored($y)']));

    // Enter Alf with y (GnW:1, GAI:2): GAI's "Alice [Enter] <name>", a form at the top of the AI agents tab, Alf's key typed into the field name, y filled from the item Alice authored (vGLP, Section 6.2); Alf's program answers yes, and Alice is its principal, a guardian and its friend, holding the record agent_of(alf, y).
    await run.list(alice, 'guardian_of');
    await run.type(alice, run.control(alice, 'field:enter::2'), 'alf');
    await run.tap(alice, run.control(alice, 'send:enter:'));
    expect(alice.atoms, containsAll(['principal_of(alf)', 'guardian_of(alf)', 'friend(alf)', 'agent_of(alf, $y)']));

    // (gai-b) Alice's AI agent Alf, its code signed by Acme Ltd and attested by her, and its Retire button (GnW:1, GAI:2, GnW:5): Alf's row, first in her Friends tab, with her record of its attestation, y as she signed it, and GAI's "Alice [Retire]", drawn in that row, retire requiring friend(alf) at her, the row filling Alf (vGLP, Section 6.2: "an act that names an entry is drawn in that entry's row").
    await run.list(alice, 'friend');
    await run.top(alice);
    final alfRow = run.control(alice, 'row:alf');
    expect(alfRow, findsOneWidget);
    expect(run.control(alice, 'act:retire:alf'), findsOneWidget);
    expect(find.descendant(of: alfRow, matching: find.text('Alf runs code signed by Acme Ltd (Alice)')),
        findsOneWidget);
    run.plain(alice);
    await run.shoot(alice, 'gai-b-alice-agents.png');

    // An act of Alf's knowing and relations, proposed by Alice, its guardian (GAI.txt, "Ward and person: Alice [Agent acquaint] <person>" and "Alice [Agent befriend] <person>"): its button in Alf's row of her AI agents tab, the row filling Alf, which opens its form, Bob picked, her machine filling y from her record; Alf's program answers yes, and Bob is asked GAI's question to his role and answers yes.
    Future<void> agentAct(String f, String asked,
        {String? aliceShot, String? bobShot}) async {
      await run.list(alice, 'guardian_of');
      await run.top(alice);
      await run.tap(alice, run.control(alice, 'act:$f:alf'));
      await run.tap(alice, run.control(alice, 'pick:$f:alf:2:bob'));
      if (aliceShot != null) {
        // The persons Alice may pick are drawn under the name of their place's type, Peer, as End guardian's AI agent is (vGLP, Section 6.2: "the name of the type where it is given none").
        run.plain(alice, allow: const {'Peer'});
        await run.shoot(alice, aliceShot);
      }
      await run.tap(alice, run.control(alice, 'send:$f:alf'));
      await run.list(bob, 'guardian_of');
      await run.top(bob);
      expect(run.on(bob, find.text(asked)), findsOneWidget);
      // Bob, holding no item he authored, sees Enter's attestation as a field and End guardian's AI agent, places their pragmas name no field for, under the names of their types, Speech and Peer (vGLP, Section 6.2).
      run.plain(bob, allow: const {'Peer', 'Speech'});
      if (bobShot != null) await run.shoot(bob, bobShot);
      await run.answer(bob, 'yes');
    }

    // Agent acquaint Alf and Bob, Alice willing (GnW:3, GAI:2): Bob records Alf with y.
    await agentAct('agent_acquaint', 'Alice asks that you come to know their AI agent Alf.');
    expect(bob.atoms, containsAll(['knows(alf)', 'agent_of(alf, $y)']));

    // Agent befriend Alf and Bob, Alice willing (GnW:3): (gai-c) Alice proposing that Alf befriend Bob, Bob picked, with the button; (gai-d) Bob asked to befriend Alf, an AI agent of Alice, "Alice asks that you and their AI agent Alf befriend." (GAI.txt, "Ward and person").
    await agentAct('agent_befriend', 'Alice asks that you and their AI agent Alf befriend.',
        aliceShot: 'gai-c-alice-asked.png', bobShot: 'gai-d-bob-asked.png');
    expect(bob.atoms, contains('friend(alf)'));

    // Alf's program, once Bob is its friend, signs an item and sends it to him (GSNP:1, GAI:1, GSN:1); Bob is not asked.
    expect(bob.atoms, contains('item($summary)'));
    expect(bob.questions('approval'), isEmpty);

    // (gai-e) Bob's Friends tab: Alf among his friends, its row showing his record of it, agent_of(alf, y): its principal, Alice, who signed y, and its developer, Acme Ltd, whose code y attests.
    await run.list(bob, 'friend');
    await run.top(bob);
    final bobsAlf = run.control(bob, 'row:alf');
    expect(bobsAlf, findsOneWidget);
    expect(find.descendant(of: bobsAlf, matching: find.text('Alf runs code signed by Acme Ltd (Alice)')),
        findsOneWidget);
    // Nothing beside Alf that his role forbids: Bob's agent_of(alf, y) shows that Alf holds agent, which no schema deletes, and Acquaint and Befriend forbid it at Alf's role (vGLP 893d4ea, Section 6.2).
    for (final f in ['acquaint', 'befriend', 'introduce']) {
      expect(run.control(bob, 'act:$f:alf'), findsNothing, reason: f);
    }
    run.plain(bob);
    await run.shoot(bob, 'gai-e-bob-friends.png');

    // (gai-a) Bob's Items tab: the item the AI agent Alf authored.
    await run.list(bob, 'item');
    await run.top(bob);
    expect(run.control(bob, 'row:$summary'), findsOneWidget);
    run.plain(bob);
    await run.shoot(bob, 'gai-a-bob-chat-alf.png');

    // Retire Alf (GnW:5): GAI's "Alice [Retire]" in Alf's row of her Friends tab, which Alice taps once Bob holds Alf's item; Alf and Bob, in retire's listed role Carol[friend(*)], are asked nothing, and Alf's friendships with Alice and with Bob end, Alice holding retired_of(alf), Bob his record and the item.
    await run.list(alice, 'friend');
    await run.top(alice);
    await run.tap(alice, run.control(alice, 'act:retire:alf'));
    expect(alice.atoms, contains('retired_of(alf)'));
    expect(alice.atoms, isNot(contains('friend(alf)')));
    expect(bob.atoms, isNot(contains('friend(alf)')));
    expect(bob.atoms, containsAll(['friend(alice)', 'agent_of(alf, $y)', 'item($summary)']));
    expect(bob.questions('approval'), isEmpty);
    expect(run.scriptedAtoms('alf'), contains('retired'));
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GAI\'s app, compiled from GAI\'s contract and pragmas: the example of '
      'GAI\'s paper in its order, Alice creating Alf with her attestation, '
      'proposing its acquaintance with Bob and its befriending him, and '
      'retiring it, Alf\'s program scripted', (tester) async {
    // GAI's app (programs/apps/gai), compiled from GAI.glp and GAI.txt with the pragmas of GnW, GSG and GKK: three tabs, Contacts, Friends and AI agents, "AI agents: the AI agents Alice is a guardian of."; no screen of it goes in GAI's paper (GAI #8 Cowork, Code_inbox.md, 2026-10-24 10:47 UTC).  The play is the example of GAI's paper (Section 3.2, the example after Example "An AI agent coming to know a person, and befriending them", and Table "The components that change in the example"; GAI at e4d935c), as play_agents of programs/apps/gai plays it (GAI #10 Cowork, Code_inbox.md, 2026-10-31 12:54 UTC): Establish Alice and Bob; Acquaint; Befriend; Alice signs the attestation y; Enter Alf with y; Agent acquaint Alf and Bob, Alice willing; Agent befriend Alf and Bob, Alice willing; and Retire Alf, which ends Alf's friendships.  Alf's item, which it signs and sends to Bob, is GSNP's, the full platform, below; there is no unfriend.
    final run = await _Run.start(tester, screens, 'gai',
        chain: const ['alice', 'alf', 'bob'],
        pairs: const [('alice', 'alf'), ('alf', 'bob'), ('bob', 'alice')],
        scripted: const {'alf'});
    final alice = run.person('alice');
    final bob = run.person('bob');
    const y = "speech(utterance('Alf runs code signed by Acme Ltd'), alice)";
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(alice, 'guardian_of', 'AI agents'), findsOneWidget);
    // Establish (GKK:1), Acquaint (GKK:2) and Befriend (GSG:1), Alice proposing and Bob answering yes.
    for (final p in [alice, bob]) {
      await run.join(p);
    }
    await run.meetByKey(alice, bob);
    await run.befriend(alice, bob);

    // Alice signs her attestation y (GAI:2): GAI's "Alice [Sign] <text>" on the AI agents tab, the text typed.
    await run.list(alice, 'guardian_of');
    await run.type(alice, run.control(alice, 'field:sign::2'), 'Alf runs code signed by Acme Ltd');
    await run.tap(alice, run.control(alice, 'send:sign:'));
    expect(alice.atoms, containsAll(['item($y)', 'authored($y)']));

    // Enter Alf with y (GnW:1, GAI:2): GAI's "Alice [Enter] <name>", a form at the top of the AI agents tab, the tab of its contract, naming no row's entry (vGLP, Section 6.2), Alf's key typed into the field name, y filled from the item Alice authored, a held atom filling the place no row and no field fills (vGLP, Section 6.2); Alf's program answers yes, and Alice is its principal, a guardian and its friend, holding the record agent_of(alf, y).
    await run.list(alice, 'guardian_of');
    await run.type(alice, run.control(alice, 'field:enter::2'), 'alf');
    await run.tap(alice, run.control(alice, 'send:enter:'));
    expect(alice.atoms, containsAll(['principal_of(alf)', 'guardian_of(alf)', 'friend(alf)', 'agent_of(alf, $y)']));
    await run.list(alice, 'guardian_of');
    await run.top(alice);
    expect(run.control(alice, 'row:alf'), findsOneWidget);
    // End guardian's form names the AI agent at a place its pragma gives no field name, drawn under the name of its type, Peer (vGLP, Section 6.2: "the name of the type where it is given none").
    run.plain(alice, allow: const {'Peer'});

    // An act of Alf's knowing and relations, proposed by Alice, its guardian (GAI.txt, "Ward and person: Alice [Agent acquaint] <person>" and "Alice [Agent befriend] <person>"): its button in Alf's row of her AI agents tab, the row filling Alf (vGLP, Section 6.2: "an act that names an entry is drawn in that entry's row"), which opens its form, Bob picked, her machine filling y from her record; Alf's program answers yes, and Bob is asked GAI's question to his role and answers yes.
    Future<void> agentAct(String f, String asked) async {
      await run.list(alice, 'guardian_of');
      await run.top(alice);
      await run.tap(alice, run.control(alice, 'act:$f:alf'));
      await run.tap(alice, run.control(alice, 'pick:$f:alf:2:bob'));
      await run.tap(alice, run.control(alice, 'send:$f:alf'));
      await run.list(bob, 'guardian_of');
      await run.top(bob);
      expect(run.on(bob, find.text(asked)), findsOneWidget);
      // Bob, holding no item he authored, sees Enter's attestation as a field and End guardian's AI agent, places their pragmas name no field for, under the names of their types, Speech and Peer (vGLP, Section 6.2: "the name of the type where it is given none").
      run.plain(bob, allow: const {'Peer', 'Speech'});
      await run.answer(bob, 'yes');
    }

    // Agent acquaint Alf and Bob, Alice willing (GnW:3, GAI:2): Bob records Alf with y.
    await agentAct('agent_acquaint', 'Alice asks that you come to know their AI agent Alf.');
    expect(bob.atoms, containsAll(['knows(alf)', 'agent_of(alf, $y)']));
    // Agent befriend Alf and Bob, Alice willing (GnW:3).
    await agentAct('agent_befriend', 'Alice asks that you and their AI agent Alf befriend.');
    expect(bob.atoms, contains('friend(alf)'));

    // Retire Alf (GnW:5): GAI's "Alice [Retire]" in Alf's row of her Friends tab, retire requiring friend(alf) at her (vGLP, Section 6.2), the row filling Alf; Alf and Bob, in retire's listed role Carol[friend(*)], are asked nothing, and Alf's friendships with Alice and with Bob end, Alice holding retired_of(alf).
    await run.list(alice, 'friend');
    await run.top(alice);
    await run.tap(alice, run.control(alice, 'act:retire:alf'));
    expect(alice.atoms, contains('retired_of(alf)'));
    expect(alice.atoms, isNot(contains('friend(alf)')));
    expect(bob.atoms, isNot(contains('friend(alf)')));
    expect(bob.atoms, containsAll(['friend(alice)', 'agent_of(alf, $y)']));
    expect(bob.questions('approval'), isEmpty);
    run.plain(alice, allow: const {'Peer'});
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  // GFF's app (programs/apps/gff), compiled down the cascade from GFF.glp and its pragmas, GFF.txt, with those of GSG and GKK, which it extends (programs/gff/GFF.txt at GLP 0a3d7997): four tabs, Contacts, Friends, Posts and Feeds, "Posts: Alice's feed." (GFF.txt at GLP 56270cb6) and "Feeds: the agents Alice follows."; the buttons Post, Follow and Unfollow, GFF's "[Post] <item>", "[Follow]" and "[Unfollow]".  Friends follow each other by an act each machine takes, friends_follow; a person follows anyone else by one step, Follow, follow(Me, B), and the machine asks every friend and follows through a friend, through_friend and ask, unguarded.  The scenario is the play of GFF's app, play_feeds (programs/apps/gff/plays.glp and README.md), each act a tap or a picked or typed field, its screens those of GFF #1 Cowork's tasks of 2026-10-27 18:00, 18:55 and 20:25 UTC, written to programs/apps/gff/screens/gff-app-<n>.png.
  testWidgets(
      'GFF\'s app on five phones, the play of GFF\'s feeds and followers: '
      '(1) Alice\'s Posts after her two posts; (2) Carol\'s Follow in Alice\'s row of her Contacts; (3) Carol\'s Feeds; '
      '(4) Eve\'s Feeds with the third item signed by Alice, Bob, Carol and '
      'Dave; (5) Dave\'s Feeds with Carol\'s path answer; (6) Unfollow in '
      'Alice\'s row of Carol\'s Feeds, and (7) Carol\'s Feeds after it', (tester) async {
    final run = await _Run.start(tester, screens, 'gff',
        chain: const ['alice', 'bob', 'carol', 'dave', 'eve']);
    final alice = run.person('alice');
    final bob = run.person('bob');
    final carol = run.person('carol');
    final dave = run.person('dave');
    final eve = run.person('eve');
    // Four tabs, the lists of GKK's, GSG's and GFF's pragmas: Contacts, Friends, Posts and Feeds.
    expect(run.listNamed(alice, 'knows', 'Contacts'), findsOneWidget);
    expect(run.listNamed(alice, 'friend', 'Friends'), findsOneWidget);
    expect(run.listNamed(alice, 'follows', 'Feeds'), findsOneWidget);
    expect(run.listNamed(alice, 'feed', 'Posts'), findsOneWidget);

    // The texts of [p]'s row [k], in order.
    List<String> rowTexts(_Person p, String k) => [
          for (final t in run.tester.widgetList<Text>(find.descendant(
              of: run.control(p, 'row:$k'), matching: find.byType(Text))))
            if (t.data != null && t.data!.length > 1) t.data!,
        ];

    // The friendships of the play: Alice and Bob, Bob and Carol, Carol and Dave, Dave and Eve; each establishes their identity (GKK:1), the first of each two comes to know the second by their key, the second answering yes (GKK:2), and then befriends them, the second answering yes (GSG:1).  Friends follow each other (GFF:2, friends_follow), each machine taking it unasked.
    for (final p in run.persons) {
      await run.join(p);
    }
    for (final (p, q) in [(alice, bob), (bob, carol), (carol, dave), (dave, eve)]) {
      await run.meetByKey(p, q);
      await run.befriend(p, q);
      expect(p.atoms, contains('follows(${q.name}, ${q.name})'));
      expect(q.atoms, contains('follows(${p.name}, ${p.name})'));
    }

    // [p] posts [text] (GFF:1): Post's field item, GFF's "<item>", and Post, GFF's "[Post]", on the tab of GFF, which proposes post(p, x), x the speech act they sign at its posting.
    String said(String text, String by) => "speech(utterance('$text'), $by)";
    Future<void> post(_Person p, String text) async {
      await run.list(p, 'feed');
      await run.type(p, run.control(p, 'field:post::2'), text);
      await run.tap(p, run.control(p, 'send:post:'));
      expect(p.atoms, contains('feed(${said(text, p.name)})'));
    }

    // Alice posts two items; Bob, her friend, follows her, and her machine delivers both to him as she signed them (GFF:4, deliver_own).
    await post(alice, 'First item');
    await post(alice, 'Second item');
    final first = said('First item', 'alice');
    final second = said('Second item', 'alice');

    // (1) Alice's Posts tab, the tab of GFF, listing feed, "Posts: Alice's feed.": at its top Post with its field item, GFF's "[Post] <item>", and a row for each item she posted, its text and under it its signer, Alice.
    await run.list(alice, 'feed');
    await run.top(alice);
    expect(run.control(alice, 'field:post::2'), findsOneWidget);
    for (final x in [first, second]) {
      expect(run.control(alice, 'row:$x'), findsOneWidget, reason: x);
    }
    expect(rowTexts(alice, first), contains('First item'));
    run.plain(alice);
    await run.shoot(alice, 'gff-app-1.png');
    expect(bob.atoms, containsAll(['item($first, alice)', 'item($second, alice)']));

    // [p] introduces [author] to their friend [q] (GKK:3): Introduce in [q]'s row of their Contacts, the author picked into its field person among the persons [p] knows; [q] is asked GKK's question and answers yes.
    Future<void> introduce(_Person p, _Person q, String author) async {
      await run.list(p, 'knows');
      await run.tap(p, run.control(p, 'act:introduce:${q.name}'));
      await run.tap(p, run.control(p, 'pick:introduce:${q.name}:3:$author'));
      await run.tap(p, run.control(p, 'send:introduce:${q.name}'));
      final a = q.approval;
      expect(formatTerm(a.view),
          contains(', introduce(${p.name}, ${q.name}, $author), none), '));
      await run.list(q, 'knows');
      await run.top(q);
      expect(run.textsOf(q, a), [
        '${personName(GAtom(p.name))} asks to introduce ${personName(GAtom(author))} to you.',
        'yes',
        'no'
      ]);
      run.plain(q);
      await run.answer(q, 'yes');
      expect(q.atoms, contains('knows($author)'));
    }

    // [p] decides to follow [author] (GFF:2, follow): Follow, GFF's "[Follow]", in [author]'s row of their Contacts, with no field; their machine asks every friend whether they follow [author] (GFF:3, ask) and follows [author] through a friend who does (through_friend), unasked.  Where [shot] is given, [p]'s Contacts are shot before the tap.
    Future<void> follow(_Person p, String author, String through,
        {String? shot}) async {
      await run.list(p, 'knows');
      await run.top(p);
      final f = run.control(p, 'act:follow:$author');
      expect(find.descendant(of: f, matching: find.text('Follow')), findsOneWidget);
      if (shot != null) {
        await run.reveal(p, f);
        run.plain(p);
        await run.shoot(p, shot);
      }
      await run.tap(p, f);
      expect(p.atoms,
          containsAll(['decided($author)', 'follows($author, $through)', 'following($author)']));
      expect(p.atoms.where((x) => x.startsWith('path($author, $through, ')), isNotEmpty,
          reason: '${p.name}: no path answer of $through');
    }

    // Bob introduces Alice to Carol (GKK:3), whom Carol must know to decide to follow her.
    await introduce(bob, carol, 'alice');

    // (2) Carol's Contacts: Follow in Alice's row.  She taps it; her machine follows Alice through Bob and asks Bob and Dave, Bob answering with a path, an item of Alice's feed he holds, and Dave that he does not follow her (GFF:3); Bob's machine delivers the two items to her, signed by him (GFF:4, deliver_on).
    await follow(carol, 'alice', 'bob', shot: 'gff-app-2.png');
    expect(carol.atoms, contains('does_not_follow(alice, dave)'));
    expect(carol.atoms, contains('item(speech($first, bob), alice)'));

    // (3) Carol's Feeds tab: Alice, the agent she follows, with Bob, the friend she follows her through, the items Bob delivered, each signed by Alice and Bob, Bob's answer, his path to Alice, and Dave's, that he does not follow her.
    await run.list(carol, 'follows');
    await run.top(carol);
    expect(rowTexts(carol, 'alice').take(2), ['Alice', 'Bob']);
    expect(rowTexts(carol, 'alice'),
        containsAll(['First item (Alice, Bob)', 'Second item (Alice, Bob)']));
    run.plain(carol);
    await run.shoot(carol, 'gff-app-3.png');

    // Carol introduces Alice to Dave, and Dave follows her through Carol; Dave introduces her to Eve, and Eve follows her through Dave.
    await introduce(carol, dave, 'alice');
    await follow(dave, 'alice', 'carol');
    expect(dave.atoms, contains('does_not_follow(alice, eve)'));

    // (5) Dave's Feeds tab: Alice, with Carol, the friend he follows her through, and the answers to his asking: Carol's path to Alice, an item of Alice's signed by Alice and Bob, and Eve's, that she does not follow her.
    await run.list(dave, 'follows');
    await run.top(dave);
    expect(rowTexts(dave, 'alice').take(2), ['Alice', 'Carol']);
    run.plain(dave);
    await run.shoot(dave, 'gff-app-5.png');

    await introduce(dave, eve, 'alice');
    await follow(eve, 'alice', 'dave');

    // Alice posts a third item, which reaches Eve signed by Alice at its posting and by Bob, Carol and Dave on its way (GFF:4).
    await post(alice, 'Third item');
    final third = said('Third item', 'alice');
    expect(eve.atoms,
        contains('item(speech(speech(speech($third, bob), carol), dave), alice)'));

    // (4) Eve's Feeds tab: Alice, followed through Dave, and the items of her feed, each with its signatures, Alice, Bob, Carol and Dave.
    await run.list(eve, 'follows');
    await run.top(eve);
    expect(rowTexts(eve, 'alice').take(3),
        ['Alice', 'Dave', 'Third item (Alice, Bob, Carol, Dave)']);
    run.plain(eve);
    await run.shoot(eve, 'gff-app-4.png');

    // (6) Carol's Feeds tab with Unfollow, GFF's "[Unfollow]", in Alice's row, the row's atom follows(alice, bob) filling Alice and Bob (vGLP, Section 6.2, "The words a platform shows").
    await run.list(carol, 'follows');
    await run.top(carol);
    final unfollow = run.control(carol, 'act:unfollow:alice');
    expect(find.descendant(of: unfollow, matching: find.text('Unfollow')), findsOneWidget);
    run.plain(carol);
    await run.shoot(carol, 'gff-app-6.png');

    // Carol ends her following of Alice (GFF:2, unfollow), which deletes her decision; Bob's machine is not asked.
    await run.tap(carol, unfollow);
    expect(bob.questions('approval'), isEmpty);
    for (final x in ['decided(alice)', 'following(alice)', 'follows(alice, bob)']) {
      expect(carol.atoms, isNot(contains(x)), reason: x);
    }
    expect(bob.atoms, isNot(contains('follower(carol, alice)')));

    // (7) Carol's Feeds tab after it: Alice gone from it; Bob and Dave, her friends, whom she follows by friendship.
    await run.top(carol);
    expect(run.control(carol, 'row:alice'), findsNothing);
    run.plain(carol);
    await run.shoot(carol, 'gff-app-7.png');

    // Alice posts a fourth item, which reaches Bob and not Carol, Dave or Eve.
    await post(alice, 'Fourth item');
    expect(bob.atoms, contains('item(${said('Fourth item', 'alice')}, alice)'));
    for (final p in [carol, dave, eve]) {
      expect(p.atoms.where((x) => x.contains('Fourth item')), isEmpty, reason: p.name);
    }
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 30)));

  // CSSN's app is compiled from its contract and its pragmas, CSSN.txt and those of the contracts it extends, GKK, GSG, GSN and GnW (programs/cssn/CSSN.txt; vGLP, Section 5.4, "The contract's words"): four tabs, Contacts, Friends, Groups and Children, "Children: the children Alice is parenting."; each button the label in brackets, Enter, Child acquaint, Child befriend, Child adult befriend, Of age, Child join, Child unfriend, Child leave, Child close and Parent unfriend; each question the sentence of the role asked, "Alice asks that Carol and your child Dave befriend."; and the answers yes and no.  Enter is a meeting schema, enter(p?, r?): a parent types the child's key into its field, CSSN's "<child>", and the request's hello asks the child's phone the question of the child's role, "Alice asks to be your parent." (Section 5.4, "The compilation").  The scenario is CSSN's Figure 3's (CSSN, Section 4.6, "In the Grassroots App").
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
        ['identity', 'created(carol)', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    // Dave has entered with Bob the same way.
    expect(run.scriptedAtoms('bob'), unorderedEquals(
        ['identity', 'created(dave)', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    // Alice and Bob come to know each other (GKK:2): on her Contacts Alice types Bob's key into Acquaint's field, GKK's "<person>", and taps Acquaint; Bob's phone is asked GKK's question and answers yes.  Bob's agent then introduces Alice's and Dave's to each other, Bob knowing both, knows being the introduction predicate (CSSN, Section 4.6.1, "The program"), so Alice's agent knows Dave's, though her state does not.
    await run.list(alice, 'knows');
    await run.type(alice, run.control(alice, 'key:acquaint'), 'bob');
    await run.tap(alice, run.control(alice, 'key-send:acquaint'));
    expect(run.scriptedSeen('bob'), contains('gave(acquaint(alice, bob), yes)'));
    expect(alice.atoms, contains('knows(bob)'));
    // Alice and Bob befriend (GSG:1): Befriend beside Bob in her Contacts, and Bob answers yes.
    await run.tap(alice, run.control(alice, 'act:befriend:bob'));
    expect(run.scriptedSeen('bob'), contains('gave(befriend(alice, bob), yes)'));
    expect(alice.atoms, contains('friend(bob)'));
    expect(alice.known, unorderedEquals(['bob', 'carol', 'dave']));
    expect(alice.atoms, isNot(contains('knows(dave)')));
    // (1) Alice's Children tab (vGLP feda043, Section 6.2, "The words a platform shows"): at its top Enter with its field child, and the button of each act that names two or more beside its proposer, each a form its places picked from the rows, Child acquaint once for child_acquaint and child_acquaint_2, Child befriend and Make parent among them; and Carol's row, the one atom of parenting, with the buttons of the acts that name one, require parenting of her at Alice's role and whose precondition Alice's state meets, Of age and Retire.  Dave, whom her agent knows, is on no tab: the tabs list the atoms of her state, and a person her agent knows is offered where she picks a person into a place.
    await run.list(alice, 'parenting');
    await run.top(alice);
    expect(run.control(alice, 'row:carol'), findsOneWidget);
    expect(run.control(alice, 'row:bob'), findsNothing);
    expect(run.control(alice, 'row:dave'), findsNothing);
    for (final f in ['of_age', 'retire']) {
      expect(run.control(alice, 'act:$f:carol'), findsOneWidget, reason: f);
    }
    for (final f in [
      'acquaint',
      'befriend',
      'child_acquaint',
      'child_acquaint_2',
      'child_befriend',
      'make_parent',
      'enter'
    ]) {
      expect(run.control(alice, 'act:$f:carol'), findsNothing, reason: f);
    }
    for (final f in ['child_acquaint', 'child_befriend', 'make_parent']) {
      expect(run.control(alice, 'top:$f'), findsOneWidget, reason: f);
    }
    expect(
        find.descendant(
            of: run.control(alice, 'top:child_acquaint'),
            matching: find.text('Child acquaint')),
        findsOneWidget);
    expect(find.text('Child acquaint'), findsOneWidget);
    // The person's own name is drawn on the first tab, Contacts, and not here.
    expect(run.texts(alice),
        containsAllInOrder(['Children', 'Child acquaint', 'Carol', 'Of age']));
    run.plain(alice);
    await run.shoot(alice, 'cssn-app-1.png');
    // Alice asks that Carol and Dave come to know each other, with Bob (CSSN:2): she taps Child acquaint, picks Carol into the first child from her Children, Dave into the second and Bob into the other parent, and taps Child acquaint, which proposes child_acquaint_2(carol, dave, alice, bob), the schema of the button whose places are those she picked.  Bob, Carol and Dave approve, and Carol and Dave know each other.
    await run.tap(alice, run.control(alice, 'top:child_acquaint'));
    await run.tap(alice, run.control(alice, 'pick:child_acquaint::1:carol'));
    await run.tap(alice, run.control(alice, 'pick:child_acquaint::2:dave'));
    await run.tap(alice, run.control(alice, 'pick:child_acquaint::4:bob'));
    await run.tap(alice, run.control(alice, 'send:child_acquaint:'));
    for (final q in ['bob', 'carol', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_acquaint_2(carol, dave, alice, bob), yes)'));
    }
    expect(run.scriptedAtoms('carol'), contains('knows(dave)'));
    expect(run.scriptedAtoms('dave'), contains('knows(carol)'));
    expect(alice.atoms, isNot(contains('knows(dave)')));
    // (2) Alice's request that Carol and Dave befriend, with Bob, as she composes it: Child befriend at the top of her Children, Carol picked into r from her Children, Dave into s and Bob into q from her Friends, and the button Child befriend, which proposes child_befriend(carol, dave, alice, bob), its roles r, s, p and q in order (CSSN, Section 4.6, "Child befriending in vGLP").
    await run.top(alice);
    await run.tap(alice, run.control(alice, 'top:child_befriend'));
    await run.tap(alice, run.control(alice, 'pick:child_befriend::1:carol'));
    await run.tap(alice, run.control(alice, 'pick:child_befriend::2:dave'));
    await run.tap(alice, run.control(alice, 'pick:child_befriend::4:bob'));
    // q, an item of friend at Alice's role, act(child_befriend, "Children", 3, [1, 2, 4], [], [item(1, parenting), item(4, friend), ...]), is picked from the rows of her Friends, Bob and Carol, and neither Dave nor Alice.
    final form = run.control(alice, 'form:child_befriend:');
    expect(form, findsOneWidget);
    expect(run.control(alice, 'pick:child_befriend::4:bob'), findsOneWidget);
    for (final q in ['dave', 'alice']) {
      expect(run.control(alice, 'pick:child_befriend::4:$q'), findsNothing);
    }
    final send = run.control(alice, 'send:child_befriend:');
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
      'asked to come to know Alice and to approve the acquaintance of Carol and Dave; (3) '
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
        ['identity', 'created(dave)', 'parenting(dave)', 'friend(dave)', 'knows(dave)']));
    // Carol has entered with Alice; Alice asks to come to know Bob by his key (GKK:2): her request's hello asks Bob, at the top of his Contacts, GKK's question "Alice wishes that you know each other.", with yes and no; he taps yes.
    expect(run.scriptedAtoms('alice'), unorderedEquals(
        ['identity', 'created(carol)', 'parenting(carol)', 'friend(carol)', 'knows(carol)']));
    final h = bob.approval;
    expect(formatTerm(h.view),
        startsWith('approval(hello(alice, acquaint(alice, bob)), '));
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
    // Alice asks that Carol and Dave come to know each other, with Bob (CSSN:2): Bob is asked, on his Children tab, the question of the other parent's role, "Alice wishes that Carol and your child Dave know each other."; Carol and Dave have approved it, and Bob taps yes.
    final m = bob.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_acquaint_2(carol, dave, alice, bob), none), ')));
    await run.list(bob, 'parenting');
    await run.top(bob);
    expect(run.textsOf(bob, m),
        ['Alice wishes that Carol and your child Dave know each other.', 'yes', 'no']);
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
      'asked to come to know Dave; (4) asked to approve Alice\'s attempt that they '
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
        ['identity', 'creator(alice)', 'child', 'friend(alice)', 'knows(alice)']));
    // Dave has entered with Bob, and Alice and Bob have come to know each other and befriended; Alice asks that Carol and Dave come to know each other, with Bob (CSSN:2), and Carol is asked, on her Children tab, the question of the first child's role, "Alice wishes that you and Dave know each other.".  Bob and Dave have approved it; she taps yes.
    final m = carol.approval;
    expect(formatTerm(m.view),
        allOf(startsWith('approval(attempt(alice, '),
            contains(', child_acquaint_2(carol, dave, alice, bob), none), ')));
    for (final q in ['bob', 'dave']) {
      expect(run.scriptedSeen(q),
          contains('gave(child_acquaint_2(carol, dave, alice, bob), yes)'));
    }
    await run.top(carol);
    expect(run.textsOf(carol, m),
        ['Alice wishes that you and Dave know each other.', 'yes', 'no']);
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
    // A child is shown no act whose own precondition at each of its guarding roles the child's state does not meet: Enter, Child unfriend, Child leave and Child close each forbid child there (vGLP 897d1ce, Section 6.2).
    expect(run.control(carol, 'key-send:enter'), findsNothing);
    for (final f in ['child_unfriend', 'child_leave', 'child_close']) {
      expect(run.control(carol, 'top:$f'), findsNothing, reason: f);
    }
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-4.png');
    await run.answer(carol, 'yes');
    expect(carol.atoms, contains('friend(dave)'));
    expect(run.scriptedAtoms('dave'), contains('friend(carol)'));
    // (5) Carol's Friends while Dave is her friend: the atoms of friend in her state, Alice, her parent and first friend, and Dave, Dave with Unfriend, the one act that names one person and requires friend of him at her role, and Alice with none; Bob, whom Alice's agent introduced to hers at the level of the machines, Alice knowing both, is known to her agent and on no tab, her state naming him in no atom.
    expect(carol.known, unorderedEquals(['alice', 'bob', 'dave']));
    await run.list(carol, 'friend');
    await run.top(carol);
    expect(run.control(carol, 'row:alice'), findsOneWidget);
    expect(run.control(carol, 'row:dave'), findsOneWidget);
    expect(run.control(carol, 'row:bob'), findsNothing);
    expect(run.control(carol, 'act:unfriend:dave'), findsOneWidget);
    // The rows in the order of her state, the newest first: Dave, then Alice; Unfriend beside Dave, and not beside Alice, unfriend(p?, q) forbidding creator(q) at Carol's role (CSSN.glp; vGLP feda043, Section 6.2: "where the viewer's state holds the atoms the act requires at the viewer's role and lacks those it forbids there").
    expect(run.texts(carol), containsAllInOrder(['Friends', 'Dave', 'Alice']));
    expect(run.control(carol, 'act:unfriend:alice'), findsNothing);
    // Message beside each friend, message requiring friend of the other at the sender's role (CSSN.glp at 50ea651c).
    for (final q in ['dave', 'alice']) {
      expect(run.control(carol, 'act:message:$q'), findsOneWidget, reason: q);
    }
    run.plain(carol);
    await run.shoot(carol, 'cssn-app-5.png');
    // Carol ends the friendship with Dave (GSG:2; unfriend(p?, q)), her own act, Unfriend beside Dave: nobody is asked.
    await run.tap(carol, run.control(carol, 'act:unfriend:dave'));
    expect(carol.questions('approval'), isEmpty);
    expect(carol.atoms, unorderedEquals([
      'identity',
      'creator(alice)',
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
