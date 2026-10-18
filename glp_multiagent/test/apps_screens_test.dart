/// The screens of GSC's two apps, GSG's and GSN's (`programs/apps/gsg`,
/// `programs/apps/gsn`), each person's as the bridge draws it on their phone,
/// written to PNG files for GAI's paper (`programs/apps/<app>/screens/`).
///
/// Each app's directory is a certified program: the canonical CVA platform of
/// its contract, `platform.vglp`, compiled to GLP when the directory loads
/// (vGLP, Section 6.3, Definition "Canonical Compilation"), and the mini-app's
/// entry `miniapp/4` (`miniapp.glp`), which runs the platform's agent with the
/// person's key as its `Peer` and its dispatcher on the agent's asks, the
/// dispatcher drawing the agent's request and approval questions and its
/// thread on the person channel.  The engine loads the directory and posts the
/// entry twice, alice's and bob's, over one conversation, the two ends of its
/// root channel handed to them as the social graph hands them and as the
/// plays of each directory do (`plays.glp`).  Each person's channel is
/// `ch(Grants?, Draws)` (vGLP, Definition "Person Channel, Person Writer, GLP
/// with Persons, Grant"): the host hands each person's bridge every term of
/// `Draws` as it arrives (bridge.dart), and extends `Grants` with each grant
/// the bridge returns, `input(Id, P, R)`.  The bridge draws each construct as
/// the default widget of its interactive type (vGLP, Section 6.2, Definition
/// "Default Widget"), and every act of the two persons is a tap, a keystroke
/// or a submission on those widgets: the acts the scripted persons of the
/// plays take (`person.glp`).  No term is hand-fed to the bridge or to the
/// program.
///
/// Each screen is the person's phone, 390 by 844 logical pixels, rendered by
/// the widget itself (RepaintBoundary.toImage) at a device pixel ratio of 3,
/// in the light theme.  The test font draws every glyph as a box, so the
/// screens are set in Roboto, the Material font, with MaterialIcons, both
/// loaded from the Flutter SDK's own copies
/// (`bin/cache/artifacts/material_fonts`).  The PNG files are written only
/// when the test runs with `--update-goldens`:
///
///     flutter test --update-goldens test/apps_screens_test.dart
///
/// Without it the test runs every step and checks every screen, and writes
/// nothing into the tree.
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
import 'package:glp_multiagent/ui_runtime/runtime.dart';
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

/// The app's one panel, the compiled program's: the constructs the bridge
/// holds open, the agent's thread among them as construct 0.
Manifest _manifest(String app) => Manifest(
      title: app,
      panels: [Panel(id: app, name: app.toUpperCase(), constructs: true)],
      activity: const [],
    );

/// One person at their phone: their bridge, the writer of their grants, and
/// the stream of draws their mini-app writes.
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

  /// The open request question: the form `menu`, the persons the agent knows
  /// and the request it asks for.
  OpenConstruct get request => _one('menu');

  /// The open approval question: the form `approval`, the attempt and the
  /// person's yes or no.
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

  /// The agent's thread, construct 0.
  OpenConstruct get output => ui.bridge.construct(0)!;

  /// The persons the open request question shows.
  String get known => formatTerm(ctorArgs(request.view).$2[0]);

  /// The agent's thread, construct 0, each transaction as its term.
  List<String> get thread {
    final v = ui.bridge.construct(0)?.view;
    return [if (v is GList) for (final t in v.items) formatTerm(t)];
  }
}

/// Alice and Bob, each running the app's mini-app on their own phone, over
/// one conversation.
class _Run {
  final WidgetTester tester;
  final String app;
  final GlpEngine engine;
  final PostedGoal play;
  final _Person alice;
  final _Person bob;

  _Run._(this.tester, this.app, this.engine, this.play, this.alice, this.bob);

  List<_Person> get persons => [alice, bob];

  static Future<_Run> start(WidgetTester tester, String app) async {
    await _loadFonts();
    // Shadows drawn as shadows, not as the test's solid outlines; restored
    // by [finish].
    debugDisableShadows = false;
    tester.view.physicalSize =
        Size(2 * (_phone.width + 2 * _gap), _phone.height + 2 * _gap);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final programs = programsDir();
    final engine = GlpEngine(rootSelfGlpPath: '$programs/self.glp');
    expect(engine.loadProgram('$programs/apps/$app'), isTrue,
        reason: 'programs/apps/$app does not load');
    // The entry twice, as the plays activate it (plays.glp), with one
    // conversation between the two executions; each person's grants the
    // caller's to write, and each person's draws read off their stream.
    final play = engine.postGoal(
        'miniapp(alice, ch(GA?, DA), '
        'ch([conversation(bob, ch(BA?, AB))], UA), []), '
        'miniapp(bob, ch(GB?, DB), '
        'ch([conversation(alice, ch(AB?, BA))], UB), [])',
        inputs: ['GA', 'GB']);
    final run = _Run._(
        tester,
        app,
        engine,
        play,
        _Person('alice', app, play.inputs['GA']!, play.variables['DA']!),
        _Person('bob', app, play.inputs['GB']!, play.variables['DB']!));
    run._drain();
    run._feed();
    await tester.pumpWidget(run._phones());
    await tester.pumpAndSettle();
    return run;
  }

  /// The run reduced until quiescent, as each event of an agent is (IGLP,
  /// Implementation Notes; vGLP, Section 5.4, "Triggers": the agent takes its
  /// own transactions until none holds).
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

  /// Put each grant the bridges returned on its person channel, run the
  /// program on it, and draw what it draws.
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

  /// The two phones side by side, each the person's agent surface on a
  /// screen of its own.
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

  // === The persons' acts on their screens ===================================

  /// [f] within [p]'s phone.
  Finder on(_Person p, Finder f) =>
      find.descendant(of: find.byKey(p.shot), matching: f);

  /// The node of kind [kind] at [at] of construct [c] on [p]'s phone.
  Finder node(_Person p, OpenConstruct c, String kind, List<int> at) =>
      on(p, find.byKey(nodeKey(c.id, kind, at)));

  /// [p]'s screen scrolled to its top where [f] is not built, the list
  /// building only what is near the screen: the questions stand at the top.
  Future<void> _reveal(_Person p, Finder f) async {
    if (f.evaluate().isEmpty) await top(p);
  }

  Future<void> _tap(Finder f) async {
    expect(f, findsOneWidget);
    await tester.ensureVisible(f);
    await tester.pumpAndSettle();
    await tester.tap(f);
    await tester.pumpAndSettle();
  }

  /// Choose the alternative [label] of the menu at [at].
  Future<void> choose(
      _Person p, OpenConstruct c, List<int> at, String label) async {
    final f = find.descendant(
        of: node(p, c, 'menu', at),
        matching: find.widgetWithText(ChoiceChip, label));
    await _reveal(p, f);
    await _tap(f.first);
  }

  /// Open the field of a recursive type at [at].
  Future<void> open(_Person p, OpenConstruct c, List<int> at) async {
    final f = on(p, find.byKey(openKey(c.id, at)));
    await _reveal(p, f);
    await _tap(f);
  }

  /// Type [text] into the field of kind [kind] at [at].
  Future<void> type(_Person p, OpenConstruct c, String kind, List<int> at,
      String text) async {
    final f = find.descendant(
        of: node(p, c, kind, at), matching: find.byType(TextField));
    await _reveal(p, f);
    expect(f, findsOneWidget, reason: '$kind field at $at');
    await tester.enterText(f, text);
    await tester.pumpAndSettle();
  }

  /// Submit the question at [at]: its grant, the run on it, and the draws.
  Future<void> submit(_Person p, OpenConstruct c, List<int> at) async {
    final f = on(p, find.byKey(submitKey(c.id, at)));
    await _reveal(p, f);
    expect(tester.widget<ElevatedButton>(f).onPressed, isNotNull,
        reason: '${p.name}: nothing to submit at $at');
    await _tap(f);
    await settle();
  }

  /// Press the button [label] of construct [c].
  Future<void> press(_Person p, OpenConstruct c, String label) async {
    final f = on(
        p,
        find.descendant(
            of: find.byKey(constructKey(c.id)),
            matching: find.widgetWithText(ElevatedButton, label)));
    await _reveal(p, f);
    await _tap(f);
    await settle();
  }

  /// [p]'s screen scrolled to its top, where the questions stand.
  Future<void> top(_Person p) => _scroll(p, false);

  /// [p]'s screen scrolled to its end, the latest of the thread.
  Future<void> end(_Person p) => _scroll(p, true);

  Future<void> _scroll(_Person p, bool toEnd) async {
    FocusManager.instance.primaryFocus?.unfocus();
    final s = tester.state<ScrollableState>(on(p, find.byType(Scrollable)).first);
    s.position.jumpTo(toEnd ? s.position.maxScrollExtent : 0);
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

  /// Alice and Bob establish their identities (GSG:1), ask to know each
  /// other, and become friends (GSG:2): Alice proposes it and Bob answers
  /// yes, each act on the widgets as the screens of GSG's app show them.
  Future<void> befriend() async {
    for (final (p, q) in [(alice, 'bob'), (bob, 'alice')]) {
      var c = p.request;
      await choose(p, c, [2], 'propose');
      await choose(p, c, [2, 1], 'establish');
      await type(p, c, 'peer', [2, 1, 1], p.name);
      await submit(p, c, [2]);
      c = p.request;
      await choose(p, c, [2], 'known');
      await type(p, c, 'peer', [2, 1], q);
      await submit(p, c, [2]);
    }
    final c = alice.request;
    await choose(alice, c, [2], 'propose');
    await choose(alice, c, [2, 1], 'befriend');
    await type(alice, c, 'peer', [2, 1, 1], 'alice');
    await type(alice, c, 'peer', [2, 1, 2], 'bob');
    await submit(alice, c, [2]);
    await press(bob, bob.approval, 'yes');
    expect(alice.thread, [
      'propose(attempt(alice, 1, 1, establish(alice)))',
      'discover(bob)',
      'propose(attempt(alice, 2, 2, befriend(alice, bob)))',
      'tally(attempt(alice, 2, 2, befriend(alice, bob)))',
      'commit(attempt(alice, 2, 2, befriend(alice, bob)))',
    ]);
    expect(bob.thread, [
      'propose(attempt(bob, 1, 1, establish(bob)))',
      'discover(alice)',
      'start(attempt(alice, 2, 2, befriend(alice, bob)))',
      'answer(attempt(alice, 2, 2, befriend(alice, bob)))',
      'vote(attempt(alice, 2, 2, befriend(alice, bob)))',
      'outcome(attempt(alice, 2, 2, befriend(alice, bob)))',
    ]);
  }

  /// Fill the field of the speech act at [at], a form of a recursive type
  /// drawn closed until opened: opened, its utterance opened and chosen,
  /// [text] typed, and its signer [signer].
  Future<void> say(_Person p, OpenConstruct c, List<int> at, String text,
      String signer) async {
    await open(p, c, at);
    await open(p, c, [...at, 1]);
    await choose(p, c, [...at, 1], 'utterance');
    await type(p, c, 'text', [...at, 1, 1], text);
    await type(p, c, 'peer', [...at, 2], signer);
  }

  /// Open, in the element [k] of [p]'s thread, the speech act at argument
  /// [arg] of its attempt's class, and its utterance: a field of a recursive
  /// type is drawn closed until the person opens it.
  Future<void> opened(_Person p, int k, List<int> arg) async {
    final at = [...elementAt(const [], k), 1, 4, ...arg];
    await open(p, p.output, at);
    await open(p, p.output, [...at, 1]);
  }

  /// [p]'s screen scrolled to bring [f] to its top.
  Future<void> show(_Person p, Finder f) async {
    FocusManager.instance.primaryFocus?.unfocus();
    await tester.pumpAndSettle();
    await _reveal(p, f);
    expect(f, findsOneWidget);
    await Scrollable.ensureVisible(tester.element(f));
    await tester.pumpAndSettle();
  }

  /// The test's painting settings restored, as the test framework requires.
  void finish() => debugDisableShadows = true;

}

void main() {
  testWidgets(
      'GSG\'s app on two phones: identity, knowing each other, friendship '
      'offered and approved, and ended', (tester) async {
    final run = await _Run.start(tester, 'gsg');
    final alice = run.alice;
    final bob = run.bob;

    // 1. Alice's request question, nobody known: she establishes her
    // identity (GSG:1), her first request, a class of one participant,
    // taken at once.
    expect(alice.known, '[]');
    var q = alice.request;
    await run.choose(alice, q, [2], 'propose');
    await run.choose(alice, q, [2, 1], 'establish');
    await run.type(alice, q, 'peer', [2, 1, 1], 'alice');
    await run.top(alice);
    await run.shoot(alice, 'gsg-01-alice-establish.png');
    await run.submit(alice, q, [2]);
    expect(alice.thread, ['propose(attempt(alice, 1, 1, establish(alice)))']);

    // 2. Her next request question, still nobody known: she asks to know
    // Bob.
    q = alice.request;
    expect(alice.known, '[]');
    await run.choose(alice, q, [2], 'known');
    await run.type(alice, q, 'peer', [2, 1], 'bob');
    await run.top(alice);
    await run.shoot(alice, 'gsg-02-alice-menu.png');
    await run.submit(alice, q, [2]);

    // Bob likewise: his identity, and to know Alice.
    q = bob.request;
    await run.choose(bob, q, [2], 'propose');
    await run.choose(bob, q, [2, 1], 'establish');
    await run.type(bob, q, 'peer', [2, 1, 1], 'bob');
    await run.submit(bob, q, [2]);
    q = bob.request;
    await run.choose(bob, q, [2], 'known');
    await run.type(bob, q, 'peer', [2, 1], 'alice');
    await run.submit(bob, q, [2]);

    // The hellos exchanged on their conversation, each knows the other.
    expect(alice.thread, [
      'propose(attempt(alice, 1, 1, establish(alice)))',
      'discover(bob)',
    ]);
    expect(bob.thread, [
      'propose(attempt(bob, 1, 1, establish(bob)))',
      'discover(alice)',
    ]);

    // 3. Alice proposes to befriend Bob (GSG:2), on her open request
    // question, asked before Bob was known.
    q = alice.request;
    await run.choose(alice, q, [2], 'propose');
    await run.choose(alice, q, [2, 1], 'befriend');
    await run.type(alice, q, 'peer', [2, 1, 1], 'alice');
    await run.type(alice, q, 'peer', [2, 1, 2], 'bob');
    await run.top(alice);
    await run.shoot(alice, 'gsg-03-alice-befriend.png');
    await run.submit(alice, q, [2]);

    // 4. Her next request question shows Bob known; her thread, her
    // proposal.
    expect(alice.known, '[bob]');
    expect(alice.thread.last,
        'propose(attempt(alice, 2, 2, befriend(alice, bob)))');
    await run.top(alice);
    await run.shoot(alice, 'gsg-04-alice-menu-bob.png');

    // 5. Bob is asked: his approval question, the attempt shown and the
    // buttons yes and no.
    expect(bob.thread.last, 'start(attempt(alice, 2, 2, befriend(alice, bob)))');
    final a = bob.approval;
    expect(formatTerm(a.view),
        'approval(attempt(alice, 2, 2, befriend(alice, bob)), input([2]))');
    await run.top(bob);
    await run.shoot(bob, 'gsg-05-bob-approval.png');

    // 6. He taps yes: the question is withdrawn, and his thread shows his
    // answer, his vote and the outcome.
    await run.press(bob, a, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(bob.thread.sublist(bob.thread.length - 3), [
      'answer(attempt(alice, 2, 2, befriend(alice, bob)))',
      'vote(attempt(alice, 2, 2, befriend(alice, bob)))',
      'outcome(attempt(alice, 2, 2, befriend(alice, bob)))',
    ]);
    await run.end(bob);
    await run.shoot(bob, 'gsg-06-bob-yes.png');

    // 7. They are friends: Alice's thread shows the tally and the commit.
    expect(alice.thread.sublist(alice.thread.length - 2), [
      'tally(attempt(alice, 2, 2, befriend(alice, bob)))',
      'commit(attempt(alice, 2, 2, befriend(alice, bob)))',
    ]);
    await run.end(alice);
    await run.shoot(alice, 'gsg-07-alice-friends.png');

    // 8. Bob ends the friendship (GSG:3), an act of his own.
    q = bob.request;
    await run.choose(bob, q, [2], 'propose');
    await run.choose(bob, q, [2, 1], 'unfriend');
    await run.type(bob, q, 'peer', [2, 1, 1], 'bob');
    await run.type(bob, q, 'peer', [2, 1, 2], 'alice');
    await run.top(bob);
    await run.shoot(bob, 'gsg-08-bob-unfriend.png');
    await run.submit(bob, q, [2]);

    // 9. Nobody else is asked: his thread shows his proposal, the tally and
    // the commit.
    expect(bob.thread.sublist(bob.thread.length - 3), [
      'propose(attempt(bob, 3, 3, unfriend(bob, alice)))',
      'tally(attempt(bob, 3, 3, unfriend(bob, alice)))',
      'commit(attempt(bob, 3, 3, unfriend(bob, alice)))',
    ]);
    await run.end(bob);
    await run.shoot(bob, 'gsg-09-bob-unfriended.png');

    // 10. Alice is not asked: her thread shows the attempt and its outcome.
    expect(alice.questions('approval'), isEmpty);
    expect(alice.thread.sublist(alice.thread.length - 2), [
      'start(attempt(bob, 3, 3, unfriend(bob, alice)))',
      'outcome(attempt(bob, 3, 3, unfriend(bob, alice)))',
    ]);
    await run.end(alice);
    await run.shoot(alice, 'gsg-10-alice-unfriended.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));

  testWidgets(
      'GSN\'s app on two phones: a message to a friend, a group with its '
      'leader and members, and a message to the group', (tester) async {
    final run = await _Run.start(tester, 'gsn');
    final alice = run.alice;
    final bob = run.bob;

    // Identity, knowing each other and friendship, as on GSG's app.
    await run.befriend();
    const hello = 'message(alice, bob, speech(utterance(hello), alice))';
    const friends = 'speech(utterance(friends), alice)';
    const create = 'create(alice, $friends)';
    const join = 'join(alice, bob, $friends)';
    const post = 'post(bob, alice, speech(utterance(hi_all), bob), $friends)';
    const deliver =
        'deliver(alice, bob, speech(utterance(hi_all), bob), $friends)';

    // 1. A message to a friend (GSN:1): Alice proposes message, herself,
    // Bob, and the speech act hello, signed by her.
    var q = alice.request;
    await run.choose(alice, q, [2], 'propose');
    await run.choose(alice, q, [2, 1], 'message');
    await run.type(alice, q, 'peer', [2, 1, 1], 'alice');
    await run.type(alice, q, 'peer', [2, 1, 2], 'bob');
    await run.say(alice, q, [2, 1, 3], 'hello', 'alice');
    await run.show(alice, run.node(alice, q, 'form', [2]));
    await run.shoot(alice, 'gsn-01-alice-message.png');
    await run.submit(alice, q, [2]);
    expect(alice.thread.sublist(alice.thread.length - 3), [
      'propose(attempt(alice, 3, 3, $hello))',
      'tally(attempt(alice, 3, 3, $hello))',
      'commit(attempt(alice, 3, 3, $hello))',
    ]);

    // 2. Bob is not asked: his thread shows the attempt, and so the message,
    // its speech act opened.
    expect(bob.questions('approval'), isEmpty);
    expect(bob.thread.sublist(bob.thread.length - 2), [
      'start(attempt(alice, 3, 3, $hello))',
      'outcome(attempt(alice, 3, 3, $hello))',
    ]);
    await run.opened(bob, bob.thread.length - 1, [3]);
    await run.end(bob);
    await run.shoot(bob, 'gsn-02-bob-message.png');

    // 3. Alice creates the group friends (GSN:2), its name a speech act
    // signed by her: one participant, taken at once, and she is its leader
    // and first member.
    q = alice.request;
    await run.choose(alice, q, [2], 'propose');
    await run.choose(alice, q, [2, 1], 'create');
    await run.type(alice, q, 'peer', [2, 1, 1], 'alice');
    await run.say(alice, q, [2, 1, 2], 'friends', 'alice');
    await run.show(alice, run.node(alice, q, 'form', [2]));
    await run.shoot(alice, 'gsn-03-alice-create.png');
    await run.submit(alice, q, [2]);
    expect(alice.thread.last, 'propose(attempt(alice, 4, 4, $create))');

    // 4. Her thread shows the group created, its name opened.
    await run.opened(alice, alice.thread.length - 1, [2]);
    await run.end(alice);
    await run.shoot(alice, 'gsn-04-alice-group-created.png');

    // 5. She adds Bob to it (GSN:3), an act of both.
    q = alice.request;
    await run.choose(alice, q, [2], 'propose');
    await run.choose(alice, q, [2, 1], 'join');
    await run.type(alice, q, 'peer', [2, 1, 1], 'alice');
    await run.type(alice, q, 'peer', [2, 1, 2], 'bob');
    await run.say(alice, q, [2, 1, 3], 'friends', 'alice');
    await run.show(alice, run.node(alice, q, 'form', [2]));
    await run.shoot(alice, 'gsn-05-alice-join.png');
    await run.submit(alice, q, [2]);
    expect(alice.thread.last, 'propose(attempt(alice, 5, 5, $join))');

    // 6. Bob's approval question: the attempt, the group's name opened, and
    // the buttons yes and no.
    final a = bob.approval;
    expect(formatTerm(a.view), 'approval(attempt(alice, 5, 5, $join), input([2]))');
    await run.open(bob, a, [1, 4, 3]);
    await run.open(bob, a, [1, 4, 3, 1]);
    await run.show(bob, run.node(bob, a, 'form', []));
    await run.shoot(bob, 'gsn-06-bob-join.png');

    // 7. He taps yes: his answer, his vote and the outcome, the group's
    // name opened.
    await run.press(bob, a, 'yes');
    expect(bob.questions('approval'), isEmpty);
    expect(bob.thread.sublist(bob.thread.length - 3), [
      'answer(attempt(alice, 5, 5, $join))',
      'vote(attempt(alice, 5, 5, $join))',
      'outcome(attempt(alice, 5, 5, $join))',
    ]);
    await run.opened(bob, bob.thread.length - 1, [3]);
    await run.end(bob);
    await run.shoot(bob, 'gsn-07-bob-joined.png');

    // 8. The group: Alice's thread shows its creation, her the leader, and
    // Bob's joining committed, the group's name opened.
    expect(alice.thread.sublist(alice.thread.length - 4), [
      'propose(attempt(alice, 4, 4, $create))',
      'propose(attempt(alice, 5, 5, $join))',
      'tally(attempt(alice, 5, 5, $join))',
      'commit(attempt(alice, 5, 5, $join))',
    ]);
    await run.opened(alice, alice.thread.length - 1, [3]);
    await run.end(alice);
    await run.shoot(alice, 'gsn-08-alice-group.png');

    // 9. A message to the group (GSN:5): Bob proposes post, himself, Alice
    // the leader, the content hi_all signed by him, and the group's name.
    q = bob.request;
    await run.choose(bob, q, [2], 'propose');
    await run.choose(bob, q, [2, 1], 'post');
    await run.type(bob, q, 'peer', [2, 1, 1], 'bob');
    await run.type(bob, q, 'peer', [2, 1, 2], 'alice');
    await run.say(bob, q, [2, 1, 3], 'hi_all', 'bob');
    await run.say(bob, q, [2, 1, 4], 'friends', 'alice');
    await run.show(bob, run.node(bob, q, 'form', [2, 1]));
    await run.shoot(bob, 'gsn-09-bob-post.png');
    await run.submit(bob, q, [2]);
    expect(bob.thread.sublist(bob.thread.length - 5), [
      'propose(attempt(bob, 6, 6, $post))',
      'tally(attempt(bob, 6, 6, $post))',
      'commit(attempt(bob, 6, 6, $post))',
      'start(attempt(alice, 7, 7, $deliver))',
      'outcome(attempt(alice, 7, 7, $deliver))',
    ]);

    // 10. Alice, the leader, is not asked: her thread shows Bob's message to
    // the group, and her machine delivering it to Bob, nobody asked.
    expect(alice.questions('approval'), isEmpty);
    expect(alice.thread.sublist(alice.thread.length - 5), [
      'start(attempt(bob, 6, 6, $post))',
      'outcome(attempt(bob, 6, 6, $post))',
      'propose(attempt(alice, 7, 7, $deliver))',
      'tally(attempt(alice, 7, 7, $deliver))',
      'commit(attempt(alice, 7, 7, $deliver))',
    ]);
    final k = alice.thread.length - 4;
    await run.opened(alice, k, [3]);
    await run.opened(alice, k, [4]);
    await run.show(alice, run.node(alice, alice.output, 'form', elementAt(const [], k)));
    await run.shoot(alice, 'gsn-10-alice-group-message.png');

    // 11. Bob, a member, receives it: his thread shows the delivery, its
    // content and the group's name opened.
    await run.opened(bob, bob.thread.length - 1, [3]);
    await run.opened(bob, bob.thread.length - 1, [4]);
    await run.end(bob);
    await run.shoot(bob, 'gsn-11-bob-group-message.png');
    run.finish();
  }, timeout: const Timeout(Duration(minutes: 20)));
}
