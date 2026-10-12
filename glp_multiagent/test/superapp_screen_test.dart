/// The Grassroots Super-App's own screen: a person trusts a compiler, installs
/// a certified mini-app, connects to a friend and invites them to it --- every
/// act willed through the construct the Dart bridge draws for the question
/// of `home.vglp`, on the real social-graph agent.
///
/// The whole path runs: `programs/social/graph/core` at `superapp_ui/3` ---
/// the super-app's bridge entry (`programs/social/graph/core/home_ui.glp`),
/// alice's agent with its person interface, `home/3` and its dispatcher, on
/// the person channel `ch(Grants?, Draws)` of vGLP's Definition "Person
/// Channel, Person Writer, GLP with Persons, Grant", and bob's agent,
/// scripted, across a crossbar.  The compiled program's dispatcher draws each
/// construct, `draw(Id, W, V)`; the bridge draws it as the default widget of
/// its interactive type (Definition "Widget Declaration, Default Widget"),
/// and alice's submissions on it go back as her grants, `input(Id, P, R)`
/// (vGLP, sections/elicitation.tex, the paragraph "The implementation"); what
/// the super-app tells her comes back on the same channel, merged with the
/// draws, to the friends view and the default display.  Nothing is simulated
/// and no term is hand-fed.
///
/// The same renderer and the same shell as the currency mini-app's screen; the
/// only thing that differs is the program, and the manifest of what its agent
/// tells its person.  The engine loads the super-app's program and posts
/// `superapp_ui(alice, Grants?, [])` through its posting call, which checks
/// the goal as every posted goal is checked (GlpEngine.postGoal); alice's
/// grants go into `Grants` through the injector the call returns, and what
/// `send_to_user/1` sends her is read off the runtime's output.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/construct_family.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_multiagent/isolate_protocol.dart'
    show runtimeTermOf, screenTermOf;
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;

Future<void> _loadFonts() async {
  Future<void> add(FontLoader l, String p) async =>
      l.addFont(Future.value(ByteData.view(File(p).readAsBytesSync().buffer)));
  final f = FontLoader('AppFont');
  await add(f, '/System/Library/Fonts/Supplemental/Arial.ttf');
  await add(f, '/System/Library/Fonts/Supplemental/Arial Bold.ttf');
  await f.load();
  final mi = FontLoader('MaterialIcons');
  await add(mi,
      '/opt/homebrew/share/flutter/bin/cache/artifacts/material_fonts/MaterialIcons-Regular.otf');
  await mi.load();
}

Widget _phoneApp(Widget surface) => Directionality(
      textDirection: TextDirection.ltr,
      child: Container(
        color: const Color(0xFF2B2B33),
        child: Center(
          child: RepaintBoundary(
            child: Container(
              width: 360,
              height: 740,
              padding: const EdgeInsets.all(10),
              decoration: BoxDecoration(
                  color: Colors.black, borderRadius: BorderRadius.circular(40)),
              child: ClipRRect(
                borderRadius: BorderRadius.circular(30),
                child: Container(
                  color: Colors.white,
                  child: MaterialApp(
                    debugShowCheckedModeBanner: false,
                    theme: ThemeData(
                        fontFamily: 'AppFont',
                        useMaterial3: true,
                        colorScheme:
                            ColorScheme.fromSeed(seedColor: Colors.green),
                        elevatedButtonTheme: ElevatedButtonThemeData(
                            style: ElevatedButton.styleFrom(
                                backgroundColor: Colors.green,
                                foregroundColor: Colors.white))),
                    home: surface,
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );

Future<void> _shot(WidgetTester tester, String path) async {
  final boundary = tester.renderObject<RenderRepaintBoundary>(
      find.byType(RepaintBoundary).first);
  final bytes = await tester.runAsync(() async {
    final img = await boundary.toImage(pixelRatio: 3.0);
    final data = await img.toByteData(format: ui.ImageByteFormat.png);
    return data!.buffer.asUint8List();
  });
  File(path).writeAsBytesSync(bytes!);
}

void main() {
  testWidgets('the super-app: trust, install, connect, invite', (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(420, 860);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';
    final core = '$repo/social/graph/core';

    // The certified mini-app the super-app installs, and the key it is
    // certified under. `.glpw` is a build product (gitignored), written by the
    // REPL's `:artefact` from Currencies' program into the super-app's own
    // directory, where `load_file/2` reads it — the route superapp_plays.glp
    // documents. The key comes from the same command rather than from the
    // container's bytes, so this test assumes nothing about its format.
    final key = await tester.runAsync(() => _writeArtefact(repo, core));
    expect(key, isNotNull);
    expect(File('$core/currency.glpw').existsSync(), isTrue);

    // What send_to_user/1 sends alice, each term whole.
    final notifies = <GTerm>[];
    final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp');
    engine.runtime.outputTermCallback = (t) => notifies.add(screenTermOf(t));
    expect(engine.loadProgram(core), isTrue);
    final play = engine.postGoal('superapp_ui(alice, Grants?, [])',
        inputs: ['Grants']);
    final grants = play.inputs['Grants']!;

    /// The play run until quiescent, as one event of the agent's is.
    void drain() {
      final result = play.scheduler.drainToQuiescence(maxCycles: 200000);
      expect(result.status, isNot(ExecutionStatus.capped),
          reason: 'the play did not quiesce');
    }

    final sends = <GTerm>[];
    final r = UiRuntime(manifest: superappManifest, onSend: sends.add);

    var fed = 0;
    void replay() {
      for (; fed < notifies.length; fed++) {
        r.handle(notifies[fed]);
      }
    }

    // Each of alice's grants, a ground term, onto her person channel.
    Future<void> settle() async {
      while (sends.isNotEmpty) {
        grants
            .inject(runtimeTermOf(sends.removeAt(0)))
            .forEach(engine.runtime.gq.enqueue);
        drain();
      }
      replay();
      await tester.pumpAndSettle();
    }

    drain();
    replay();

    // The person interface asks its question as soon as it runs, before the
    // person has done anything: the stream of her requests, one construct,
    // drawn as the default widget of Stream(Request)? --- an input box of a
    // menu of the four forms of Request ::= trust(CompilerKey) ;
    // install(AppName, Artefact) ; invite(AppName, Friend) ; connect(Friend),
    // each field labelled by the name of its declared type, a Key a peer
    // field and a String a text field.
    const requestForm = "input_box(menu([form(trust, [field('CompilerKey', peer)]), "
        "form(install, [field('AppName', text), field('Artefact', text)]), "
        "form(invite, [field('AppName', text), field('Friend', peer)]), "
        "form(connect, [field('Friend', peer)])]))";
    expect(r.bridge.constructs.map((c) => formatTerm(c.widget)), [requestForm]);
    final form = r.bridge.constructs.single;
    expect(formatTerm(form.view), 'input([])');

    await tester.pumpWidget(_phoneApp(AgentSurface(agentId: 'alice', runtime: r)));
    await tester.pumpAndSettle();

    Finder inForm(Finder f) =>
        find.descendant(of: find.byKey(constructKey(form.id)), matching: f);

    /// Choose one of the menu's forms, fill its fields, and send it: one
    /// request on the request form, one element of the stream.
    Future<void> request(String alternative, Map<List<int>, (String, String)> fields,
        {String? formShot}) async {
      await tester.tap(inForm(find.widgetWithText(ChoiceChip, alternative)));
      await tester.pumpAndSettle();
      expect(inForm(find.byType(TextField)), findsNWidgets(fields.length));
      for (final e in fields.entries) {
        final (kind, text) = e.value;
        await tester.enterText(
            find.descendant(
                of: find.byKey(nodeKey(form.id, kind, e.key)),
                matching: find.byType(TextField)),
            text);
      }
      await tester.pumpAndSettle();
      if (formShot != null) await _shot(tester, formShot);
      await tester.tap(find.byKey(submitKey(form.id, const [])));
      await tester.pumpAndSettle();
      await settle();
    }

    // The form offers the four acts of the person's own super-app, a menu of
    // their forms, and only those: the alternatives of Request.
    for (final label in ['trust', 'install', 'invite', 'connect']) {
      expect(inForm(find.widgetWithText(ChoiceChip, label)), findsOneWidget);
    }
    expect(inForm(find.byType(ChoiceChip)), findsNWidgets(4));
    await _shot(tester, '/private/tmp/superapp-forms.png');

    // --- Trust the compiler that certified the mini-app: its key, a Key and
    // so a peer field. ---------------------------------------------------------
    await request('trust', {
      [1]: ('peer', key!)
    }, formShot: '/private/tmp/superapp-trust-form.png');
    expect(sends, isEmpty);

    // --- Install it: the name it takes, and the file load_file/2 reads. -----
    await request('install', {
      [1]: ('text', 'currency'),
      [2]: ('text', 'currency.glpw'),
    }, formShot: '/private/tmp/superapp-install-form.png');

    // --- Connect to bob; his script consents, and the friends view fills. ---
    await request('connect', {
      [1]: ('peer', 'bob')
    });
    expect((r.store.lists['friends'] ?? const []).map(formatTerm), ['bob']);
    await _shot(tester, '/private/tmp/superapp-connected.png');

    // --- Invite him to the mini-app now that they are friends. -------------
    await request('invite', {
      [1]: ('text', 'currency'),
      [2]: ('peer', 'bob'),
    });
    expect(find.text('FRIENDS'), findsOneWidget);
    expect(find.text('Bob'), findsOneWidget);
    await _shot(tester, '/private/tmp/superapp-invited.png');

    // The question stands: a stream question takes one element per
    // submission and stays open while its stream does (vGLP, Remark
    // "Persistence"), so the same construct takes every request, and it is
    // the only one open.
    expect(r.bridge.constructs.map((c) => c.id), [form.id]);
    expect(find.byKey(constructKey(form.id)), findsOneWidget);
  }, timeout: const Timeout(Duration(minutes: 3)));
}

/// Write the currency's certified artefact into the super-app's directory and
/// return the compiler's key it is certified under — what the person trusts.
///
/// This is the REPL's `:artefact <program> <to>`, the one route that produces
/// a `.glpw`, run here so the test carries its own fixture.
Future<String?> _writeArtefact(String repo, String core) async {
  final runtime = Directory('$repo/../glp_runtime').absolute.path;
  final out = await Process.run(
    'bash',
    [
      '-c',
      "printf ':artefact $repo/currencies/coins/currency $core\n:quit\n' | bin/glpc"
    ],
    workingDirectory: runtime,
  );
  final m = RegExp(r'certified under ([0-9a-f]{64})')
      .firstMatch('${out.stdout}${out.stderr}');
  return m?.group(1);
}
