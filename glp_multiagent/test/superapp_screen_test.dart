/// The Grassroots Super-App's own screen: a person trusts a compiler, installs
/// a certified mini-app, connects to a friend and invites them to it — every
/// act willed through a construct derived from `home.vglp`'s display
/// declarations, on the real social-graph agent.
///
/// The same renderer and the same shell as the currency mini-app's screen; the
/// only thing that differs is the manifest, which is the image of a different
/// program's declarations. That is the claim this test is here to hold.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/superapp_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

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

    final lines = <String>[];
    final agent = AgentRuntime(
      agentId: 'alice',
      glpSources: const [],
      programDir: core,
      goalLabel: 'superapp_ui/3',
      rootSelfGlpPath: '$repo/self.glp',
      friends: const ['bob'],
    );
    agent.onOutput = lines.add;
    agent.onLog = (_, __) {};
    agent.onSendMadMessage = (_, __) async {};

    final sends = <String>[];
    final r = UiRuntime(manifest: superappManifest, onSend: sends.add);

    var fed = 0;
    void replay() {
      for (; fed < lines.length; fed++) {
        final l = lines[fed];
        if (l.startsWith('< ')) r.handleLine(l.substring(2));
      }
    }

    Future<void> settle() async {
      while (sends.isNotEmpty) {
        await tester.runAsync(() => agent.injectUserInput(sends.removeAt(0)));
      }
      replay();
      await tester.pumpAndSettle();
    }

    await tester.runAsync(() => agent.initialize());
    replay();

    // The person interface poses its four acts as soon as it runs.
    expect(r.standing.keys.toSet(), {'home_1', 'home_2', 'home_3', 'home_4'});

    await tester.pumpWidget(_phoneApp(AgentSurface(agentId: 'alice', runtime: r)));
    await tester.pumpAndSettle();

    Future<void> compose(String label, List<String> values,
        {String? formShot, String? sheetShot}) async {
      await tester.tap(find.byType(FloatingActionButton));
      await tester.pumpAndSettle();
      if (sheetShot != null) await _shot(tester, sheetShot);
      await tester.tap(find.text(label));
      await tester.pumpAndSettle();
      final fields = find.byType(TextField);
      expect(fields, findsNWidgets(values.length));
      for (var i = 0; i < values.length; i++) {
        await tester.enterText(fields.at(i), values[i]);
      }
      if (formShot != null) {
        await tester.pumpAndSettle();
        await _shot(tester, formShot);
      }
      await tester.tap(find.widgetWithText(ElevatedButton, 'Send'));
      await tester.pumpAndSettle();
      await settle();
    }

    // The panel's "+" is the four acts of the person's own super-app.
    await tester.tap(find.byType(FloatingActionButton));
    await tester.pumpAndSettle();
    for (final label in [
      'Trust a compiler',
      'Install a mini-app',
      'Invite a friend',
      'Connect'
    ]) {
      expect(find.text(label), findsOneWidget);
    }
    await _shot(tester, '/private/tmp/superapp-forms.png');
    await tester.tapAt(const Offset(180, 100)); // dismiss the sheet
    await tester.pumpAndSettle();

    // --- Trust the compiler that certified the mini-app. The key is a GLP
    // String, not an atom: the clause's answer type is xs_home_1(String). ---
    await compose('Trust a compiler', [key!],
        formShot: '/private/tmp/superapp-trust-form.png');
    expect(sends, isEmpty);

    // --- Install it: the file load_file/2 reads, and the name it takes. -----
    await compose('Install a mini-app', ['currency.glpw', 'currency'],
        formShot: '/private/tmp/superapp-install-form.png');

    // --- Connect to bob; his script consents, and the friends view fills. ---
    await compose('Connect', ['bob']);
    expect((r.store.lists['friends'] ?? const []).map(formatTerm), ['bob']);
    await _shot(tester, '/private/tmp/superapp-connected.png');

    // --- Invite him to the mini-app now that they are friends. -------------
    await compose('Invite a friend', ['currency', 'bob']);
    expect(find.text('FRIENDS'), findsOneWidget);
    expect(find.text('Bob'), findsOneWidget);
    await _shot(tester, '/private/tmp/superapp-invited.png');

    // Every act consumed its ask and the goal posed the next, so all four are
    // offered again — a volition is offered iff it is pending.
    expect(r.standing.keys.toSet(), {'home_1', 'home_2', 'home_3', 'home_4'});
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
      "printf ':artefact $repo/coins/currency $core\n:quit\n' | bin/glpc"
    ],
    workingDirectory: runtime,
  );
  final m = RegExp(r'certified under ([0-9a-f]{64})')
      .firstMatch('${out.stdout}${out.stderr}');
  return m?.group(1);
}
