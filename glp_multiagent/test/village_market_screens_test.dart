/// The village market of "Grassroots Currencies: Turning Mutual Trust into
/// Liquidity", Appendix "From the Contract to the App", on the six villagers'
/// screens: the figure `village-market-coins.png`.
///
/// Each phone is one villager's own execution of the currency mini-app under
/// the Grassroots Super-App, its screen the constructs the Dart bridge draws
/// for `programs/currencies/coins/currency/coins_agent.vglp` and the views of
/// what its agent writes, rendered by the same `AgentSurface` and
/// `coinsManifest` that render the request form and the proposal card of that
/// appendix, in the same phone shell and at the same scale.
///
/// The play `market_ui/1` runs the whole six-agent market and sends the events
/// of the villager it names to the user's stream as well
/// (`programs/currencies/coins/self.glp`, "the six balances views of the
/// paper's Appendix C.2 are drawn from it, one villager per run").  The test
/// runs that play directly, as test code: the coins program loaded by the
/// engine and `market_ui(V)` posted to it, type-checked as every posted goal
/// is, with no host --- the host posts `superapp/3` and nothing of Currencies'
/// (GSG Section 5.1; Currencies #7 Cowork, 2026-10-04 09:15 UTC, item 2).  The
/// six agents run in one play in every run; the app's interface code renders
/// one of them, so the figure is six runs of the same market, each rendering
/// its own villager's screen.  Nothing is fed to a surface that its agent did
/// not send, and no screen is copied from another.
///
/// The image this test writes is the figure.  Under `market_ui/1` the app
/// observes and does not answer: `village.glp`'s `villager/4` sends the named
/// villager's events --- the constructs the dispatcher draws and withdraws,
/// and the screen --- through `tee/3` to the app and to the scripted person,
/// and the person's grants go to the dispatcher and not to the app.  "The
/// construct process withdraws when every question it holds is answered"
/// (vGLP, sections/elicitation.tex, "The Implementation: Compiling vGLP onto
/// GLP"), so the app removes every card the scripted person answered, and the
/// test requires that no construct but the request form, whose stream
/// question stays open, stands on any phone at the end of the run.
///
/// The agent's screen reaches the person only as the thread of construct 0,
/// each message drawn by the widget of the reserved type Output ::=
/// ScreenContent (vGLP 169b008, Definition "Canonical Compilation"; vGLP,
/// 2026-10-13 19:25 UTC, items 2 and 3); the manifest names no view.  Each
/// phone shows the request form and under it that thread, and is cut just
/// below its latest holdings message, and the test checks those holdings
/// against GC's Table 1, at Currencies' request of 2026-10-14 08:48 UTC,
/// item 1.  Each shell closes its lower edge with the bezel and corner radius
/// of its upper edge.
library;

import 'dart:io';
import 'dart:math' as math;
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/isolate_protocol.dart' show screenTermOf;
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

/// One villager: its name, and the friends it holds a conversation with in
/// `village.glp`'s `market/1`.
class _Villager {
  final String id;
  final String name;
  final List<String> friends;
  const _Villager(this.id, this.name, this.friends);
}

const _villagers = <_Villager>[
  _Villager('alice', 'Alice', ['bob', 'charlie', 'eve']),
  _Villager('bob', 'Bob', ['alice', 'diana']),
  _Villager('charlie', 'Charlie', ['alice', 'eve']),
  _Villager('diana', 'Diana', ['bob', 'frank']),
  _Villager('eve', 'Eve', ['alice', 'charlie', 'frank']),
  _Villager('frank', 'Frank', ['diana', 'eve']),
];

/// The holdings the run must end at, GC's Table 1 (Currencies, 2026-10-14
/// 08:48 UTC, item 1).
const _expected = <String, Map<String, String>>{
  'alice': {'bob': '5', 'charlie': '15', 'alice': '8', 'frank': '4'},
  'bob': {'alice': '10', 'diana': '20'},
  'charlie': {'alice': '10', 'eve': '10', 'charlie': '6', 'frank': '5'},
  'diana': {'bob': '24', 'frank': '13', 'diana': '8'},
  'eve': {'charlie': '4', 'frank': '1', 'alice': '2', 'bob': '10'},
  'frank': {'diana': '7', 'eve': '10', 'frank': '5'},
};

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

/// The phone shell of `coins_screen_test.dart`, which took the Swap form and
/// the proposal card already in the paper: the same width, bezel, corner
/// radius and theme, and within it the screen, [height] less the bezel above
/// and below, the shell closing its lower edge as its upper edge is closed.
Widget _phone(Widget surface, {required double height}) => Container(
      width: 360,
      height: height,
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
                colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
                elevatedButtonTheme: ElevatedButtonThemeData(
                    style: ElevatedButton.styleFrom(
                        backgroundColor: Colors.green,
                        foregroundColor: Colors.white))),
            home: surface,
          ),
        ),
      ),
    );

Widget _cell(_Villager v, UiRuntime r, double height) => Column(
      mainAxisSize: MainAxisSize.min,
      children: [
        _phone(AgentSurface(agentId: v.id, runtime: r, muteNotices: true),
            height: height),
        const SizedBox(height: 10),
        Text(v.name,
            style: const TextStyle(
                fontFamily: 'AppFont',
                fontSize: 22,
                fontWeight: FontWeight.bold,
                color: Colors.black)),
      ],
    );

void main() {
  testWidgets('the village market on the six villagers\' screens',
      (tester) async {
    await _loadFonts();
    tester.view.physicalSize = const Size(1160, 1640);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(tester.view.reset);

    final repo = Directory('../programs').existsSync()
        ? Directory('../programs').absolute.path
        : '/Users/udi/Grassroots/GLP/programs';

    final runtimes = <String, UiRuntime>{};
    for (final v in _villagers) {
      final notifies = <GTerm>[];
      // programs/currencies/coins is a program: currency/ is the certified
      // mini-app and this directory adds village.glp, which stands in for the
      // super-app and for the six persons.  The play is run directly: the
      // engine loads the program and runs the posted goal market_ui(V), and
      // what send_to_user/1 sends is read off the runtime's output.
      final engine = GlpEngine(rootSelfGlpPath: '$repo/self.glp')
        ..maxCycles = 50000000;
      engine.runtime.outputTermCallback = (t) => notifies.add(screenTermOf(t));

      final r = UiRuntime(manifest: coinsManifest, onSend: (_) {});
      final result = (await tester.runAsync(() async {
        engine.loadProgram('$repo/currencies/coins');
        return engine.runGoal('market_ui(${v.id})');
      }))!;
      // A run that failed or did not quiesce is half a run and its screen
      // means nothing.
      expect(result.failed || result.capped, isFalse,
          reason: '${v.id}: the market did not run to the end: '
              '${result.status.name} ${result.error ?? ''}');
      for (final t in notifies) {
        r.handle(t);
      }
      // The manifest names no view: nothing but the bridge's constructs.
      expect(r.store.lists, isEmpty);
      expect(r.store.balances, isEmpty);
      // The screen is the agent's report of its own holdings, its latest
      // holdings message, and it is Currencies' list: the figure is checked
      // against it, never fitted to it.
      expect(_holdingsOf(r), _expected[v.id],
          reason: '${v.id}: the screen does not report the holdings of GC\'s '
              'Table 1 (Currencies, 2026-10-14 08:48 UTC)');
      runtimes[v.id] = r;
    }

    Widget grid(Map<String, double> heights) {
      Widget row(Iterable<_Villager> vs) => Row(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              for (final v in vs) ...[
                if (v != vs.first) const SizedBox(width: 18),
                _cell(v, runtimes[v.id]!, heights[v.id]!),
              ]
            ],
          );
      return Directionality(
        textDirection: TextDirection.ltr,
        child: Center(
          child: RepaintBoundary(
            child: Column(
              mainAxisSize: MainAxisSize.min,
              children: [
                row(_villagers.sublist(0, 3)),
                const SizedBox(height: 24),
                row(_villagers.sublist(3, 6)),
              ],
            ),
          ),
        ),
      );
    }

    Finder surfaceOf(_Villager v) =>
        find.byWidgetPredicate((w) => w is AgentSurface && w.agentId == v.id);

    // The cut, measured from the layout: each phone laid out tall enough to
    // hold its whole screen, and its latest holdings message found in the
    // thread; each shell is cut just below it.
    const tall = 6000.0;
    tester.view.physicalSize = const Size(1160, 2 * tall + 200);
    await tester.pumpWidget(grid({for (final v in _villagers) v.id: tall}));
    await tester.pumpAndSettle();
    final heights = <String, double>{};
    for (final v in _villagers) {
      final top = tester.getRect(surfaceOf(v)).top;
      final k = _latestHoldings(runtimes[v.id]!);
      final at = _elementAt(k);
      final message = find.descendant(
          of: surfaceOf(v), matching: find.byKey(_formKey(at)));
      expect(message, findsOneWidget,
          reason: '${v.id}: its latest holdings message is drawn');
      final bottom = tester.getRect(message).bottom - top;
      // The screen's top, 10 points of bezel above it, the message's card
      // margin and padding below it, and the bezel under the screen.
      heights[v.id] = (bottom + 16 + 10 + 10).ceilToDouble();
    }
    // ignore: avoid_print
    print('[cut] each phone is cut below its latest holdings message: '
        '$heights');

    final tallest = heights.values.reduce(math.max);
    tester.view.physicalSize = Size(1160, 2 * tallest + 200);
    await tester.pumpWidget(grid(heights));
    await tester.pumpAndSettle();
    // No phone has a + button: the panel has no compose form, the request
    // form being a construct the bridge draws.
    expect(find.byType(FloatingActionButton), findsNothing);
    // Each phone shows its holdings, headed by the functor, the field named
    // Holdings, each lot a form of its own.
    for (final v in _villagers) {
      final latest = find.descendant(
          of: surfaceOf(v),
          matching:
              find.byKey(_formKey(_elementAt(_latestHoldings(runtimes[v.id]!)))));
      expect(find.descendant(of: latest, matching: find.text('holdings')),
          findsOneWidget, reason: v.id);
      expect(find.descendant(of: latest, matching: find.text('Holdings')),
          findsOneWidget, reason: v.id);
      expect(find.descendant(of: latest, matching: find.text('lot')),
          findsNWidgets(_expected[v.id]!.length), reason: v.id);
    }

    final boundary = tester.renderObject<RenderRepaintBoundary>(
        find.byType(RepaintBoundary).first);
    final bytes = await tester.runAsync(() async {
      final img = await boundary.toImage(pixelRatio: 3.0);
      final data = await img.toByteData(format: ui.ImageByteFormat.png);
      return data!.buffer.asUint8List();
    });
    File('/private/tmp/village-market-coins.png').writeAsBytesSync(bytes!);

    // The stale-card census: a card the dispatcher did not withdraw would
    // stand above the thread, so the figure requires none on any phone ---
    // every construct open at the end is the request form, the agent's one
    // question, Stream(Request)?, which stays open while its stream does, and
    // the output thread, construct 0.
    final census = {
      for (final v in _villagers)
        v.name: runtimes[v.id]!
            .bridge
            .constructs
            .where((c) =>
                c.id != 0 && !formatTerm(c.widget).startsWith('input_box('))
            .length
    };
    // ignore: avoid_print
    print('[stale cards] $census');
    final stale = {...census}..removeWhere((_, n) => n == 0);
    expect(stale, isEmpty,
        reason: 'cards the app never retired: $stale --- the image just '
            'written is not the figure');
    // And the request form stands on every phone, once, beside the thread.
    for (final v in _villagers) {
      expect(runtimes[v.id]!.bridge.constructs.map((c) => c.id).toSet(),
          {0, 1},
          reason: '${v.id}: the output thread and the request form, and '
              'nothing else, stand');
    }
  }, timeout: const Timeout(Duration(minutes: 15)));
}

/// The elements of the output thread, construct 0, of [r].
List<GTerm> _output(UiRuntime r) {
  final v = r.bridge.construct(0)?.view;
  return v is GList ? v.items : const [];
}

/// The index of the latest holdings message in [r]'s output thread.
int _latestHoldings(UiRuntime r) =>
    _output(r).lastIndexWhere((m) => ctorArgs(m).$1 == 'holdings');

/// The holdings [r]'s latest holdings message reports, issuer to amount.
Map<String, String> _holdingsOf(UiRuntime r) {
  final k = _latestHoldings(r);
  if (k < 0) return const {};
  final lots = ctorArgs(_output(r)[k]).$2.single as GList;
  return {
    for (final l in lots.items)
      formatTerm(ctorArgs(l).$2[0]): formatTerm(ctorArgs(l).$2[1])
  };
}

/// The position of element [k], from 0, of the thread at the root: 2 for
/// each element before it and then 1 (construct_family.dart, elementAt).
List<int> _elementAt(int k) => [for (var j = 0; j < k; j++) 2, 1];

/// The key of the form drawn at [at] of construct 0.
ValueKey<String> _formKey(List<int> at) => ValueKey('0:form:[${at.join(', ')}]');
