/// The village market of "Grassroots Currencies: Turning Mutual Trust into
/// Liquidity", Appendix "From the Contract to the App", on the six villagers'
/// screens: the figure `village-market-coins.png`.
///
/// Each phone is one villager's own execution of the currency mini-app under
/// the Grassroots Super-App, its screen derived from the display declarations
/// of `programs/currencies/coins/currency/coins_agent.vglp` and rendered by
/// the same `AgentSurface` and `coinsManifest` that render the Swap form and
/// the proposal card of that appendix, in the same phone shell and at the same
/// scale.
///
/// `village_ui/3` runs the whole six-agent market and puts the villager it
/// names on the app's own person channel (`programs/currencies/coins/self.glp`,
/// "the app shows that villager's screen as the market runs, which is how the
/// six balances views ... are taken, one villager per run").  The six agents
/// run in one harness in every run; the app renders one of them, so the figure
/// is six runs of the same market, each rendering its own villager's screen
/// through the app's own path.  Nothing is fed to a surface that its agent did
/// not send, and no screen is copied from another.
///
/// The image this test writes is the figure.  Under `village_ui/3` the app
/// observes and does not answer: `village.glp`'s `villager/4` sends the named
/// villager's events through `tee/3` to the app and to the scripted person,
/// and the person's answers go to the mediator and not to the app.  The
/// mediator sends `closed(ReqId)` when an ask is answered or declined and
/// `aborted(ReqId)` when it is aborted (`coins_agent.glp`, `med/4`), so the
/// app retires every card the scripted person answered, and the test requires
/// that no card stands on any phone at the end of the run.
///
/// The SCREEN view is suppressed in this test, at Currencies' request of
/// 2026-09-24: it shows each screen message by its arguments with the functor
/// dropped, and GC makes no claim about it.  Each phone is rendered from
/// [_balancesOnly], a manifest local to this test: `coinsManifest`'s one panel
/// with that view left out, and the same forms, the same card and the same
/// BALANCES view.  No library code is changed.  The runtime handles cards,
/// `closed` and `aborted` before it tries any view, and ignores a screen
/// message no view matches, so the holdings and the cards are those of
/// `coinsManifest`; the test checks this against a runtime under
/// `coinsManifest` fed the same lines.
///
/// The phones are cut below the balances, at Currencies' request of
/// 2026-09-24: each shell is laid out as tall as the longest list, four rows,
/// and one row's height under it, measured from the layout, and closes its
/// lower edge with the bezel and corner radius of its upper edge, so the +
/// button at the foot of the screen falls outside it.
library;

import 'dart:io';
import 'dart:math' as math;
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
import 'package:glp_multiagent/ui_runtime/manifest.dart';
import 'package:glp_multiagent/ui_runtime/runtime.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';
import 'package:glp_runtime/multiagent/agent_runtime.dart';

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

/// The holdings the run must end at, Currencies' own list.
const _expected = <String, Map<String, String>>{
  'alice': {'bob': '5', 'charlie': '15', 'alice': '8', 'frank': '4'},
  'bob': {'alice': '10', 'diana': '20'},
  'charlie': {'alice': '10', 'eve': '10', 'charlie': '6', 'frank': '5'},
  'diana': {'bob': '24', 'frank': '13', 'diana': '8'},
  'eve': {'charlie': '4', 'frank': '1', 'alice': '2', 'bob': '10'},
  'frank': {'diana': '7', 'eve': '10', 'frank': '5'},
};

/// `coinsManifest` with its SCREEN view, the list view of the default display,
/// left out, and every other element of it kept as the same object: the
/// panel's id and name, its four forms, its swap card and its BALANCES view.
final Manifest _balancesOnly = () {
  final p = coinsManifest.panels.single;
  return Manifest(
    title: coinsManifest.title,
    panels: [
      Panel(
        id: p.id,
        name: p.name,
        friends: p.friends,
        wallet: p.wallet,
        chat: p.chat,
        groups: p.groups,
        commands: p.commands,
        inbox: p.inbox,
        views: [for (final v in p.views) if (v.store != 'screen') v],
      ),
    ],
    activity: coinsManifest.activity,
    state: coinsManifest.state,
  );
}();

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

/// The height of the screen within the shell of `coins_screen_test.dart`: its
/// 740 points less the 10-point bezel above and below.
const _screen = 720.0;

/// The phone shell of `coins_screen_test.dart`, which took the Swap form and
/// the proposal card already in the paper: the same width, bezel, corner radius
/// and theme, and within it the same screen, laid out [_screen] points tall.
/// The shell there is 740 points tall; a shorter [height] shows the top of that
/// screen, and the shell closes its lower edge as its upper edge is closed.
Widget _phone(Widget surface, {required double height}) => Container(
      width: 360,
      height: height,
      padding: const EdgeInsets.all(10),
      decoration: BoxDecoration(
          color: Colors.black, borderRadius: BorderRadius.circular(40)),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(30),
        child: OverflowBox(
          alignment: Alignment.topCenter,
          minHeight: _screen,
          maxHeight: _screen,
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
      final lines = <String>[];
      final agent = AgentRuntime(
        agentId: v.id,
        glpSources: const [],
        // programs/currencies/coins is a program: currency/ is the certified
        // mini-app and this directory adds village.glp, which stands in for
        // the super-app and for the six persons.
        programDir: '$repo/currencies/coins',
        goalLabel: 'village_ui/3',
        rootSelfGlpPath: '$repo/self.glp',
        friends: v.friends,
      )..maxQuiescenceCycles = 5000000;
      agent.onOutput = lines.add;
      agent.onLog = (_, __) {};
      agent.onSendMadMessage = (_, __) async {};

      final r = UiRuntime(manifest: _balancesOnly, onSend: (_) {});
      // The same lines under the app's manifest, SCREEN view included, so that
      // the test shows that leaving the view out changes no holding and no card.
      final full = UiRuntime(manifest: coinsManifest, onSend: (_) {});
      await tester.runAsync(() => agent.initialize());
      // A run that did not quiesce is half a run and its screen means nothing.
      expect(lines.where((l) => l.contains('[ERROR]')), isEmpty,
          reason: '${v.id}: the market did not run to the end');
      for (final l in lines) {
        if (l.startsWith('< ')) {
          r.handleLine(l.substring(2));
          full.handleLine(l.substring(2));
        }
      }
      String state(UiRuntime x) => [
            x.store.balances.map((s, b) =>
                MapEntry(s, b.map((k, a) => MapEntry(k, formatTerm(a))))),
            [
              for (final c in x.inbox)
                '${c.itemKey} ${c.asks.map((k, a) => MapEntry(k, formatTerm(a)))}'
            ],
            x.standing.map((k, a) => MapEntry(k, formatTerm(a))),
          ].toString();
      expect(state(r), state(full),
          reason: '${v.id}: leaving the SCREEN view out changed the holdings '
              'or the cards');
      // The screen is the agent's report of its own holdings, and it is
      // Currencies' list: the figure is checked against it, never fitted to it.
      expect(
          r.store.balances['balances']!
              .map((k, v2) => MapEntry(k, formatTerm(v2))),
          _expected[v.id],
          reason: '${v.id}: the screen does not report the holdings of the '
              'paper (Currencies, 2026-09-20 12:44 UTC)');
      runtimes[v.id] = r;
    }

    Widget grid(double height) {
      Widget row(Iterable<_Villager> vs) => Row(
            mainAxisSize: MainAxisSize.min,
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              for (final v in vs) ...[
                if (v != vs.first) const SizedBox(width: 18),
                _cell(v, runtimes[v.id]!, height),
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

    // The cut, measured from the layout: the phones laid out 740 points tall,
    // as in `coins_screen_test.dart`, and each balance row found on each
    // screen.  The shell is cut one row's height below the lowest last row.
    await tester.pumpWidget(grid(740));
    await tester.pumpAndSettle();
    final rowHeights = <double>{};
    var balancesEnd = 0.0;
    for (final v in _villagers) {
      final top = tester.getRect(surfaceOf(v)).top;
      final rows =
          find.descendant(of: surfaceOf(v), matching: find.byType(ListTile));
      expect(rows, findsNWidgets(_expected[v.id]!.length),
          reason: '${v.id}: a balance row per holding');
      for (var i = 0; i < _expected[v.id]!.length; i++) {
        final rect = tester.getRect(rows.at(i));
        rowHeights.add(rect.height);
        balancesEnd = math.max(balancesEnd, rect.bottom - top);
      }
    }
    expect(rowHeights, hasLength(1), reason: 'the balance rows differ in height');
    final rowHeight = rowHeights.single;
    final height = (balancesEnd + rowHeight + 20).ceilToDouble();
    // ignore: avoid_print
    print('[cut] the balances end $balancesEnd points down the screen, a row '
        'is $rowHeight points, so the phone is $height points tall');

    await tester.pumpWidget(grid(height));
    await tester.pumpAndSettle();
    // Every phone shows its BALANCES view, and none shows a SCREEN view.
    expect(find.text('BALANCES'), findsNWidgets(_villagers.length));
    expect(find.text('SCREEN'), findsNothing);
    // The + button stands at the foot of the 720-point screen, below the
    // shell's lower edge, so no phone shows it.
    for (final v in _villagers) {
      final screenBottom = tester.getRect(surfaceOf(v)).top + height - 20;
      final plus = tester.getRect(find.descendant(
          of: surfaceOf(v), matching: find.byType(FloatingActionButton)));
      expect(plus.top, greaterThanOrEqualTo(screenBottom),
          reason: '${v.id}: the + button shows above the cut');
    }

    final boundary = tester.renderObject<RenderRepaintBoundary>(
        find.byType(RepaintBoundary).first);
    final bytes = await tester.runAsync(() async {
      final img = await boundary.toImage(pixelRatio: 3.0);
      final data = await img.toByteData(format: ui.ImageByteFormat.png);
      return data!.buffer.asUint8List();
    });
    File('/private/tmp/village-market-coins.png').writeAsBytesSync(bytes!);

    // The stale-card census: a card the app did not retire would stand above
    // the BALANCES view, so the figure requires none on any phone.
    final census = {
      for (final v in _villagers) v.name: runtimes[v.id]!.inbox.length
    };
    // ignore: avoid_print
    print('[stale cards] $census');
    final stale = {...census}..removeWhere((_, n) => n == 0);
    expect(stale, isEmpty,
        reason: 'cards the app never retired: $stale --- the image just '
            'written is not the figure');
  }, timeout: const Timeout(Duration(minutes: 15)));
}
