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
/// 🔴 THE FIGURE IS NOT READY TO SHIP, AND THE FAULT IS NOT THIS FILE'S TO FIX.
/// Under `village_ui/3` the app is a passive observer: `village.glp:97--104`
/// tees the villager's events to the app AND to the scripted person, and the
/// person's answers go back on `Answers`, which the app never sees
/// (`self.glp:325`, `village_ui(Id, _, _)`, discards the app's own answer
/// stream).  The mediator emits `closed(ReqId)` for an ask it ABORTS and none
/// for an ask that is FULFILLED --- the interface that gave the answer retires
/// its own card, as `coins_screen_test.dart` does on Accept --- so an observing
/// app is never told, and every `respond_swap_1` card the scripted person
/// accepted stands open on the screen for ever.  Measured here: Alice and Diana
/// have no `accept` in their scripts and their screens are clean; Bob, Charlie,
/// Eve and Frank have two each and carry two stale cards each, which bury the
/// balances view.  The holdings below are right in all six; the screens are not.
/// Either `village.glp` routes the app's own `Answers` into the villager it
/// names, or the mediator emits `closed(ReqId)` on a fulfilled ask as it does
/// on an aborted one --- the first is Currencies', the second vGLP's, and
/// neither is decided here.
library;

import 'dart:io';
import 'dart:ui' as ui;

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glp_multiagent/manifests/coins_ui.dart';
import 'package:glp_multiagent/ui_runtime/agent_surface.dart';
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
/// the proposal card already in the paper: same size, same bezel, same theme.
Widget _phone(Widget surface) => Container(
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

Widget _cell(_Villager v, UiRuntime r) => Column(
      mainAxisSize: MainAxisSize.min,
      children: [
        _phone(AgentSurface(agentId: v.id, runtime: r, muteNotices: true)),
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

      final r = UiRuntime(manifest: coinsManifest, onSend: (_) {});
      await tester.runAsync(() => agent.initialize());
      // A run that did not quiesce is half a run and its screen means nothing.
      expect(lines.where((l) => l.contains('[ERROR]')), isEmpty,
          reason: '${v.id}: the market did not run to the end');
      for (final l in lines) {
        if (l.startsWith('< ')) r.handleLine(l.substring(2));
      }
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

    Widget row(Iterable<_Villager> vs) => Row(
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            for (final v in vs) ...[
              if (v != vs.first) const SizedBox(width: 18),
              _cell(v, runtimes[v.id]!),
            ]
          ],
        );

    await tester.pumpWidget(Directionality(
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
    ));
    await tester.pumpAndSettle();

    final boundary = tester.renderObject<RenderRepaintBoundary>(
        find.byType(RepaintBoundary).first);
    final bytes = await tester.runAsync(() async {
      final img = await boundary.toImage(pixelRatio: 3.0);
      final data = await img.toByteData(format: ui.ImageByteFormat.png);
      return data!.buffer.asUint8List();
    });
    File('/private/tmp/village-market-coins.png').writeAsBytesSync(bytes!);

    // The stale-card census, so that a run of this test says out loud why the
    // image it just wrote is not the figure.  See the fault at the head of
    // this file.
    final stale = {
      for (final v in _villagers) v.name: runtimes[v.id]!.inbox.length
    }..removeWhere((_, n) => n == 0);
    if (stale.isNotEmpty) {
      // ignore: avoid_print
      print('[NOT THE FIGURE] cards answered by the scripted person and never '
          'retired on the app\'s screen: $stale --- village.glp:97--104 tees '
          'the events to the app but not the answers, and a fulfilled ask '
          'carries no closed(ReqId).');
    }
  }, timeout: const Timeout(Duration(minutes: 15)));
}
