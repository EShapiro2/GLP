/// GrassApp Village Market — the six-agent scenario of Grassroots-Bonds §8.2 on
/// the live phone UI, all six agents at once.
///
/// Runs `play_village` in-process, captures each agent's `tagged(Id, Notify)`
/// stream, and replays it into six live GrassApp surfaces, each opened on its
/// own "Your wallet" view, arranged in a 2×3 grid — so the whole economy plays out
/// across the village's phones: mutual credit lines forming, Diana's loans as
/// dated bonds, payments, Eve's portfolio swap, Charlie's escrow for the dock,
/// Frank's redemptions, and Alice selling bob-debt to Eve.
///
/// Compute happens on launch; a Play button then paces the replay, so a screen
/// recording started before Play captures the whole script cleanly. This is the
/// source of the village-market figure.
library;

import 'dart:async';
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:glp_runtime/engine/glp_engine.dart';

import 'glp_sources.dart';
import 'manifests/grassapp_ui.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/runtime.dart';

void main() => runApp(const VillageApp());

class VillageApp extends StatelessWidget {
  const VillageApp({super.key});
  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'GrassApp Village Market',
        debugShowCheckedModeBanner: false,
        theme: ThemeData(
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.green),
          useMaterial3: true,
        ),
        home: const VillageScreen(),
      );
}

/// One villager: notify-stream id (lowercase), display name, trade, header hue.
class _Villager {
  final String id;
  final String name;
  final String role;
  final Color color;
  const _Villager(this.id, this.name, this.role, this.color);
}

const _villagers = <_Villager>[
  _Villager('alice', 'Alice', 'Baker', Color(0xFF3949AB)),
  _Villager('bob', 'Bob', 'Farmer', Color(0xFF00897B)),
  _Villager('charlie', 'Charlie', 'Carpenter', Color(0xFF6D4C41)),
  _Villager('diana', 'Diana', 'Doctor', Color(0xFFC2185B)),
  _Villager('eve', 'Eve', 'Teacher', Color(0xFF546E7A)),
  _Villager('frank', 'Frank', 'Fisherman', Color(0xFF00838F)),
];

/// One replayable notify: which agent's phone, and the term to hand its runtime.
class _Ev {
  final String agent;
  final String notify;
  const _Ev(this.agent, this.notify);
}

/// The Currencies panel index in the grassapp manifest (Friends, Currencies, Chats).
const int _currenciesPanel = 1;

/// Milliseconds per notify during replay — sets the video's pace.
const int _stepMs = 55;

/// Figure mode hides the app chrome (title bar, Play button, status ticker) and
/// auto-plays on load, so a recording or screenshot shows only the six phones —
/// the village-market figure. Flip to false for the interactive, labelled view.
const bool _figureMode = true;

class VillageScreen extends StatefulWidget {
  const VillageScreen({super.key});
  @override
  State<VillageScreen> createState() => _VillageScreenState();
}

class _VillageScreenState extends State<VillageScreen> {
  Map<String, UiRuntime> _uis = {};
  final List<_Ev> _events = [];
  int _cursor = 0;
  int _gen = 0; // bumps on replay to force fresh AgentSurface state
  Timer? _timer;
  String _status = 'Loading…';
  bool _ready = false;

  @override
  void initState() {
    super.initState();
    _freshRuntimes();
    WidgetsBinding.instance.addPostFrameCallback((_) => _compute());
  }

  @override
  void dispose() {
    _timer?.cancel();
    super.dispose();
  }

  void _freshRuntimes() {
    _uis = {
      for (final v in _villagers)
        v.id: UiRuntime(manifest: grassrootsManifest, onSend: (_) {})
          ..onChange = () {
            if (mounted) setState(() {});
          }
    };
  }

  /// Run the village once and capture its notify stream, tagged per agent.
  Future<void> _compute() async {
    setState(() => _status = 'Running the village…');
    const files = [
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_village_headless.glp',
    ];
    try {
      final glp = await resolveGlpPaths();
      final engine = GlpEngine(rootSelfGlpPath: glp.rootSelfGlp)
        ..strictTypes = false
        ..maxCycles = 5000000;
      for (final f in files) {
        engine.loadSource(
            File('${glp.grassappDir}/$f').readAsStringSync(),
            filename: f);
      }
      final captured = <String>[];
      engine.runtime.outputCallback = captured.add;
      await engine.runGoal('play_village.');
      for (final line in captured) {
        final ev = _parseTagged(line);
        if (ev != null) _events.add(ev);
      }
      setState(() {
        _ready = true;
        _status = '${_events.length} events ready — playing…';
      });
      // Auto-play a beat after compute, so a screen recording started before
      // launch captures the whole replay without needing a tap. The Play button
      // remains for a manual re-run.
      Future.delayed(
          const Duration(milliseconds: 900), () => mounted ? _play() : null);
    } catch (e) {
      setState(() => _status = 'Error: $e');
    }
  }

  /// `tagged(agent, notify)` -> `_Ev`; null for any other line.
  _Ev? _parseTagged(String raw) {
    final line = raw.trim();
    final i = line.indexOf('tagged(');
    if (i < 0) return null;
    var s = line.substring(i + 7);
    if (!s.endsWith(')')) return null;
    s = s.substring(0, s.length - 1);
    final c = s.indexOf(',');
    if (c < 0) return null;
    return _Ev(s.substring(0, c).trim(), s.substring(c + 1).trim());
  }

  void _play() {
    _timer?.cancel();
    setState(() {
      _gen++;
      _freshRuntimes();
      _cursor = 0;
      _status = 'Playing…';
    });
    _timer = Timer.periodic(const Duration(milliseconds: _stepMs), (t) {
      if (_cursor >= _events.length) {
        t.cancel();
        setState(() => _status = 'Village market — complete');
        return;
      }
      final ev = _events[_cursor++];
      _uis[ev.agent]?.handleLine(ev.notify);
      setState(() => _status = '${ev.agent}  ◂  ${ev.notify}');
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: const Color(0xFF1B1B1F),
      appBar: _figureMode ? null : AppBar(
        backgroundColor: const Color(0xFF0B3D2E),
        foregroundColor: Colors.white,
        title: const Text('Village Market — six grassroots phones',
            style: TextStyle(fontSize: 15, fontWeight: FontWeight.bold)),
        actions: [
          Padding(
            padding: const EdgeInsets.only(right: 12),
            child: FilledButton.icon(
              style: FilledButton.styleFrom(
                  backgroundColor: Colors.green.shade600),
              onPressed: _ready ? _play : null,
              icon: const Icon(Icons.play_arrow),
              label: const Text('Play'),
            ),
          ),
        ],
      ),
      body: SafeArea(
        child: Column(
          children: [
            Expanded(
              child: Row(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  for (final v in _villagers.take(3)) Expanded(child: _cell(v)),
                ],
              ),
            ),
            Expanded(
              child: Row(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  for (final v in _villagers.skip(3)) Expanded(child: _cell(v)),
                ],
              ),
            ),
            if (!_figureMode) _statusBar(),
          ],
        ),
      ),
    );
  }

  /// One villager's phone: a bezel-framed GrassApp surface with a coloured
  /// name plate, so the six read as six distinct phones, not one split screen.
  Widget _cell(_Villager v) {
    return Padding(
      padding: const EdgeInsets.all(5),
      child: Center(
        child: AspectRatio(
          aspectRatio: 0.58,
          child: Container(
            decoration: BoxDecoration(
              color: Colors.black,
              borderRadius: BorderRadius.circular(26),
              boxShadow: const [
                BoxShadow(
                    color: Colors.black54, blurRadius: 14, spreadRadius: 1),
              ],
            ),
            padding: const EdgeInsets.all(5),
            child: ClipRRect(
              borderRadius: BorderRadius.circular(21),
              child: Column(
                children: [
                  Container(
                    width: double.infinity,
                    padding: const EdgeInsets.symmetric(
                        horizontal: 10, vertical: 4),
                    color: v.color,
                    child: Text('${v.name} · ${v.role}',
                        textAlign: TextAlign.center,
                        style: const TextStyle(
                            color: Colors.white,
                            fontWeight: FontWeight.bold,
                            fontSize: 11)),
                  ),
                  Expanded(
                    child: AgentSurface(
                      key: ValueKey('${v.id}-$_gen'),
                      agentId: v.name,
                      runtime: _uis[v.id]!,
                      initialPanel: _currenciesPanel,
                      openSelfWallet: true,
                      muteNotices: true,
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }

  Widget _statusBar() => Container(
        width: double.infinity,
        color: const Color(0xFF0B3D2E),
        padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
        child: Text(_status,
            maxLines: 1,
            overflow: TextOverflow.ellipsis,
            style: const TextStyle(
                color: Color(0xFF9BE7C4),
                fontFamily: 'monospace',
                fontSize: 12)),
      );
}
