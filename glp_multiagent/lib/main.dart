/// GrassApp / Social Graph — live single-isolate scenarios.
///
/// One device, one person, the two constructs and the screen (paper §6): the
/// active scenario runs its platform's compiled pair — the agent and the UI
/// mediator, the manual compilation of the platform's volition-guarded
/// clauses — in an agent isolate. The mediator's UserNotify stream renders as
/// inbox cards and screen state; its UserCmd stream is written by the compose
/// forms and card taps. Two scenarios, selectable on desktop:
///
///  - **GrassApp** (default; the phone deploy): programs/grassapp —
///    grassapp_agent + grassapp_mediator, three panels (Friends/Currencies/Chats).
///  - **Social Graph**: programs/social/graph — agent.glp + ui/mediator.glp,
///    the canonical graph pair, booted by play_ui_boot.glp.
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter/material.dart';

import 'isolate_protocol.dart';
import 'glp_sources.dart';
import 'manifests/grassapp_ui.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/runtime.dart';

// =============================================================================
// SHARED FILE LOGGER
// =============================================================================

/// Shared file logger for tracing execution across coordinator and agents.
/// Uses simple file append to avoid stream conflicts across isolates.
class TraceLogger {
  static final TraceLogger _instance = TraceLogger._();
  static TraceLogger get instance => _instance;

  TraceLogger._();

  static const String _logPath = '/tmp/glp_multiagent_trace.log';
  bool _initialized = false;

  void init({bool clear = true}) {
    if (_initialized) return;
    final file = File(_logPath);
    if (clear && file.existsSync()) {
      file.deleteSync();
    }
    _initialized = true;
    log('TRACE', '=== Logger initialized ===');
  }

  void log(String source, String message) {
    final timestamp = DateTime.now().toIso8601String().substring(11, 23);
    final line = '[$timestamp] [$source] $message\n';
    try {
      File(_logPath).writeAsStringSync(line, mode: FileMode.append, flush: true);
    } catch (_) {}
    debugPrint(line.trim());
  }

  void close() {
    _initialized = false;
  }
}

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CoordinatorApp());
}

class CoordinatorApp extends StatelessWidget {
  const CoordinatorApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'GrassApp',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.green,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.green,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.green,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.green,
            foregroundColor: Colors.white,
          ),
        ),
      ),
      home: const CoordinatorScreen(),
    );
  }
}

// =============================================================================
// PER-AGENT UI STATE
// =============================================================================

class AgentState {
  final String agentId;
  SendPort? commandPort;
  bool initialized = false;
  String status = 'Spawning...';

  /// Manifest-driven UI runtime: inbox cards, compose forms, screen state.
  UiRuntime? ui;

  AgentState(this.agentId);
}

// =============================================================================
// COORDINATOR SCREEN
// =============================================================================

class CoordinatorScreen extends StatefulWidget {
  const CoordinatorScreen({super.key});

  @override
  State<CoordinatorScreen> createState() => _CoordinatorScreenState();
}

class _CoordinatorScreenState extends State<CoordinatorScreen> {
  final Map<String, AgentState> _agents = {};

  /// The live scenario isolate (kept so it can be disposed cleanly).
  Isolate? _scenarioIsolate;

  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription? _replySubscription;

  @override
  void initState() {
    super.initState();

    TraceLogger.instance.init(clear: true);
    TraceLogger.instance.log('COORD', 'Coordinator started (isolate mode)');

    // Listen for messages from the agent isolate.
    _replySubscription = _replyPort.listen(_handleAgentMessage);

    // Clean smartphone UI: auto-launch the default scenario so Bob's surface
    // populates on open.
    WidgetsBinding.instance.addPostFrameCallback((_) => _spawnScenario());
  }

  void _handleAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.commandPort = msg.commandPort;
        state.initialized = true;
        state.status = 'Ready';
        TraceLogger.instance.log('COORD', '${msg.agentId} ready');
        // Currencies scenario: ask Bob for his opening balance so the Wallet shows
        // his starting coins immediately. (The graph agent has no balance.)
        if (msg.agentId == 'Bob') {
          state.commandPort!.send(UserInput('balance'));
        }
      }
      setState(() {});
    } else if (msg is AgentOutput) {
      final state = _agents[msg.agentId];
      if (state != null) {
        // The isolate marks genuine _output/1 notifies with a leading '< ';
        // debug chatter ([INIT]…, > …) has no such marker. Strip the transport
        // marker so the runtime receives the bare ground term.
        final boundaryLine =
            msg.line.startsWith('< ') ? msg.line.substring(2) : msg.line;
        state.ui?.handleLine(boundaryLine);
        setState(() {});
      }
    } else if (msg is AgentLog) {
      TraceLogger.instance.log(msg.tag, msg.message);
    } else if (msg is AgentSendMad) {
      // Single-isolate scenarios have no cross-isolate routing.
      TraceLogger.instance.log('COORD',
          'Unexpected MAD send from ${msg.agentId} to ${msg.to} — dropped');
    } else if (msg is AgentError) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.status = 'Error: ${msg.error}';
        setState(() {});
      }
      TraceLogger.instance.log('COORD', 'ERROR from ${msg.agentId}: ${msg.error}');
    }
  }

  @override
  void dispose() {
    _replySubscription?.cancel();
    _replyPort.close();
    super.dispose();
  }

  // ===========================================================================
  // SCENARIO SPAWNING
  // ===========================================================================

  /// Spawn the live single-isolate scenario for [_scenario]: ONE heap runs the
  /// platform's real agent + ui_mediator for Bob (live UI) plus alice/charlie
  /// (scenario actors) wired through an in-heap crossbar. The actors cold-call
  /// Bob on launch, so his inbox shows two friend-offer cards; accepting each
  /// forms the friendship. Only Bob has a surface.
  Future<void> _spawnScenario() async {
    await _closeAll();

    // Resolve the GLP source tree — repo on desktop, bundled assets copied
    // into Documents on iOS (see glp_sources.dart).
    final glp = await resolveGlpPaths();

    final bob = AgentState('Bob');
    late final InitAgent initMsg;

    // The GrassApp: the one integrated program (social graph + one-to-one and
    // group messaging + coins), loaded in order.
    const scenarioFiles = [
      'self.glp',
      'currency_txn.glp',
      'grassapp_agent.glp',
      'grassapp_mediator.glp',
      'play_grassapp_boot.glp',
    ];
    final paths = [for (final f in scenarioFiles) '${glp.grassappDir}/$f'];
    final sources = <String>[];
    for (final p in paths) {
      final file = File(p);
      if (!file.existsSync()) {
        TraceLogger.instance.log('COORD', 'scenario file not found: $p');
        setState(() {});
        return;
      }
      sources.add(await file.readAsString());
    }
    bob.ui = UiRuntime(
      manifest: grassrootsManifest,
      onSend: (text) => bob.commandPort?.send(UserInput(text)),
    );
    initMsg = InitAgent(
      agentId: 'Bob',
      glpSources: sources,
      glpSourcePaths: paths,
      rootSelfGlpPath: glp.rootSelfGlp,
      friends: const ['alice', 'charlie'],
      replyPort: _replyPort.sendPort,
      deferStart: false,
    );

    bob.ui!.onChange = () => setState(() {});
    _agents['Bob'] = bob;

    try {
      _scenarioIsolate = await Isolate.spawn(agentIsolateEntry, initMsg);
      TraceLogger.instance.log('COORD', 'GrassApp started');
    } catch (e) {
      TraceLogger.instance.log('COORD', 'ERROR spawning scenario: $e');
    }
    setState(() {});
  }

  Future<void> _closeAll() async {
    _scenarioIsolate?.kill(priority: Isolate.immediate);
    _scenarioIsolate = null;
    for (final agent in _agents.values) {
      agent.commandPort?.send(DisposeAgent());
    }
    _agents.clear();
    setState(() {});
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    final bob = _agents['Bob'];
    final surface = (bob != null && bob.ui != null)
        ? AgentSurface(agentId: bob.agentId, runtime: bob.ui!)
        : const _Booting();
    // On a real phone (iOS) the device is the frame: fill the screen and let
    // the OS draw the status bar. On desktop, draw the simulated phone bezel.
    if (Platform.isIOS) {
      return Material(color: Colors.white, child: surface);
    }
    return Scaffold(
      backgroundColor: const Color(0xFF2B2B33),
      body: Center(child: _PhoneFrame(child: surface)),
    );
  }
}

// =============================================================================
// Clean smartphone presentation
// =============================================================================

/// A phone-shaped bezel that frames the agent's surface.
class _PhoneFrame extends StatelessWidget {
  final Widget child;
  const _PhoneFrame({required this.child});

  @override
  Widget build(BuildContext context) {
    return Container(
      width: 400,
      height: 860,
      padding: const EdgeInsets.all(12),
      decoration: BoxDecoration(
        color: Colors.black,
        borderRadius: BorderRadius.circular(44),
        boxShadow: const [
          BoxShadow(color: Colors.black54, blurRadius: 40, spreadRadius: 4),
        ],
      ),
      child: ClipRRect(
        borderRadius: BorderRadius.circular(32),
        child: Container(
          color: Colors.white,
          child: Column(
            children: [
              _statusBar(),
              Expanded(child: child),
            ],
          ),
        ),
      ),
    );
  }

  Widget _statusBar() => Container(
        height: 28,
        color: Colors.green,
        padding: const EdgeInsets.symmetric(horizontal: 18),
        child: const Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: [
            Text('9:41',
                style: TextStyle(
                    color: Colors.white,
                    fontSize: 13,
                    fontWeight: FontWeight.w700)),
            Row(children: [
              Icon(Icons.signal_cellular_alt, color: Colors.white, size: 14),
              SizedBox(width: 5),
              Icon(Icons.wifi, color: Colors.white, size: 14),
              SizedBox(width: 5),
              Icon(Icons.battery_full, color: Colors.white, size: 16),
            ]),
          ],
        ),
      );
}

class _Booting extends StatelessWidget {
  const _Booting();
  @override
  Widget build(BuildContext context) => const Center(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            CircularProgressIndicator(color: Colors.green),
            SizedBox(height: 16),
            Text('Starting…', style: TextStyle(color: Colors.grey)),
          ],
        ),
      );
}
