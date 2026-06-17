/// GLP Social Graph — Simulated SG Plays
///
/// Runs SG plays (fplay1-3) via REPL subprocess, with tagged output
/// parsed and routed to per-agent read-only panels (Alice, Bob, Charlie).
library;

import 'dart:async';
import 'dart:io';
import 'dart:isolate';

import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/repl_play_runner.dart';

import 'isolate_protocol.dart';
import 'mad_router.dart';
import 'manifests/gsg.dart';
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

/// Resolve the GLP program directory. Relative in dev (cwd = glp_multiagent/),
/// absolute fallback for the release .app bundle (cwd differs).
String _resolveGlpDir() {
  const rel = '../programs/book/social_graph';
  if (Directory(rel).existsSync()) return Directory(rel).absolute.path;
  const fallback = '/Users/udi/Grassroots/GLP/programs/book/social_graph';
  if (Directory(fallback).existsSync()) return fallback;
  return rel;
}

final _defaultGlpDir = _resolveGlpDir();

/// Resolve absolute path to programs/self.glp.
String _resolveRootSelfGlpPath() {
  final candidate = File('../programs/self.glp').absolute.path;
  if (File(candidate).existsSync()) return candidate;
  const fallback = '/Users/udi/Grassroots/GLP/programs/self.glp';
  if (File(fallback).existsSync()) return fallback;
  return candidate;
}

final _rootSelfGlpPath = _resolveRootSelfGlpPath();

/// GLP files loaded for UI agents (order matters: shared first, then boot)
const _glpFiles = [
  'self.glp',
  'typed_social_agent.glp',
  'typed_ui_mediator.glp',
  'play_ui_boot.glp',
];

// =============================================================================
// ENTRY POINT
// =============================================================================

void main() {
  runApp(const CoordinatorApp());
}

// =============================================================================
// COORDINATOR APP
// =============================================================================

class CoordinatorApp extends StatelessWidget {
  const CoordinatorApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Social Graph',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        primarySwatch: Colors.orange,
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.orange,
          brightness: Brightness.light,
        ),
        appBarTheme: const AppBarTheme(
          backgroundColor: Colors.orange,
          foregroundColor: Colors.white,
        ),
        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.orange,
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
  final List<String> friends;
  final bool readOnly;
  SendPort? commandPort;
  bool initialized = false;
  String status = 'Spawning...';
  final List<String> outputLog = [];
  /// Manifest-driven two-surface UI runtime (null for read-only play panels).
  UiRuntime? ui;
  int goalCount = 0;
  int heapVars = 0;
  int wpSize = 0;
  int mpSize = 0;
  final TextEditingController inputController = TextEditingController();
  final ScrollController scrollController = ScrollController();
  final FocusNode inputFocusNode = FocusNode();

  AgentState(this.agentId, this.friends, {this.readOnly = false});

  void dispose() {
    inputController.dispose();
    scrollController.dispose();
    inputFocusNode.dispose();
  }
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
  final List<String> _log = [];
  final TextEditingController _glpPathController =
      TextEditingController(text: _defaultGlpDir);
  String _currentGlpDir = _defaultGlpDir;
  List<String>? _cachedGlpSources;
  List<String> _cachedGlpPaths = const [];

  final ReceivePort _replyPort = ReceivePort();
  StreamSubscription? _replySubscription;

  /// Live (interactive) trio orchestration: deferred start until all ports
  /// are registered, so agent-to-agent routing has no races.
  int _expectedAgentCount = 0;
  Completer<void>? _allReadyCompleter;

  @override
  void initState() {
    super.initState();

    TraceLogger.instance.init(clear: true);
    TraceLogger.instance.log('COORD', 'Coordinator started (isolate mode)');

    IsolateRouter.instance.onLogUpdate = () {
      setState(() {});
    };

    // Listen for messages from all agent isolates.
    _replySubscription = _replyPort.listen(_handleAgentMessage);

    _log.add('Coordinator started (isolate mode)');
    _log.add('GLP dir: $_currentGlpDir');

    // Clean smartphone UI: auto-launch the single-isolate scenario so Bob's
    // surface populates on open. No dev controls.
    WidgetsBinding.instance.addPostFrameCallback((_) => _spawnScenario());
  }

  void _handleAgentMessage(dynamic msg) {
    if (msg is AgentReady) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.commandPort = msg.commandPort;
        state.initialized = true;
        state.status = 'Ready';
        IsolateRouter.instance.register(msg.agentId, msg.commandPort);
        TraceLogger.instance.log('COORD', '${msg.agentId} ready');
      }
      setState(() {});

      // Deferred-start trio: once all ports are registered, release GLP init.
      final readyCount =
          _agents.values.where((a) => a.commandPort != null).length;
      if (readyCount >= _expectedAgentCount &&
          _allReadyCompleter != null &&
          !_allReadyCompleter!.isCompleted) {
        _allReadyCompleter!.complete();
      }
    } else if (msg is AgentOutput) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.outputLog.add(msg.line);
        // The maGLP isolate marks genuine _output/1 notifies with a leading
        // '< '; debug chatter ([INIT]…, > …) has no such marker. Strip the
        // transport marker so the runtime receives the bare ground term.
        final boundaryLine =
            msg.line.startsWith('< ') ? msg.line.substring(2) : msg.line;
        state.ui?.handleLine(boundaryLine);
        setState(() {});
        _scrollAgentToBottom(state);
      }
    } else if (msg is AgentLog) {
      TraceLogger.instance.log(msg.tag, msg.message);
    } else if (msg is AgentSendMad) {
      TraceLogger.instance.log(
          'COORD', 'ROUTING: ${msg.agentId} -> ${msg.to} (${msg.payload.length} bytes)');
      IsolateRouter.instance.route(msg.agentId, msg.to, msg.payload);
    } else if (msg is AgentStats) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.goalCount = msg.goals;
        state.heapVars = msg.heap;
        state.wpSize = msg.wp;
        state.mpSize = msg.mp;
        setState(() {});
      }
    } else if (msg is AgentError) {
      final state = _agents[msg.agentId];
      if (state != null) {
        state.status = 'Error: ${msg.error}';
        setState(() {});
      }
      TraceLogger.instance.log('COORD', 'ERROR from ${msg.agentId}: ${msg.error}');
    }
  }

  void _scrollAgentToBottom(AgentState agent) {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (agent.scrollController.hasClients) {
        agent.scrollController.animateTo(
          agent.scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 200),
          curve: Curves.easeOut,
        );
      }
    });
  }

  @override
  void dispose() {
    _replySubscription?.cancel();
    _replyPort.close();
    _glpPathController.dispose();
    for (final agent in _agents.values) {
      agent.dispose();
    }
    super.dispose();
  }

  // ===========================================================================
  // GLP SOURCE LOADING
  // ===========================================================================

  Future<void> _updateGlpPath() async {
    final newDir = _glpPathController.text.trim();
    if (newDir.isEmpty || !Directory(newDir).existsSync()) {
      setState(() {
        _log.add('ERROR: Directory not found: $newDir');
      });
      return;
    }

    try {
      final sources = <String>[];
      final paths = <String>[];
      for (final filename in _glpFiles) {
        final file = File('$newDir/$filename');
        if (!file.existsSync()) {
          setState(() {
            _log.add('ERROR: File not found: $newDir/$filename');
          });
          return;
        }
        sources.add(await file.readAsString());
        paths.add(file.absolute.path);
      }
      _cachedGlpSources = sources;
      _cachedGlpPaths = paths;

      setState(() {
        _currentGlpDir = newDir;
        final totalChars = sources.fold<int>(0, (sum, s) => sum + s.length);
        _log.add(
            'GLP loaded: ${_glpFiles.join(", ")} ($totalChars chars in ${sources.length} files)');
      });
    } catch (e) {
      setState(() {
        _log.add('ERROR reading files: $e');
      });
    }
  }

  // ===========================================================================
  // AGENT SPAWNING
  // ===========================================================================

  Future<void> _spawnAgent(String agentId, List<String> friends) async {
    if (_agents.containsKey(agentId)) {
      _log.add('$agentId already spawned');
      setState(() {});
      return;
    }

    // Load GLP source if not cached
    if (_cachedGlpSources == null) {
      await _updateGlpPath();
      if (_cachedGlpSources == null) {
        _log.add('ERROR: Could not load GLP source');
        setState(() {});
        return;
      }
    }

    final agentState = AgentState(agentId, friends);
    _agents[agentId] = agentState;
    setState(() {});

    final initMsg = InitAgent(
      agentId: agentId,
      glpSources: _cachedGlpSources!,
      glpSourcePaths: _cachedGlpPaths,
      rootSelfGlpPath: _rootSelfGlpPath,
      friends: friends,
      replyPort: _replyPort.sendPort,
    );

    try {
      await Isolate.spawn(agentIsolateEntry, initMsg);
      TraceLogger.instance.log('COORD', 'Spawned isolate for $agentId');
      _log.add('Spawned $agentId (friends=${friends.join(", ")})');
      setState(() {});
    } catch (e) {
      _log.add('ERROR spawning $agentId: $e');
      _agents.remove(agentId);
      agentState.dispose();
      setState(() {});
    }
  }

  Future<void> _spawnLinearTopology() async {
    // Linear: Alice↔Bob↔Charlie
    await _spawnAgent('Alice', ['Bob']);
    await _spawnAgent('Bob', ['Alice', 'Charlie']);
    await _spawnAgent('Charlie', ['Bob']);
  }

  /// Spawn the interactive trio (Alice/Bob/Charlie), each driven by the GSG
  /// manifest's two-surface UI.  Deferred start: spawn all, wait for every port
  /// to register, then release GLP init — so cold-call routing has no races.
  Future<void> _spawnLiveTrio() async {
    await _closeAll();

    if (_cachedGlpSources == null) {
      await _updateGlpPath();
      if (_cachedGlpSources == null) {
        _log.add('ERROR: Could not load GLP source');
        setState(() {});
        return;
      }
    }

    const trio = {
      'Alice': ['Bob'],
      'Bob': ['Alice', 'Charlie'],
      'Charlie': ['Bob'],
    };

    _expectedAgentCount = trio.length;
    _allReadyCompleter = Completer<void>();

    for (final entry in trio.entries) {
      final agentId = entry.key;
      final friends = entry.value;
      final agentState = AgentState(agentId, friends);
      agentState.ui = UiRuntime(
        manifest: gsgManifest,
        onSend: (text) => agentState.commandPort?.send(UserInput(text)),
      );
      agentState.ui!.onChange = () => setState(() {});
      _agents[agentId] = agentState;

      final initMsg = InitAgent(
        agentId: agentId,
        glpSources: _cachedGlpSources!,
        glpSourcePaths: _cachedGlpPaths,
        rootSelfGlpPath: _rootSelfGlpPath,
        friends: friends,
        replyPort: _replyPort.sendPort,
        deferStart: true,
      );

      try {
        await Isolate.spawn(agentIsolateEntry, initMsg);
        _log.add('Spawned $agentId (friends=${friends.join(", ")})');
      } catch (e) {
        _log.add('ERROR spawning $agentId: $e');
      }
    }
    setState(() {});

    await _allReadyCompleter!.future;

    for (final agent in _agents.values) {
      agent.commandPort?.send(StartAgent());
    }
    _log.add('Live trio started (manifest UI)');
    setState(() {});
  }

  /// Single-isolate scenario: ONE heap runs the real agent/4 + ui_mediator/5 for
  /// bob (live UI) plus alice/charlie (scenario actors) wired through an in-heap
  /// crossbar — no MAD, so it sidesteps the receive/3 `_w` bug. alice and charlie
  /// cold-call bob on launch, so bob's inbox shows two befriend cards; accepting
  /// each forms the friendship (connected both sides). Only bob has a surface.
  Future<void> _spawnScenario() async {
    await _closeAll();

    const scenarioFiles = [
      'self.glp',
      'typed_social_agent.glp',
      'typed_ui_mediator.glp',
      'play_scenario_boot.glp',
    ];
    final paths = [for (final f in scenarioFiles) '$_currentGlpDir/$f'];
    final sources = <String>[];
    for (final p in paths) {
      final file = File(p);
      if (!file.existsSync()) {
        _log.add('ERROR: scenario file not found: $p');
        setState(() {});
        return;
      }
      sources.add(await file.readAsString());
    }

    final bob = AgentState('Bob', const ['alice', 'charlie']);
    bob.ui = UiRuntime(
      manifest: gsgManifest,
      onSend: (text) => bob.commandPort?.send(UserInput(text)),
    );
    bob.ui!.onChange = () => setState(() {});
    _agents['Bob'] = bob;

    final initMsg = InitAgent(
      agentId: 'Bob',
      glpSources: sources,
      glpSourcePaths: paths,
      rootSelfGlpPath: _rootSelfGlpPath,
      friends: const ['alice', 'charlie'],
      replyPort: _replyPort.sendPort,
      // Single agent in the isolate: no inter-isolate routing race, so start now.
      deferStart: false,
    );

    try {
      await Isolate.spawn(agentIsolateEntry, initMsg);
      _log.add('Scenario started (single isolate; alice & charlie cold-call bob)');
    } catch (e) {
      _log.add('ERROR spawning scenario: $e');
    }
    setState(() {});
  }

  Future<void> _closeAll() async {
    // Kill REPL subprocess if running
    _playRunner?.kill();
    _playRunner = null;

    for (final agent in _agents.values) {
      if (agent.commandPort != null) {
        agent.commandPort!.send(DisposeAgent());
        IsolateRouter.instance.unregister(agent.agentId);
      }
      agent.dispose();
    }
    _agents.clear();
    _expectedAgentCount = 0;
    _allReadyCompleter = null;
    IsolateRouter.instance.clearLog();
    _log.add('Closed all agents');
    setState(() {});
  }

  // ===========================================================================
  // SIMULATED PLAYS (via ReplPlayRunner)
  // ===========================================================================

  ReplPlayRunner? _playRunner;

  /// Resolve the GLP repo root from the glp_multiagent working directory.
  /// The app may run from the repo (development) or from a bundle (release).
  static String _resolveRepoRoot() {
    // In development, cwd is glp_multiagent/ and repo root is ..
    // Check for the glp_runtime sibling directory as a landmark.
    final devRoot = Directory.current.parent.path;
    if (Directory('$devRoot/glp_runtime').existsSync()) {
      return devRoot;
    }
    // Fallback: try absolute path (Udi's machine)
    const fallback = '/Users/udi/Grassroots/GLP';
    if (Directory('$fallback/glp_runtime').existsSync()) {
      return fallback;
    }
    return devRoot; // best guess
  }

  Future<void> _runPlay(int playNumber) async {
    await _closeAll();

    // Create read-only agent panels (Alice, Bob, Charlie)
    for (final id in ['Alice', 'Bob', 'Charlie']) {
      final agent = AgentState(id, [], readOnly: true);
      agent.initialized = true;
      agent.status = 'Play $playNumber';
      _agents[id] = agent;
    }

    final repoRoot = _resolveRepoRoot();
    setState(() {
      _log.add('Starting fplay$playNumber (repo: $repoRoot)...');
    });

    final runner = ReplPlayRunner(repoRoot: repoRoot);
    _playRunner = runner;

    runner.onOutput = (output) {
      // Capitalize to match AgentState keys (Alice, Bob, Charlie)
      final key = output.agentId[0].toUpperCase() + output.agentId.substring(1);
      final state = _agents[key];
      if (state == null) return;

      final displayLine = output.kind == 'cmd' ? '> ${output.content}' : '< ${output.content}';
      state.outputLog.add(displayLine);
      setState(() {});
      _scrollAgentToBottom(state);
    };

    runner.onLog = (line) {
      TraceLogger.instance.log('REPL', line);
    };

    runner.onError = (error) {
      TraceLogger.instance.log('REPL-ERR', error);
      setState(() {
        _log.add('REPL ERROR: $error');
      });
    };

    runner.onDone = (exitCode) {
      _playRunner = null;
      setState(() {
        _log.add('fplay$playNumber finished (exit $exitCode)');
      });
    };

    await runner.run(playNumber);
  }

  // ===========================================================================
  // INPUT HANDLING
  // ===========================================================================

  void _sendInputToAgent(AgentState agent) {
    final text = agent.inputController.text.trim();
    if (text.isEmpty || agent.commandPort == null) return;
    agent.inputController.clear();
    agent.commandPort!.send(UserInput(text));
    agent.inputFocusNode.requestFocus();
  }

  // ===========================================================================
  // BUILD
  // ===========================================================================

  @override
  Widget build(BuildContext context) {
    // Clean §7.4 smartphone presentation: one device, Bob's two-surface UI.
    final bob = _agents['Bob'];
    return Scaffold(
      backgroundColor: const Color(0xFF2B2B33),
      body: Center(
        child: _PhoneFrame(
          child: (bob != null && bob.ui != null)
              ? AgentSurface(agentId: bob.agentId, runtime: bob.ui!)
              : const _Booting(),
        ),
      ),
    );
  }

  Widget _buildGlpPathBar() {
    return Container(
      padding: const EdgeInsets.all(8.0),
      color: Colors.orange.shade100,
      child: Row(
        children: [
          const Text('GLP dir: ', style: TextStyle(fontWeight: FontWeight.bold)),
          Expanded(
            child: TextField(
              controller: _glpPathController,
              style: const TextStyle(fontSize: 12, fontFamily: 'monospace'),
              decoration: const InputDecoration(
                isDense: true,
                contentPadding:
                    EdgeInsets.symmetric(horizontal: 8, vertical: 8),
                border: OutlineInputBorder(),
              ),
              onSubmitted: (_) => _updateGlpPath(),
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton(
            onPressed: _updateGlpPath,
            child: const Text('Set'),
          ),
        ],
      ),
    );
  }

  Widget _buildControlBar() {
    return Container(
      padding: const EdgeInsets.all(16.0),
      color: Colors.orange.shade50,
      child: Row(
        children: [
          ElevatedButton.icon(
            onPressed: _spawnLiveTrio,
            icon: const Icon(Icons.people),
            label: const Text('Live Trio'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.deepOrange,
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: _spawnScenario,
            icon: const Icon(Icons.auto_awesome),
            label: const Text('Scenario'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.indigo,
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: _closeAll,
            icon: const Icon(Icons.stop),
            label: const Text('Close All'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.grey,
            ),
          ),
          const SizedBox(width: 16),
          ElevatedButton.icon(
            onPressed: () => _runPlay(1),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 1'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.green,
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(2),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 2'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.green,
            ),
          ),
          const SizedBox(width: 8),
          ElevatedButton.icon(
            onPressed: () => _runPlay(3),
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play 3'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.green,
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildAgentPanel(AgentState agent) {
    // Manifest-driven agents render the generic two-surface UI.
    if (agent.ui != null) {
      return Container(
        decoration: BoxDecoration(
          border: Border(right: BorderSide(color: Colors.grey.shade300)),
        ),
        child: Column(
          children: [
            Expanded(
              child: AgentSurface(agentId: agent.agentId, runtime: agent.ui!),
            ),
            _agentStatusBar(agent),
          ],
        ),
      );
    }
    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(color: Colors.grey.shade300),
        ),
      ),
      child: Column(
        children: [
          // Agent header
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 6.0),
            color: Colors.orange,
            child: Row(
              children: [
                Text(
                  '${agent.agentId} (${agent.friends.join(", ")})',
                  style: const TextStyle(
                    color: Colors.white,
                    fontWeight: FontWeight.bold,
                    fontSize: 13,
                  ),
                ),
              ],
            ),
          ),
          // Output log
          Expanded(
            child: Container(
              color: Colors.grey.shade100,
              child: ListView.builder(
                controller: agent.scrollController,
                padding: const EdgeInsets.all(8.0),
                itemCount: agent.outputLog.length,
                itemBuilder: (context, index) {
                  final line = agent.outputLog[index];
                  return Padding(
                    padding: const EdgeInsets.symmetric(vertical: 2.0),
                    child: Text(
                      line,
                      style: TextStyle(
                        fontFamily: 'monospace',
                        fontSize: 13,
                        color: _outputLineColor(line),
                        fontWeight: _outputLineBold(line)
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                    ),
                  );
                },
              ),
            ),
          ),
          // Input area (hidden for read-only play panels)
          if (!agent.readOnly)
            Container(
              padding: const EdgeInsets.all(8.0),
              color: Colors.orange.shade50,
              child: Row(
                children: [
                  Expanded(
                    child: TextField(
                      controller: agent.inputController,
                      focusNode: agent.inputFocusNode,
                      enabled: agent.initialized,
                      decoration: InputDecoration(
                        hintText: agent.initialized
                            ? 'connect ${agent.friends.isNotEmpty ? agent.friends.first.toLowerCase() : "friend"}'
                            : 'Initializing...',
                        border: OutlineInputBorder(
                          borderRadius: BorderRadius.circular(8),
                        ),
                        contentPadding: const EdgeInsets.symmetric(
                          horizontal: 12,
                          vertical: 8,
                        ),
                      ),
                      onSubmitted: (_) => _sendInputToAgent(agent),
                    ),
                  ),
                  const SizedBox(width: 8),
                  ElevatedButton(
                    onPressed:
                        agent.initialized ? () => _sendInputToAgent(agent) : null,
                    child: const Text('Send'),
                  ),
                ],
              ),
            ),
          // Status bar
          Container(
            padding:
                const EdgeInsets.symmetric(horizontal: 8.0, vertical: 8.0),
            color: Colors.orange.shade100,
            child: Row(
              children: [
                Text(
                  'G:${agent.goalCount} H:${agent.heapVars} W:${agent.wpSize} M:${agent.mpSize}',
                  style: const TextStyle(
                      fontWeight: FontWeight.w500, fontSize: 10),
                ),
                const SizedBox(width: 8),
                Container(
                  width: 8,
                  height: 8,
                  decoration: BoxDecoration(
                    shape: BoxShape.circle,
                    color: agent.initialized ? Colors.orange : Colors.grey,
                  ),
                ),
                const SizedBox(width: 4),
                Expanded(
                  child: Text(
                    agent.status,
                    style: const TextStyle(fontSize: 10),
                    overflow: TextOverflow.ellipsis,
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _agentStatusBar(AgentState agent) {
    return Container(
      padding: const EdgeInsets.symmetric(horizontal: 8.0, vertical: 8.0),
      color: Colors.orange.shade100,
      child: Row(
        children: [
          Text(
            'G:${agent.goalCount} H:${agent.heapVars} W:${agent.wpSize} M:${agent.mpSize}',
            style: const TextStyle(fontWeight: FontWeight.w500, fontSize: 10),
          ),
          const SizedBox(width: 8),
          Container(
            width: 8,
            height: 8,
            decoration: BoxDecoration(
              shape: BoxShape.circle,
              color: agent.initialized ? Colors.orange : Colors.grey,
            ),
          ),
          const SizedBox(width: 4),
          Expanded(
            child: Text(
              agent.status,
              style: const TextStyle(fontSize: 10),
              overflow: TextOverflow.ellipsis,
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildRoutingLog() {
    final log = IsolateRouter.instance.routingLog;
    return Container(
      height: 100,
      color: Colors.grey.shade100,
      child: ListView.builder(
        padding: const EdgeInsets.all(8.0),
        itemCount: log.length,
        itemBuilder: (context, index) {
          return Text(
            log[index],
            style: const TextStyle(fontFamily: 'monospace', fontSize: 12),
          );
        },
      ),
    );
  }

  Widget _buildCoordinatorLog() {
    return Container(
      height: 56,
      color: Colors.orange.shade50,
      child: ListView.builder(
        padding: const EdgeInsets.all(8.0),
        itemCount: _log.length,
        itemBuilder: (context, index) {
          return Text(
            _log[index],
            style: const TextStyle(fontSize: 11),
          );
        },
      ),
    );
  }

  // ===========================================================================
  // OUTPUT LINE STYLING (same as previous AgentScreen)
  // ===========================================================================

  Color _outputLineColor(String line) {
    if (line.startsWith('>')) return Colors.blue.shade800;
    if (line.startsWith('<')) return Colors.green.shade800;
    if (line.startsWith('[SEND') || line.startsWith('[IRMA SEND')) {
      return Colors.purple.shade800;
    }
    if (line.startsWith('[RECV') || line.startsWith('[IRMA RECV')) {
      return Colors.teal.shade800;
    }
    if (line.startsWith('[IRMA')) return Colors.deepPurple.shade800;
    if (line.startsWith('[')) return Colors.orange.shade800;
    return Colors.black87;
  }

  bool _outputLineBold(String line) {
    return line.startsWith('[') || line.startsWith('<');
  }
}

// =============================================================================
// Clean smartphone presentation (paper §7.4)
// =============================================================================

/// A phone-shaped bezel that frames the agent's two-surface UI.
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
        color: Colors.orange,
        padding: const EdgeInsets.symmetric(horizontal: 16),
        child: Row(
          mainAxisAlignment: MainAxisAlignment.spaceBetween,
          children: const [
            Text('grassroots',
                style: TextStyle(
                    color: Colors.white,
                    fontSize: 12,
                    fontWeight: FontWeight.w600)),
            Row(children: [
              Icon(Icons.wifi, color: Colors.white, size: 14),
              SizedBox(width: 6),
              Icon(Icons.battery_full, color: Colors.white, size: 14),
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
            CircularProgressIndicator(color: Colors.orange),
            SizedBox(height: 16),
            Text('Starting…', style: TextStyle(color: Colors.grey)),
          ],
        ),
      );
}
